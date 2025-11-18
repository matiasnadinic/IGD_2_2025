## =============================================================================
## SCRIPT MAESTRO FINAL: MICROSIMULACIÓN + CLUSTERING + ANÁLISIS ESTADÍSTICO
## =============================================================================

## -----------------------------------------------------------------------------
## 0. LIBRERÍAS Y CONFIGURACIÓN
## -----------------------------------------------------------------------------
message("--- (0/8) CARGANDO LIBRERÍAS ---")

library(rakeR)       # Microsimulación
library(RPostgres)   # Conexión BD
library(DBI)         # Manejo BD
library(sf)          # Mapas
library(dplyr)       # Datos
library(tidyr)       # Transformación de datos (para boxplots)
library(ggplot2)     # Gráficos
library(factoextra)  # Clusters
library(cluster)     # K-Means
library(stringr)     # Texto

# Semilla para resultados replicables
set.seed(123)

# Tema Gráfico Unificado
tema_pro = theme_minimal() +
  theme(
    plot.title = element_text(face="bold", size=14, hjust=0.5),
    plot.subtitle = element_text(size=10, color="grey40", hjust=0.5),
    panel.grid.major = element_line(color="grey90", linetype="dashed"),
    legend.position = "bottom"
  )


## -----------------------------------------------------------------------------
## 1. MICROSIMULACIÓN (GENERACIÓN DE DATOS)
## -----------------------------------------------------------------------------
message("--- (1/8) EJECUTANDO MICROSIMULACIÓN ---")

# 1.1 Carga
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"
casen = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

# 1.2 Pre-procesamiento
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

# Selección y Limpieza CASEN
casen = casen[, c("estrato", "esc", "edad", "sexo", "e6a", "e1")]
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$e1 = as.numeric(unclass(casen$e1))

# Variable Analfabetismo
casen$no_lee_escribe = ifelse(is.na(casen$e1), 0, ifelse(casen$e1 == 1, 0, 1))
casen$e1 = NULL

# Imputación Escolaridad
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
casen$ID = as.character(seq_len(nrow(casen)))

# Recodificación
casen$edad_cat = cut(casen$edad, breaks = c(0,30,40,50,60,70,80,Inf), labels = age_levels, right = FALSE, include.lowest = TRUE)
casen$esc_cat = factor(with(casen, ifelse(esc == 0, esc_levels[1], ifelse(esc <= 8, esc_levels[2], ifelse(esc <= 12, esc_levels[3], esc_levels[4])))), levels = esc_levels)
casen$sexo_cat = factor(ifelse(casen$sexo == 2, sexo_levels[1], ifelse(casen$sexo == 1, sexo_levels[2], NA)), levels = sexo_levels)

# Algoritmo rakeR
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)
comunas_comunes = intersect(names(cons_censo_comunas), names(inds_list))

sim_list = lapply(comunas_comunes, function(zona) {
  cons_i = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  tmp = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  w_frac = weight(cons = cons_i, inds = inds_i, vars = c("Edad","Escolaridad","Sexo"))
  sim_i  = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i, tmp[, c("ID","no_lee_escribe")], by = "ID", all.x = TRUE)
})
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")


## -----------------------------------------------------------------------------
## 2. CÁLCULO DE TASAS (AGREGACIÓN)
## -----------------------------------------------------------------------------
message("--- (2/8) CALCULANDO TASAS POR ZONA ---")
df_tasas = sim_df %>%
  group_by(zone) %>%
  summarise(n_total = n(), n_no_lee = sum(no_lee_escribe, na.rm = TRUE)) %>%
  mutate(tasa_analfabetismo = n_no_lee / n_total)
df_tasas$zone = trimws(as.character(df_tasas$zone))


## -----------------------------------------------------------------------------
## 3. CONEXIÓN Y UNIÓN DE DATOS (MASTER DATAFRAME)
## -----------------------------------------------------------------------------
message("--- (3/8) CONECTANDO A BD Y UNIENDO VARIABLES ---")

con = dbConnect(RPostgres::Postgres(), dbname="censo_rm_2017", host="localhost", port=5432, user="postgres", password="postgres")

# Geometría
query_geo = "SELECT geocodigo, geom FROM dpa.zonas_censales_rm"
zonas_sf = st_read(con, query = query_geo, quiet = TRUE)
zonas_sf$geocodigo = trimws(as.character(zonas_sf$geocodigo))

# Datos Reales (Migración/Educación)
sql_indicadores = "
SELECT z.geocodigo, c.nom_comuna,
  ROUND(COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0 / NULLIF(COUNT(*), 0), 2) AS ptje_migrantes,
  ROUND(COUNT(*) FILTER (WHERE p.p13 = 3) * 100.0 / NULLIF(COUNT(*) FILTER (WHERE p.p13 IN (1, 2, 3)), 0), 2) AS ptje_nunca_asistio
FROM public.personas p
JOIN public.hogares h ON p.hogar_ref_id = h.hogar_ref_id
JOIN public.viviendas v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN public.comunas c ON z.codigo_comuna = c.codigo_comuna
GROUP BY z.geocodigo, c.nom_comuna;"
df_censo_real = dbGetQuery(con, sql_indicadores)
df_censo_real$geocodigo = trimws(as.character(df_censo_real$geocodigo))

# Filtro Gran Santiago y Unión Final
comunas_gs = c("SANTIAGO", "CERRILLOS", "CERRO NAVIA", "CONCHALI", "EL BOSQUE", "ESTACION CENTRAL", "HUECHURABA", "INDEPENDENCIA", "LA CISTERNA", "LA FLORIDA", "LA GRANJA", "LA PINTANA", "LA REINA", "LAS CONDES", "LO BARNECHEA", "LO ESPEJO", "LO PRADO", "MACUL", "MAIPU", "ÑUÑOA", "PEDRO AGUIRRE CERDA", "PEÑALOLEN", "PROVIDENCIA", "PUDAHUEL", "QUILICURA", "QUINTA NORMAL", "RECOLETA", "RENCA", "SAN JOAQUIN", "SAN MIGUEL", "SAN RAMON", "VITACURA", "PUENTE ALTO", "SAN BERNARDO")

sf_master = zonas_sf %>%
  dplyr::inner_join(df_censo_real, by = "geocodigo") %>%
  dplyr::inner_join(df_tasas, by = c("geocodigo" = "zone")) %>%
  filter(trimws(nom_comuna) %in% comunas_gs) %>%
  na.omit()

# Bordes para mapas
sf_bordes = sf_master %>% group_by(nom_comuna) %>% summarise()


## -----------------------------------------------------------------------------
## 4. MAPA 1: TASA DE ANALFABETISMO
## -----------------------------------------------------------------------------
message("--- (4/8) GENERANDO MAPA DE ANALFABETISMO ---")

mapa_1 = ggplot() +
  geom_sf(data=sf_master, aes(fill=tasa_analfabetismo), lwd=0, color=NA) +
  geom_sf(data=sf_bordes, fill=NA, color="white", lwd=0.3) +
  geom_sf_text(data=sf_bordes, aes(label=nom_comuna), size=2.5, color="white", fontface="bold", check_overlap=TRUE) +
  scale_fill_viridis_c(option="magma", direction=-1, labels=scales::percent, name="Tasa") +
  labs(title="1. Tasa de Analfabetismo Estimada", subtitle="Microsimulación") + tema_pro
print(mapa_1)


## -----------------------------------------------------------------------------
## 5. EJECUCIÓN DE K-MEANS
## -----------------------------------------------------------------------------
message("--- (5/8) EJECUTANDO CLUSTERING ---")

# Datos numéricos
datos_cluster = sf_master %>% st_drop_geometry() %>% select(tasa_analfabetismo, ptje_migrantes, ptje_nunca_asistio)
datos_scaled  = scale(datos_cluster)

# Método del Codo
print(fviz_nbclust(datos_scaled, kmeans, method="wss") + labs(title="Método del Codo"))
# [Image of elbow method graph]


# K-Means (k=4)
set.seed(123)
km = kmeans(datos_scaled, centers=4, nstart=25)
sf_master$cluster = as.factor(km$cluster)


## -----------------------------------------------------------------------------
## 6. ANÁLISIS ESTADÍSTICO DE PERFILES (¡NUEVO!)
## -----------------------------------------------------------------------------
message("--- (6/8) DIAGNÓSTICO ESTADÍSTICO (POR QUÉ ES CADA CLUSTER) ---")

# 6.1 Tabla Resumen (Promedios Reales)
resumen_stats = sf_master %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    Zonas = n(),
    `Media Analfabetismo` = round(mean(tasa_analfabetismo)*100, 2), # En porcentaje
    `Media Migración (%)` = round(mean(ptje_migrantes), 2),
    `Media Rezago Educ (%)` = round(mean(ptje_nunca_asistio), 2)
  )
print("--- TABLA DE PROMEDIOS POR CLUSTER ---")
print(resumen_stats)

# 6.2 Gráfico de Dispersión (Scatter Plot)
plot_dispersim = ggplot(sf_master, aes(x = ptje_migrantes, y = tasa_analfabetismo, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.2) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Dispersión: Migración vs Analfabetismo",
       subtitle = "Visualización de la separación de grupos en 2 dimensiones",
       x = "% Migrantes (Censo)", y = "Tasa Analfabetismo (Simulado)") +
  tema_pro
print(plot_dispersim)

# 6.3 Gráfico de Cajas (Boxplots)
# Transformamos a formato largo para graficar la distribución
datos_largos = sf_master %>%
  st_drop_geometry() %>%
  select(cluster, tasa_analfabetismo, ptje_migrantes, ptje_nunca_asistio) %>%
  pivot_longer(cols = -cluster, names_to = "Variable", values_to = "Valor")

boxplots = ggplot(datos_largos, aes(x=cluster, y=Valor, fill=cluster)) +
  geom_boxplot(alpha=0.7, outlier.size=0.5) +
  facet_wrap(~Variable, scales="free_y", labeller = labeller(Variable = c(
    tasa_analfabetismo = "Tasa Analfabetismo (0-1)",
    ptje_migrantes = "% Migrantes (0-100)",
    ptje_nunca_asistio = "% Nunca Asistió (0-100)"
  ))) +
  scale_fill_brewer(palette="Set1") +
  labs(title="Distribución de Variables por Clúster", 
       subtitle="Evidencia estadística para la caracterización de grupos") +
  tema_pro
print(boxplots)


## -----------------------------------------------------------------------------
## 7. MAPA 2: CLUSTERS PERFILADOS (ETIQUETAS AUTOMÁTICAS)
## -----------------------------------------------------------------------------
message("--- (7/8) GENERANDO MAPA DE CLUSTERS ---")

# Generación de Etiquetas Inteligentes
g_analf = mean(sf_master$tasa_analfabetismo); sd_analf = sd(sf_master$tasa_analfabetismo)
g_migra = mean(sf_master$ptje_migrantes);     sd_migra = sd(sf_master$ptje_migrantes)
g_educ  = mean(sf_master$ptje_nunca_asistio); sd_educ  = sd(sf_master$ptje_nunca_asistio)

perfiles = sf_master %>% st_drop_geometry() %>% group_by(cluster) %>%
  summarise(m_analf=mean(tasa_analfabetismo), m_migra=mean(ptje_migrantes), m_educ=mean(ptje_nunca_asistio)) %>%
  mutate(
    txt_migra = case_when(m_migra > g_migra + sd_migra ~ "Migr. MUY ALTA", m_migra > g_migra ~ "Migr. Alta", TRUE ~ "Migr. Baja"),
    txt_analf = case_when(m_analf > g_analf + sd_analf ~ "Analf. CRÍTICO", m_analf > g_analf ~ "Analf. Alto", TRUE ~ "Analf. Bajo"),
    txt_educ  = case_when(m_educ > g_educ + sd_educ ~ "(Rezago Esc.)", TRUE ~ "")
  ) %>%
  mutate(label_final = paste0("C", cluster, ": ", txt_migra, " / ", txt_analf, " ", txt_educ))

sf_master = left_join(sf_master, perfiles, by="cluster")

mapa_2 = ggplot() +
  geom_sf(data=sf_master, aes(fill=label_final), lwd=0, color=NA) +
  geom_sf(data=sf_bordes, fill=NA, color="white", lwd=0.3) +
  geom_sf_text(data=sf_bordes, aes(label=nom_comuna), size=2.5, color="black", 
               fontface="bold", check_overlap=TRUE, bg.color="white", bg.r=0.1) +
  scale_fill_brewer(palette="Set1", name="Perfil") +
  labs(title="2. Clusters Socioeconómicos", subtitle="K-Means (k=4)") + tema_pro
print(mapa_2)


## -----------------------------------------------------------------------------
## 8. MAPA 3: VARIABILIDAD (SHANNON)
## -----------------------------------------------------------------------------
message("--- (8/8) GENERANDO MAPA DE SHANNON ---")

df_shannon = sf_master %>% st_drop_geometry() %>%
  group_by(nom_comuna, cluster) %>% summarise(n=n(), .groups='drop') %>%
  group_by(nom_comuna) %>% mutate(prop=n/sum(n)) %>%
  summarise(indice_shannon = -sum(prop * log(prop))) %>% ungroup()

sf_shannon = sf_bordes %>% left_join(df_shannon, by="nom_comuna")

mapa_3 = ggplot() +
  geom_sf(data=sf_shannon, aes(fill=indice_shannon), color="white", lwd=0.5) +
  geom_sf_text(data=sf_shannon, aes(label=nom_comuna), size=2.5, color="black", check_overlap=TRUE) +
  scale_fill_viridis_c(option="plasma", name="Índice H") +
  labs(title="3. Variabilidad Intra-Comunal", subtitle="Claro = Heterogéneo | Oscuro = Segregado") + tema_pro
print(mapa_3)

message("--- PROCESO FINALIZADO ---")