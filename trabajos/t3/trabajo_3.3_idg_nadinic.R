## --------------------------------------------------------
## 0. LIBRERÍAS (TODAS)
## --------------------------------------------------------
# Para Microsimulación
library(rakeR)
library(MASS)      # Para polr() (Imputación)
library(stringr)   # Para limpiar códigos (str_trim, str_pad)
library(dplyr)
library(biscale)   # Para mapas bivariados (opcional)

# Para Conexión BD y Geometrías
library(RPostgres)
library(DBI)
library(sf)

# Para Clustering y Visualización
library(factoextra) # Para fviz_nbclust
library(ggplot2)
library(GGally)     # Para ggpairs
library(viridis)    # Para mapa de Shannon

# -----------------------------------------------------------------------------
# PARTE 1: EJECUTAR LA MICROSIMULACIÓN (CORREGIDA PARA data.frame)
# -----------------------------------------------------------------------------
# El objetivo es generar el dataframe 'df_zonas_simulado'

cat("--- INICIANDO PARTE 1: MICROSIMULACIÓN ---\n")

# --- 1.1: Entradas ---
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"
cons_censo_df = readRDS(ruta_censo)

# --- 1.2: Preparación Censo (Categorías) ---
col_cons = sort(setdiff(names(cons_censo_df),c("GEOCODIGO","COMUNA")))
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esc", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

# --- 1.3: Preparación CASEN (Consolidado) ---
PONDERADOR_CASEN <- "expr"
vars_micro = c("estrato", "esc", "edad", "sexo", "e6a", "e1", PONDERADOR_CASEN)
casen_preparado_raw <- readRDS(ruta_casen)[, vars_micro, drop = FALSE]

# --- !! CORRECCIÓN 1: Forzar a data.frame (NO tibble) !! ---
casen_preparado <- as.data.frame(casen_preparado_raw)
# --- !! FIN DE LA CORRECCIÓN !! ---

casen_preparado$comuna <- substr(as.character(casen_preparado$estrato), 1, 5)
casen_preparado$estrato <- NULL
casen_preparado$esc <- as.integer(unclass(casen_preparado$esc))
casen_preparado$edad <- as.integer(unclass(casen_preparado$edad))
casen_preparado$e6a <- as.numeric(unclass(casen_preparado$e6a))
casen_preparado$sexo <- as.integer(unclass(casen_preparado$sexo))
casen_preparado$e1 <- as.numeric(unclass(casen_preparado$e1))
# Renombramos 'ponderador' a 'weight' que es lo que rakeR espera
casen_preparado$weight <- as.numeric(unclass(casen_preparado[[PONDERADOR_CASEN]]))
casen_preparado[[PONDERADOR_CASEN]] <- NULL # Eliminar la columna original

# Imputación 'esc'
casos_completos_esc <- !is.na(casen_preparado$esc) & !is.na(casen_preparado$e6a)
modelo_glm <- glm(esc ~ e6a, data = casen_preparado[casos_completos_esc, ], family = poisson(link = "log"))
filas_para_imputar_esc <- is.na(casen_preparado$esc) & !is.na(casen_preparado$e6a)
predicciones_glm <- predict(modelo_glm, newdata = casen_preparado[filas_para_imputar_esc, ], type = "response")
casen_preparado$esc[filas_para_imputar_esc] <- round(predicciones_glm)

# Imputación 'e1'
casen_preparado$e1 <- factor(casen_preparado$e1, levels = 1:4, ordered = TRUE)
casos_completos_e1 <- !is.na(casen_preparado$e1) & !is.na(casen_preparado$esc)
modelo_imputacion_e1 <- polr(e1 ~ esc, data = casen_preparado[casos_completos_e1, ])
filas_para_imputar_e1 <- is.na(casen_preparado$e1) & !is.na(casen_preparado$esc)
predicciones_e1 <- predict(modelo_imputacion_e1, newdata = casen_preparado[filas_para_imputar_e1, ])
casen_preparado$e1[filas_para_imputar_e1] <- predicciones_e1

# Limpieza antes de categorizar
casen_preparado <- casen_preparado[
  !is.na(casen_preparado$weight) & !is.na(casen_preparado$esc) & 
    !is.na(casen_preparado$edad) & !is.na(casen_preparado$sexo), 
]
casen_preparado$e6a <- NULL

# Crear Categorías Homologadas
casen_preparado$sexo_cat <- ifelse(casen_preparado$sexo == 1, "sexo_m", "sexo_f")
casen_preparado$edad_cat <- cut(casen_preparado$edad,
                                breaks = c(-Inf, 30, 40, 50, 60, 70, 80, Inf),
                                labels = age_levels, right = TRUE)
casen_preparado$esc_cat <- cut(casen_preparado$esc,
                               breaks = c(-Inf, 0, 8, 12, Inf),
                               labels = esc_levels, right = TRUE)

# --- Limpieza Final ---
# Filtramos NAs creados por 'cut'
casen_preparado <- casen_preparado[
  !is.na(casen_preparado$sexo_cat) & !is.na(casen_preparado$edad_cat) & !is.na(casen_preparado$esc_cat),
]
# Convertimos a 'character' (texto). Esto es correcto para rakeR.
casen_preparado$sexo_cat <- as.character(casen_preparado$sexo_cat)
casen_preparado$edad_cat <- as.character(casen_preparado$edad_cat)
casen_preparado$esc_cat <- as.character(casen_preparado$esc_cat)


# --- 1.4: Preparación Censo (Raking) ---
vars_raking <- c("GEOCODIGO", age_levels, esc_levels, sexo_levels)
cons_censo_input_raw <- cons_censo_df[, vars_raking]
# --- !! CORRECCIÓN 2: Forzar a data.frame (NO tibble) !! ---
cons_censo_input <- as.data.frame(cons_censo_input_raw)
# --- !! FIN DE LA CORRECCIÓN !! ---


# --- 1.5: Ejecutar Microsimulación (RakeR) ---
cat("Ejecutando Microsimulación (RakeR)...\n")

# El orden alfabético es correcto (edad, esc, sexo)
control_vars <- c("edad_cat", "esc_cat", "sexo_cat")

# Forzar GEOCODIGO a character (texto) es correcto
cons_censo_input$GEOCODIGO <- as.character(cons_censo_input$GEOCODIGO)

# Creamos el DF 'limpio' para weight()
vars_para_weight <- c(control_vars, "weight")
casen_limpio_para_weight <- casen_preparado[, vars_para_weight]

# 1. Pasamos el DF 'limpio' a weight()
pesos_simulados_matriz <- rakeR::weight(
  cons = cons_censo_input,
  inds = casen_limpio_para_weight, # <-- USAMOS EL DF LIMPIO
  vars = control_vars
)

cat("Matriz de pesos creada. Ejecutando extract...\n")

# 2. Pasamos el DF 'completo' a extract()
poblacion_sintetica_df <- rakeR::extract(
  weights = pesos_simulados_matriz,
  inds = casen_preparado, # <-- USAMOS EL DF COMPLETO
  id = "GEOCODIGO"
)

# --- 1.6: Agregar Resultados Simulados (SECCIÓN 8) ---
cat("Agregando resultados simulados por Zona...\n")
df_zonas_simulado <- poblacion_sintetica_df %>%
  filter(edad >= 15) %>% 
  group_by(GEOCODIGO) %>%
  summarise(
    # Variable Simulada 1
    esc_promedio_sim = mean(esc, na.rm = TRUE),
    
    # Variable Simulada 2 (Usando tu variable 'e1' imputada)
    poblacion_objetivo_sim = n(),
    total_alfabetizados_sim = sum(e1 == "1", na.rm = TRUE), # Asumiendo 1 = Sabe leer
    tasa_alfabetizacion_sim = (total_alfabetizados_sim / poblacion_objetivo_sim) * 100
  ) %>%
  ungroup()

cat("--- FIN PARTE 1: 'df_zonas_simulado' CREADO ---\n")
print(head(df_zonas_simulado))

# 2. Pasamos el DF 'completo' a extract()
poblacion_sintetica_df <- rakeR::extract(
  weights = pesos_simulados_matriz,
  inds = casen_preparado, # <-- USAMOS EL DF COMPLETO
  id = "GEOCODIGO"
)

# --- 1.6: Agregar Resultados Simulados (SECCIÓN 8) ---
cat("Agregando resultados simulados por Zona...\n")
df_zonas_simulado <- poblacion_sintetica_df %>%
  filter(edad >= 15) %>% 
  group_by(GEOCODIGO) %>%
  summarise(
    # Variable Simulada 1
    esc_promedio_sim = mean(esc, na.rm = TRUE),
    
    # Variable Simulada 2 (Usando tu variable 'e1' imputada)
    poblacion_objetivo_sim = n(),
    total_alfabetizados_sim = sum(e1 == "1", na.rm = TRUE), # Asumiendo 1 = Sabe leer
    tasa_alfabetizacion_sim = (total_alfabetizados_sim / poblacion_objetivo_sim) * 100
  ) %>%
  ungroup()

cat("--- FIN PARTE 1: 'df_zonas_simulado' CREADO ---\n")
print(head(df_zonas_simulado))


# -----------------------------------------------------------------------------
# PARTE 2: CARGAR DATOS ADICIONALES PARA CLUSTERING
# (Geometrías, Ingreso y Migrantes)
# -----------------------------------------------------------------------------
cat("\n--- INICIANDO PARTE 2: CARGA DE DATOS PARA CLÚSTER ---\n")

# --- 2.1: Cargar Geometría y Mediana de Ingreso (del script de clúster) ---
zonas_gs_ingreso = st_read("data/zonas_gs_ingreso.geojson")

# --- 2.2: Cargar Conexión a BD (del script de clúster) ---
con = dbConnect(
  RPostgres::Postgres(),
  dbname    = "censo_rm_2017",
  host      = "localhost",
  port      = 5432,
  user      = "postgres",
  password  = "postgres"
)

# --- 2.3: Consulta SQL para Migrantes y Alfabetismo (p13) ---
sql_indicadores = "
SELECT
  z.geocodigo AS geocodigo,
  c.nom_comuna,
  
  -- Variable 3: Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,
  
  -- Variable 4: Porcentaje 'Nunca Asistió' (p13 = 3)
  ROUND(
    COUNT(*) FILTER (WHERE p.p13 = 3) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p13 IN (1, 2, 3)), 0),
  2) AS ptje_nunca_asistio

FROM public.personas    AS p
JOIN public.hogares     AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas   AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas       AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas     AS c ON z.codigo_comuna   = c.codigo_comuna
GROUP BY z.geocodigo, c.nom_comuna;
"
df_indicadores = dbGetQuery(con, sql_indicadores)
dbDisconnect(con)
cat("Datos de Ingreso, Migrantes y P13 cargados.\n")


# -----------------------------------------------------------------------------
# PARTE 3: EL GRAN MERGE (Unir todo)
# -----------------------------------------------------------------------------
cat("\n--- INICIANDO PARTE 3: UNIENDO TODAS LAS FUENTES ---\n")

# 1. Unimos GeoJSON (ingreso, geom) con SQL (migrantes, p13)
zonas_cluster_df <- merge(
  zonas_gs_ingreso,
  df_indicadores[, c('geocodigo', 'nom_comuna', 'ptje_migrantes', 'ptje_nunca_asistio')], 
  by = "geocodigo",
  all.x = TRUE
)

# 2. Limpiamos GEOCODIGO de los datos simulados para el merge
df_zonas_simulado$geocodigo <- as.character(df_zonas_simulado$GEOCODIGO)

# 3. Unimos el resultado anterior con los datos SIMULADOS
sf_master_cluster <- merge(
  zonas_cluster_df,
  df_zonas_simulado[, c('geocodigo', 'esc_promedio_sim', 'tasa_alfabetizacion_sim')],
  by = "geocodigo",
  all.x = TRUE
)

cat("Merge completado. Dataframe 'sf_master_cluster' listo.\n")


# -----------------------------------------------------------------------------
# PARTE 4: EJECUTAR EL CLUSTERING (K-MEANS)
# -----------------------------------------------------------------------------
cat("\n--- INICIANDO PARTE 4: ANÁLISIS DE CLÚSTER (K-MEANS) ---\n")

# --- 4.1: Seleccionar variables y escalar ---
vars_para_cluster <- c(
  'mediana_ingreso',
  'ptje_migrantes',
  'esc_promedio_sim',
  'tasa_alfabetizacion_sim'
)

vars_clusters <- st_drop_geometry(sf_master_cluster[, vars_para_cluster])
vars_clusters <- na.omit(vars_clusters)
vars_scaled <- scale(vars_clusters)

# --- 4.2: Método del codo (Re-evaluar K) ---
cat("Calculando Método del Codo (revisar K=3 o K=4)...\n")
print(
  fviz_nbclust(vars_scaled, kmeans, method = "wss") +
    labs(title = "Método del Codo (Nuevas Variables)", x = "Número de clusters (k)", y = "WSS")
)

# --- 4.3: Ejecutar kmeans ---
set.seed(123)
k <- 3 # <-- AJUSTA ESTE VALOR SEGÚN EL CODO
km <- kmeans(vars_scaled, centers = k, nstart = 25)

# --- 4.4: Asignar Clústeres (Manejo de NAs) ---
idx_completos <- complete.cases(
  st_drop_geometry(sf_master_cluster[, vars_para_cluster])
)
sf_master_cluster$cluster <- NA
sf_master_cluster$cluster[idx_completos] <- km$cluster
sf_master_cluster$cluster <- as.factor(sf_master_cluster$cluster)

# --- 4.5: Análisis Estadístico (Nuevos Perfiles) ---
vars_clusters_completos <- vars_clusters
vars_clusters_completos$cluster <- as.factor(km$cluster)

cluster_summary <- aggregate(. ~ cluster, data = vars_clusters_completos, FUN = mean)

cat("\n--- NUEVO PERFIL DE CLÚSTERES (Medias) ---")
print(cluster_summary)
# ¡DEBES ACTUALIZAR TUS ETIQUETAS DE LEYENDA BASADO EN ESTA TABLA!


# -----------------------------------------------------------------------------
# PARTE 5: VISUALIZACIÓN FINAL (Mapas)
# -----------------------------------------------------------------------------
cat("\n--- INICIANDO PARTE 5: GENERANDO MAPAS FINALES ---\n")

# --- 5.1: Cargar Geometría de Comunas (Para bordes y mapa Shannon) ---
con <- dbConnect(RPostgres::Postgres(), dbname = "censo_rm_2017", host = "localhost", port = 5432, user = "postgres", password = "postgres")
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago = st_read(con, query = sql_comunas)
dbDisconnect(con)

# Filtramos el mapa de clústeres a la Provincia de Santiago
sf_mapa_final <- sf_master_cluster %>%
  filter(nom_provin == "SANTIAGO") 
bbox <- st_bbox(sf_mapa_final)

# --- 5.2: Mapa de Clústeres (Basado en nuevas variables) ---
# !! IMPORTANTE: Ajusta estas etiquetas según tu 'cluster_summary' !!
labels_leyenda_final = c(
  "1" = "Perfil 1 (Revisar tabla)",
  "2" = "Perfil 2 (Revisar tabla)",
  "3" = "Perfil 3 (Revisar tabla)"
)

mapa_clusters_final = ggplot() +
  geom_sf(data = sf_mapa_final, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2, fontface = "bold") +
  scale_fill_brewer(
    palette = "Set2", 
    name = "Perfil Socioeconómico (Simulado)",
    labels = labels_leyenda_final, 
    na.value = "grey80"
  ) +
  labs(
    title = "Distribución Socioeconómica en la Provincia de Santiago",
    subtitle = "Basado en Ingreso, Migración y variables simuladas (Escolaridad, Alfabetismo)",
    caption = "Fuente: Censo 2017 + Microsimulación CASEN"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  )
print(mapa_clusters_final)

# --- 5.3: Mapa de Variabilidad (Shannon) ---
# (Usa nom_comuna.y por el conflicto del merge)
df_proporciones_comunal <- st_drop_geometry(sf_master_cluster) %>%
  filter(!is.na(cluster) & !is.na(nom_comuna.y)) %>% 
  group_by(nom_comuna.y, cluster) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(nom_comuna.y) %>%
  mutate(prop = n / sum(n)) %>% 
  ungroup()

df_shannon <- df_proporciones_comunal %>%
  group_by(nom_comuna = nom_comuna.y) %>% 
  summarise(shannon_index = -sum(prop * log(prop))) %>%
  ungroup()

sf_comunas_shannon <- merge(
  sf_comunas_santiago,
  df_shannon,
  by = "nom_comcomuna", 
  all.x = TRUE
)

mapa_comunal_shannon <- ggplot() +
  geom_sf(data = sf_comunas_shannon, aes(fill = shannon_index), color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2.0, fontface = "bold", color = "white") +
  scale_fill_viridis_c(option = "C", name = "Índice de Shannon (H)") +
  labs(
    title = "Variabilidad Socioeconómica Intra-Comunal (Nuevos Clústeres)",
    subtitle = "Índice de Shannon (Provincia de Santiago)",
    caption = "Fuente: Censo 2017 + Microsimulación CASEN"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
    legend.position = "bottom"
  )
print(mapa_comunal_shannon)

cat("\n--- PROCESO COMPLETO FINALIZADO ---\n")