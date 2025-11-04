# =============================================================================
# SECCIÓN 1: LIBRERÍAS
# =============================================================================
# Carga de todas las librerías al inicio
library(rakeR)
library(RPostgres)
library(DBI)
library(MASS)      # Para polr() (Imputación de e1)
library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)   # Para unir mapas y leyendas
library(biscale)
library(stringr)   # Para limpiar códigos (str_trim, str_pad)


# =============================================================================
# SECCIÓN 2: ENTRADAS
# =============================================================================
## Rutas de las entradas
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

## Cargar los dataframes crudos
casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)


# =============================================================================
# SECCIÓN 3: PREPARACIÓN DE DATOS CENSO (Obtener Categorías)
# =============================================================================
# Identificamos los nombres de las columnas que usaremos como 'objetivo'
col_cons = sort(setdiff(names(cons_censo_df),c("GEOCODIGO","COMUNA")))

## Generamos los niveles para edad, escolaridad y sexo
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esc", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

cat("--- Categorías de Censo identificadas ---\n")
print(age_levels)
print(esc_levels)
print(sexo_levels)


# =============================================================================
# SECCIÓN 4: PREPARACIÓN DE DATOS CASEN (Consolidado)
# =============================================================================
# Esta sección carga, limpia, imputa y categoriza los datos de CASEN
# en un solo dataframe: 'casen_preparado'

# --- 4.1. Cargar CASEN (incluyendo Ponderador 'expr') ---
PONDERADOR_CASEN <- "expr" # Identificado de tu captura de pantalla

vars_micro = c(
  "estrato", "esc", "edad", "sexo", "e6a", "e1", 
  PONDERADOR_CASEN 
)
casen_preparado <- readRDS(ruta_casen)[, vars_micro, drop = FALSE]

# --- 4.2. Limpieza básica ---
casen_preparado$comuna <- substr(as.character(casen_preparado$estrato), 1, 5)
casen_preparado$estrato <- NULL
casen_preparado$esc <- as.integer(unclass(casen_preparado$esc))
casen_preparado$edad <- as.integer(unclass(casen_preparado$edad))
casen_preparado$e6a <- as.numeric(unclass(casen_preparado$e6a))
casen_preparado$sexo <- as.integer(unclass(casen_preparado$sexo))
casen_preparado$e1 <- as.numeric(unclass(casen_preparado$e1))
casen_preparado$ponderador <- as.numeric(unclass(casen_preparado[[PONDERADOR_CASEN]]))

# --- 4.3. Imputación 'esc' (Escolaridad) ---
casos_completos_esc <- !is.na(casen_preparado$esc) & !is.na(casen_preparado$e6a)
modelo_glm <- glm(esc ~ e6a, data = casen_preparado[casos_completos_esc, ], family = poisson(link = "log"))
filas_para_imputar_esc <- is.na(casen_preparado$esc) & !is.na(casen_preparado$e6a)
predicciones_glm <- predict(modelo_glm, newdata = casen_preparado[filas_para_imputar_esc, ], type = "response")
casen_preparado$esc[filas_para_imputar_esc] <- round(predicciones_glm)
summary(modelo_glm) # Imprime el resumen del modelo

# --- 4.4. Imputación 'e1' (Alfabetización) ---
casen_preparado$e1 <- factor(casen_preparado$e1, levels = 1:4, ordered = TRUE)
casos_completos_e1 <- !is.na(casen_preparado$e1) & !is.na(casen_preparado$esc)
modelo_imputacion_e1 <- polr(e1 ~ esc, data = casen_preparado[casos_completos_e1, ])
filas_para_imputar_e1 <- is.na(casen_preparado$e1) & !is.na(casen_preparado$esc)
predicciones_e1 <- predict(modelo_imputacion_e1, newdata = casen_preparado[filas_para_imputar_e1, ])
casen_preparado$e1[filas_para_imputar_e1] <- predicciones_e1
summary(modelo_imputacion_e1) # Imprime el resumen del modelo

# --- 4.5. Limpieza antes de categorizar ---
# Descartamos filas que no sirven para el modelo (sin ponderador o sin datos clave)
casen_preparado <- casen_preparado[
  !is.na(casen_preparado$ponderador) & 
    !is.na(casen_preparado$esc) & 
    !is.na(casen_preparado$edad) & 
    !is.na(casen_preparado$sexo), 
]
casen_preparado$e6a <- NULL # Ya no la necesitamos

# --- 4.6. Crear Categorías Homologadas (para Raking) ---
# Creamos las categorías en CASEN que coinciden EXACTAMENTE con el Censo
casen_preparado$sexo_cat <- ifelse(casen_preparado$sexo == 1, "sexo_m", "sexo_f")

casen_preparado$edad_cat <- cut(casen_preparado$edad,
                                breaks = c(-Inf, 30, 40, 50, 60, 70, 80, Inf),
                                labels = age_levels, # Usamos los niveles del Censo
                                right = TRUE
)

casen_preparado$esc_cat <- cut(casen_preparado$esc,
                               breaks = c(-Inf, 0, 8, 12, Inf),
                               labels = esc_levels, # Usamos los niveles del Censo
                               right = TRUE
)

# --- 4.7. Limpieza Final (¡IMPORTANTE!) ---
# Nos aseguramos de eliminar filas con NAs en las nuevas categorías
# RakeR no puede manejar NAs en las variables de control
casen_preparado <- casen_preparado[
  !is.na(casen_preparado$sexo_cat) &
    !is.na(casen_preparado$edad_cat) &
    !is.na(casen_preparado$esc_cat),
]

# --- 4.8. CORRECCIÓN FINAL (Factor a Character) ---
# Forzamos que todas las categorías sean TEXTO (character)
# para que coincidan 100% con los nombres de columna del Censo
casen_preparado$sexo_cat <- as.character(casen_preparado$sexo_cat)
casen_preparado$edad_cat <- as.character(casen_preparado$edad_cat)
casen_preparado$esc_cat <- as.character(casen_preparado$esc_cat)


cat("\n--- DATAFRAME 'casen_preparado' LISTO ---\n")
print(head(casen_preparado))


# =============================================================================
# SECCIÓN 5: ANÁLISIS ESPACIAL AGREGADO (Mapas de Comuna)
# =============================================================================
# NOTA: Esta sección se deja comentada. Los mapas de comuna fallan
# porque la tabla 'dpa.comunas_v' no tiene un código de 5 dígitos (ej. 13101)
# para unirse con los datos de CASEN (que sí lo tienen).
# El proyecto principal (microsimulación por zona) no se ve afectado.

cat("\n--- Iniciando SECCIÓN 5: Mapas Agregados por Comuna (Omitida) ---\n")

# --- 5.2. Cargar Geometrías de Comunas (RM) ---
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "censo_v_2017", host = "localhost", port = 5432,
  user = "postgres", password = "postgres" 
)
# Dejamos la conexión 'con' abierta para usarla en la Sección 9


# =============================================================================
# SECCIÓN 6: PREPARACIÓN MICROSIMULACIÓN (Datos Censo)
# =============================================================================
# Preparamos los datos "objetivo" (constraints) del Censo.

cat("\n--- Preparando datos de Censo para Raking ---\n")
vars_raking <- c("GEOCODIGO", age_levels, esc_levels, sexo_levels)
cons_censo_input <- cons_censo_df[, vars_raking]
print(head(cons_censo_input))


# =============================================================================
# SECCIÓN 7: EJECUTAR LA MICROSIMULACIÓN (RakeR) [CORREGIDO]
# =============================================================================
# Este es el núcleo del proyecto: crear la población sintética.

cat("\n--- Iniciando Microsimulación (Raking) ---\n")
cat("Esto puede tardar varios minutos...\n")

# --- ¡CORRECCIÓN 1: Nombres de variables de control! ---
# Debe ser los nombres de las COLUMNAS, no los niveles
control_vars <- c("sexo_cat", "edad_cat", "esc_cat")

# --- ¡CORRECCIÓN 2: Nombres de argumentos en rakeR::weight! ---
# (Se usan 'cons' e 'inds')
pesos_simulados_matriz <- rakeR::weight(
  cons = cons_censo_input,      # (constraints = Censo)
  inds = casen_preparado,       # (individuals = Casen)
  vars = control_vars
)
cat("Nuevos pesos (IPF) calculados.\n")

# --- ¡CORRECCIÓN 3: Nombres de argumentos en rakeR::extract! ---
# (Se usan 'weights', 'inds', 'id')
poblacion_sintetica_df <- rakeR::extract(
  weights = pesos_simulados_matriz, 
  inds = casen_preparado,
  id = "GEOCODIGO"
)
cat("Población sintética generada. Total de filas:", nrow(poblacion_sintetica_df), "\n")


# =============================================================================
# SECCIÓN 8: AGREGAR RESULTADOS SIMULADOS POR ZONA CENSAL
# =============================================================================
# Ahora colapsamos la población sintética para obtener UN valor por zona censal.

cat("\n--- Agregando resultados de la simulación por Zona Censal ---\n")

df_zonas_simulado <- poblacion_sintetica_df %>%
  filter(edad >= 15) %>% 
  group_by(GEOCODIGO) %>%
  summarise(
    # Variable 1: Escolaridad promedio SIMULADA
    esc_promedio_sim = mean(esc, na.rm = TRUE),
    
    # Variable 2: Tasa de Alfabetización SIMULADA
    poblacion_objetivo_sim = n(),
    total_alfabetizados_sim = sum(e1 == "1", na.rm = TRUE),
    tasa_alfabetizacion_sim = (total_alfabetizados_sim / poblacion_objetivo_sim) * 100
  ) %>%
  ungroup()

print(head(df_zonas_simulado))


# =============================================================================
# SECCIÓN 9: CARGAR GEOMETRÍA DE ZONAS CENSALES (RM)
# =============================================================================
cat("\n--- Cargando geometrías de Zonas Censales (RM) ---\n")

# Usamos la misma conexión 'con' de la Sección 5
sql_geometria_zonas_rm <- "
SELECT
  geocodigo::text AS GEOCODIGO,
  geom,
  nom_comuna
FROM dpa.zonas_censales_v
WHERE 
  region = '13'; -- Filtramos por RM (como texto)
"

sf_zonas_rm <- st_read(con, query = sql_geometria_zonas_rm)
dbDisconnect(con) # Ahora sí cerramos la conexión
cat('Zonas Censales de la RM cargadas:', nrow(sf_zonas_rm), '\n')


# =============================================================================
# SECCIÓN 10: COMBINAR DATOS SIMULADOS Y GEOMETRÍAS
# =============================================================================
cat("\n--- Uniendo datos simulados con geometrías de Zonas ---\n")

# Estandarizamos el GEOCODIGO a 11 dígitos con ceros (como en tu ref.)
df_zonas_simulado$GEOCODIGO_clean <- str_pad(
  str_trim(as.character(df_zonas_simulado$GEOCODIGO)), 11, "left", "0"
)
sf_zonas_rm$GEOCODIGO_clean <- str_pad(
  str_trim(as.character(sf_zonas_rm$GEOCODIGO)), 11, "left", "0"
)

# Unimos los datos simulados al mapa de zonas censales
sf_mapa_zonas <- merge(sf_zonas_rm, df_zonas_simulado, 
                       by.x = "GEOCODIGO_clean", 
                       by.y = "GEOCODIGO_clean", 
                       all.x = TRUE)

cat("\n--- REVISIÓN DEL MERGE DE ZONAS (NO DEBE MOSTRAR NA) ---\n")
print(head(sf_mapa_zonas[c("nom_comuna", "esc_promedio_sim", "tasa_alfabetizacion_sim")]))


# =============================================================================
# SECCIÓN 11: MAPAS FINALES POR ZONA CENSAL (con Estilo de Referencia)
# =============================================================================
# (Adaptado de las Secciones 6 y 8 de tu script de Valparaíso)

# --- 11.1 Mapa de Escolaridad Simulada ---
cat("Generando mapa de escolaridad simulada...\n")
map_esc_sim <- ggplot(sf_mapa_zonas) +
  geom_sf(aes(fill = esc_promedio_sim), color = "gray80", size = 0.005) + # Estilo de ref.
  scale_fill_viridis_c(option = "C", na.value = "gray90") + # Color de ref.
  labs(title = "Escolaridad Promedio SIMULADA por Zona Censal (RM)", 
       fill = "Años Prom.") +
  theme_minimal() + # Tema de ref.
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(map_esc_sim)



# --- 11.2 Mapa de Alfabetización Simulada ---
cat("Generando mapa de alfabetización simulada...\n")
map_alf_sim <- ggplot(sf_mapa_zonas) +
  geom_sf(aes(fill = tasa_alfabetizacion_sim), color = "gray80", size = 0.005) + # Estilo de ref.
  scale_fill_viridis_c(option = "C", na.value = "gray90") + # Color de ref.
  labs(title = "Tasa de Alfabetización SIMULADA por Zona Censal (RM)", 
       fill = "% Alfabetiz.") +
  theme_minimal() + # Tema de ref.
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(map_alf_sim)



# --- 11.3 Mapa Bivariado (Resultado Final) ---
cat("Generando mapa bivariado...\n")
# Filtramos NA para que biscale no falle
sf_mapa_zonas_filtrado <- sf_mapa_zonas[
  !is.na(sf_mapa_zonas$esc_promedio_sim) & 
    !is.na(sf_mapa_zonas$tasa_alfabetizacion_sim), 
]

sf_mapa_bi_zonas <- bi_class(
  sf_mapa_zonas_filtrado, 
  x = esc_promedio_sim, 
  y = tasa_alfabetizacion_sim, 
  dim = 2, 
  style = "quantile"
)

# Estilo de mapa bivariado de tu referencia
mapa_bivariado_plot_zonas <- ggplot() +
  geom_sf(data = sf_mapa_bi_zonas, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_mapa_zonas, fill = NA, color = 'black', size = 0.005) + # Borde de ref.
  bi_scale_fill(pal = 'DkBlue', dim = 2) + # Paleta de ref.
  labs(title = 'Escolaridad vs. Alfabetización (Simulado por Zona Censal)') +
  theme_minimal() + # Tema de ref.
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.2)
  )

leyenda_bivariada_zonas <- bi_legend(
  pal = 'DkBlue', dim = 2, 
  xlab = 'Mayor Escolaridad →', 
  ylab = 'Mayor Alfabetización →', 
  size = 10
)

mapa_final_zonas <- ggdraw() +
  draw_plot(mapa_bivariado_plot_zonas, 0, 0, 1, 1) + 
  draw_plot(leyenda_bivariada_zonas, 0.75, 0.05, 0.2, 0.2) # Posición de leyenda de ref.

print(mapa_final_zonas)



# --- FIN DEL PROYECTO ---
cat("\n--- PROYECTO COMPLETADO. ---\n")