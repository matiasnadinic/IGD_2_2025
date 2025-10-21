# =============================================================================
# 1) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)
library(stringr) 

# =============================================================================
# 2) CONEXIÓN A BASE DE DATOS
# =============================================================================
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "censo_v_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# =============================================================================
# 3) CONSULTA SQL: Porcentaje de Profesionales y mujeres
# =============================================================================
df_profesional_mujeres <- dbGetQuery(con, "
SELECT 
  z.geocodigo,
  c.nom_comuna, 
  COUNT(*) FILTER (WHERE p.p15 >= 12 AND p.p15 <= 14) AS total_profesionales,
  ROUND(
    COUNT(*) FILTER (WHERE p.p15 >= 12 AND p.p15 <= 14) * 100.0 /
    COUNT(*) FILTER (WHERE p.p09 > 18),
    2
  ) AS tasa_profesionales,
  COUNT(*) FILTER (WHERE p.p08 = 2) AS total_mujeres,
  ROUND(
    COUNT(*) FILTER (WHERE p.p08 = 2) * 100.0 /
    COUNT(*),
    2
  ) AS porcentaje_mujeres
FROM personas p
JOIN hogares h ON h.hogar_ref_id = p.hogar_ref_id 
JOIN viviendas v ON h.vivienda_ref_id = v.vivienda_ref_id 
JOIN zonas z ON z.zonaloc_ref_id = v.zonaloc_ref_id
JOIN comunas c ON z.codigo_comuna = c.codigo_comuna 
GROUP BY z.geocodigo, c.nom_comuna
ORDER BY tasa_profesionales DESC;
")

# =============================================================================
# 4) CARGAR GEOMETRÍA DE ZONAS CENSALES - PROVINCIA DE VALPARAÍSO CONTINENTAL
# =============================================================================
sql_geometria <- "
SELECT
  geocodigo::text AS geocodigo,
  geom,
  nom_comuna
FROM dpa.zonas_censales_v
WHERE 
  nom_region ILIKE 'REGIÓN DE VALPARAÍSO'
  AND provincia = '51' 
  AND comuna NOT IN ('51501', '52701');
"

sf_zonas <- st_read(con, query = sql_geometria)
cat('Zonas cargadas:', nrow(sf_zonas), '\n')

# =============================================================================
# 5) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================

sf_zonas$geocodigo <- as.character(sf_zonas$geocodigo)
df_profesional_mujeres$geocodigo <- as.character(df_profesional_mujeres$geocodigo)

sf_zonas$geocodigo <- str_pad(str_trim(sf_zonas$geocodigo), 11, pad = "0")
df_profesional_mujeres$geocodigo <- str_pad(str_trim(df_profesional_mujeres$geocodigo), 11, pad = "0")

sf_mapa <- merge(sf_zonas, df_profesional_mujeres, by = "geocodigo", all.x = TRUE)

# =============================================================================
# 6) MAPAS TEMÁTICOS - PROVINCIA DE VALPARAÍSO CONTINENTAL
# =============================================================================

ultramar_codigos <- c("51501", "52701") 
sf_mapa_continental <- sf_mapa[
  !substr(sf_mapa$geocodigo, 1, 5) %in% ultramar_codigos, 
]

# --- Definir límites manuales para la visualización continental (FORZADO) ---
# AJUSTE HORIZONTAL (CENTRO)
X_MIN_VALPO <- -71.75 
X_MAX_VALPO <- -71.20 
# ✅ CORRECCIÓN: Se baja el límite sur para asegurar que no corte polígonos
Y_MIN_VALPO <- -33.60 
Y_MAX_VALPO <- -32.50 

map_profesionales <- ggplot(sf_mapa_continental) +
  geom_sf(aes(fill = tasa_profesionales), color = "gray80", size = 0.005) +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  labs(title = "% de Profesionales Provincia de Valparaíso", fill = "%") +
  coord_sf(
    xlim = c(X_MIN_VALPO, X_MAX_VALPO),
    ylim = c(Y_MIN_VALPO, Y_MAX_VALPO),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

map_mujeres <- ggplot(sf_mapa_continental) +
  geom_sf(aes(fill = porcentaje_mujeres), color = "gray80", size = 0.005) +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  labs(title = "% de Mujeres Provincia de Valparaíso", fill = "%") +
  coord_sf(
    xlim = c(X_MIN_VALPO, X_MAX_VALPO),
    ylim = c(Y_MIN_VALPO, Y_MAX_VALPO),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(map_profesionales)
print(map_mujeres)


# =============================================================================
# 7) GRÁFICO DE DISPERSIÓN POR CUADRANTES
# =============================================================================
if(length(unique(sf_mapa_continental$tasa_profesionales[!is.na(sf_mapa_continental$tasa_profesionales)])) > 1 &
   length(unique(sf_mapa_continental$porcentaje_mujeres[!is.na(sf_mapa_continental$porcentaje_mujeres)])) > 1) {
  
  mediana_profesionales <- median(sf_mapa_continental$tasa_profesionales, na.rm = TRUE)
  mediana_mujeres <- median(sf_mapa_continental$porcentaje_mujeres, na.rm = TRUE)
  
  sf_mapa_continental$cuadrante <- with(sf_mapa_continental, ifelse(
    tasa_profesionales >= mediana_profesionales & porcentaje_mujeres >= mediana_mujeres, 'Alta tasa / Alto % mujeres',
    ifelse(tasa_profesionales >= mediana_profesionales & porcentaje_mujeres < mediana_mujeres, 'Alta tasa / Bajo % mujeres',
           ifelse(tasa_profesionales < mediana_profesionales & porcentaje_mujeres < mediana_mujeres, 'Baja tasa / Bajo % mujeres',
                  'Baja tasa / Alto % mujeres'))))
  
  grafico_dispersion <- ggplot(sf_mapa_continental, aes(x = tasa_profesionales, y = porcentaje_mujeres, color = cuadrante)) +
    geom_point(size = 2) +
    geom_vline(xintercept = mediana_profesionales, linetype = "dashed") +
    geom_hline(yintercept = mediana_mujeres, linetype = "dashed") +
    scale_color_manual(values = c(
      'Alta tasa / Alto % mujeres' = '#08519c',
      'Alta tasa / Bajo % mujeres' = '#6baed6',
      'Baja tasa / Bajo % mujeres' = '#eff3ff',
      'Baja tasa / Alto % mujeres' = '#bdd7e7'
    )) +
    labs(x = "% Profesionales", y = "% Mujeres", title = "Dispersión: % Profesionales vs % Mujeres") +
    theme_minimal()
  
  print(grafico_dispersion)
} else {
  mediana_profesionales <- NA
  mediana_mujeres <- NA
  warning("No hay suficientes valores únicos en 'tasa_profesionales' o 'porcentaje_mujeres' para calcular medianas.")
}


# =============================================================================
# 8) MAPA BIVARIADO CON BISCALE - SOLUCIÓN ESTÁNDAR (CON ETIQUETAS EJE)
# =============================================================================

# --- 1️⃣ Clasificación estándar por Cuantiles (2x2) ---
sf_mapa_bi <- bi_class(
  sf_mapa_continental, 
  x = tasa_profesionales, 
  y = porcentaje_mujeres, 
  dim = 2, 
  style = "quantile" 
)

# --- 2️⃣ Crear puntos seguros (para el flujo, aunque no se usen etiquetas) ---
sf_centroides <- st_point_on_surface(sf_mapa_continental)

# --- 4️⃣ Crear mapa bivariado SIN etiquetas de comunas y con grilla ---
mapa_bivariado_plot <- ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA) +
  geom_sf(data = sf_mapa_continental, fill = NA, color = 'black', size = 0.005) +
  # Usamos theme_minimal para tener la grilla/ejes de lat/lon
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.2)
  ) +
  bi_scale_fill(pal = 'DkBlue', dim = 2) + 
  labs(title = '% Profesionales vs % Mujeres Provincia Valparaíso') +
  coord_sf(
    xlim = c(X_MIN_VALPO, X_MAX_VALPO), 
    ylim = c(Y_MIN_VALPO, Y_MAX_VALPO), 
    expand = FALSE
  )

# --- 5️⃣ Crear leyenda bivariada con etiquetas EJE ---
leyenda_bivariada <- bi_legend(
  pal = 'DkBlue', dim = 2, 
  xlab = '% Profesionales →', 
  ylab = '% Mujeres →', 
  size = 8
)

# --- 6️⃣ Combinar mapa + leyenda (Centrado y maximizado) ---
mapa_final <- ggdraw() +
  draw_plot(mapa_bivariado_plot, x = 0, y = 0, width = 1, height = 1) + 
  draw_plot(leyenda_bivariada, x = 0.75, y = 0.05, width = 0.2, height = 0.2) 

# --- 7️⃣ Mostrar mapa final ---
print(mapa_final)

# =============================================================================
# 9) CIERRE DE CONEXIÓN
# =============================================================================
dbDisconnect(con)
cat("Conexión cerrada correctamente.\n")