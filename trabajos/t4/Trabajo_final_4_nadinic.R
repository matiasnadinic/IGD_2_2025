# =============================================================================
# BLOQUE 1: CONFIGURACIÓN INICIAL
# Objetivo: Cargar las herramientas necesarias para el análisis.
# =============================================================================
library(haven)    # Permite leer archivos de Stata (.dta), formato común de CASEN/EPF.
library(dplyr)    # Herramienta clave para filtrar, agrupar y crear variables (mutate).
library(pROC)     # Necesaria para calcular la curva ROC y evaluar el modelo Logit.
library(ggplot2)  # Para generar gráficos de calidad para la presentación.

# =============================================================================
# BLOQUE 2: CARGA Y PREPARACIÓN DE LA EPF (ENTRENAMIENTO)
# Objetivo: Construir la base de datos "real" con la que el modelo aprenderá.
# =============================================================================

# 1. Cargar las bases de datos originales
# Ajusta la ruta ("data/...") según donde tengas guardados tus archivos.
personas_epf    <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos_epf     <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades_epf <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif_epf       <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# 2. Procesar la variable dependiente (El Gasto) [cite: 10]
# Filtramos solo el ítem "Gasolina" (CCIF 07.2.2.02.01).
# Agrupamos por 'folio' (Hogar) porque el gasto en bencina es familiar, no individual.
gasto_gasolina <- gastos_epf %>%
  filter(ccif == "07.2.2.02.01") %>%
  group_by(folio) %>%
  # Sumamos todos los registros del mes para obtener el gasto total mensual del hogar.
  summarise(gasto_mensual = sum(gasto, na.rm = TRUE)) 

# 3. Procesar las variables explicativas (Características del Hogar) [cite: 12]
# Usamos al Jefe de Hogar como representante para obtener ingreso, educación y tamaño familiar.
hogares_epf <- personas_epf %>%
  # Filtramos para quedarnos solo con el Jefe de Hogar (o persona 1 si pco1 no existe)
  filter(if("pco1" %in% names(.)) pco1 == 1 else n_linea == 1) %>% 
  filter(macrozona == 2) %>%  # Filtro opcional: Gran Santiago (para homogeneidad)
  select(folio, ing_disp_hog_hd_ai, npersonas, edue) # Seleccionamos solo lo útil

# =============================================================================
# BLOQUE 3: CREACIÓN DEL DATASET PARA EL MODELO (MASTER TABLE)
# Objetivo: Unir gastos y características, y crear variables matemáticas.
# =============================================================================

# 1. Unimos la tabla de personas con la de gastos usando el 'folio' como llave
base_modelo <- left_join(hogares_epf, gasto_gasolina, by = "folio")

# 2. Limpieza y Transformación de Variables
base_modelo <- base_modelo %>%
  mutate(
    # Los hogares que no aparecieron en la tabla de gastos tienen NA. Los convertimos a 0.
    gasto_mensual = ifelse(is.na(gasto_mensual), 0, gasto_mensual),
    
    # Variable Binaria (Dummy): 1 si gastó algo, 0 si no gastó nada.
    # Esta es la variable dependiente (Y) para el Modelo 1 (Logit).
    d_gasto = ifelse(gasto_mensual > 0, 1, 0),
    
    # Ingreso per cápita: Ingreso total del hogar dividido por n integrantes.
    ingreso_pc = ing_disp_hog_hd_ai / npersonas,
    
    # Logaritmo del Ingreso: Suaviza la distribución y mejora el ajuste lineal.
    # Sumamos +1 para evitar error matemático log(0).
    log_ingreso = log(ingreso_pc + 1),
    
    # Categorización de Educación: Agrupamos los años de estudio en niveles claros.
    # Esto facilita la interpretación económica de los coeficientes.
    educacion_niv = case_when(
      edue <= 12 ~ "Basica_Media",
      edue > 12 ~ "Superior",
      TRUE ~ "Otro"
    )
  ) %>%
  # Filtramos datos erróneos donde el ingreso reportado sea 0 o menor.
  filter(ing_disp_hog_hd_ai > 0)

# =============================================================================
# BLOQUE 3.5: ANÁLISIS EXPLORATORIO (GRÁFICOS DE CORRELACIÓN)
# Objetivo: Visualizar la relación entre variables antes de modelar.
# =============================================================================

# Para los gráficos, usamos solo a quienes tienen Gasto > 0 (para ver montos)
datos_graficos <- base_modelo %>% filter(gasto_mensual > 0)

# --- GRÁFICO 1: RELACIÓN INGRESO VS GASTO (SCATTER PLOT) ---
# Muestra si a mayor ingreso, la gente gasta más en gasolina.
# La línea azul representa la tendencia suavizada.
ggplot(datos_graficos, aes(x = ingreso_pc, y = gasto_mensual)) +
  geom_point(alpha = 0.3, color = "darkblue") +  # Puntos semitransparentes
  geom_smooth(method = "lm", color = "red") +    # Línea de regresión lineal
  labs(title = "Correlación: Ingreso vs Gasto en Gasolina",
       x = "Ingreso Per Cápita ($)",
       y = "Gasto Mensual en Gasolina ($)") +
  scale_x_continuous(labels = scales::dollar) +  # Formato dinero
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()

# --- GRÁFICO 2: GASTO SEGÚN NIVEL EDUCACIONAL (BOXPLOT) ---
# Muestra si los niveles educativos más altos tienen gastos medianos mayores.
ggplot(datos_graficos, aes(x = educacion_niv, y = gasto_mensual, fill = educacion_niv)) +
  geom_boxplot() +
  labs(title = "Distribución del Gasto según Educación",
       x = "Nivel Educacional del Jefe de Hogar",
       y = "Gasto Mensual ($)") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  theme(legend.position = "none")

# --- GRÁFICO 3: GASTO SEGÚN TAMAÑO DEL HOGAR ---
# Muestra cómo varía el gasto si hay más personas en la casa.
ggplot(datos_graficos, aes(x = factor(npersonas), y = gasto_mensual)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Gasto en Gasolina según Tamaño del Hogar",
       x = "Número de Personas en el Hogar",
       y = "Gasto Mensual ($)") +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 400000)) + # Limitamos eje Y para ver mejor
  theme_minimal()

# --- GRÁFICO 4: PROBABILIDAD DE COMPRA (BINARIA) VS INGRESO ---
# Este gráfico es vital para justificar tu modelo LOGIT.
# Muestra cómo aumenta la proporción de gente que tiene auto a medida que sube el ingreso.
ggplot(base_modelo, aes(x = log_ingreso, y = d_gasto)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "green") +
  labs(title = "Probabilidad de tener Auto/Gasto según Ingreso",
       subtitle = "Curva Logística (S-Shape)",
       x = "Logaritmo del Ingreso",
       y = "Probabilidad de Gasto (0 a 1)") +
  theme_minimal()


# =============================================================================
# BLOQUE 4: ESTIMACIÓN DEL MODELO DE DOS PARTES [cite: 14]
# Objetivo: Calcular matemáticamente la propensión a gastar y el monto.
# =============================================================================

# PARTE A: Modelo Logit (Probabilidad de Compra) [cite: 15]
# Pregunta: ¿Qué características aumentan la probabilidad de comprar gasolina?
modelo_logit <- glm(d_gasto ~ log_ingreso + educacion_niv + npersonas, 
                    data = base_modelo, 
                    family = binomial(link = "logit"))

# Mostramos resultados: Mirar los asteriscos (***) para ver significancia.
summary(modelo_logit)

# PARTE B: Modelo Lineal OLS (Monto del Gasto) [cite: 16]
# Pregunta: Dado que compran gasolina, ¿cuánto gastan?
# Filtramos solo a los que gastaron (> 0) para entrenar esta parte.
base_positiva <- base_modelo %>% filter(gasto_mensual > 0)

# Usamos log(gasto) como variable dependiente para normalizar los residuos.
modelo_lineal <- lm(log(gasto_mensual) ~ log_ingreso + educacion_niv + npersonas, 
                    data = base_positiva)

summary(modelo_lineal)

# =============================================================================
# BLOQUE 5: EVALUACIÓN DE DESEMPEÑO [cite: 25]
# Objetivo: Demostrar que el modelo es confiable antes de usarlo.
# =============================================================================

# 1. Evaluación del Logit (Curva ROC y AUC)
prob_pred <- predict(modelo_logit, type = "response") # Probabilidades predichas
roc_obj <- roc(base_modelo$d_gasto, prob_pred)        # Cálculo de métricas ROC
plot(roc_obj, main = "Curva ROC - Capacidad Predictiva") # Gráfico para la ppt
print(paste("AUC:", auc(roc_obj))) # Un AUC > 0.7 es bueno.

# 2. Matriz de Confusión (Aciertos vs Errores)
pred_clase <- ifelse(prob_pred > 0.5, 1, 0) # Cortamos en 50% de probabilidad
tabla_confusion <- table(Predicho = pred_clase, Real = base_modelo$d_gasto)
print(tabla_confusion)

# =============================================================================
# BLOQUE 6: IMPUTACIÓN EN CASEN (ARREGLADO)
# =============================================================================

# 1. Cargar CASEN
casen_2022 <- readRDS("data/casen_rm.rds")

# 2. Homologación y Limpieza
casen_prep <- casen_2022 %>%
  # A. CÁLCULO DE PERSONAS
  add_count(folio, name = "npersonas_calc") %>% 
  
  # B. FILTROS INICIALES (Jefe de Hogar y Datos Válidos)
  filter(pco1 == 1) %>% 
  filter(!is.na(esc)) %>%        # <--- FIX 1: Eliminar si no sabemos escolaridad
  filter(!is.na(ytotcor)) %>%    # <--- FIX 2: Eliminar si no sabemos ingreso
  
  mutate(
    # C. CREACIÓN DE VARIABLES (Exactamente igual que el modelo)
    ingreso_pc = ytotcor / npersonas_calc,  
    log_ingreso = log(ingreso_pc + 1),
    npersonas = npersonas_calc,
    
    # D. CATEGORÍAS DE EDUCACIÓN
    # Solo usamos las categorías que el modelo conoce.
    # Si hay algún dato raro, lo forzamos a 'Basica_Media' para que no falle.
    educacion_niv = case_when(
      esc <= 12 ~ "Basica_Media",
      esc > 12 ~ "Superior",
      TRUE ~ "Basica_Media" # <--- FIX 3: Evitamos crear la categoría "Otro"
    )
  ) %>%
  # Limpieza final de seguridad
  filter(!is.na(ingreso_pc)) 

# 3. Predicción (Ahora sí debería funcionar)

# A. Probabilidad
casen_prep$prob_gasto <- predict(modelo_logit, newdata = casen_prep, type = "response")

# B. Monto
pred_log_monto <- predict(modelo_lineal, newdata = casen_prep)
casen_prep$monto_condicional <- exp(pred_log_monto)

# C. Gasto Final
casen_prep$gasto_gasolina_imputado <- casen_prep$prob_gasto * casen_prep$monto_condicional

# 4. Revisión Final
print("Resumen del gasto imputado:")
summary(casen_prep$gasto_gasolina_imputado)

# Gráfico
par(mfrow=c(1,2))
hist(base_modelo$gasto_mensual[base_modelo$gasto_mensual>0], 
     main="EPF Real", col="skyblue", xlab="Gasto $")
hist(casen_prep$gasto_gasolina_imputado[casen_prep$gasto_gasolina_imputado>1000], 
     main="CASEN Imputada", col="lightgreen", xlab="Gasto Estimado $")
    
  



# =============================================================================
# TRABAJO FINAL: ESTRATEGIA OPTIMIZADA + HUFF MODEL
# =============================================================================



# =============================================================================
# CÓDIGO MAESTRO: GEOMARKETING OPTIMIZADO + HEXÁGONOS + DATOS DUROS
# =============================================================================

# 1. CARGA DE LIBRERÍAS Y CONFIGURACIÓN
if (!require("pacman")) install.packages("pacman")
pacman::p_load(chilemapas, osmdata, sf, dplyr, leaflet, ggplot2, scales, gridExtra, tidyr)

# Función de Limpieza
limpiar_texto <- function(x) {
  x <- toupper(as.character(x))
  x <- iconv(x, to = "ASCII//TRANSLIT") 
  return(trimws(x))
}

# =============================================================================
# PASO 1: DATOS BÁSICOS & OFERTA
# =============================================================================
print("1. Cargando cartografía y oferta...")
mapa_base <- mapa_comunas %>% 
  filter(codigo_region == "13") %>% 
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna), by = "codigo_comuna") %>% 
  st_as_sf() %>% st_transform(4326) %>% 
  mutate(comuna_join = limpiar_texto(nombre_comuna))

oferta_actual <- tryCatch({
  q <- opq(bbox = getbb("Santiago Metropolitan Region, Chile"), timeout = 60) %>% 
    add_osm_feature(key = "amenity", value = "fuel")
  datos <- osmdata_sf(q)
  st_transform(datos$osm_points %>% select(osm_id, geometry), 4326)
}, error = function(e) {
  st_sf(osm_id = "1", geometry = st_sfc(st_point(c(-70.6, -33.45))), crs = 4326) 
})

# =============================================================================
# PASO 2: CÁLCULO DE DEMANDA (CENSO + CASEN)
# =============================================================================

datos_censo <- censo_2017_comunas %>%
  group_by(codigo_comuna) %>%
  summarise(pob = sum(poblacion, na.rm=T)) %>% mutate(hog = pob/3)

demanda_casen <- casen_prep %>%
  mutate(cj = limpiar_texto(r1b_comuna_esp)) %>% 
  group_by(cj) %>% 
  summarise(gasto_promedio = mean(gasto_gasolina_imputado, na.rm=T))

mapa_demanda <- mapa_base %>%
  left_join(datos_censo, by="codigo_comuna") %>% 
  left_join(demanda_casen, by=c("comuna_join"="cj")) %>%
  mutate(gasto_promedio = replace_na(gasto_promedio, 0),
         demanda_potencial_total = ifelse(is.na(gasto_promedio*hog), 0, gasto_promedio*hog)) %>% 
  st_make_valid()

# =============================================================================
# PASO 3: DETECCIÓN DE OPORTUNIDADES (GAP ANALYSIS)
# =============================================================================
print("3. Detectando Océanos Azules...")
zona_cobertura_simple <- st_transform(oferta_actual, 32719) %>% 
  st_buffer(dist = 640) %>% st_union() %>% st_transform(4326) %>% st_make_valid()

zonas_sin_servicio <- st_difference(mapa_demanda, zona_cobertura_simple)

zonas_oportunidad <- zonas_sin_servicio %>%
  select(nombre_comuna, codigo_comuna, demanda_potencial_total, geometry) %>%
  arrange(desc(demanda_potencial_total))

umbral <- quantile(zonas_oportunidad$demanda_potencial_total, 0.90)
clusters_visuales <- mapa_demanda %>% 
  filter(nombre_comuna %in% (zonas_oportunidad %>% filter(demanda_potencial_total >= umbral) %>% pull(nombre_comuna)))

# =============================================================================
# PASO 4: OPTIMIZACIÓN Y MODELO HUFF
# =============================================================================
mejor_zona <- zonas_oportunidad[1, ]
nombre_ganadora <- mejor_zona$nombre_comuna
print(paste("GANADOR:", nombre_ganadora))

zonas_censo_local <- mapa_zonas %>% 
  filter(codigo_region=="13", codigo_comuna==mejor_zona$codigo_comuna) %>%
  inner_join(censo_2017_zonas, by="geocodigo") %>% 
  filter(poblacion>0) %>% st_as_sf() %>% st_transform(4326) %>% st_make_valid()

interseccion <- st_intersection(mejor_zona, zonas_censo_local)
optimo <- if(nrow(interseccion)>0) st_centroid(interseccion %>% arrange(desc(poblacion)) %>% slice(1)) else st_centroid(mejor_zona)

# =============================================================================
# PASO 4.5: MODELO HUFF (HEXÁGONOS + ZONAS) - AQUÍ ESTÁ EL CAMBIO IMPORTANTE
# =============================================================================
print("Generando Hexágonos y calculando Huff...")

# 1. PREPARAR GEOMETRÍA (Radio 3km)
punto_utm <- st_transform(optimo, 32719)
buffer_sf <- st_sf(geometry = st_geometry(st_buffer(punto_utm, dist = 3000)))

# --- A. GRID HEXAGONAL (VISUALIZACIÓN CONTINUA) ---
# Creamos la malla de hexágonos (250m)
grid_geom <- st_make_grid(buffer_sf, cellsize = 250, square = FALSE)
grid_sf <- st_sf(geometry = grid_geom)
# Recortamos para que quede redonda dentro del buffer
grid_recortado <- st_intersection(grid_sf, buffer_sf)
grid_final <- st_transform(grid_recortado, 4326)

# Calculamos Huff para cada Hexágono
cent_grid <- st_centroid(grid_final)
d_mia_g <- pmax(as.numeric(st_distance(cent_grid, optimo)), 10)
idx_comp_g <- st_nearest_feature(cent_grid, oferta_actual)
d_comp_g <- pmax(as.numeric(st_distance(cent_grid, oferta_actual[idx_comp_g,], by_element = TRUE)), 10)
# Fórmula Probabilidad
grid_final$probabilidad <- (1/d_mia_g^2) / ((1/d_mia_g^2) + (1/d_comp_g^2))


# --- B. ZONAS CENSALES (CÁLCULO ECONÓMICO REAL) ---
# Traemos zonas de toda la región para cruzar fronteras comunales
zonas_regionales <- mapa_zonas %>% filter(codigo_region == "13") %>% 
  left_join(censo_2017_zonas, by="geocodigo") %>% 
  mutate(poblacion = replace_na(poblacion, 0), hogares = poblacion/3) %>%
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna), by="codigo_comuna") %>%
  mutate(comuna_join = limpiar_texto(nombre_comuna)) %>%
  left_join(demanda_casen, by=c("comuna_join"="cj")) %>% 
  mutate(gasto_promedio = replace_na(gasto_promedio, 0)) %>% st_as_sf() %>% st_transform(4326) %>% st_make_valid()

# Recortamos manzanas
zonas_huff <- st_intersection(zonas_regionales, st_transform(buffer_sf, 4326))

# Calculamos Huff para manzanas
cent_zonas <- st_centroid(zonas_huff)
d_mia_z <- pmax(as.numeric(st_distance(cent_zonas, optimo)), 10)
idx_comp_z <- st_nearest_feature(cent_zonas, oferta_actual)
d_comp_z <- pmax(as.numeric(st_distance(cent_zonas, oferta_actual[idx_comp_z,], by_element = TRUE)), 10)
zonas_huff$probabilidad <- (1/d_mia_z^2) / ((1/d_mia_z^2) + (1/d_comp_z^2))

# =============================================================================
# PASO 5: CÁLCULO DE DATOS DUROS (KPIs)
# =============================================================================
zonas_huff$poblacion_capturada <- zonas_huff$poblacion * zonas_huff$probabilidad
zonas_huff$ingreso_capturado <- zonas_huff$hogares * zonas_huff$gasto_promedio * zonas_huff$probabilidad

# Totales
total_pob_area <- sum(zonas_huff$poblacion, na.rm=T)
total_pob_capturada <- sum(zonas_huff$poblacion_capturada, na.rm=T)
total_ingresos <- sum(zonas_huff$ingreso_capturado, na.rm=T)
market_share <- (total_pob_capturada / total_pob_area)

print("=== REPORTE EJECUTIVO (KPIs) ===")
print(paste("Ingreso Mensual Est.: ", label_dollar(prefix="$", big.mark=".")(round(total_ingresos))))
print(paste("Población Capturada:  ", label_comma()(round(total_pob_capturada))))
print("================================")

# =============================================================================
# PASO 6: VALIDACIÓN VIAL (ISÓCRONA)
# =============================================================================
tryCatch({
  q_calles <- opq(bbox = st_bbox(st_transform(buffer_sf, 4326)), timeout = 60) %>% 
    add_osm_feature(key = "highway", value = c("primary", "secondary", "tertiary"))
  red_vial <- st_transform(osmdata_sf(q_calles)$osm_lines, 32719)
  isocrona_real <- if(!is.null(red_vial)) st_buffer(st_intersection(red_vial, st_buffer(punto_utm, 2800)), 80) %>% st_union() %>% st_transform(4326) else st_buffer(optimo, 0.008)
}, error = function(e) { isocrona_real <<- st_buffer(optimo, 0.008) })

# =============================================================================
# PASO 7: VISOR INTERACTIVO FINAL (CON HEXÁGONOS + DATOS)
# =============================================================================
fmt_clp <- label_dollar(prefix = "$", big.mark = ".", accuracy = 1)
fmt_mm <- label_dollar(scale = 1e-6, suffix = " MM", accuracy = 0.1)

# Paletas
pal_huff <- colorNumeric("RdYlBu", domain = c(0, 1), reverse = TRUE)
pal_demanda <- colorNumeric("YlOrRd", domain = mapa_demanda$demanda_potencial_total)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # A. CAPA 1: CONTEXTO MACRO (Demanda General)
  addPolygons(data = mapa_demanda, fillColor = ~pal_demanda(demanda_potencial_total), 
              color="transparent", weight=1, fillOpacity=0.5, group="1. Demanda General ($)", 
              popup = ~paste("<b>", nombre_comuna, "</b><br>Demanda:", fmt_mm(demanda_potencial_total))) %>%
  
  # B. CAPA 2: CLUSTERS (HUECOS)
  addPolygons(data = clusters_visuales, color = "#C0392B", weight = 2, fillOpacity = 0.1, group = "2. Clusters de Oportunidad") %>%
  
  # C. CAPA 3: HEXÁGONOS (VISUALIZACIÓN CONTINUA - LO QUE FALTABA)
  addPolygons(data = grid_final, 
              fillColor = ~pal_huff(probabilidad), 
              color="white", weight=0, fillOpacity=0.4, 
              group="3. Probabilidad (Hexágonos)",
              popup = ~paste("Probabilidad Teórica:", percent(probabilidad, 0.1))) %>%
  
  # D. CAPA 4: MANZANAS (DATOS DUROS ECONÓMICOS)
  addPolygons(data = zonas_huff, 
              fillColor = ~pal_huff(probabilidad), 
              color="white", weight=0.5, fillOpacity=0.8, 
              group="4. Ingresos por Manzana",
              popup = ~paste("<b>Zona Censal</b><br>",
                             "Población Real:", round(poblacion), "<br>",
                             "<b>Clientes Captados:</b>", round(poblacion_capturada), "<br>",
                             "<b>Ingreso Estimado:</b>", fmt_clp(ingreso_capturado))) %>%
  
  # E. CAPA 5: VALIDACIÓN VIAL
  addPolygons(data = isocrona_real, color="#00AA00", weight=2, fillOpacity=0.1, group="5. Isócrona Vial") %>%
  addCircleMarkers(data = oferta_actual, color="black", radius=3, group="Competencia") %>%
  
  # F. GANADOR
  addMarkers(data = optimo, popup = paste("GANADOR:<br>Venta Est.:", fmt_mm(total_ingresos))) %>%
  
  # G. CONTROLES
  addLegend(pal = pal_huff, values = c(0,1), title = "Prob. Huff", position = "bottomright") %>%
  addLayersControl(overlayGroups = c("3. Probabilidad (Hexágonos)", "4. Ingresos por Manzana", "2. Clusters de Oportunidad", "1. Demanda General ($)", "5. Isócrona Vial", "Competencia"), 
                   options = layersControlOptions(collapsed=FALSE)) %>%
  # H. COBERTURA ACTUAL
  addPolygons(data = zona_cobertura_simple, color = "#2980B9", weight = 1, fillOpacity = 0.3, group = "Cobertura Actual") %>%
  
  hideGroup(c("1. Demanda General ($)", "Competencia", "2. Clusters de Oportunidad"))
