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
