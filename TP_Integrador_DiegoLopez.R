## =============================================================
## Laboratorio de Python y R
## Trabajo Practico Integrador
## Alumno Diego Sebastian Lopez
## Fecha 20/09/2026
## =============================================================

## =============================================================
# DATOS DE PANEL - EMPRESAS OPERADORAS DE ACTIVOS, CAPITAL INTENSIVAS
## ¿Los costos operativos son explicados por la antiguedad de los activos?
## ¿Siguen la forma de U que representa el comportamiento típico de fallas de activos en funcion de su antiguedad?
## Esta hipotesis implica que la operacion y mantenimiento de los activos estan determinados por su antiguedad (y por las fallas).
## Tambien implica que los costos operativos que no dependen de los activos se mantienen estables
## =============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)
library(zoo)

dir.create("data", showWarnings = FALSE)

# Si data/panel_construido.csv ya existe, se salta la consulta via API
usar_datos_guardados <- file.exists("data/panel_construido.csv")
#usar_datos_guardados <- FALSE ## para importar datos nuevos

if (!usar_datos_guardados) {

## =============================================================
## PARTE IA — Universo de empresas a evaluar
## =============================================================
## Empresas Oil & Gas (eje central del trabajo), y Electricas /
## Telecomunicaciones como sectores secundarios de comparacion
## (tambien intensivos en activo fijo, pero con dinamica distinta).
## =============================================================

empresas_meta <- tibble::tribble(
  ~ticker, ~nombre,                        ~sector,

  # --- Oil & Gas: integradas ---------------------------------
  "XOM",   "ExxonMobil",                    "Oil & Gas",
  "CVX",   "Chevron",                       "Oil & Gas",

  # --- Oil & Gas: exploracion y produccion (E&P) --------------
  "COP",   "ConocoPhillips",                "Oil & Gas",
  "EOG",   "EOG Resources",                 "Oil & Gas",
  "DVN",   "Devon Energy",                  "Oil & Gas",
  "FANG",  "Diamondback Energy",            "Oil & Gas",
  "APA",   "APA Corporation (Apache)",      "Oil & Gas",
  "OXY",   "Occidental Petroleum",          "Oil & Gas",
  "MRO",   "Marathon Oil",                  "Oil & Gas",
  "HES",   "Hess Corporation",              "Oil & Gas",
  "PXD",   "Pioneer Natural Resources",     "Oil & Gas",

  # --- Oil & Gas: refinacion -----------------------------------
  "MPC",   "Marathon Petroleum",            "Oil & Gas",
  "VLO",   "Valero Energy",                 "Oil & Gas",
  "PSX",   "Phillips 66",                   "Oil & Gas",

  # --- Oil & Gas: midstream (pipelines / gas / GNL) -------------
  "WMB",   "Williams Companies",            "Oil & Gas",
  "KMI",   "Kinder Morgan",                 "Oil & Gas",
  "OKE",   "ONEOK",                         "Oil & Gas",
  "ET",    "Energy Transfer",               "Oil & Gas",
  "EPD",   "Enterprise Products Partners",  "Oil & Gas",
  "TRGP",  "Targa Resources",               "Oil & Gas",
  "LNG",   "Cheniere Energy",               "Oil & Gas",

  # --- Oil & Gas: servicios petroleros ---------------------------
  "SLB",   "SLB (ex-Schlumberger)",         "Oil & Gas",
  "HAL",   "Halliburton",                   "Oil & Gas",
  "BKR",   "Baker Hughes",                  "Oil & Gas",

  # --- Electrico (utilities reguladas) ----------------------------
  "DUK",   "Duke Energy",                   "Electrico",
  "SO",    "Southern Company",              "Electrico",
  "AEP",   "American Electric Power",       "Electrico",
  "NEE",   "NextEra Energy",                "Electrico",
  "D",     "Dominion Energy",               "Electrico",
  "EXC",   "Exelon",                        "Electrico",
  "XEL",   "Xcel Energy",                   "Electrico",
  "ED",    "Consolidated Edison",           "Electrico",
  "PCG",   "PG&E Corporation",              "Electrico",
  "PPL",   "PPL Corporation",               "Electrico",
  "FE",    "FirstEnergy",                   "Electrico",
  "WEC",   "WEC Energy Group",              "Electrico",
  "ES",    "Eversource Energy",             "Electrico",

  # --- Telecomunicaciones -----------------------------------------
  "T",     "AT&T",                          "Telecom",
  "VZ",    "Verizon Communications",        "Telecom",
  "TMUS",  "T-Mobile US",                   "Telecom",
  "CMCSA", "Comcast",                       "Telecom"
)

cat("Universo de empresas:", nrow(empresas_meta), "\n")
empresas_meta %>% count(sector, name = "n_empresas") %>% print()

write.csv(empresas_meta, "data/empresas_meta.csv", row.names = FALSE)


## =============================================================
## PARTE IB — Armado del panel empresa-año desde SEC EDGAR (XBRL)
## =============================================================
## Fuente: https://data.sec.gov/api/xbrl/companyconcept/...
## No requiere API key, pero SEC exige un header User-Agent
## (nombre/proyecto + email de contacto).
## =============================================================

user_agent_sec <- "TP UTDT Laboratorio R - diegoslopez1991@gmail.com"
tickers <- empresas_meta$ticker

# --- IB.1. Mapear ticker -> CIK -----------------------------------
tickers_url <- "https://www.sec.gov/files/company_tickers.json"
resp_tickers <- GET(tickers_url, add_headers(`User-Agent` = user_agent_sec))
stopifnot(status_code(resp_tickers) == 200)

# company_tickers.json es un objeto JSON indexado por clave numerica
# simplifyVector = FALSE + bind_rows() lo arma bien, fila por registro.
tickers_raw <- fromJSON(content(resp_tickers, as = "text", encoding = "UTF-8"),
                         simplifyVector = FALSE)

tickers_df <- bind_rows(tickers_raw) %>%
  mutate(ticker = toupper(ticker),
         cik10  = sprintf("%010d", as.integer(cik_str)))

cik_map <- tickers_df %>% filter(ticker %in% tickers)

cat("Empresas encontradas (con CIK):", nrow(cik_map), "de", length(tickers), "\n")
tickers_sin_cik <- setdiff(tickers, cik_map$ticker)
if (length(tickers_sin_cik) > 0) {
  cat("Sin CIK (probablemente delisted / fusionada):", tickers_sin_cik, "\n")
  empresas_meta %>%
    filter(ticker %in% tickers_sin_cik) %>%
    write.csv("data/empresas_sin_cik.csv", row.names = FALSE)
}

# --- IB.2. Funcion para bajar datos de una empresa -----
get_concept <- function(cik10, tag, taxonomy = "us-gaap") {

  url <- sprintf(
    "https://data.sec.gov/api/xbrl/companyconcept/CIK%s/%s/%s.json",
    cik10, taxonomy, tag
  )

  resp <- GET(url, add_headers(`User-Agent` = user_agent_sec))
  Sys.sleep(0.15)  # para no saturar de pedidos a la API de SEC

  if (status_code(resp) != 200) return(NULL)

  dat <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  units_usd <- dat$units$USD
  if (is.null(units_usd)) return(NULL)
  units_usd <- as_tibble(units_usd)

  df <- units_usd %>% filter(form == "10-K")
  if (nrow(df) == 0) return(NULL)

  if ("start" %in% names(df)) {
    df <- df %>%
      mutate(dur_dias = as.numeric(as.Date(end) - as.Date(start))) %>%
      filter(dur_dias > 300, dur_dias < 400) %>%
      select(-dur_dias)
  }

  df <- df %>%
    group_by(end) %>%
    filter(filed == max(filed)) %>%
    ungroup() %>%
    distinct(end, .keep_all = TRUE)

  # fy: año fiscal que SEC le asigna a ese reporte (viene tal cual en
  # el JSON). Es la variable de tiempo del panel; se declara como tal
  # recien en la Parte III al construir el pdata.frame.
  df %>% transmute(fy = fy, end = end, val = val, tag = tag)
}

# --- IB.3. Tags de interes -----------------------------------------
tags_interes <- c(
  "PropertyPlantAndEquipmentGross",
  "AccumulatedDepreciationDepletionAndAmortizationPropertyPlantAndEquipment",
  "PropertyPlantAndEquipmentNet",
  "CostsAndExpenses",
  "PaymentsToAcquirePropertyPlantAndEquipment",
  "DepreciationDepletionAndAmortization"
)

# --- IB.4. Se crea el panel ------------------
panel_list <- list()

for (i in seq_len(nrow(cik_map))) {
  tk    <- cik_map$ticker[i]
  cik10 <- cik_map$cik10[i]
  cat("Bajando:", tk, "(", i, "/", nrow(cik_map), ")\n")

  for (tag in tags_interes) {
    d <- get_concept(cik10, tag)
    if (!is.null(d)) {
      d$ticker <- tk
      panel_list[[paste(tk, tag)]] <- d
    }
  }
}

panel_long <- bind_rows(panel_list)
write.csv(panel_long, "data/panel_long_raw.csv", row.names = FALSE)

# --- IB.5. Pasar a formato ancho y construir variables ----------------
panel_wide <- panel_long %>%
  select(ticker, fy, tag, val) %>%
  distinct(ticker, fy, tag, .keep_all = TRUE) %>%
  pivot_wider(names_from = tag, values_from = val)

panel <- panel_wide %>%
  rename(
    ppe_bruto       = PropertyPlantAndEquipmentGross,
    dep_acumulada   = AccumulatedDepreciationDepletionAndAmortizationPropertyPlantAndEquipment,
    ppe_neto        = PropertyPlantAndEquipmentNet,
    costos_totales  = CostsAndExpenses,
    capex           = PaymentsToAcquirePropertyPlantAndEquipment,
    dep_amort_anual = DepreciationDepletionAndAmortization
  ) %>%
  mutate(
    inversion_pct_activos = capex / ppe_bruto,
    costo_operativo_sin_dya = if_else(
      is.na(costos_totales) | is.na(dep_amort_anual) | dep_amort_anual < 0 |
        (costos_totales - dep_amort_anual) <= 0,
      NA_real_,
      costos_totales - dep_amort_anual
    )
  ) %>%
  left_join(empresas_meta %>% select(ticker, nombre, sector), by = "ticker") %>%
  mutate(
    # Agrupa Electrico + Telecom para tener un grupo de comparacion
    # con suficientes empresas (clusters) frente a Oil & Gas
    grupo_comparacion = if_else(sector == "Oil & Gas", "Oil & Gas",
                                 "Servicios regulados / redes (Electrico + Telecom)")
  ) %>%
  relocate(ticker, nombre, sector, grupo_comparacion, fy) %>%
  arrange(sector, ticker, fy)

write.csv(panel, "data/panel_construido.csv", row.names = FALSE)

cat("\nPanel final (con NA sin filtrar):", nrow(panel), "filas,",
    n_distinct(panel$ticker), "empresas,",
    "años", min(panel$fy, na.rm = TRUE), "-", max(panel$fy, na.rm = TRUE), "\n")

# --- IB.6. Diagnostico de empresas faltantes --------------------------
# Dos causas distintas por las que una empresa puede faltar:
#  (a) nunca se le encontro CIK (ya exportado arriba, empresas_sin_cik.csv)
#  (b) tenia CIK, pero NINGUN tag devolvio datos -> exportado aca.
tickers_sin_datos <- setdiff(cik_map$ticker, unique(panel$ticker))
if (length(tickers_sin_datos) > 0) {
  empresas_meta %>%
    filter(ticker %in% tickers_sin_datos) %>%
    write.csv("data/empresas_sin_datos.csv", row.names = FALSE)
  cat("Empresas con CIK pero sin ninguna fila en el panel:", length(tickers_sin_datos), "\n")
}

} else {
  cat("data/panel_construido.csv ya existe - se reusan los datos guardados")
}


## =============================================================
## PARTE II — Analisis exploratorio (EDA)
## =============================================================

panel <- read.csv("data/panel_construido.csv", stringsAsFactors = FALSE)

# --- II.1. Estructura general -----------------------------------
head(panel, 10)
dim(panel)
str(panel)

cat("Dimensiones:", nrow(panel), "filas x", ncol(panel), "columnas\n")
cat("Empresas incluidas:", n_distinct(panel$ticker), "\n")
cat("Rango de años:", min(panel$fy), "-", max(panel$fy), "\n")

panel %>% count(ticker, name = "años_disponibles") %>% arrange(años_disponibles)

# --- II.2. dep_acumulada cruda: valores negativos y construccion de
# antiguedad_proxy ---------------------------------------------------

ggplot(panel, aes(x = fy, y = dep_acumulada)) +
  geom_point(aes(color = sector), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Depreciacion acumulada por año",
       x = "Año", y = "Depreciacion acumulada (USD)", color = "Sector") +
  theme_minimal(base_size = 13)

cat("Valores negativos en dep_acumulada:",
    sum(panel$dep_acumulada < 0, na.rm = TRUE), "\n")
panel %>% filter(dep_acumulada < 0) %>%
  select(ticker, fy, dep_acumulada, ppe_bruto)

cobertura_por_empresa <- panel %>%
  group_by(ticker, sector) %>%
  summarise(
    n_años                 = n(),
    pct_na_ppe_bruto       = round(100 * mean(is.na(ppe_bruto)), 0),
    pct_na_dep_acumulada   = round(100 * mean(is.na(dep_acumulada)), 0),
    pct_na_costos_totales  = round(100 * mean(is.na(costos_totales)), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_na_costos_totales), desc(pct_na_ppe_bruto))
print(cobertura_por_empresa, n = Inf)
write.csv(cobertura_por_empresa, "data/cobertura_por_empresa.csv", row.names = FALSE)

# Interpolacion lineal DENTRO de cada empresa (zoo::na.approx, orden por
# fy) para los huecos sueltos de ppe_bruto/dep_acumulada detectados

panel <- panel %>%
  arrange(ticker, fy) %>%
  group_by(ticker) %>%
  mutate(
    ppe_bruto     = na.approx(ppe_bruto, x = fy, na.rm = FALSE),
    dep_acumulada = na.approx(dep_acumulada, x = fy, na.rm = FALSE)
  ) %>%
  ungroup()

# antiguedad_proxy se construye DESPUES de la interpolacion, para que
# los huecos rellenados en ppe_bruto/dep_acumulada la beneficien.
panel <- panel %>%
  mutate(
    antiguedad_proxy = if_else(
      is.na(dep_acumulada) | dep_acumulada < 0 | is.na(ppe_bruto),
      NA_real_,
      dep_acumulada / ppe_bruto
    ),
    antiguedad2 = antiguedad_proxy^2
  )

summary(panel$antiguedad_proxy)

ggplot(panel, aes(x = antiguedad_proxy, fill = sector)) +
  geom_histogram(bins = 25, alpha = 0.85, color = "white", position = "stack") +
  geom_vline(xintercept = c(0, 1), linetype = "dashed", color = "gray40") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribucion de la antiguedad proxy del activo fijo",
       subtitle = "dep. acumulada / PP&E bruto",
       x = "Antiguedad proxy", y = "Frecuencia", fill = "Sector") +
  theme_minimal(base_size = 13)
cat("Observaciones fuera de [0, 1]:",
    sum(panel$antiguedad_proxy < 0 | panel$antiguedad_proxy > 1, na.rm = TRUE), "\n")

# --- II.3. Valores faltantes -------------------------------------
panel %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  arrange(desc(n_faltantes))

ggplot(panel %>% mutate(sin_antiguedad = is.na(antiguedad_proxy)),
       aes(x = fy, y = ticker, fill = sin_antiguedad)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                     labels = c("Dato disponible", "Faltante")) +
  labs(title = "Disponibilidad de antiguedad_proxy por empresa y año",
       x = "Año", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12)

ggplot(panel %>% mutate(sin_costos = is.na(costos_totales)),
       aes(x = fy, y = ticker, fill = sin_costos)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                     labels = c("Dato disponible", "Faltante")) +
  labs(title = "Disponibilidad de costos_totales por empresa y año",
       x = "Año", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12)

# Interseccion de los dos mapas anteriores: empresa-año que tiene AMBOS

ggplot(panel %>% mutate(usable_modelo = !is.na(antiguedad_proxy) &
                           !is.na(costos_totales) & costos_totales > 0),
       aes(x = fy, y = ticker, fill = usable_modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("FALSE" = "tomato", "TRUE" = "steelblue"),
                     labels = c("No usable", "Usable para el modelo")) +
  labs(title = "Empresa-año con datos de antiguedad Y costos",
       subtitle = "Lo que efectivamente entra a panel_principal",
       x = "Año", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12)

# --- II.4. Distribuciones univariadas -----------------------------
ggplot(panel, aes(x = log(costos_totales))) +
  geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7, color = "white") +
  labs(title = "Distribucion de log(costo operativo total)",
       x = "log(Costos y gastos totales)", y = "Frecuencia") +
  theme_minimal(base_size = 13)

ggplot(panel, aes(x = inversion_pct_activos)) +
  geom_histogram(bins = 25, fill = "seagreen", alpha = 0.7, color = "white") +
  labs(title = "Inversion anual como % del activo fijo bruto",
       x = "Capex / PP&E bruto", y = "Frecuencia") +
  theme_minimal(base_size = 13)

# --- II.5. El panel completo, de un vistazo  --
ggplot(panel, aes(x = fy, y = ticker, fill = antiguedad_proxy)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Antiguedad proxy del activo fijo por empresa y año",
       x = "Año", y = NULL, fill = "Antiguedad\nproxy") +
  theme_minimal(base_size = 12)

# --- II.6. Relaciones bivariadas (agregado, sin controlar por empresa) --
ggplot(panel, aes(x = antiguedad_proxy, y = log(costos_totales))) +
  geom_point(aes(color = ticker), alpha = 0.6, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = TRUE) +
  labs(title = "log(costo operativo) vs antiguedad del activo fijo",
       subtitle = "Ajuste cuadratico agregado (sin controlar por empresa)",
       x = "Antiguedad proxy", y = "log(Costos totales)") +
  theme_minimal(base_size = 13)

# --- II.7. Evolucion temporal agregada, por sector -----------------
# Ponderados por tamaño de empresa (ppe_bruto como peso).
resumen_anual_sector_pond <- panel %>%
  filter(!is.na(ppe_bruto), ppe_bruto > 0) %>%
  group_by(sector, fy) %>%
  summarise(
    antiguedad_proxy_prom = weighted.mean(antiguedad_proxy, w = ppe_bruto, na.rm = TRUE),
    log_costos_prom       = weighted.mean(log(costos_totales), w = ppe_bruto, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(resumen_anual_sector_pond, aes(x = fy, y = antiguedad_proxy_prom, color = sector)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Antiguedad promedio por sector, en el tiempo",
       subtitle = "Ponderado por ppe_bruto",
       x = "Año", y = "Antiguedad proxy (promedio ponderado)", color = "Sector") +
  theme_minimal(base_size = 13)

ggplot(resumen_anual_sector_pond, aes(x = fy, y = log_costos_prom, color = sector)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "log(costo operativo) promedio por sector, en el tiempo",
       subtitle = "Ponderado por ppe_bruto",
       x = "Año", y = "log(costos_totales) (promedio ponderado)", color = "Sector") +
  theme_minimal(base_size = 13)


## =============================================================
## PARTE III — Modelo de datos de panel
## =============================================================
## log(costos_totales) ~ antiguedad_proxy + antiguedad2 (+ controles)
## Si se cumple la "bañera": antiguedad_proxy < 0 y antiguedad2 > 0.
## =============================================================

# --- III.1. Filtro final para el modelo ------------------------------

panel_principal <- panel %>%
  filter(!is.na(antiguedad_proxy), !is.na(costos_totales), costos_totales > 0)

cat("Muestra principal:", nrow(panel_principal), "filas,",
    n_distinct(panel_principal$ticker), "empresas\n")
panel_principal %>% distinct(ticker, sector) %>% count(sector) %>% print()

write.csv(panel_principal, "data/panel_modelo_principal.csv", row.names = FALSE)

pdata_principal <- pdata.frame(panel_principal, index = c("ticker", "fy"))

print(pdim(pdata_principal))  

# --- III.2. Modelo principal: Pooled, Efectos Fijos, Efectos Aleatorios --

formula_principal <- log(costos_totales) ~ antiguedad_proxy + antiguedad2 + log(ppe_bruto)

modelo_pooled <- plm(formula_principal, data = pdata_principal, model = "pooling")
modelo_fe     <- plm(formula_principal, data = pdata_principal, model = "within")
modelo_re     <- plm(formula_principal, data = pdata_principal, model = "random")

cat("\n--- POOLED OLS ---\n"); print(summary(modelo_pooled))
cat("\n--- EFECTOS FIJOS (within) ---\n"); print(summary(modelo_fe))
cat("\n--- EFECTOS ALEATORIOS ---\n"); print(summary(modelo_re))

# --- III.3. Test de Hausman: FE vs RE ---------------------------------
test_hausman <- phtest(modelo_fe, modelo_re)
print(test_hausman)

# --- III.4. Errores estandar robustos --
coefs_fe <- coeftest(modelo_fe, vcov = vcovHC(modelo_fe, type = "HC1", cluster = "group"))
print(coefs_fe)

# --- III.5. Efectos fijos de dos vias (empresa + año) --------

modelo_fe_2vias <- plm(formula_principal, data = pdata_principal, model = "within", effect = "twoways")
cat("\n--- TWFE (empresa + año) ---\n")
coefs_fe_2vias <- coeftest(modelo_fe_2vias, vcov = vcovHC(modelo_fe_2vias, type = "HC1", cluster = "group"))
print(coefs_fe_2vias)

# --- III.6. Costo sin el componente de D&A --------------------
panel_sin_dya <- panel_principal %>% filter(!is.na(costo_operativo_sin_dya), costo_operativo_sin_dya > 0)
cat("\nMuestra con costo_operativo_sin_dya disponible:", nrow(panel_sin_dya), "filas,",
    n_distinct(panel_sin_dya$ticker), "empresas\n")

pdata_sin_dya <- pdata.frame(panel_sin_dya, index = c("ticker", "fy"))
modelo_fe_sin_dya <- plm(log(costo_operativo_sin_dya) ~ antiguedad_proxy + antiguedad2 + log(ppe_bruto),
                         data = pdata_sin_dya, model = "within")
cat("\n--- Costo SIN componente de D&A ---\n")
print(summary(modelo_fe_sin_dya))
coefs_fe_sin_dya <- coeftest(modelo_fe_sin_dya, vcov = vcovHC(modelo_fe_sin_dya, type = "HC1", cluster = "group"))
print(coefs_fe_sin_dya)

# --- III.7. Efecto fijo solo por año (sin efecto fijo de empresa) --
modelo_solo_tiempo <- plm(formula_principal, data = pdata_principal, model = "within", effect = "time")
cat("\n--- Efecto fijo solo por año (con control de tamaño) ---\n")
print(summary(modelo_solo_tiempo))
coefs_solo_tiempo <- coeftest(modelo_solo_tiempo, vcov = vcovHC(modelo_solo_tiempo, type = "HC1", cluster = "group"))
print(coefs_solo_tiempo)

# --- III.8. Grafico final: curva estimada vs datos (muestra principal) --

media_antiguedad  <- mean(panel_principal$antiguedad_proxy, na.rm = TRUE)
media_antiguedad2 <- mean(panel_principal$antiguedad2, na.rm = TRUE)
media_log_costo   <- mean(log(panel_principal$costos_totales), na.rm = TRUE)

rango <- seq(0, 1, by = 0.01)
b0 <- media_log_costo - b_antiguedad * media_antiguedad - b_antiguedad2 * media_antiguedad2
curva <- data.frame(especificacion = "FE (empresa) - modelo principal", antiguedad_proxy = rango,
                    log_costo_ajustado = b0 + b_antiguedad * rango + b_antiguedad2 * rango^2)

b_antiguedad_2v  <- coefs_fe_2vias["antiguedad_proxy", "Estimate"]
b_antiguedad2_2v <- coefs_fe_2vias["antiguedad2", "Estimate"]
b0_2v <- media_log_costo - b_antiguedad_2v * media_antiguedad - b_antiguedad2_2v * media_antiguedad2
curva_2vias <- data.frame(especificacion = "FE dos vias (empresa + año)", antiguedad_proxy = rango,
                          log_costo_ajustado = b0_2v + b_antiguedad_2v * rango + b_antiguedad2_2v * rango^2)

ggplot(panel_principal, aes(x = antiguedad_proxy, y = log(costos_totales))) +
  geom_point(aes(color = grupo_comparacion), alpha = 0.35, size = 1.8) +
  geom_line(data = bind_rows(curva, curva_2vias),
            aes(x = antiguedad_proxy, y = log_costo_ajustado,
                linetype = especificacion, linewidth = especificacion),
            color = "grey15") +
  scale_linewidth_manual(values = c("FE (empresa) - modelo principal" = 1.1,
                                    "TWFE (empresa + año)" = 0.7),
                         guide = "none") +
  scale_linetype_manual(values = c("FE (empresa) - modelo principal" = "solid",
                                   "TWFE (empresa + año)" = "22")) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Costo operativo vs. antigüedad del activo fijo",
       subtitle = "Curvas de Efectos Fijos (controlando por tamaño)",
       x = "Antigüedad proxy (dep. acumulada / PP&E bruto)",
       y = "log(costos_totales)", color = "Grupo", linetype = "Especificación") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "vertical",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

