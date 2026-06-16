# -------------------------------------------------------------------------
# SCRIPT DI ESTRAZIONE E AGGREGAZIONE DATI TELEMETRICI F1 (MULTI-STAGIONE)
# -------------------------------------------------------------------------

library(f1dataR)
library(tidyverse)
library(reticulate)
# setup_fastf1() solo la prima volta

# --- 1. DEFINIZIONE PARAMETRI ---
seasons <- c(2019, 2020, 2021)
session_type <- "Q"

# Fallback locale in caso l'API Jolpica (utilizzata da f1dataR) sia offline (Ergast shutdown)
fallback_data <- list(
  "2019" = list(tracks = 1:21, drivers = c("ALB","BOT","GAS","GIO","GRO","HAM","HUL","KUB","KVY","LEC","MAG","NOR","PER","RAI","RIC","RUS","SAI","STR","VER","VET")),
  "2020" = list(tracks = 1:17, drivers = c("AIT","ALB","BOT","FIT","GAS","GIO","GRO","HAM","HUL","KVY","LAT","LEC","MAG","NOR","OCO","PER","RAI","RIC","RUS","SAI","STR","VER","VET")),
  "2021" = list(tracks = 1:22, drivers = c("ALO","BOT","GAS","GIO","HAM","KUB","LAT","LEC","MAZ","MSC","NOR","OCO","PER","RAI","RIC","RUS","SAI","STR","TSU","VER","VET"))
)

# --- 2. CONFIGURAZIONE CACHE ---
# Abilitiamo la cache imponendo una cartella locale per evitare 
# blocchi causati dalle richieste console interattive nell'estrazione massiva.
if (!dir.exists("f1cache")) {
  dir.create("f1cache", showWarnings = FALSE)
}
options(f1dataR.cache = file.path(getwd(), "f1cache"))
Sys.setenv(F1_DATAR_CACHE = file.path(getwd(), "f1cache")) 

# --- 3. CICLO DI ESTRAZIONE E AGGREGAZIONE ---
lista_dati <- list()
counter <- 1
fallimenti <- c() # Log per raccogliere le stringhe identitarie dei dati non trovati

for (s in seasons) {
  
  cat(sprintf("\n=======================================================\n"))
  cat(sprintf("        CARICAMENTO DATI STAGIONE %s          \n", s))
  cat(sprintf("=======================================================\n"))
  
  # --- Estraggo Round Dinamicamente (Tracks) ---
  schedule_df <- tryCatch(
    load_schedule(season = s),
    error = function(e){
      return(data.frame())
    }
  )
  
  if (is.null(schedule_df) || nrow(schedule_df) == 0 || !"round" %in% colnames(schedule_df)) {
    cat(sprintf("ATTENZIONE: API Jolpica in pausa. Uso calendario fallback locale per %s.\n", s))
    tracks <- fallback_data[[as.character(s)]]$tracks
  } else {
    tracks <- schedule_df$round
  }
  
  # --- Estraggo Piloti Dinamicamente (Drivers) ---
  drivers_df <- tryCatch(
    load_drivers(season = s),
    error = function(e){
      return(data.frame())
    }
  )
  
  if (is.null(drivers_df) || nrow(drivers_df) == 0) {
    cat(sprintf("ATTENZIONE: API Jolpica in pausa. Uso piloti fallback locale per %s.\n", s))
    drivers <- fallback_data[[as.character(s)]]$drivers
  } else {
    # Troviamo la colonna col codice 3-lettere o usiamo la migliore a disposizione per compatibilità
    if ("driver_code" %in% colnames(drivers_df)) {
      drivers <- drivers_df$driver_code
    } else if ("code" %in% colnames(drivers_df)) {
      drivers <- drivers_df$code
    } else if ("driverId" %in% colnames(drivers_df)) {
      drivers <- drivers_df$driverId
    } else {
      # Fallback stringa non-standard se f1dataR dovesse mutare versione
      drivers <- drivers_df[[1]]
    }
    
    # Pulizia codici piloti: rimuoviamo NaN o vuoti
    drivers <- as.character(drivers[!is.na(drivers) & drivers != ""])
  }
  
  cat(sprintf(" => Trovati %s Round e %s Piloti validi.\n", length(tracks), length(drivers)))
  
  # --- Loop annidato su Round e Piloti per la Stagione Corrente ---
  for (j in tracks) {
    for (i in drivers) {
      cat(sprintf("Estrazione: Season %s | Round %s | Driver %s ... ", s, j, i))
      
      # Blocco protettissimo tramite tryCatch: 
      data <- tryCatch({
        load_driver_telemetry(
          season = s,
          round = j, 
          session = session_type,
          driver = i,  
          laps = "fastest"     
        )
      }, error = function(e){
        return(NULL)
      })
      
      # Controllo se i dati sono stati scaricati ed esiste la struttura
      if (is.null(data) || nrow(data) == 0) {
        cat("ERRORE (dati mancanti)\n")
        fallimenti <- c(fallimenti, paste("S", s, " - R", j, "-", i))
        
        # Aggiungiamo riga coerente pre-riempita di NA
        lista_dati[[counter]] <- tibble(
          season = as.integer(s),
          session = session_type,
          round = as.integer(j),
          name = i,
          final_time = NA_real_,
          speed_mean = NA_real_,
          speed_max = NA_real_,
          speed_min = NA_real_,
          speed_std = NA_real_,
          speed_range = NA_real_,
          rpm_mean = NA_real_,
          rpm_max = NA_real_,
          rpm_std = NA_real_,
          throttle_mean = NA_real_,
          throttle_std = NA_real_,
          throttle_full_ratio = NA_real_,
          brake_ratio = NA_real_,
          gear_mean = NA_real_,
          gear_std = NA_real_,
          distance_total = NA_real_
        )
        
      } else {
        cat("OK\n")
        
        # Gestione affidabile variabile freni ("brake")
        data <- data %>%
          mutate(
            brake_num = suppressWarnings(as.numeric(brake)),
            brake_active = if_else(!is.na(brake_num) & brake_num > 0, 1, 0)
          )
        
        # Riga aggregata di feature per giro di qualifica target 
        riga_aggregata <- data %>%
          summarise(
            season = as.integer(s),
            session = session_type,
            round = as.integer(j),
            name = i,
            
            final_time = as.numeric(max(time, na.rm = TRUE)),
            
            speed_mean = mean(speed, na.rm = TRUE),
            speed_max = max(speed, na.rm = TRUE),
            speed_min = min(speed, na.rm = TRUE),
            speed_std = sd(speed, na.rm = TRUE),
            
            rpm_mean = mean(rpm, na.rm = TRUE),
            rpm_max = max(rpm, na.rm = TRUE),
            rpm_std = sd(rpm, na.rm = TRUE),
            
            throttle_mean = mean(throttle, na.rm = TRUE),
            throttle_std = sd(throttle, na.rm = TRUE),
            throttle_full_ratio = sum(throttle >= 95, na.rm = TRUE) / n(),
            
            brake_ratio = sum(brake_active == 1, na.rm = TRUE) / n(),
            
            gear_mean = mean(n_gear, na.rm = TRUE),
            gear_std = sd(n_gear, na.rm = TRUE),
            
            distance_total = if("distance" %in% colnames(.)) max(distance, na.rm = TRUE) else NA_real_
          ) %>%
          mutate(
            speed_range = speed_max - speed_min
          )
        
        lista_dati[[counter]] <- riga_aggregata
      }
      
      counter <- counter + 1
    }
  }
}

# --- 4. COSTRUZIONE DATASET MULTI-STAGIONE FINALE ---
df_final <- bind_rows(lista_dati)

# --- 5. CONTROLLI STATISTICI FINALI E PULIZIA ---
cat("\n=======================================================\n")
cat("      RIEPILOGO ESTRAZIONE DATI (2019-2021)\n")
cat("=======================================================\n")

cat("=> Osservazioni totali processate (incluse fallite):", nrow(df_final), "\n")
cat("=> Osservazioni mancanti (NA eliminati):", sum(is.na(df_final$final_time)), "\n")
if(length(fallimenti) > 0){
  cat("=> Totale Pilota-Round assenti:", length(fallimenti), "\n")
}

# Eliminiamo le righe "bucate"
df_clean <- df_final %>% filter(!is.na(final_time))

cat("=> Dimensione dataset PULITO (righe x colonne):", nrow(df_clean), "x", ncol(df_clean), "\n")
cat("\n=> Nomi colonne definitive:\n", paste(colnames(df_clean), collapse = ", "), "\n")

cat("\n=======================================================\n")
cat("      RIPARTIZIONE E DETTAGLI DATI ESISTENTI\n")
cat("=======================================================\n")

cat("\n=> Numero di osservazioni per stagione:\n")
print(table(df_clean$season))

cat("\n=> Numero di ROUND per stagione (almeno 1 pilota per giro completo):\n")
print(tapply(df_clean$round, df_clean$season, function(x) length(unique(x))))

cat("\n=> Numero di osservazioni raccolte per singolo PILOTA:\n")
print(table(df_clean$name))

# --- 6. ESPORTAZIONE CSV DEFINITIVA ---
file_out <- "final_dataset_multi_season.csv"
write_csv(df_clean, file_out)
cat(sprintf("\n>>> Il dataset multi-stagione è stato SALVATO in locale come: '%s' <<<\n", file_out))

# --- 7. MODELLO BASE PER VERIFICA ---
cat("\n=======================================================\n")
cat("        MODELLO APPLICATIVO LINEARE SEMPLICE (LM)\n")
cat("=======================================================\n")

model_lm <- lm(
  final_time ~ speed_mean + speed_range + rpm_mean + 
    throttle_full_ratio + brake_ratio + gear_mean, 
  data = df_clean
)
print(summary(model_lm))
