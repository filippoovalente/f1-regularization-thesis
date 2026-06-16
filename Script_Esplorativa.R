# =============================================================================
# ANALISI DESCRITTIVA ED ESPLORATIVA — Tesi F1 Telemetria
# Capitolo 3: Descrizione dei dati e analisi esplorativa
# =============================================================================

library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(scales)

# ---- Tema grafico uniforme --------------------------------------------------

theme_tesi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(colour = "grey40", size = base_size - 1,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(colour = "grey55", size = base_size - 2,
                                      hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(size = base_size - 1, colour = "grey30"),
      axis.text        = element_text(size = base_size - 2, colour = "grey40"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(size = base_size - 1),
      legend.text      = element_text(size = base_size - 2),
      plot.margin      = margin(12, 12, 8, 12)
    )
}

# Palette colori
COL_STAGIONI <- c("2019" = "#E8384F", "2020" = "#3B82C4", "2021" = "#27AE60")
COL_BASE     <- "#2C3E50"
COL_ACC      <- "#E8384F"

# ---- Caricamento e preprocessing -------------------------------------------

data <- read.csv("DATI COMPLETI.csv", header = TRUE)

data$session        <- NULL
data$distance_total <- NULL
data$round          <- NULL
data$name           <- as.factor(data$name)
data$season         <- as.factor(data$season)
data <- data[-853,]
# Rimozione osservazione anomala (distance_total = 0 → giro non completato)
# Nota: distance_total è già rimossa, ma speed_range = 0 individua la stessa obs
data <- data %>% filter(speed_range > 0)

cat("Dataset finale:", nrow(data), "osservazioni,", ncol(data) - 3, "predittori numerici\n")
cat("Piloti:", nlevels(data$name), "| Stagioni: 2019 (411), 2020 (339), 2021 (433)\n\n")


# =============================================================================
# 3.2.1  VARIABILE RISPOSTA: final_time
# =============================================================================

# ---- Istogramma + densità ---------------------------------------------------

p_hist <- ggplot(data, aes(x = final_time)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = COL_BASE, colour = "white", linewidth = 0.3) +
  geom_density(colour = COL_ACC, linewidth = 0.9) +
  geom_vline(aes(xintercept = median(final_time)),
             linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  annotate("text", x = median(data$final_time) + 1.5, y = Inf,
           label = paste0("Mediana: ", round(median(data$final_time), 2), "s"),
           vjust = 1.5, hjust = 0, size = 3.2, colour = "grey30") +
  labs(title = "Distribuzione del tempo sul giro in qualifica",
       subtitle = "Istogramma con stima della densità kernel",
       x = "Tempo (secondi)", y = "Densità") +
  theme_tesi()
p_hist

# ---- Boxplot per stagione ---------------------------------------------------

p_box_stagione <- ggplot(data, aes(x = season, y = final_time, fill = season)) +
  geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 1.5,
               outlier.fill = "white", outlier.colour = "grey50",
               outlier.stroke = 0.4, linewidth = 0.4) +
  scale_fill_manual(values = COL_STAGIONI, guide = "none") +
  labs(title = "Distribuzione di final_time per stagione",
       x = "Stagione", y = "Tempo (secondi)") +
  theme_tesi()
p_box_stagione

# ---- Distribuzione per circuito (round) — violin ----------------------------
# Solo i round presenti in tutte e tre le stagioni per comparabilità

p_violin_stagione <- ggplot(data, aes(x = season, y = final_time, fill = season)) +
  geom_violin(trim = TRUE, scale = "width", linewidth = 0.3, alpha = 0.85) +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA,
               linewidth = 0.35, colour = "grey30") +
  scale_fill_manual(values = COL_STAGIONI, guide = "none") +
  labs(title = "Distribuzione del tempo per stagione",
       subtitle = "Violin plot con boxplot interno",
       x = "Stagione", y = "Tempo (secondi)") +
  theme_tesi()

p_violin_stagione


  # =============================================================================
# 3.2.2  PREDITTORI NUMERICI — statistiche descrittive e distribuzione
# =============================================================================

predittori_num <- data %>%
  select(where(is.numeric), -final_time) %>%
  names()

# ---- Tabella riassuntiva (per LaTeX / knitr) --------------------------------

tab_desc <- data %>%
  select(all_of(predittori_num)) %>%
  pivot_longer(everything(), names_to = "Variabile", values_to = "val") %>%
  group_by(Variabile) %>%
  summarise(
    Media  = mean(val),
    Mediana = median(val),
    SD     = sd(val),
    Min    = min(val),
    Max    = max(val),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(tab_desc)

# ---- Boxplot affiancati per tutti i predittori (normalizzati) ---------------

p_boxplot_pred <- data %>%
  select(all_of(predittori_num)) %>%
  mutate(across(everything(), scale)) %>%       # z-score per confrontabilità
  pivot_longer(everything(), names_to = "Variabile", values_to = "z") %>%
  mutate(Variabile = fct_reorder(Variabile, z, .fun = median)) %>%
  ggplot(aes(x = Variabile, y = z)) +
  geom_boxplot(fill = COL_BASE, colour = "grey40", alpha = 0.8,
               outlier.size = 0.8, outlier.colour = COL_ACC,
               linewidth = 0.4, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60",
             linewidth = 0.4) +
  coord_flip() +
  labs(title = "Distribuzione dei predittori (standardizzati)",
       x = NULL, y = "Valore standardizzato (z-score)") +
  theme_tesi()

print(p_boxplot_pred)


# =============================================================================
# 3.2.3  CORRELAZIONI
# =============================================================================

# ---- Matrice di correlazione con ggcorrplot --------------------------------

vars_corr <- data %>% select(final_time, all_of(predittori_num))
corr_mat  <- cor(vars_corr, use = "complete.obs")

p_corr <- ggcorrplot(corr_mat,
                     method   = "square",
                     type     = "lower",
                     lab      = TRUE,
                     lab_size = 2.8,
                     colors   = c("#3B82C4", "white", "#E8384F"),
                     outline.color = "white",
                     tl.cex   = 9,
                     tl.col   = "grey30",
                     title    = "Matrice di correlazione — predittori e variabile risposta"
) +
  theme(plot.title = element_text(face = "bold", size = 11,
                                  margin = margin(b = 6)),
        legend.position = "right")

print(p_corr)

# ---- Correlazioni con final_time — lollipop chart --------------------------

corr_ft <- corr_mat["final_time", ] %>%
  as.data.frame() %>%
  rownames_to_column("Variabile") %>%
  rename(r = ".") %>%
  filter(Variabile != "final_time") %>%
  mutate(
    direzione = if_else(r > 0, "Positiva", "Negativa"),
    Variabile = fct_reorder(Variabile, r)
  )

p_lollipop <- ggplot(corr_ft, aes(x = r, y = Variabile, colour = direzione)) +
  geom_segment(aes(x = 0, xend = r, y = Variabile, yend = Variabile),
               linewidth = 0.7) +
  geom_point(size = 3.5) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.4) +
  scale_colour_manual(values = c("Positiva" = COL_ACC, "Negativa" = "#3B82C4"),
                      name = "Direzione") +
  scale_x_continuous(limits = c(-0.5, 0.85), breaks = seq(-0.4, 0.8, 0.2)) +
  labs(title = "Correlazione di Pearson con final_time",
       x = "Correlazione (r)", y = NULL) +
  theme_tesi() +
  theme(legend.position = "right")

print(p_lollipop)


# =============================================================================
# 3.2.4  ANALISI PER PILOTA
# =============================================================================

# ---- Tempo medio per pilota (dot plot) -------------------------------------

tab_pilota <- data %>%
  group_by(name) %>%
  summarise(
    media    = mean(final_time),
    mediana  = median(final_time),
    sd       = sd(final_time),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(name = fct_reorder(name, media))

p_pilota <- ggplot(tab_pilota, aes(x = media, y = name)) +
  geom_errorbarh(aes(xmin = media - sd, xmax = media + sd),
                 height = 0.35, colour = "grey70", linewidth = 0.5) +
  geom_point(aes(size = n), colour = COL_ACC, alpha = 0.85) +
  scale_size_continuous(range = c(2, 6), name = "N osservazioni") +
  labs(title = "Tempo medio in qualifica per pilota",
       subtitle = "Punto = media; barre = ±1 deviazione standard",
       x = "Tempo medio (secondi)", y = NULL,
       caption = "Piloti con poche osservazioni riflettono sostituzioni o stagioni parziali") +
  theme_tesi() +
  theme(legend.position = "right")

print(p_pilota)

# ---- Numero osservazioni per pilota ----------------------------------------

p_nobs <- ggplot(tab_pilota, aes(x = n, y = name, fill = n)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "grey80", high = COL_BASE, guide = "none") +
  geom_text(aes(label = n), hjust = -0.2, size = 3, colour = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Numero di osservazioni per pilota",
       x = "N osservazioni", y = NULL) +
  theme_tesi()

print(p_nobs)


# =============================================================================
# 3.2.5  RELAZIONI TRA PREDITTORI E final_time (scatter selezionati)
# =============================================================================

# Le 4 variabili con correlazione assoluta più alta
top_vars <- corr_ft %>%
  arrange(desc(abs(r))) %>%
  slice_head(n = 4) %>%
  pull(Variabile) %>%
  as.character()   # <-- aggiunta questa riga

scatter_list <- map(top_vars, function(v) {
  r_val <- round(corr_mat["final_time", v], 3)
  ggplot(data, aes(x = .data[[v]], y = final_time, colour = season)) +
    geom_point(alpha = 0.35, size = 0.9) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                colour = COL_BASE, fill = "grey80", linewidth = 0.8) +
    scale_colour_manual(values = COL_STAGIONI, name = "Stagione") +
    labs(title = v,
         subtitle = paste0("r = ", r_val),
         x = v, y = "final_time (s)") +
    theme_tesi() +
    theme(legend.position = "none", plot.title = element_text(size = 10))
})

p_scatter <- wrap_plots(scatter_list, ncol = 2) +
  plot_annotation(
    title    = "Scatter plot: predittori più correlati con final_time",
    subtitle = "Retta OLS con intervallo di confidenza al 95%; punti colorati per stagione",
    theme    = theme(plot.title    = element_text(face = "bold", size = 12),
                     plot.subtitle = element_text(colour = "grey40", size = 10))
  )

print(p_scatter)

m1 <- lm(final_time ~., data = data)
summary(m1)

# =============================================================================
# 3.2.6  MULTICOLLINEARITA' TRA PREDITTORI
# =============================================================================

# ---- VIF (Variance Inflation Factor) ---------------------------------------

library(car)

m_vif <- lm(final_time ~ ., data = data %>%
              select(where(is.numeric), -speed_range))

vif_vals <- vif(m_vif)
tab_vif <- tibble(
  Variabile = names(vif_vals),
  VIF       = round(vif_vals, 2)
) %>%
  mutate(
    Livello   = case_when(
      VIF < 5  ~ "Basso (< 5)",
      VIF < 10 ~ "Moderato (5–10)",
      TRUE     ~ "Alto (> 10)"
    ),
    Variabile = fct_reorder(Variabile, VIF)
  )

print(tab_vif)

p_vif <- ggplot(tab_vif, aes(x = VIF, y = Variabile, fill = Livello)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = c(5, 10), linetype = "dashed",
             colour = c("#E8A838", "#E8384F"), linewidth = 0.6) +
  annotate("text", x = 5.3, y = 0.7, label = "VIF = 5",
           colour = "#E8A838", size = 3, hjust = 0) +
  annotate("text", x = 10.3, y = 0.7, label = "VIF = 10",
           colour = "#E8384F", size = 3, hjust = 0) +
  scale_fill_manual(values = c("Basso (< 5)"      = "#27AE60",
                               "Moderato (5–10)"  = "#E8A838",
                               "Alto (> 10)"      = "#E8384F"),
                    name = "Livello VIF") +
  labs(title = "Variance Inflation Factor per predittore",
       subtitle = "Calcolato sul modello OLS senza variabili categoriali",
       x = "VIF", y = NULL) +
  theme_tesi() +
  theme(legend.position = "right")

print(p_vif)


# =============================================================================
# SALVATAGGIO GRAFICI (opzionale — decommentare per uso in LaTeX)
# =============================================================================

ggsave("fig3_1_distribuzione_finaltime.pdf", 
       p_hist,        width=7, height=4.5, device=cairo_pdf)
ggsave("fig3_2_boxplot_stagione.pdf",        
       p_box_stagione, width=5, height=4,   device=cairo_pdf)
ggsave("fig3_3_violin_stagione.pdf",        
       p_violin_stagione, width=5, height=4,   device=cairo_pdf)
ggsave("fig3_4_boxplot_predittori.pdf",      
       p_boxplot_pred, width=7, height=5,   device=cairo_pdf)
ggsave("fig3_5_matrice_corr.pdf",            
       p_corr,         width=8, height=7,   device=cairo_pdf)
ggsave("fig3_6_lollipop_corr.pdf",           
       p_lollipop,     width=7, height=5,   device=cairo_pdf)
ggsave("fig3_7_tempo_per_pilota.pdf",        
       p_pilota,       width=7, height=7,   device=cairo_pdf)
ggsave("fig3_8_nobs_pilota.pdf",             
       p_nobs,         width=6, height=7,   device=cairo_pdf)
ggsave("fig3_9_scatter_top4.pdf",            
       p_scatter,      width=10, height=9,   device=cairo_pdf)
ggsave("fig3_10_vif.pdf",                    
       p_vif,          width=7, height=5,   device=cairo_pdf)