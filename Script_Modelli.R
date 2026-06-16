library(glmnet)
library(tidyverse)
citation("glmnet")

data <- read.csv("DATI COMPLETI.csv", header = TRUE)
data
str(data)
data$session        <- NULL
data$distance_total <- NULL
data$round          <- NULL
data$name           <- as.factor(data$name)
data$season         <- as.factor(data$season)
data$speed_range <- NULL
data <- data[-853,]
set.seed(100)
index <- sample(nrow(data), 0.75*nrow(data))
train <- data[index,]
test <- data[-index,]

quantile(data$final_time, probs = c(0.25, 0.50, 0.75))

# OLS ---------------------------------------------------------------------

m0 <- lm(final_time ~., data = train)
summary(m0)
fit_train <- predict(m0, newdata = train)
rmse_OLS_train <- sqrt(mean((fit_train - train$final_time)^2))
rmse_OLS_train

fit_test  <- predict(m0,newdata = test)
rmse_OLS_test <- sqrt(mean((test$final_time - fit_test)^2))
rmse_OLS_test
par(mfrow = c(2,2
              ))
plot(m0, which = 1:4)

data[105,]

# LASSO -------------------------------------------------------------------

X_train <- model.matrix(final_time ~ ., data = train)[, -1]
X_test  <- model.matrix(final_time ~ ., data = test)[, -1]
y_train <- train$final_time
y_test  <- test$final_time

cv_lasso  <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
coef(cv_lasso)

fit_lasso_train <- predict(cv_lasso, newx = X_train, s = "lambda.1se")
rmse_lasso_train <- sqrt(mean((fit_lasso_train - y_train)^2))

fit_lasso_test <- predict(cv_lasso, newx = X_test, s = "lambda.1se")
rmse_lasso_test  <- sqrt(mean((fit_lasso_test  - y_test)^2))

rmse_lasso_train
rmse_lasso_test

# ---- Lasso Relax ----
sd(data$final_time)
cv_lasso_relax  <- cv.glmnet(X_train, y_train, alpha = 1, 
                             nfolds = 10, relax = TRUE)
coef(cv_lasso_relax)

fit_lasso_train_relax <- predict(cv_lasso_relax, newx = X_train, 
                                 s = "lambda.1se")
rmse_lasso_train_relax <- sqrt(mean((fit_lasso_train_relax - y_train)^2))

fit_lasso_test_relax <- predict(cv_lasso_relax, newx = X_test, s = "lambda.1se")
rmse_lasso_test_relax  <- sqrt(mean((fit_lasso_test_relax  - y_test)^2))

rmse_lasso_train_relax
rmse_lasso_test_relax


# RIDGE -------------------------------------------------------------------

cv_ridge  <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)

fit_ridge_train <- predict(cv_ridge, newx = X_train, s = "lambda.1se")
rmse_ridge_train <- sqrt(mean((fit_ridge_train - y_train)^2))

fit_ridge_test <- predict(cv_ridge, newx = X_test, s = "lambda.1se")
rmse_ridge_test  <- sqrt(mean((fit_ridge_test  - y_test)^2))

rmse_ridge_train
rmse_ridge_test


# Elastic Net -------------------------------------------------------------

cv_el  <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 10)


fit_el_train <- predict(cv_el, newx = X_train, s = "lambda.1se")
rmse_el_train <- sqrt(mean((fit_el_train - y_train)^2))

fit_el_test <- predict(cv_el, newx = X_test, s = "lambda.1se")
rmse_el_test  <- sqrt(mean((fit_el_test  - y_test)^2))

rmse_el_train
rmse_el_test


# Cross-Validation su tutto il dataset ----------------------------------
K <- 10

X_full <- model.matrix(final_time ~ ., data = data)[, -1]
y_full <- data$final_time

# stratificazione per pilota
folds <- integer(nrow(data))
for (lev in levels(data$name)) {
  idx <- which(data$name == lev)
  idx <- idx[sample(length(idx))]
  folds[idx] <- ((seq_along(idx) - 1) %% K) + 1
}

rmse_val <- matrix(NA, K, 10)
rmse_tr  <- matrix(NA, K, 10)

for (k in 1:K) {
  
  idx_val <- which(folds == k)
  X_tr <- X_full[-idx_val, ]              
  y_tr <- y_full[-idx_val]
  X_va <- X_full[idx_val, , drop = FALSE] 
  y_va <- y_full[idx_val]
  
  df_tr <- data.frame(final_time = y_tr, X_tr)
  df_va <- data.frame(X_va)
  
  ols_k <- lm(final_time ~ ., data = df_tr)
  rmse_val[k, 1] <- sqrt(mean((y_va - predict(ols_k, newdata = df_va))^2, na.rm = TRUE))
  rmse_tr[k,  1] <- sqrt(mean((y_tr - predict(ols_k, newdata = df_tr))^2, na.rm = TRUE))
  
  cv_lasso_k <- cv.glmnet(X_tr, y_tr, alpha = 1, nfolds = 5)
  rmse_val[k, 2] <- sqrt(mean((y_va - predict(cv_lasso_k, newx = X_va, s = "lambda.min"))^2))
  rmse_val[k, 3] <- sqrt(mean((y_va - predict(cv_lasso_k, newx = X_va, s = "lambda.1se"))^2))
  rmse_tr[k,  2] <- sqrt(mean((y_tr - predict(cv_lasso_k, newx = X_tr, s = "lambda.min"))^2))
  rmse_tr[k,  3] <- sqrt(mean((y_tr - predict(cv_lasso_k, newx = X_tr, s = "lambda.1se"))^2))
  
  cv_relax_k <- cv.glmnet(X_tr, y_tr, alpha = 1, nfolds = 5, relax = TRUE)
  rmse_val[k, 4] <- sqrt(mean((y_va - predict(cv_relax_k, newx = X_va, s = "lambda.min"))^2))
  rmse_val[k, 5] <- sqrt(mean((y_va - predict(cv_relax_k, newx = X_va, s = "lambda.1se"))^2))
  rmse_tr[k,  4] <- sqrt(mean((y_tr - predict(cv_relax_k, newx = X_tr, s = "lambda.min"))^2))
  rmse_tr[k,  5] <- sqrt(mean((y_tr - predict(cv_relax_k, newx = X_tr, s = "lambda.1se"))^2))
  
  cv_ridge_k <- cv.glmnet(X_tr, y_tr, alpha = 0, nfolds = 5)
  rmse_val[k, 6] <- sqrt(mean((y_va - predict(cv_ridge_k, newx = X_va, s = "lambda.min"))^2))
  rmse_val[k, 7] <- sqrt(mean((y_va - predict(cv_ridge_k, newx = X_va, s = "lambda.1se"))^2))
  rmse_tr[k,  6] <- sqrt(mean((y_tr - predict(cv_ridge_k, newx = X_tr, s = "lambda.min"))^2))
  rmse_tr[k,  7] <- sqrt(mean((y_tr - predict(cv_ridge_k, newx = X_tr, s = "lambda.1se"))^2))
  
  cv_el_k <- cv.glmnet(X_tr, y_tr, alpha = 0.5, nfolds = 5)
  rmse_val[k, 8] <- sqrt(mean((y_va - predict(cv_el_k, newx = X_va, s = "lambda.min"))^2))
  rmse_val[k, 9] <- sqrt(mean((y_va - predict(cv_el_k, newx = X_va, s = "lambda.1se"))^2))
  rmse_tr[k,  8] <- sqrt(mean((y_tr - predict(cv_el_k, newx = X_tr, s = "lambda.min"))^2))
  rmse_tr[k,  9] <- sqrt(mean((y_tr - predict(cv_el_k, newx = X_tr, s = "lambda.1se"))^2))
  
  step_full_k <- lm(final_time ~ ., data = df_tr)
  step_null_k <- lm(final_time ~ 1, data = df_tr)
  step_k <- step(step_null_k,
                 scope = list(lower = step_null_k, upper = step_full_k),
                 direction = "both", trace = 0)
  rmse_val[k, 10] <- sqrt(mean((y_va - predict(step_k, newdata = df_va))^2, na.rm = TRUE))
  rmse_tr[k,  10] <- sqrt(mean((y_tr - predict(step_k, newdata = df_tr))^2, na.rm = TRUE))
}

?cv.glmnet

modelli <- c("OLS", "LASSO (min)", "LASSO (1se)",
             "Relax (min)", "Relax (1se)",
             "Ridge (min)", "Ridge (1se)",
             "Elastic-Net (min)", "Elastic-Net (1se)",
             "Stepwise OLS")

cv_results <- data.frame(
  Modello    = modelli,
  RMSE_medio = colMeans(rmse_val),
  RMSE_sd    = apply(rmse_val, 2, sd)
)
cv_results


overfitting <- data.frame(
  Modello         = modelli,
  RMSE_training   = colMeans(rmse_tr),
  RMSE_validation = colMeans(rmse_val)
)
overfitting$Gap <- overfitting$RMSE_validation - overfitting$RMSE_training
gap_ols <- overfitting$Gap[overfitting$Modello == "OLS"]
overfitting$Gap_rel_OLS <- (overfitting$Gap - gap_ols) / gap_ols * 100
overfitting

sd(data$final_time)
# Grafico
ov_long <- pivot_longer(overfitting, cols = c(RMSE_training, RMSE_validation),
                        names_to = "Tipo", values_to = "RMSE")
ov_plot <- subset(ov_long, grepl("1se|OLS", Modello))

ggplot(ov_plot, aes(x = Modello, y = RMSE, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("RMSE_training" = "blue", "RMSE_validation" = "orange"),
                    labels = c("Training", "Validation")) +
  labs(title = "RMSE medio: Training vs Validation (10-Fold CV)", x = NULL, 
       y = "RMSE medio", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


# BARPLOT GAP RELATIVO ----------------------------------------------------

gap_plot <- overfitting
gap_plot$ref <- ifelse(gap_plot$Modello == "OLS", "OLS", "Penalizzati / Stepwise")

gap_plot$Modello <- factor(gap_plot$Modello,
                           levels = gap_plot$Modello[order(gap_plot$Gap)])

ggplot(gap_plot, aes(x = Gap, y = Modello, fill = ref)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", Gap)),
            hjust = -0.2, size = 3.3, colour = "grey20") +
  scale_fill_manual(values = c("OLS" = "#E10600",
                               "Penalizzati / Stepwise" = "#1A2B4A")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Divario di overfitting per modello",
       subtitle = expression("Gap = RMSE"["validation"]*" - RMSE"["training"]*"  (10-Fold CV)"),
       x = "Gap (secondi)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40", margin = margin(b = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "top",
    legend.justification = "left"
  )
ggsave("grafico_gap.pdf", width = 8, height = 5, units = "in")
