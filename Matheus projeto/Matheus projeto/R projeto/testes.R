# teste do arima
{
#| echo: false
#| message: false
#| warning: false

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# 1. CARGA E LIMPEZA DO SPOT (CEPEA)
df_spot <- read.csv2("cepea-consulta-20260320091531.csv", skip = 4, header = FALSE) %>%
  rename(mes_ano = V1, preco_spot = V2) %>%
  mutate(data = my(mes_ano),
         preco_spot = as.numeric(preco_spot)) %>%
  filter(!is.na(data)) %>%
  arrange(data)

# 2. CARGA E LIMPEZA DO FUTURO (INVESTING/CHICAGO)
# Corrigindo a ordem cronológica aqui com arrange(data)
df_futuro <- read.csv("Dados Históricos - Milho Chicago Futuros.csv", sep=",", dec=",") %>%
  mutate(data = dmy(Data), 
         preco_futuro = as.numeric(Último)) %>%
  filter(!is.na(data)) %>%
  arrange(data) %>%
  select(data, preco_futuro)

# 3. UNIÃO DAS SÉRIES (INNER JOIN)
df_comparativo <- df_spot %>%
  inner_join(df_futuro, by = "data") %>%
  mutate(base = preco_spot - preco_futuro)

# 4. GRÁFICO 1: OS DOIS JUNTOS
# Transformando para formato longo para o ggplot
df_longo <- df_comparativo %>%
  select(data, preco_spot, preco_futuro) %>%
  pivot_longer(cols = c(preco_spot, preco_futuro), 
               names_to = "Tipo", values_to = "Valor")

ggplot(df_longo, aes(x = data, y = Valor, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("preco_spot" = "darkgreen", "preco_futuro" = "darkblue"),
                     labels = c("Futuro (B3/Chicago)", "Spot (Cepea)")) +
  scale_x_date(date_labels = "%m/%y", date_breaks = "4 months") +
  labs(title = "Comparativo: Preço Spot vs. Preço Futuro",
       subtitle = "Alinhamento cronológico das séries",
       x = "Período", y = "R$/Saca", color = "Série") +
  theme_minimal()`


######################

#| echo: false
#| message: false
#| warning: false

# 5. GRÁFICO 2: RISCO DE BASE
ggplot(df_comparativo, aes(x = data, y = base)) +
  geom_area(fill = "purple", alpha = 0.3) +
  geom_line(color = "purple", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(date_labels = "%m/%y", date_breaks = "4 months") +
  labs(title = "Análise do Risco de Base",
       subtitle = "Cálculo: Preço Spot - Preço Futuro",
       x = "Período", y = "Valor da Base (R$)") +
  theme_minimal()
}
###########################
{
#| echo: false
#| message: false
#| warning: false

library(dplyr)
library(ggplot2)
library(lubridate)
library(fBasics) 

# 1. CARGA E ALINHAMENTO (Essencial para o risco de base)
# Spot (Cepea)
df_spot <- read.csv2("cepea-consulta-20260320091531.csv", skip = 4, header = FALSE) %>%
  rename(mes_ano = V1, preco_spot = V2) %>%
  mutate(data = my(mes_ano), preco_spot = as.numeric(preco_spot)) %>%
  filter(!is.na(data)) %>%
  arrange(data)

# Futuro (Arquivo que você subiu)
df_futuro <- read.csv("Dados Históricos - Milho Chicago Futuros.csv", sep=",", dec=",") %>%
  mutate(data = dmy(Data), preco_futuro = as.numeric(Último)) %>%
  filter(!is.na(data)) %>%
  arrange(data) %>%
  select(data, preco_futuro)

# Criando a variável de Risco de Base
df_base <- df_spot %>%
  inner_join(df_futuro, by = "data") %>%
  mutate(valor_base = preco_spot - preco_futuro)

# --- ANÁLISE ESTATÍSTICA DO RISCO DE BASE ---

# A. Histograma de Distribuição da Base
# Se o histograma estiver centrado no 0, o hedge é muito eficiente.
hist(df_base$valor_base, breaks = "Sturges", col = "purple", 
     main = "Distribuição do Risco de Base (Spot - Futuro)", 
     xlab = "Valor da Base (R$)", ylab = "Frequência")

# B. Cálculo de Volatilidade da Base
# O desvio padrão aqui mede a incerteza do hedge.
sd_base <- sd(df_base$valor_base, na.rm = TRUE)
mean_base <- mean(df_base$valor_base, na.rm = TRUE)
cv_base <- (sd_base / abs(mean_base)) * 100

print(paste("Base Média (Bias): ", round(mean_base, 2)))
print(paste("Desvio Padrão da Base (Risco de Base): ", round(sd_base, 2)))
print(paste("Coeficiente de Variação da Base: ", round(cv_base, 2), "%"))

# C. Teste de Normalidade (Jarque-Bera) para a Base
# Verifica se os desvios da base são imprevisíveis (normais)
jb_teste <- jarqueberaTest(df_base$valor_base)
print(jb_teste)
}
##########################
{
#| echo: false
#| message: false
#| warning: false

library(dplyr)
library(lubridate)
library(urca)      
library(forecast)  
library(tidyr)
library(ggplot2)

# 1. CARGA E LIMPEZA UNIFICADA (Spot e Futuro)
df_spot <- read.csv2("cepea-consulta-20260320091531.csv", skip = 4, header = FALSE) %>%
  rename(mes_ano = V1, preco = V2) %>%
  mutate(data = my(mes_ano), preco = as.numeric(preco)) %>%
  filter(!is.na(data), !is.na(preco)) %>%
  arrange(data)

df_futuro <- read.csv("Dados Históricos - Milho Chicago Futuros.csv", sep=",", dec=",") %>%
  mutate(data = dmy(Data), preco = as.numeric(Último)) %>%
  filter(!is.na(data), !is.na(preco)) %>%
  arrange(data) %>%
  select(data, preco)

# 2. CRIAÇÃO DAS SÉRIES TEMPORAIS (LOG)
# Spot
ts_spot_log <- ts(log(df_spot$preco), frequency = 12, 
                  start = c(year(min(df_spot$data)), month(min(df_spot$data))))

# Futuro (Alinhado com o Spot)
ts_futuro_log <- ts(log(df_futuro$preco), frequency = 12, 
                    start = c(year(min(df_futuro$data)), month(min(df_futuro$data))))

# 3. ESTIMAÇÃO DOS MODELOS ARIMA(1,1,3)
# Ajustamos o mesmo modelo para ambas as séries para comparar a trajetória
fit_spot <- Arima(ts_spot_log, order = c(1, 1, 3), method = "ML")
fit_futuro <- Arima(ts_futuro_log, order = c(1, 1, 3), method = "ML")

# 4. GERAÇÃO DAS PREVISÕES (h = 3 meses)
prev_spot <- forecast(fit_spot, h = 3)
prev_futuro <- forecast(fit_futuro, h = 3)

# 5. GRÁFICO DAS DUAS PREVISÕES JUNTAS
# Usamos o autoplot do pacote forecast para um visual moderno e limpo
autoplot(ts_spot_log, series = "Histórico Spot (Cepea)") +
  autolayer(ts_futuro_log, series = "Histórico Futuro (B3)") +
  autolayer(prev_spot, series = "Prev. Spot", PI = FALSE, linetype = "dashed") +
  autolayer(prev_futuro, series = "Prev. Futuro", PI = FALSE, linetype = "dashed") +
  labs(title = "Projeção ARIMA(1,1,3): Spot vs Futuro",
       subtitle = "Previsão para os próximos 3 meses (em Log)",
       x = "Anos", y = "Preço (Log)",
       colour = "Séries") +
  theme_minimal() +
  scale_color_manual(values = c("Histórico Spot (Cepea)" = "darkgreen", 
                                "Histórico Futuro (B3)" = "darkblue",
                                "Prev. Spot" = "darkgreen", 
                                "Prev. Futuro" = "darkblue"))

# 6. ANÁLISE DE RETORNO E RISCO (COMPARATIVO SD)
sd_spot <- sd(df_spot$preco)
# Precisamos unir para calcular o SD da base corretamente
df_base_calc <- df_spot %>% 
  inner_join(df_futuro, by = "data", suffix = c("_s", "_f")) %>%
  mutate(base = preco_s - preco_f)

sd_base_val <- sd(df_base_calc$base)

cat("\n--- Comparativo de Risco ---\n")
cat("Volatilidade do Mercado (SD Spot): R$", round(sd_spot, 2), "\n")
cat("Volatilidade do Hedge (SD Base): R$", round(sd_base_val, 2), "\n")
cat("Eficiência: Redução de", round((1 - (sd_base_val/sd_spot))*100, 2), "% no risco operacional.\n")
}

{#| echo: false
  #| message: false
  #| warning: false
  #| results: 'hide'
  
  library(dplyr)
  library(lubridate)
  library(urca)      # Testes de Raiz Unitária
  library(forecast)  # Modelos ARIMA e Previsão
  
  # 1. CARGA E LIMPEZA (Garantindo que o preço seja numérico)
  nome_arq <- "cepea-consulta-20260320091531.csv"
  dados_brutos <- read.csv2(nome_arq, skip = 4, header = FALSE)
  
  df_limpo <- dados_brutos %>%
    rename(mes_ano = V1, preco = V2) %>%
    mutate(
      data = my(mes_ano),
      preco = as.numeric(preco) # Garante que o log() funcione
    ) %>%
    filter(!is.na(data), !is.na(preco))
  
  # 2. CRIAR SÉRIE TEMPORAL E LOG (Usando o df_limpo correto)
  # Note: Usamos df_limpo$preco agora
  log_milho <- ts(log(df_limpo$preco), frequency = 12, 
                  start = c(year(min(df_limpo$data)), month(min(df_limpo$data))))
  
  # 3. TESTES DE RAIZ UNITÁRIA (Dickey-Fuller - Os 3 modelos que você usa)
  # Modelo 1: Sem intercepto e sem tendência
  dickey1 <- ur.df(log_milho, type = "none", lags = 0)
  summary(dickey1)
  
  # Modelo 2: Com intercepto (drift)
  dickey2 <- ur.df(log_milho, type = "drift", lags = 0)
  summary(dickey2)
  
  # Modelo 3: Com intercepto e tendência
  dickey3 <- ur.df(log_milho, type = "trend", lags = 0)
  summary(dickey3)
  
  # 4. IDENTIFICAÇÃO (FAC e FAPC)
  par(mfrow=c(1,2))
  acf(log_milho, lag.max = 36, main="FAC Log Milho", col="red")
  pacf(log_milho, lag.max = 36, main="FAPC Log Milho", col="red")
  par(mfrow=c(1,1))
  
  # 5. TESTES DE AUTOCORRELAÇÃO (Ljung-Box)
  Box.test(log_milho, lag = 1, type = "Ljung-Box")
  Box.test(log_milho, lag = 12, type = "Ljung-Box")
  
  # 6. ESTIMAÇÃO DO SEU MODELO ESPECÍFICO ARIMA(1,1,3)
  Arima113 <- Arima(log_milho, order = c(1, 1, 3), method = "ML")
  print("--- Coeficientes Arima 113 ---")
  print(Arima113$coef)
  
  # Diagnóstico dos resíduos (O gráfico que você pediu)
  tsdiag(Arima113)
  
  # 7. ACURÁCIA DO MODELO
  accuracy(Arima113)
  
  # 8. Gerar a previsão para os próximos 3 meses (h = 3)
  # Usamos a função forecast para facilitar a plotagem gráfica
  previsao_grafico <- forecast(Arima113, h = 3)
  
  # 9. Plotar o gráfico da série em LOG (Previsão do modelo)
  plot(previsao_grafico, 
       main = "Previsão do Preço do Milho - Modelo ARIMA(1,1,3)",
       xlab = "Anos", 
       ylab = "Preço (em Log)",
       col = "black", 
       fcol = "blue", # Cor da linha da previsão
       shadecols = "lightblue") # Cor da área de incerteza
}

{#| echo: false
  #| message: false
  #| warning: false
  
  library(dplyr)
  library(lubridate)
  library(urca)      
  library(forecast)  
  library(ggplot2)
  
  # 1. CARGA E LIMPEZA (Spot e Futuro)
  df_spot <- read.csv2("cepea-consulta-20260320091531.csv", skip = 4, header = FALSE) %>%
    rename(mes_ano = V1, preco = V2) %>%
    mutate(data = my(mes_ano), preco = as.numeric(preco)) %>%
    filter(!is.na(data), !is.na(preco)) %>%
    arrange(data)
  
  df_futuro <- read.csv("Dados Históricos - Milho Chicago Futuros.csv", sep=",", dec=",") %>%
    mutate(data = dmy(Data), preco = as.numeric(Último)) %>%
    filter(!is.na(data), !is.na(preco)) %>%
    arrange(data) %>%
    select(data, preco)
  
  # 2. CRIAR SÉRIES TEMPORAIS EM LOG
  log_spot <- ts(log(df_spot$preco), frequency = 12, 
                 start = c(year(min(df_spot$data)), month(min(df_spot$data))))
  
  log_futuro <- ts(log(df_futuro$preco), frequency = 12, 
                   start = c(year(min(df_futuro$data)), month(min(df_futuro$data))))
  
  # 3. ESTIMAÇÃO DOS MODELOS ARIMA(1,1,3)
  fit_spot <- Arima(log_spot, order = c(1, 1, 3), method = "ML")
  fit_futuro <- Arima(log_futuro, order = c(1, 1, 3), method = "ML")
  
  # --- DIAGNÓSTICO DO MODELO SPOT ---
  # Gera: Resíduos, ACF dos Resíduos e P-valores do Ljung-Box
  print("--- Diagnóstico de Resíduos: Preço Spot ---")
  tsdiag(fit_spot)
  
  # 4. GERAÇÃO DAS PREVISÕES (h = 3 meses)
  prev_spot <- forecast(fit_spot, h = 3)
  prev_futuro <- forecast(fit_futuro, h = 3)
  
  # 5. GRÁFICO DAS DUAS PREVISÕES JUNTAS
  # Usando autoplot para combinar as séries e as áreas de previsão
  autoplot(log_spot, series = "Spot (Cepea)") +
    autolayer(log_futuro, series = "Futuro (B3)") +
    autolayer(prev_spot, series = "Prev. Spot", PI = TRUE, alpha = 0.2) +
    autolayer(prev_futuro, series = "Prev. Futuro", PI = TRUE, alpha = 0.2) +
    scale_color_manual(values = c("Spot (Cepea)" = "darkgreen", 
                                  "Futuro (B3)" = "darkblue",
                                  "Prev. Spot" = "darkgreen", 
                                  "Prev. Futuro" = "darkblue")) +
    scale_fill_manual(values = c("Prev. Spot" = "green", 
                                 "Prev. Futuro" = "blue")) +
    labs(title = "Previsão Conjunta ARIMA(1,1,3): Spot vs Futuro",
         subtitle = "Séries em Logaritmo com bandas de confiança",
         x = "Anos", y = "Preço (Log)") +
    theme_minimal()}