# Dívida

# Códigos Finais

# UNIVERSIDADE FEDERAL DA PARAÍBA
# CENTRO DE CIENCIAS SOCIAIS APLICADAS
# DEPARTAMENTO DE ECONOMIA
# CURSO DE ECONOMETRIA
# PROF.DR.SINÉZIO FERNANDES MAIA
# ALUNO: MATHEUS AUGUSTO DA SILVA MEDLEY


##########
# DÍVIDA #
##########

# Resultados:

# Último valor da amostra --------------- 10251233
# Pela taxa de crescimento linear: ------ 11388343 - 12651586
# Pela taxa de crescimento geométrica: -- 13251032 - 17128656
# Pelo logarítmo: ----------------------- 11746049 - 13458836
# Pelo Arima: --------------------------- 10402846 - 10344029
# Pelo MQO: ----------------------------- 10186217 -
# Pelo VAR: ----------------------------- 10291764 -                   /10276676
# Observação: Todos os resultados ficam ligeiramente maiores
# quando usamos o deflateBR, se comparado ao uso do numeroindice
# fornecido pelo professor.

# Pacotes

install.packages("rbcb")
install.packages('deflateBR')
install.packages("lmtest")
install.packages("tseries")
install.packages("fBasics")
install.packages("urca")
install.packages("forecast")
install.packages("vars")

library(tseries)
library(urca)


#Obtenção dos dados
{
  {
    rm(list=ls())
    options(scipen=999999)
    options(max.print=100000)
  }
  #install.packages("rbcb")
  library(rbcb)
  datainic <- "2003-01-01" ; datafim <- "2024-12-01"
  
  27810
  moedadata <- get_series(1786, start_date = datainic, end_date = datafim) #M1
  
  
  dividadata <- get_series(4182, start_date = datainic, end_date = datafim)
  primariodata <- get_series(2143, start_date = datainic, end_date = datafim)
  moedadata <- get_series(27810, start_date = datainic, end_date = datafim) #M2
  jurosdata <- get_series(4390, start_date = datainic, end_date = datafim) # Mensal
  m1data  <- get_series(27791, start_date = datainic, end_date = datafim) # Mensal
  
  divida <- ts(dividadata$`4182`, start = c(2003,01), frequency = 12)
  primario <- ts(primariodata$`2143`, start = c(2003,01), frequency = 12)
  moeda <- ts(moedadata$`27810`, start = c(2003,01), frequency = 12)
  juros <- ts(jurosdata$`4390`, start = c(2003,01), frequency = 12)
  m1 <- ts(m1data$`27791`, start = c(2003,01), frequency = 12)
  
  #install.packages('deflateBR')
  library(deflateBR)
  times=seq(as.Date("2003/1/1"), by = "month", length.out = 264); times
  
  divida_deflacionada_by_deflateBR = deflate(divida, nominal_dates = times, real_date = '12/2024', index = 'ipca')
  primario_deflateBR = deflate(primario, nominal_dates = times, real_date = '12/2024', index = 'ipca')
  moeda_deflateBR <- deflate(moeda, nominal_dates = times, real_date = '12/2024', index = 'ipca')
  moeda_deflateBR <- moeda_deflateBR/1000
  # Não faz sentido dedeflacionar os juros juros_deflateBR <- deflate(juros, nominal_dates = times, real_date = '12/2024', index = 'ipca')
  m1_deflateBR <- deflate(m1, nominal_dates = times, real_date = '12/2024', index = 'ipca')
  m1_deflateBR <- m1_deflateBR/1000
  
  divida_nominal_janela = window(divida, frequency=12, start=c(2022,1), end=c(2024,12))
  divida_janela_deflateBR = window(divida_deflacionada_by_deflateBR, frequency=12, start=c(2022,1), end=c(2024,12))
  primario_janela = window(primario_deflateBR, frequency=12, start=c(2022,1), end=c(2024,12))
  moeda_janela = window(moeda_deflateBR, frequency=12, start=c(2022,1), end=c(2024,12))
  juros_janela = window(juros, frequency=12, start=c(2022,1), end=c(2024,12))
  m1_janela = window(m1_deflateBR, frequency=12, start=c(2022,1), end=c(2024,12))
  
  divida_janela_deflateBR
  primario_janela
  moeda_janela
  juros_janela
  m1_janela
  colnames(divida_janela_deflateBR)
  colnames(primario_janela)
  
  
  {
    #a seguir estão alguns métodos poucos ortodoxos (eu cometi erros graves)
    # pode ignorar tranquilarmente, ou aprender com meus erros
    write.table(divida, file = "divida.txt")
    #divida <- read.table("divida.txt",head=T)
    # O deflateBR não funciona se o dado for um .txt, provavelmente eu deveria usar
    # o $ para selecionar a coluna ou algo assim.
    print(divida)
    
    # numero indice
    # Ignora que isso aqui ta errado pq na verdade deixa todos os valores
    # da série iguais quando "deflaciona"
    # #divida. First: 1163160.1, last: 10251233.21
    #dividanumeroindice = (divida * 100) / 10251233.21
    #print(dividanumeroindice)
    
    # o metodo a seguir aumenta a variação dos dados então é inútil
    #ipcadata <- get_series(13522, start_date = datainic, end_date = datafim)
    #ipca <- ts(ipcadata$`13522`, start = c(2003,01), frequency = 12)
    #print(ipca)
    #divida_deflacionada <- (divida/ipca)*4.83 ;tail(divida_deflacionada)
    #divida_deflacionada <- ts(divida_deflacionada, start = c(2003,01), frequency = 12)
    #write.table(divida_deflacionada, file = "divida_deflacionada.txt")
    #print(divida_deflacionada)
    #divida_deflacionada_janela = window(divida_deflacionada, frequency=12, start=c(2022,1), end=c(2024,12))
  }
}

# Análise dos dados
{
  
  #par(mfrow=c(1,1))
  histograma_divida <- hist(divida,breaks="Sturges", col = "green")
  histograma_divida_nominal_janela <- hist(divida_nominal_janela,  breaks = "Sturges", col = "darkgreen")
  histograma_divida_deflacionada_deflateBR <- hist(divida_deflacionada_by_deflateBR, breaks = "Sturges", col = "green")
  histograma_divida_deflacionada_janela_deflateBR <- hist(divida_janela_deflateBR,  breaks = "Sturges", col = "darkgreen")
  
  # Fiquei com preguiça, vou só mostrar a série inteira mesmo
  histograma_primario <- hist(primario,breaks="Sturges", col = "green")
  histograma_moeda <- hist(moeda,breaks="Sturges", col = "green")
  histograma_juros <- hist(juros,breaks="Sturges", col = "green")
  histograma_m1 <- hist(m1,breaks="Sturges", col = "green")
  
  # o argumento breaks="sturges" especifica que o número de intervalos
  # (ou "bins") para o histograma deve ser determinado pela regra de
  # Sturges, que é uma fórmula usada para calcular o número
  # ideal de intervalos com base no tamanho da amostra.
  
  #Desvio padrão
  # Gráfico bonito mostrando o desvio padrão (code by gemini):
  
  library(dplyr)
  sd_divida <- sd(divida)
  sd_divida_nominal_janela <- sd(divida_nominal_janela)
  sd_divida_deflacionada_by_deflateBR <- sd(divida_deflacionada_by_deflateBR)
  sd_divida_janela_deflateBR <- sd(divida_janela_deflateBR)
  sd_primario <- sd(primario)
  sd_primario_deflateBR <- sd(primario_deflateBR)
  sd_primario_janela <- sd(primario_janela)
  sd_moeda <-sd(moeda)
  sd_moeda_deflateBR <- sd(moeda_deflateBR)
  sd_moeda_janela <- sd(moeda_janela)
  sd_juros <-sd(juros)
  sd_juros_janela <- sd(juros_janela)
  
  
  # Crie um data frame com os nomes das séries e seus desvios padrão
  desvios_padrao <- data.frame(
    Serie = c("Dívida Nominal (2003 - 2024)",
              "Dívida Nominal (2022 - 2024)",
              "Dívida Deflacionada (2003 - 2024)",
              "Dívida Deflacionada (2022 - 2024)",
              "Primário Nominal (2003 - 2024)",
              "Primário Deflacionado (2003 - 2024)",
              "Primário Deflacionado (2022 - 2024)",
              "Moeda Nominal (2003 - 2024)",
              "Moeda Deflacionado (2003 - 2024)",
              "Moeda Deflacionado (2022 - 2024)",
              "Juros (2003 - 2024)",
              "Juros (2022 - 2024)"),
    DesvioPadrao = c(sd_divida,
                     sd_divida_nominal_janela,
                     sd_divida_deflacionada_by_deflateBR,
                     sd_divida_janela_deflateBR,
                     sd_primario,
                     sd_primario_deflateBR,
                     sd_primario_janela,
                     sd_moeda,
                     sd_moeda_deflateBR,
                     sd_moeda_janela,
                     sd_juros,
                     sd_juros_janela
    )
  )
  
  # Ordene o data frame pelo desvio padrão para facilitar a visualização (opcional)
  desvios_padrao <- desvios_padrao %>%
    arrange(DesvioPadrao)
  
  # Use a biblioteca ggplot2 para criar o gráfico de barras
  library(ggplot2)
  
  ggplot(desvios_padrao, aes(x = reorder(Serie, DesvioPadrao), y = DesvioPadrao, fill = Serie)) +
    geom_bar(stat = "identity") +
    coord_flip() + # Inverte as barras para melhor leitura dos rótulos longos
    labs(title = "Comparação dos Desvios Padrão das Séries da Dívida",
         x = "Série da Dívida",
         y = "Desvio Padrão") +
    theme_minimal() +
    theme(legend.position = "none") # Remove a legenda pois os rótulos já estão no eixo x
  
  
  # Coeficiente de variação:
  library(dplyr)
  library(ggplot2)
  
  # Calcular os coeficientes de variação
  coef.var.divida <- sd(divida) / mean(divida) * 100
  coef.var.divida.nominal.janela <- sd(divida_nominal_janela) / mean(divida_nominal_janela) * 100
  coef.var.divida.deflacionada.by.deflateBR <- sd(divida_deflacionada_by_deflateBR) / mean(divida_deflacionada_by_deflateBR) * 100
  coef.var.divida.janela.deflateBR <- sd(divida_janela_deflateBR) / mean(divida_janela_deflateBR) * 100
  coef.var.primario <- sd(primario) / mean(primario) * 100
  coef.var.primario.deflateBR <- sd(primario_deflateBR) / mean(primario_deflateBR) * 100
  coef.var.primario.janela <- sd(primario_janela) / mean(primario_janela) * 100
  coef.var.moeda <- sd(moeda) / mean(moeda) * 100
  coef.var.moeda.deflateBR <- sd(moeda_deflateBR) / mean(moeda_deflateBR) * 100
  coef.var.moeda.janela <- sd(moeda_janela) / mean(moeda_janela) * 100
  coef.var.juros <- sd(juros) / mean(juros) * 100
  coef.var.juros.janela <- sd(juros_janela) / mean(juros_janela) * 100
  
  
  # Crie um data frame com os nomes das séries e seus coeficientes de variação
  coef.vars <- data.frame(
    Serie = c("Dívida Nominal (2003 - 2024)",
              "Dívida Nominal (2022 - 2024)",
              "Dívida Deflacionada (2003 - 2024)",
              "Dívida Deflacionada (2022 - 2024)",
              "Primário Nominal (2003 - 2024)",
              "Primário Deflacionado (2003 - 2024)",
              "Primário Deflacionado (2022 - 2024)",
              "Moeda Nominal (2003 - 2024)",
              "Moeda Deflacionado (2003 - 2024)",
              "Moeda Deflacionado (2022 - 2024)",
              "Juros (2003 - 2024)",
              "Juros (2022 - 2024)"),
    coef.var = c(coef.var.divida,
                 coef.var.divida.nominal.janela,
                 coef.var.divida.deflacionada.by.deflateBR,
                 coef.var.divida.janela.deflateBR,
                 coef.var.primario,
                 coef.var.primario.deflateBR,
                 coef.var.primario.janela,
                 coef.var.moeda,
                 coef.var.moeda.deflateBR,
                 coef.var.moeda.janela,
                 coef.var.juros,
                 coef.var.juros.janela)
  )
  
  # Ordene o data frame pelo coef.var para facilitar a visualização (opcional)
  coef.vars <- coef.vars %>%
    arrange(coef.var)
  
  # Use a biblioteca ggplot2 para criar o gráfico de barras
  ggplot(coef.vars, aes(x = reorder(Serie, coef.var), y = coef.var, fill = Serie)) +
    geom_bar(stat = "identity") +
    coord_flip() + # Inverte as barras para melhor leitura dos rótulos longos
    labs(title = "Comparação dos coeficientes de variação das Séries da Dívida",
         x = "Série da Dívida",
         y = "Coeficiente de variação (%)") +
    theme_minimal() +
    theme(legend.position = "none") # Remove a legenda pois os rótulos já estão no eixo x
  
  
  # JARQUEBERA
  library(fBasics)
  jarqueberaTest(divida)
  jarqueberaTest(divida_nominal_janela)
  jarqueberaTest(divida_deflacionada_by_deflateBR)
  jarqueberaTest(divida_janela_deflateBR)
  
  # Graças aos testes, percebemos que os dados no intervalo de
  # 2003 a 2024 não tem uma distribuição normal, mesmo se deflacionados.
  # no entanto, com a janela, indepente se é ou não deflacionado,
  # há normalidade nos dados. Uma  distribuição é anormal quando, apesar de
  # haver uma média nos dados, existem alguns valores na observaçao que
  # são muito diferentes dos demais. Ex.: Renda.
  
  qqnorm(divida)
  qqline(divida)
  qqnorm(divida_nominal_janela)
  qqline(divida_nominal_janela)
  qqnorm(divida_deflacionada_by_deflateBR)
  qqline(divida_deflacionada_by_deflateBR)
  qqnorm(divida_janela_deflateBR)
  qqline(divida_janela_deflateBR)
  # Bem, a dispersão é menor na série enjanelada, de resto, é tudo meio ruim.
  
  shapiro.test(divida)
  shapiro.test(divida_nominal_janela)
  shapiro.test(divida_deflacionada_by_deflateBR)
  shapiro.test(divida_janela_deflateBR)
  # A estatistica W não parece ruim, ja que deve estar entre 0 e 1, no entanto
  # o p-valor ta melhor na série enjanelada
  
  decom_divida <-decompose(divida, type="additive"); plot(decom_divida)
  decom_divida_nominal_janela <- decompose(divida_nominal_janela, type="additive"); plot(decom_divida_nominal_janela)
  decom_divida_deflacionada_by_deflateBR <- decompose(divida_deflacionada_by_deflateBR, type="additive"); plot(decom_divida_deflacionada_by_deflateBR)
  decom_divida_janela_deflateBR <- decompose(divida_janela_deflateBR, type="additive"); plot(decom_divida_nominal_janela)
  
  decom_divida <-decompose(divida, type="multiplicative"); plot(decom_divida)
  decom_divida_nominal_janela <- decompose(divida_nominal_janela, type="multiplicative"); plot(decom_divida_nominal_janela)
  decom_divida_deflacionada_by_deflateBR <- decompose(divida_deflacionada_by_deflateBR, type="multiplicative"); plot(decom_divida_deflacionada_by_deflateBR)
  decom_divida_janela_deflateBR <- decompose(divida_janela_deflateBR, type="multiplicative"); plot(decom_divida_nominal_janela)
  
}

# Tendencia dos dados
{
  # Primário
  
  # Moeda
  
  # Juros
  
  
  
  
}

# Prevendo a dívida
{
  # Dívida em 12/2024:
  10251233
  {
    ################
    #--- Linear ---#
    ################
    
    library("dplyr")
    last(divida) # é 10251233.21
    n=length(divida_janela_deflateBR)
    txlineardivida<-((divida_janela_deflateBR[n]/divida_janela_deflateBR[1])-1)*100
    txlineardivida
    lindividanov <- 10251233 * (1 + (txlineardivida/100))
    lindividanov # Previsão: 11388343
    lindividanov2 <- lindividanov * (1 + (txlineardivida/100))
    lindividanov2 # Previsão: 12651586
  } # Linear ^
  {
    ####################
    #--- Geométrico ---#
    ####################
    txdividaGeo <-(((divida_janela_deflateBR[n]/divida_janela_deflateBR[1])^(1/n))-1)*100
    txdividaGeo
    geodividanov <- 10251233 * (1 + txdividaGeo)
    geodividanov # Previsão: 13251032
    geodividanov2 <- geodividanov * (1 + txdividaGeo )
    geodividanov2 # 17128656
    
  } # Geométrico ^
  {
    #############
    #--- Log ---#
    #############
    #install.packages("lmtest")
    n = length(divida) #evitando um erro posterior
    t=seq(1,n)
    library(lmtest)
    min(divida)
    max(divida)
    somapositiva <- min(divida)*-1 +2
    ldivida=log(divida+somapositiva)
    length(t); length(divida); length(ldivida)
    Txdividalog=lm(log(ldivida)~t)
    Txdividalog
    Cresc=(Txdividalog$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    logdividanov <- 10251233 * (1 + Cresc.Med)
    logdividanov # Previsão: 11746049
    logdividanov2 <- logdividanov * (1 + Cresc.Med)
    logdividanov2 # Previsão: 13458836
    
  } # Log ^
  {
    
    ###############
    #--- Arima ---#
    ###############
    
    #install.packages("fBasics")
    ####################################################################
    #        	TESTE DE RAIZ UNITÁRIA de Dickey-Fuller (DF)          	#
    ####################################################################
    
    #install.packages("urca")
    library(urca)
    
    ## teste dickey fuller
    # Modelo 1 ( Sem intercepto e sem tendência)
    
    dickey1 <- ur.df(divida_janela_deflateBR, lags=0 , type= "none")
    summary(dickey1)
    
    # Modelo 2 ( Com intercepto e sem tendência)
    
    dickey2 <- ur.df(divida_janela_deflateBR, lags=0 , type= "drift")
    summary(dickey2)
    
    # Modelo 3 ( Com intercepto e com tendência)
    
    dickey3 <- ur.df(divida_janela_deflateBR, lags=0 , type= "trend")
    summary(dickey3)
    
    ## --------- TESTE de Dickey-Fuller Aplicado (ADF) ---------
    
    # Modelo 1 ( Sem intercepto e sem tendência)
    
    dickey4 <- ur.df(divida_janela_deflateBR, lags=4 , type= "trend")
    summary(dickey4)
    
    # Modelo 2 ( Com intercepto e sem tendência)
    
    dickey5 <- ur.df(divida_janela_deflateBR, lags=10 , type= "trend")
    summary(dickey5)
    
    ### ----- TESTE da Raiz Unitária - Critério da Parcimônia
    
    dfuller1 <- ur.df(divida_janela_deflateBR, lags=12 , type= "none" ,selectlags = "BIC" )
    summary(dfuller1)
    
    dfuller1B <- ur.df(divida_janela_deflateBR, lags=12 , type= "none" ,selectlags = "AIC" )
    summary(dfuller1B)
    
    dfuller1c <- ur.df(divida_janela_deflateBR, lags=12 , type= "none" ,selectlags = "Fixed" )
    summary(dfuller1c)
    
    
    ## Modelo 2 - Com intecepto e sem tendência
    
    dfuller2 <- ur.df(divida_janela_deflateBR, lags=10 , type= "drift" ,selectlags = "BIC" )
    summary(dfuller2)
    
    ## Modelo 3 - Com intercepto e com tendência
    
    dfuller3 <- ur.df(divida_janela_deflateBR, lags=10 , type= "trend" ,selectlags = "BIC" )
    summary(dfuller3)
    
    
    ### Aplicando ADF - 1 Diferença
    
    dfuller1 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "none" ,selectlags = "BIC" )
    summary(dfuller1)
    
    dfuller2 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "drift" ,selectlags = "BIC" )
    summary(dfuller2)
    
    dfuller3 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "trend" ,selectlags = "BIC" )
    summary(dfuller3)
    
    #### Aplicando ADF - 2 Diferença
    
    dfuller1 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "none" ,selectlags = "BIC" )
    summary(dfuller1)
    
    dfuller2 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "drift" ,selectlags = "BIC" )
    summary(dfuller2)
    
    dfuller3 <- ur.df(diff(divida_janela_deflateBR), lags=10 , type= "trend" ,selectlags = "BIC" )
    summary(dfuller3)
    
    ################################
    
    ################################
    #         	log          	#
    ################################
    
    
    logdivida=log(divida_janela_deflateBR)
    
    plot.ts(logdivida, col="blue",
            main="Log da divida", xlab="Anos", lwd=2)
    
    #acf vai fazer o negocio sem logoratmizar
    {
      # Ao revisitar os códigos do guja, notei que usamos o
      # guja211 (que está em série temporal) em alguns trechos do arima
      # e em outros trechos usamos o guja211M (que não está em série temporal)
      # se usar série temporal em tudo, por alguma razão os testes
      # ficam com maus resultados, mas do jeito que fiz abaixo da certo:
      # acf(divida_deflacionada_by_deflateBR, lag=36,plot=F)
      # logdividam=log(dividamensal)
      # acf(logdividam, lag=36,plot=F)
      # talvez isso se deva a alguma limitação do acf para ler ts (série temporal).
      # por isso fiz esse codigos aqui para evitar problemas a seguir
      write.table(dividadata, file = "dividadata.txt")
      dividadatamensal <- read.table("dividadata.txt",head=T)
      dividamensal = read.table("dividadatamd.txt",head=T)
      # certamente há explicação decente para isso, mas os meios práticos
      # (IA) não foram precisos. A resposta deve estar nos livros.
      #print(dividamensal)
      #print(dividadata)
      #print(divida_janela_deflateBR)
      
    }
    
    acf(divida_deflacionada_by_deflateBR, lag=36,plot=F)
    logdividam=log(dividamensal)
    acf(logdividam, lag=36,plot=F)
    
    #----------correlograma da FAC----------------#
    
    acf(divida_janela_deflateBR, lag=36, col="red", lwd=3, main="divida")
    acf(logdivida,lag=36, col="red", main="Logaritmo do divida")
    
    pacf(divida, lag=36,plot=F)
    pacf(divida, lag=36, col="red", lwd=2)
    
    Box.test(logdivida, lag=1, type="Box-Pierce")
    Box.test(logdivida, lag=2, type="Box-Pierce")
    Box.test(logdivida, lag=4, type="Box-Pierce")
    Box.test(logdivida, lag=8, type="Box-Pierce")
    Box.test(logdivida, lag=12, type="Box-Pierce")
    Box.test(logdivida, lag=16, type="Box-Pierce")
    Box.test(logdivida, lag=20, type="Box-Pierce")
    
    Box.test(logdivida, lag=1, type="Ljung-Box")
    Box.test(logdivida, lag=2, type="Ljung-Box")
    Box.test(logdivida, lag=4, type="Ljung-Box")
    Box.test(logdivida, lag=8, type="Ljung-Box")
    Box.test(logdivida, lag=12, type="Ljung-Box")
    Box.test(logdivida, lag=16, type="Ljung-Box")
    Box.test(logdivida, lag=20, type="Ljung-Box")
    
    # estima dos modelos arima (p,d,q)
    #install.packages("forecast")
    library(forecast)
    
    Arima111 <- Arima(logdivida,order=c(1,1,1),method=c("ML"))
    Arima111
    Arima111$coef
    
    Arima111X <- Arima(logdivida,order=c(1,1,1),method=c("ML"), include.drift = T)
    Arima111X
    
    plot(Arima111X)
    
    plot(residuals(Arima111))
    
    residuosArima111 <- Arima111$residuals
    residuosArima111
    plot(residuosArima111)
    
    ###--- 18 02 2025 ---###
    
    ##-- Testes de arimas e visualizações --##
    {
      #Arima011<-Arima(logdivida,order=c(0,1,1),method=c("ML"))
      #Arima011
      #Arima011$coef
      #tsdiag(Arima011)
      
      #Arima012<-Arima(logdivida,order=c(0,1,2),method=c("ML"))
      #Arima012
      #Arima012$coef
      #tsdiag(Arima012)
      
      
      #Arima111<-Arima(logdivida,order=c(1,1,1),method=c("ML"))
      #Arima111
      #Arima111$coef
      #tsdiag(Arima111)
      
      
      #Arima112<-Arima(logdivida,order=c(1,1,2),method=c("ML"))
      #Arima112
      #Arima112$coef
      #tsdiag(Arima112)
      
      
      #Arima023<-Arima(logdivida,order=c(0,2,3),method=c("ML"))
      #Arima023
      #Arima023$coef
      #tsdiag(Arima023)
      
      
      Arima113<-Arima(logdivida,order=c(1,1,3),method=c("ML"))
      Arima113
      Arima113$coef
      tsdiag(Arima113)
      
      
      #Arima110<-Arima(logdivida,order=c(1,1,0),method=c("ML"))
      #Arima110
      #Arima110$coef
      #tsdiag(Arima110)
      
      #Arima212<-Arima(logdivida,order=c(2,1,2),method=c("ML"))
      #Arima212
      #Arima212$coef
      #tsdiag(Arima212)
      
      #Arima221<-Arima(logdivida,order=c(2,2,1),method=c("ML"))
      #Arima221
      #Arima221$coef
      #tsdiag(Arima221)
      
      Arima311 <- Arima(logdivida, order = c(3, 1, 1), method = c("ML"))
      Arima311
      Arima311$coef
      tsdiag(Arima311)
      
      #Arima120 <- Arima(logdivida, order = c(1, 2, 0), method = c("ML"))
      #Arima120
      #Arima120$coef
      #tsdiag(Arima120)
      
      #Arima022 <- Arima(logdivida, order = c(0, 2, 2), method = c("ML"))
      #Arima022
      #Arima022$coef
      #tsdiag(Arima022)
      
      #Arima210 <- Arima(logdivida, order = c(2, 1, 0), method = c("ML"))
      #Arima210
      #Arima210$coef
      #tsdiag(Arima210)
      
      Arima321 <- Arima(logdivida, order = c(3, 2, 1), method = c("ML"))
      Arima321
      Arima321$coef
      tsdiag(Arima321)
      
      Arima213 <- Arima(logdivida, order = c(2, 1, 3), method = c("ML"))
      Arima213
      Arima213$coef
      tsdiag(Arima213)
      
      #Arima122 <- Arima(logdivida, order = c(1, 2, 2), method = c("ML"))
      #Arima122
      #Arima122$coef
      #tsdiag(Arima122)
      
      #Arima031 <- Arima(logdivida, order = c(0, 3, 1), method = c("ML"))
      #Arima031
      #Arima031$coef
      #tsdiag(Arima031)
      
      Arima410 <- Arima(logdivida, order = c(4, 1, 0), method = c("ML"))
      Arima410
      Arima410$coef
      tsdiag(Arima410)
      
      Arima014 <- Arima(logdivida, order = c(0, 1, 4), method = c("ML"))
      Arima014
      Arima014$coef
      tsdiag(Arima014)
      
      #Arima220 <- Arima(logdivida, order = c(2, 2, 0), method = c("ML"))
      #Arima220
      #Arima220$coef
      #tsdiag(Arima220)
      
      Arima024 <- Arima(logdivida, order = c(0, 2, 4), method = c("ML"))
      Arima024
      Arima024$coef
      tsdiag(Arima024)
      
      Arima401 <- Arima(logdivida, order = c(4, 0, 1), method = c("ML")) # Se a série fosse estacionária
      Arima401
      Arima401$coef
      tsdiag(Arima401)
      
      Arima322 <- Arima(logdivida, order = c(3, 2, 2), method = c("ML"))
      Arima322
      Arima322$coef
      tsdiag(Arima322)
      
      #Arima231 <- Arima(logdivida, order = c(2, 3, 1), method = c("ML"))
      #Arima231
      #Arima231$coef
      #tsdiag(Arima231)
      
      #Arima132 <- Arima(logdivida, order = c(1, 3, 2), method = c("ML"))
      #Arima132
      #Arima132$coef
      #tsdiag(Arima132)
      
      Arima411 <- Arima(logdivida, order = c(4, 1, 1), method = c("ML"))
      Arima411
      Arima411$coef
      tsdiag(Arima411)
      
      #Arima221 <- Arima(logdivida, order = c(2, 2, 1), method = c("ML"))
      #Arima221
      #Arima221$coef
      #tsdiag(Arima221)
      
      #Arima222 <- Arima(logdivida, order = c(2, 2, 2), method = c("ML"))
      #Arima222
      #Arima222$coef
      #tsdiag(Arima222)
      
      #Arima222 <- Arima(logdivida, order = c(2, 3, 2), method = c("ML"))
      #Arima222
      #Arima222$coef
      #tsdiag(Arima222)
      
      autoArima1 <- auto.arima(logdivida, method = "ML")
      print("--- autoArima1 ---")
      print(autoArima1)
      print(autoArima1$coef)
      tsdiag(autoArima1)
      
    }
    
    ########################
    #    	ARIMA     	#
    ########################
    
    
    previsaoarima111 <- predict(Arima111, n.ahead = 4)
    previsaoarima111
    tail(logdivida, 4)
    accuracy(Arima111)
    
    #previsaoarima112 <- predict(Arima112, n.ahead = 4)
    #previsaoarima112
    #tail(logdivida, 4)
    #accuracy(Arima112)
    
    previsaoarima113 <- predict(Arima113, n.ahead = 4)
    previsaoarima113
    tail(logdivida, 4)
    accuracy(Arima113)
    
    print(Arima113)
    
    summary(Arima113)
    
    dividaprevisaolog <- 16.15759
    dividaprevisaosemlog <- exp(dividaprevisaolog)
    dividaprevisaosemlog # Previsão: 10402846
    dividaprevisaolog2 <- 16.15192
    dividaprevisaosemlog2 <- exp(dividaprevisaolog2)
    dividaprevisaosemlog2 # Previsão: 10344029
    
    
  } # Arima ^
  {
    ################
    #--- MQO ---#
    ################
    
    #-- MQO Normal --#
    {
      dados_combinados_ts <- ts.union(divida_janela_deflateBR,
                                      primario_janela,
                                      moeda_janela,
                                      juros_janela,
                                      m1_janela)
      
      colnames(dados_combinados_ts) <- c("divida", "primario", "moeda", "juros", "m1")
      
      print(dados_combinados_ts)
      class(dados_combinados_ts) # Deve ser 'mts' (multivariate time series)
      
      # Converter para um data.frame (opcional)
      dados_combinados_df <- as.data.frame(dados_combinados_ts)
      print(head(dados_combinados_df))
      
      # Modelo
      formula_mqo <- divida ~ primario + moeda + juros + m1
      # lm()
      modelo_mqo <- lm(formula_mqo, data = dados_combinados_df)
      summary(modelo_mqo)
      
      predict(modelo_mqo)
      # Ao menos parece OK, mas segundo e gemeni eu preciso atualizar as variaveis
      # indepentes com os valores futuros do mesmo período que eu preciso prever
      # então fiz isso abaixo no MQO com log
      
      
      
    }
    #-- MQO Log --#
    {
      any(divida_janela_deflateBR <= 0) # F
      any(primario_janela <= 0) # T
      any(moeda_janela <= 0) # F
      any(juros_janela <= 0) # F
      any(m1_janela <= 0) # F
      # Vou remover M1 pq não tem significancia
      
      min(primario_janela) # -89834.13
      positivador_primario <- -89834.13*-1 + 2
      
      dados_log <- ts.union(log(divida_janela_deflateBR),
                            log(primario_janela + positivador_primario),
                            log(moeda_janela),
                            log(juros_janela))
      
      colnames(dados_log) <- c("divida", "primario", "moeda", "juros")
      print(dados_log)
      # Converter para um data.frame (opcional)
      dados_log <- as.data.frame(dados_log)
      print(head(dados_log))
      
      # Remover M1 deu uns problemas, por isso tem que fazer o que está no código a seguir
      # -- Removendo M1 -- #
      if ("m1" %in% colnames(dados_log)) {
        dados_log$m1 <- NULL
        print("Coluna 'm1' removida de dados_log.")
        str(dados_log)
      } else {
        print("Coluna 'm1' não encontrada em dados_log.")
        str(dados_log)
      }
      
      # -- Definindo e Estimando o Modelo -- #
      #rm(modelo_mqo_log) # Remove qualquer modelo antigo
      #rm(formula_mqo_log) # Remove qualquer fórmula antiga
      
      formula_mqo_log <- divida ~ primario + moeda + juros
      # lm()
      modelo_mqo_log <- lm(formula_mqo_log, data = dados_log)
      summary(modelo_mqo_log)
      
      # O código a seguir parece funcionar para prever
      predict(modelo_mqo_log)
      dividaprevisaolog <- 16.04003
      dividaprevisaosemlog <- exp(dividaprevisaolog); dividaprevisaosemlog
      
      # Mas, segundo o gemini, para prever com MQO é preciso atualizar as variáveis
      # independentes, o que eu acho realmente estranho
      # Então usei arima para prever as variaveis independentes
      # e depois atualizar o banco de dados (assim como sugeriu o gemini)
      
      Juros_arima113<-Arima(log(juros_janela),order=c(1,1,3),method=c("ML"))
      previsaoJuros_arima113 <- predict(Juros_arima113, n.ahead = 2)
      previsaoJuros_arima113
      jurosprevisaolog <- -0.176506
      jurosprevisaosemlog <- exp(jurosprevisaolog); jurosprevisaosemlog
      # Previsão: 0.8381937
      jurosprevisaolog2 <- -0.1393376
      jurosprevisaosemlog2 <- exp(jurosprevisaolog2); jurosprevisaosemlog2
      # Previsão: 0.8699343
      jurosprevisaosemlog <- c( jurosprevisaosemlog, jurosprevisaosemlog2)
      
      moeda_arima113<-Arima(log(moeda_janela),order=c(1,1,3),method=c("ML"))
      previsaomoeda_arima113 <- predict(moeda_arima113, n.ahead = 2)
      previsaomoeda_arima113
      moedaprevisaolog <- 15.72941
      moedaprevisaosemlog <- exp(moedaprevisaolog); moedaprevisaosemlog
      # Previsão: 6779473
      moedaprevisaolog2 <- 15.74894
      moedaprevisaosemlog2 <- exp(moedaprevisaolog2); moedaprevisaosemlog2
      # 6913178
      moedaprevisaosemlog <- c(moedaprevisaolog, moedaprevisaolog2)
      
      primario_arima113<-Arima(log(primario_janela + positivador_primario),order=c(1,1,3),method=c("ML"))
      previsaoprimario_arima113 <- predict(primario_arima113, n.ahead = 2)
      previsaoprimario_arima113
      primarioprevisaolog <- 9.504423
      primarioprevisaosemlog <- exp(primarioprevisaolog); primarioprevisaosemlog
      # Previsão: 13418.95
      primarioprevisaolog2 <-  10.150652
      primarioprevisaosemlog2 <- exp(primarioprevisaolog2); primarioprevisaosemlog2
      # Previsão: 25607.79
      primarioprevisaosemlog <- c(primarioprevisaolog, primarioprevisaolog2)
      
      # Criar um data.frame com as previsões das variáveis independentes para o próximo período
      novos_dados_previsao <- data.frame(
        primario = primarioprevisaolog,
        moeda = moedaprevisaolog,
        juros = jurosprevisaolog
      )
      
      # Prever o log da dívida para o próximo período
      previsao_log_divida_futura <- predict(modelo_mqo_log, newdata = novos_dados_previsao)
      print(previsao_log_divida_futura)
      
      # Exponenciar para obter a previsão da dívida na escala original
      previsao_divida_futura <- exp(previsao_log_divida_futura)
      previsao_divida_futura
      # Previsão: 10186217
      
      
      
    }
    
    # Pelas observações, M1 não é válido, mas M2 é
    
    
  } # MQO ^
  {
    #############
    #--- VAR ---#
    #############
    
    # Talvez eu tenha buscado os dados errados, pois eles diferem dos
    # dados disponibilizados pelo professor. Da mesma forma, os resultados
    # com os testes também indicam que não há signifancia nas variaveis
    # independentes que busquei. Thats pretty boring,
    
    # O que deixou meu modelo legal (com os dados fornecidos pelo prof)
    # foi enjanelar, mas o ideal é usar o diff, até pq assim ele passa
    # melhor no teste de impulso-resposta
    # por isso fiz dois VARs. O primeiro usando os dados fornecidos pelo prof
    # que passa melhor
    
    {
      
      # -- Var feito com os dados disponibilizados pelo prof
      
      
      
      Divida=read.table("Divida.txt",head=T)
      Primario=read.table("Primario.txt",head=T)
      Juros=read.table("Juros.txt",head=T)
      Moeda=read.table("Moeda.txt",head=T)
      
      Divida <- ts(Divida,frequency = 12, start = c(2003,1)) #;Divida;plot(Divida)
      Primario <- ts(Primario,frequency = 12, start = c(2003,1)) #;Primario;plot(Primario)
      Juros <- ts(Juros,frequency = 12, start = c(2003,1)) #;Juros;plot(Juros)
      Moeda <- ts(Moeda,frequency = 12, start = c(2003,1))# ;Moeda;plot(Moeda)
      Moeda <- Moeda/1000; Moeda;# plot.ts(Moeda)
      
      library(deflateBR)
      times=seq(as.Date("2003/1/1"), by = "month", length.out = 264); times
      Divida = deflate(Divida, nominal_dates = times, real_date = '12/2024', index = 'ipca')
      Primario = deflate(Primario, nominal_dates = times, real_date = '12/2024', index = 'ipca')
      Moeda <- deflate(Moeda, nominal_dates = times, real_date = '12/2024', index = 'ipca')
      # Não faz sentido dedeflacionar os juros Juros <- deflate(Juros, nominal_dates = times, real_date = '12/2024', index = 'ipca')
      
      
      library(tseries)
      library(urca)
      adf.test(Divida)$p.value
      adf.test(Primario)$p.value
      adf.test(Moeda)$p.value
      adf.test(Juros)$p.value
      
      Divida <- diff(Divida)
      Primario <- diff(Primario)
      Moeda <- diff(Moeda)
      Juros <- diff(Juros)
      
      adf.test(Divida)$p.value
      adf.test(Primario)$p.value
      adf.test(Moeda)$p.value
      adf.test(Juros)$p.value
      
      # Quando uma variável precisa de 2 diferença, todas as outras também
      # devem estar em 2 diferença
      
      dados=cbind(Divida, Moeda, Juros,  Primario); dados
      colnames(dados)
      colnames(Divida)
      colnames(Primario)
      
      dados <- window(dados, frequency=12, start=c(2020,1), end=c(2024,12)) #; tail(dados)
      
      
      #install.packages("vars")
      library("vars")
      VARselect(dados, lag.max = 20)
      VAR1ct <- VAR(dados, p = 3, type = "both")
      str(dados)
      summary(VAR1ct)
      decomp_VAR1ct <- fevd(VAR1ct, n.ahead = 36)
      decomp_VAR1ct
      
      #### Testes de Performance dos Modelos Estimados
      
      ##### Diagnóstico dos Resíduos - Portmanteu Ho: Ausência Correlação
      serial<-serial.test(VAR1ct, lags.pt = 12, type = "PT.asymptotic")
      serial
      
      ####### Teste de Presença de Procedimento de ARCH (Heteroscedasticidade)
      arch.test(VAR1ct, multivariate.only=TRUE, lags.mult = 6)
      #arch.test(VAR1ct, multivariate.only=FALSE)
      
      ######  Teste de Normalidade dos Resíduos  ###################
      JarqueBera <-normality.test(VAR1ct,multivariate.only=T)
      JarqueBera
      
      shapiro<-shapiro.test(resid(VAR1ct))
      shapiro
      
      ######  Teste Visual de Estabilidade #######
      Estabilidade <- stability(VAR1ct)
      plot(Estabilidade)
      
      ##### Teste de Raiz (Autovalores)
      roots(VAR1ct, modulus=TRUE)
      
      plot(roots(VAR1ct, modulus=TRUE))
      
      ######## Impulso-Resposta ######## Impulso X --> Resposta em Y e X
      
      impulso_VAR1ct <- irf(VAR1ct, n.ahead = 10)
      impulso_VAR1ct
      plot(impulso_VAR1ct)
      
      #### Análise Individual
      #VAR1ct <- VAR(dados, p = 6)
      plot(irf(VAR1ct, n.ahead = 36, impulse = "Primario", response = "Divida", boot = F))
      plot(irf(VAR1ct, n.ahead = 36, impulse = "Juros", response = "Divida", boot = F))
      plot(irf(VAR1ct, n.ahead = 36, impulse = "Moeda", response = "Divida", boot = F))
      
      grangertest(Divida~Primario, order  = 6)  ## Primário Causa Dívida
      grangertest(Primario~Divida, order = 6)   ## Dívida não Causa Primário
      
      grangertest(Primario~Juros, order = 6)
      grangertest(Juros~Primario, order = 6)
      
      grangertest(Primario~Moeda, order = 6)
      grangertest(Moeda~Primario, order = 6)
      
      grangertest(Divida~Juros, order = 6)
      grangertest(Juros~Divida, order = 6)
      
      grangertest(Divida~Moeda, order = 6)
      grangertest(Moeda~Divida, order = 6)
      
      #-- Previsão
      
      VAR1ct=VAR(dados, p = 6, type = "both")
      summary(VAR1ct)
      
      previsao<-predict(VAR1ct, n.ahead=6, ci=0.95)
      previsao
      plot(previsao)
      fanchart(previsao)
      Divida_nivel <- ts(read.table("Divida.txt",head=T), frequency = 12, start = c(2003,1))
      Divida <- diff(Divida_nivel)
      Divida_nivel_janela <- window(Divida_nivel, start=c(2020,1), end=c(2024,12))
      ultimo_valor_divida <- tail(Divida_nivel_janela, 1)
      previsao_diff_divida <- previsao$fcst$Divida[,1]
      previsao_nivel_divida <- as.numeric(ultimo_valor_divida) + cumsum(previsao_diff_divida)
      previsao_nivel_divida
      10291764
      # esse é o var feito com os dados disponibilzado com os dados do prof
    }
    
    
    {
      
      # Segunda VAR, feito com dados puxados direto do BC
      
      any(divida_janela_deflateBR <= 0) # F
      any(primario_janela <= 0) # T
      any(moeda_janela <= 0) # F
      any(juros_janela <= 0) # F
      
      divida_log = log(divida_janela_deflateBR)
      positivadorprimario = min(primario_janela)*-1 +2
      primario_log = log(primario_janela + positivadorprimario)
      juros_log = log(juros_janela)
      moeda_log = log(moeda_janela)
      
      # Eu não fiz simplismente min()*min() pq na hora de fazer o log fica
      # Um valor muito alto e por isso o log bota um valor igual para todas
      # as variáveis da serie
      
      adf.test(divida_log)$p.value
      adf.test(primario_log)$p.value
      adf.test(moeda_log)$p.value
      adf.test(juros_log)$p.value
      
      divida_diff <- diff(divida_log)
      primario_diff <- diff(primario_log)
      moeda_diff <- diff(moeda_log)
      juros_diff <- diff(juros_log)
      
      adf.test(divida_diff)$p.value
      adf.test(primario_diff)$p.value
      adf.test(moeda_diff)$p.value
      adf.test(juros_diff)$p.value
      
      # Quando uma variável precisa de 2 diferença, todas as outras também
      # devem estar em 2 diferença
      
      divida_diff2 <- diff(divida_diff)
      primario_diff2 <- diff(primario_diff)
      moeda_diff2 <- diff(moeda_diff)
      juros_diff2 <- diff(juros_diff)
      
      adf.test(divida_diff2)$p.value
      adf.test(primario_diff2)$p.value
      adf.test(moeda_diff2)$p.value
      adf.test(juros_diff2)$p.value
      
      dados_diff <- cbind(divida_diff2, moeda_diff2, primario_diff2, juros_diff2)
      
      # os dados já estão enjanelados, então não há pra que fazer isso mais uma vez
      
      #install.packages("vars")
      library("vars")
      VARselect(dados_diff, lag.max = 20)
      var_divida_diff = VAR(dados_diff, p = 4, type = "both")
      summary(var_divida_diff)
      decomp_var_divida_diff <- fevd(var_divida_diff, n.ahead = 36)
      decomp_var_divida_diff
      
      print(dados_diff)
      
      #### Testes de Performance dos Modelos Estimados
      {
        
        ##### Diagnóstico dos Resíduos - Portmanteu Ho: Ausência Correlação
        serial<-serial.test(var_divida_diff, lags.pt = 12, type = "PT.asymptotic")
        serial
        
        ####### Teste de Presença de Procedimento de ARCH (Heteroscedasticidade)
        arch.test(var_divida_diff, multivariate.only=TRUE, lags.mult = 6)
        #arch.test(var_divida_diff, multivariate.only=FALSE)
        
        ######  Teste de Normalidade dos Resíduos  ###################
        JarqueBera <-normality.test(var_divida_diff,multivariate.only=T)
        JarqueBera
        
        shapiro<-shapiro.test(resid(var_divida_diff))
        shapiro
        
        ######  Teste Visual de Estabilidade #######
        Estabilidade <- stability(var_divida_diff)
        plot(Estabilidade)
        
        ##### Teste de Raiz (Autovalores)
        roots(var_divida_diff, modulus=TRUE)
        
        plot(roots(var_divida_diff, modulus=TRUE))
      }
      
      ######## Impulso-Resposta ######## Impulso X --> Resposta em Y e X
      
      impulso_var_divida_diff <- irf(var_divida_diff, n.ahead = 10)
      impulso_var_divida_diff
      plot(impulso_var_divida_diff)
      
      #### Análise Individual
      plot(irf(var_divida_diff, n.ahead = 36, impulse = "primario_diff2", response = "divida_diff2", boot = F))
      plot(irf(var_divida_diff, n.ahead = 36, impulse = "juros_diff2", response = "divida_diff2", boot = F))
      plot(irf(var_divida_diff, n.ahead = 36, impulse = "moeda_diff2", response = "divida_diff2", boot = F))
      
      ####
      
      grangertest(divida_diff2~primario_diff2, order  = 6)  ## Primário Causa Dívida
      grangertest(primario_diff2~divida_diff2, order = 6)   ## Dívida não Causa Primário
      
      grangertest(primario_diff2~juros_diff2, order = 6)
      grangertest(juros_diff2~primario_diff2, order = 6)
      
      grangertest(primario_diff2~moeda_diff2, order = 6)
      grangertest(moeda_diff2~primario_diff2, order = 6)
      
      grangertest(divida_diff2~juros_diff2, order = 6)
      grangertest(juros_diff2~divida_diff2, order = 6)
      
      grangertest(divida_diff2~moeda_diff2, order = 6)
      grangertest(moeda_diff2~divida_diff2, order = 6)
      
      ###------------------------------------------###
      
      library(forecast)
      ###### Alternativa 01
      
      var_divida_diff=VAR(dados_diff, p = 6, type = "both")
      #summary(var_divida_diff) aparentemente está com multicolinearidade por isso da erro
      
      previsao_divida_var<-predict(var_divida_diff, n.ahead=6, ci=0.95)
      previsao_divida_var
      plot(previsao_divida_var)
      fanchart(previsao_divida_var)
      ultimo_valor_log <- tail(divida_log, 1)
      previsao_diff2 <- previsao_divida_var$fcst$divida_diff2[,1]
      previsao_diff1 <- cumsum(previsao_diff2)
      previsao_log <- as.numeric(ultimo_valor_log) + cumsum(previsao_diff1)
      previsao_nivel <- exp(previsao_log)
      plot(previsao_nivel, type = "l", main = "Previsão da Dívida em Nível")
      previsao_nivel <- exp(previsao_log)
      print(previsao_nivel)
      10276676
      
      # Esse é o 2o VAR que fiz. Busque usar o primeiro pois ele é mais significativo
    }
  } # VAR ^
  
  
  
  
}
