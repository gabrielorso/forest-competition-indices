  
  options(scipen=999)
  
  
  ### 1. carregando pacotes ####
  library(plyr)         #Manipulacao de dados
  library(dplyr)        #Manipulacao de dados
  library(ggforce)      #Utilitarios para ggplot
  library(RColorBrewer) #Utilitarios para ggplot
  library(ggplot2)      #Graficos
  library(gridExtra)    #Utilitarios para ggplot
  library(spatstat)     #Analises espaciais
  library(readr)        #Para Manipulacao de dados
  library(tibble)       #rownames_to_columns e columns_to_rownames
  
  
  #### 2. Carregando base de dados ####
  dados <- read.csv2("fake_data.csv", stringsAsFactors = TRUE) 
  head(dados)
  str(dados)
  
  
  
  # 3. Calculando Diametros ----
  dados$d7  <- round(dados$c7/pi,2)
  dados$d10 <- round(dados$c10/pi,2)
  dados$d13 <- round(dados$c13/pi,2)
  dados$d16 <- round(dados$c16/pi,2)
  
  # 3.1 Calculando areas basais ----
  dados$g7 <- round((dados$d7**2*pi)/40000,4)
  dados$g10 <- round((dados$d10**2*pi)/40000,4)
  dados$g13 <- round((dados$d13**2*pi)/40000,4)
  dados$g16 <- round((dados$d16**2*pi)/40000,4)
  
  
  # 4. Calculando Incrementos ----
  # Aqui, nenhuma linha (individuo) que nao possui o incremento de tal ocasiao (em virtude de terem morrido antes ou entrado na medicao depois de tal periodo) foi excluida, visto que valores ausentes se apresentam como NA no dataframe e sao naturalmente ignorados na maioria das operacoes do R.
  
  #Dessa forma, operacoes como 10-NA resultara em NA, entao nao ha perigo de arvores que ingressaram no fim do periodo, por exemplo, apresentarem incremento.
  
  dados$ipa0710 <- round((dados$d10-dados$d7)/3,2)      # IPA 2007-2010
  dados$ipa0713 <- round((dados$d13-dados$d7)/6,2)      # IPA 2007-2013
  dados$ipa0716 <- round((dados$d16-dados$d7)/9,2)      # IPA 2007-2016
  dados$ipa1013 <- round((dados$d13-dados$d10)/3,2)     # IPA 2010-2013
  dados$ipa1016 <- round((dados$d16-dados$d10)/6,2)     # IPA 2010-2016
  dados$ipa1316 <- round((dados$d16-dados$d13)/3,2)     # IPA 2013-2016
  
  dados$ipa0710.100 <- dados$ipa0710/dados$d7*100
  dados$ipa0713.100 <- dados$ipa0713/dados$d7*100
  dados$ipa0716.100 <- dados$ipa0716/dados$d7*100
  dados$ipa1013.100 <- dados$ipa1013/dados$d10*100
  dados$ipa1016.100 <- dados$ipa1016/dados$d10*100
  dados$ipa1316.100 <- dados$ipa1316/dados$d13*100
  
  
  #### 4.1 IPA medio por especie ----
  
  sp <- data.frame("nome" = levels(dados$nome))
  
  # IPA por especie em 2007-2010
  sp$ipa0710 <- as.vector(tapply(X = dados$ipa0710, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  # IPA por especie em 2007-2013
  sp$ipa0713 <- as.vector(tapply(X = dados$ipa0713, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  # IPA por especie em 2007-2016
  sp$ipa0716 <- as.vector(tapply(X = dados$ipa0716, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  # IPA por especie em 2010-2013
  sp$ipa1013 <- as.vector(tapply(X = dados$ipa1013, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  # IPA por especie em 2010-2013
  sp$ipa1016 <- as.vector(tapply(X = dados$ipa1016, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  # IPA por especie em 2013-2016
  sp$ipa1316 <- as.vector(tapply(X = dados$ipa1316, INDEX = dados$nome, FUN = mean, na.rm = T))
  
  
  # 5. DAP Medio por especie ----
  
  # Media de DAP em 2007
  sp$d7 <- as.vector(tapply(X = dados$d7[is.na(dados$ipa0710) != TRUE], INDEX = dados$nome[is.na(dados$ipa0710) != TRUE], FUN = mean, na.rm=T))
  
  # Media de DAP em 2010
  sp$d10 <- as.vector(tapply(X = dados$d10[is.na(dados$ipa0710) != TRUE], INDEX = dados$nome[is.na(dados$ipa0710) != TRUE], FUN = mean, na.rm=T))
  
  # Media de DAP em 2013
  sp$d13 <- as.vector(tapply(X = dados$d13[is.na(dados$ipa1316) != TRUE], INDEX = dados$nome[is.na(dados$ipa1316) != TRUE], FUN = mean, na.rm=T))
  
  # Media de DAP em 2016
  sp$d16 <- as.vector(tapply(X = dados$d16[is.na(dados$ipa1316) != TRUE], INDEX = dados$nome[is.na(dados$ipa1316) != TRUE], FUN = mean, na.rm=T))
  
  # 5.1 DAP percentil 95% por especie ----
  sp$d7.95 <- as.vector(tapply(X = dados$d7[is.na(dados$ipa0710) != TRUE], INDEX = dados$nome[is.na(dados$ipa0710) != TRUE], FUN= quantile, probs=0.95, na.rm=T))
  
  sp$d10.95 <- as.vector(tapply(X = dados$d10[is.na(dados$ipa0710) != TRUE], INDEX = dados$nome[is.na(dados$ipa0710) != TRUE], FUN= quantile, probs=0.95, na.rm=T))
  
  sp$d13.95 <- as.vector(tapply(X = dados$d13[is.na(dados$ipa1316) != TRUE], INDEX = dados$nome[is.na(dados$ipa1316) != TRUE], FUN= quantile, probs=0.95, na.rm=T))
  
  sp$d16.95 <- as.vector(tapply(X = dados$d16[is.na(dados$ipa1316) != TRUE], INDEX = dados$nome[is.na(dados$ipa1316) != TRUE], FUN= quantile, probs=0.95, na.rm=T))
  
  
  ### 6. Contagem de individuos por especie ----
  # Aqui foram contabilizados os individuos por ano de medicao, alem dos individuos que possuem incremento em cada periodo (2007-2010, 2013-2016 e 2007-2016)
  
  # individuos por especie em 2007
  sp$n.d7 <- as.vector(tapply(X = dados$d7[!is.na(dados$d7)], INDEX = dados$nome[!is.na(dados$d7)], FUN = NROW))
  
  # individuos por especie em 2010
  sp$n.d10 <- as.vector(tapply(X = dados$d10[!is.na(dados$d10)], INDEX = dados$nome[!is.na(dados$d10)], FUN = NROW))
  
  # individuos por especie em 2013
  sp$n.d13 <- as.vector(tapply(X = dados$d13[!is.na(dados$d13)], INDEX = dados$nome[!is.na(dados$d13)], FUN = NROW))
  
  # individuos por especie em 2016
  sp$n.d16 <- as.vector(tapply(X = dados$d16[!is.na(dados$d16)], INDEX = dados$nome[!is.na(dados$d16)], FUN = NROW))
  
  # individuos por especie em 2007-2010
  sp$n.0710 <- as.vector(tapply(X = dados$ipa0710[!is.na(dados$ipa0710)], INDEX = dados$nome[!is.na(dados$ipa0710)], FUN = NROW))
  
  # individuos por especie em 2007-2013
  sp$n.0713 <- as.vector(tapply(X = dados$ipa0713[!is.na(dados$ipa0713)], INDEX = dados$nome[!is.na(dados$ipa0713)], FUN = NROW))
  
  # individuos por especie em 2007-2016
  sp$n.0716 <- as.vector(tapply(X = dados$ipa0716[!is.na(dados$ipa0716)], INDEX = dados$nome[!is.na(dados$ipa0716)], FUN = NROW))
  
  # individuos por especie em 2010-2013
  sp$n.1013 <- as.vector(tapply(X = dados$ipa1013[!is.na(dados$ipa1013)], INDEX = dados$nome[!is.na(dados$ipa1013)], FUN = NROW))
  
  # individuos por especie em 2010-2016
  sp$n.1016 <- as.vector(tapply(X = dados$ipa1016[!is.na(dados$ipa1016)], INDEX = dados$nome[!is.na(dados$ipa1016)], FUN = NROW))
  
  # individuos por especie em 2013-2016
  sp$n.1316 <- as.vector(tapply(X = dados$ipa1316[!is.na(dados$ipa1316)], INDEX = dados$nome[!is.na(dados$ipa1316)], FUN = NROW))
  
  
  head(sp)
  
  # 7. Adicionando a classificacao sucessional para cada especie (classif) ----
  
  classe <- dados[,c("nome","classif")]
  classe <- unique(classe)
  
  sp <- merge(classe,sp,by="nome")
  
  head(sp)
  
  rm(classe)
  
  #___________
  
 
  
  #8  Cient e Classif em variaveis numericas ----
  
  especies <- sort(unique(dados$cient))
  especies <- data.frame("numcient" = seq(1,length(especies),1), "cient" = levels(especies))
  
  dados <- dplyr::left_join(x=dados,
                     y = especies,
                     by = "cient")
  
  classesucess <- sort(unique(dados$classif))
  classesucess <- data.frame("numclassif" = seq(1,8,1), "classif" = levels(classesucess))
  
  dados <- dplyr::left_join(x=dados,
                     y = classesucess,
                     by = "classif")
  
  
  rm(especies,classesucess)  
  
  
  #________________________________________________________________________
  ###################### 9. Criando subparcelas ###########################
  #________________________________________________________________________
  
  # Serao criadas duas matrizes/dataframes de divisas (X e Y), a fim de re-esquadrejar os individuos em parcelas menores e calcular os indices dentro de cada subparcela. Cada matriz/dataframe sera composta de uma coluna com o nome da subparcela, outra coluna com o limite inferior, e outra com o limite superior. Os intervalos de cada subparcela (LS - LI) se darao por meio de um comprimento (R) estabelecido.
  
  
  #################### 9.1 Subparcelas de 100 metros ######################
  
  R <- 100
  
  
  # Coordenada X 
  summary(dados$coordx)
  
  I.x <- plyr::round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling) #Calcula o intervalo e arredonda para cima, para um valor multiplo de R.
  
  n.x <- I.x/R #Numero de subparcelas no eixo X
  
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1)) #Nomes das subparcelas
  
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) #Limites inferiores. O ultimo valor da sequencia (to=) e a menor coordenada o intervalo - R.
  
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) #Limites superiores. Da mesma forma, o ultimo valor da sequencia e a coordenada minima + o intervalo.
  
  # Criando a Matriz
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  head(matriz.x)
  
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  
  # Coordenada Y 
  summary(dados$coordy)
  
  I.y <- plyr::round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  
  n.y <- I.y/R
  
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  
  # Atribuindo as subparcelas para cada observacao 
  #Coordenada X
  
  for (i in 1:dim(dados)[1]) {
    
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]
    
  }
  
  
  #Coordenada Y
  
  for (i in 1:dim(dados)[1]) {
    
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]
    
  }
  
  rm(matriz.x,matriz.y,R,i)
  
  # Cada observacao ficou com uma coluna identificando a subparcela x e uma identificando a subparcela y:
  
  head(dados[,c("id","vulgar","subx","suby")])
  
  # Agora basta juntar as duas colunas, de maneira similar ao comando concatenar() no excel, separando-as por um ponto. Dessa forma, cada observacao ficara contida na respectiva subparcela.
  
  dados$sub100 <- paste0(dados$subx,sep=".",dados$suby)
  
  head(dados[,c("id","vulgar","sub100")])
  
  #Pode-se remover as colunas subx e suby do dataframe, pois nao serao mais utilizadas
  dados <- dados[,-c(51,52)]
  
  # Grafico das subparcelas 
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub100)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub100))))
  
  
  
  
  #################### 9.2 Subparcelas de 50 metros ###########################
  R <- 50
  
  # Coordenada X 
  I.x <- plyr::round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling) 
  n.x <- I.x/R 
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1)) 
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) 
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) 
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  # Coordenada Y 
  I.y <- plyr::round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  n.y <- I.y/R
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  # Atribuindo as subparcelas a cada observacao 
  #Coordenada X
  for (i in 1:dim(dados)[1]) {
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]}
  
  #Coordenada Y
  for (i in 1:dim(dados)[1]) {
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]}
  rm(matriz.x,matriz.y,R,i)
  
  # Juntando as colunas subx e suby em uma nova coluna chamada sub
  dados$sub50 <- paste0(dados$subx,sep=".",dados$suby)
  dados <- dados[,-c(52,53)]
  
  # Grafico das subparcelas ----
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub50)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub50))))
  
  
  #################### 9.3 Subparcelas de 25 metros ###########################
  R <- 25
  
  # Coordenada X 
  I.x <- plyr::round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling) 
  n.x <- I.x/R 
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1)) 
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) 
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) 
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  # Coordenada Y 
  I.y <- plyr::round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  n.y <- I.y/R
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  # Atribuindo as subparcelas a cada observacao 
  #Coordenada X
  for (i in 1:dim(dados)[1]) {
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]}
  
  #Coordenada Y
  for (i in 1:dim(dados)[1]) {
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]}
  rm(matriz.x,matriz.y,R,i)
  
  # Juntando as colunas subx e suby em uma nova coluna chamada sub
  dados$sub25 <- paste0(dados$subx,sep=".",dados$suby)
  dados <- dados[,-c(53,54)]
  
  # Grafico das subparcelas 
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub25)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub25))))
  
  
  #################### 9.4 Subparcelas de 20 metros ###########################
  R <- 20
  
  
  # Coordenada X 
  I.x <- plyr::round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling) 
  n.x <- I.x/R 
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1)) 
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) 
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) 
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  # Coordenada Y 
  I.y <- plyr::round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  n.y <- I.y/R
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  # Atribuindo as subparcelas a cada observacao 
  #Coordenada X
  for (i in 1:dim(dados)[1]) {
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]}
  
  #Coordenada Y
  for (i in 1:dim(dados)[1]) {
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]}
  rm(matriz.x,matriz.y,R,i)
  
  # Juntando as colunas subx e suby em uma nova coluna chamada sub
  dados$sub20 <- paste0(dados$subx,sep=".",dados$suby)
  dados <- dados[,-c(54,55)]
  
  # Grafico das subparcelas 
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub20)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub20))))
  
  
  #################### 9.5 Subparcelas de 15 metros ###########################
  R <- 15
  
  # Coordenada X 
  I.x <- round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling) 
  n.x <- I.x/R
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1))
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) 
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) 
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  # Coordenada Y 
  I.y <- round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  n.y <- I.y/R
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  # Atribuindo as subparcelas a cada observacao 
  #Coordenada X
  for (i in 1:dim(dados)[1]) {
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]}
  
  #Coordenada Y
  for (i in 1:dim(dados)[1]) {
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]}
  rm(matriz.x,matriz.y,R,i)
  
  # Juntando as colunas subx e suby em uma nova coluna chamada sub
  dados$sub15 <- paste0(dados$subx,sep=".",dados$suby)
  dados <- dados[,-c(55,56)]
  
  # Grafico das subparcelas 
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub15)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub15))))
  
  
  #################### 9.6 Subparcelas de 10 metros ###########################
  R <- 10
  
  # Coordenada X 
  I.x <- round_any(x = max(dados$coordx)-min(dados$coordx),accuracy = R,f = ceiling)
  n.x <- I.x/R
  sub.x <- paste0(rep("X",n.x),seq(from = 1, to = n.x, by = 1)) 
  LI.x <- seq(from = min(dados$coordx), to = (min(dados$coordx)+I.x)-R, by = R) 
  LS.x <- seq(from = min(dados$coordx)+R, to = (min(dados$coordx)+I.x), by = R) 
  matriz.x <- data.frame(sub.x,LI.x,LS.x,stringsAsFactors = F)
  rm(I.x,n.x,sub.x,LI.x,LS.x)
  
  # Coordenada Y 
  I.y <- round_any(max(dados$coordy)-min(dados$coordy), accuracy = R, f = ceiling)
  n.y <- I.y/R
  sub.y <- paste0(rep("Y",n.y), seq(from = 1, to = n.y, by = 1))
  LI.y <- seq(from = min(dados$coordy), to = (min(dados$coordy)+I.y)-R, by = R)
  LS.y <- seq(from = min(dados$coordy)+R, to = (min(dados$coordy)+I.y), by = R)
  matriz.y <- data.frame(sub.y, LI.y, LS.y,stringsAsFactors = FALSE)
  rm(I.y,n.y,sub.y,LI.y,LS.y)
  
  # Atribuindo as subparcelas a cada observacao 
  #Coordenada X
  for (i in 1:dim(dados)[1]) {
    dados$subx[i] <- matriz.x$sub.x[(dados$coordx[i] >= matriz.x$LI.x) &
                                      (dados$coordx[i] < matriz.x$LS.x)]}
  
  #Coordenada Y
  for (i in 1:dim(dados)[1]) {
    dados$suby[i] <- matriz.y$sub.y[(dados$coordy[i] >= matriz.y$LI.y) &
                                      (dados$coordy[i] < matriz.y$LS.y)]}
  rm(matriz.x,matriz.y,R,i)
  
  # Juntando as colunas subx e suby em uma nova coluna chamada sub
  dados$sub10 <- paste0(dados$subx,sep=".",dados$suby)
  dados <- dados[,-c(56,57)]
  
  # Grafico das subparcelas 
  paleta <- colorRampPalette(brewer.pal(12,"Paired"))
  
  ggplot(dados[!is.na(dados$d16),]) + geom_point(aes(x=coordx,y=coordy,color=sub10)) + theme_bw() + theme(legend.position = "none") + scale_color_manual(values=paleta(length(unique(dados$sub10))))
  
  
  
  ######################### 10. IID para cada Subparcela ########################
  
  ####################### 10.1 G (Area Basal em m2/par) ##########################
  
  #Excluindo a Area transversal da arvore alvo. Do ponto de vista da competicao, manter a Area transversal da arvore alvo no indice pode inflar o valor do mesmo pelo efeito da propria arvore, especialmente para as parcelas mais pequenas. Ou seja, uma arvore de grandes dimensoes podera aumentar o valor do indice por conta de seu proprio tamanho.
  
  
  # Subparcela 100 m 
  
  #G7.100
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.100[i] <- ifelse(test = !is.na(dados$g7[i]),
                              yes = sum(dados$g7[dados$sub100 == dados$sub100[i]], na.rm=T) - dados$g7[i],
                              no = NA)
  }
  
  
  
  # Subparcela 50 m 
  
  #G7.50
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.50[i] <- ifelse(test = !is.na(dados$g7[i]),
                            yes = sum(dados$g7[dados$sub50 == dados$sub50[i]], na.rm=T) - dados$g7[i],
                            no = NA)
    
  }
  
  
  # Subparcela 25 m
  
  #G7.25
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.25[i] <- ifelse(test = !is.na(dados$g7[i]),
                            yes = sum(dados$g7[dados$sub25 == dados$sub25[i]], na.rm=T) - dados$g7[i],
                            no = NA)
    
  }

  
  # Subparcela 20 m
  
  #G7.20
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.20[i] <- ifelse(test = !is.na(dados$g7[i]),
                            yes = sum(dados$g7[dados$sub20 == dados$sub20[i]], na.rm=T) - dados$g7[i],
                            no = NA)
    
  }
  
  
  # Subparcela 15 m
  
  #G7.15
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.15[i] <- ifelse(test = !is.na(dados$g7[i]),
                            yes = sum(dados$g7[dados$sub15 == dados$sub15[i]], na.rm=T) - dados$g7[i],
                            no = NA)
    
  }
  
  
  # Subparcela 10 m
  
  #G7.10
  
  for (i in 1:dim(dados)[1]) {
    
    dados$G7.10[i] <- ifelse(test = !is.na(dados$g7[i]),
                            yes = sum(dados$g7[dados$sub10 == dados$sub10[i]], na.rm=T) - dados$g7[i],
                            no = NA)
    
  }
  
  
  ####################### 10.1.1 G (Area Basal em m2/par) ##########################
  
  #Incluindo a árvore alvo, para fins de comparação.
  
  
  # Subparcela 100 m 
  
  #G7.100
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.100[i] <- ifelse(test = !is.na(dados$g7[i]),
                              yes = sum(dados$g7[dados$sub100 == dados$sub100[i]], na.rm=T),
                              no = NA)
  }
  
  
  
  # Subparcela 50 m 
  
  #G7.50
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.50[i] <- ifelse(test = !is.na(dados$g7[i]),
                             yes = sum(dados$g7[dados$sub50 == dados$sub50[i]], na.rm=T),
                             no = NA)
    
  }
  
  
  # Subparcela 25 m
  
  #G7.25
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.25[i] <- ifelse(test = !is.na(dados$g7[i]),
                             yes = sum(dados$g7[dados$sub25 == dados$sub25[i]], na.rm=T),
                             no = NA)
    
  }
  
  
  # Subparcela 20 m
  
  #G7.20
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.20[i] <- ifelse(test = !is.na(dados$g7[i]),
                             yes = sum(dados$g7[dados$sub20 == dados$sub20[i]], na.rm=T),
                             no = NA)
    
  }
  
  
  # Subparcela 15 m
  
  #G7.15
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.15[i] <- ifelse(test = !is.na(dados$g7[i]),
                             yes = sum(dados$g7[dados$sub15 == dados$sub15[i]], na.rm=T),
                             no = NA)
    
  }
  
  
  # Subparcela 10 m
  
  #G7.10
  
  for (i in 1:dim(dados)[1]) {
    
    dados$Gnorm7.10[i] <- ifelse(test = !is.na(dados$g7[i]),
                             yes = sum(dados$g7[dados$sub10 == dados$sub10[i]], na.rm=T),
                             no = NA)
    
  }
  
  
  
  ############### 10.2 BAL (Basal Area of Larger Trees em m2/par) ##############
  
  
  # Subparcela 100 m
  
  #bal7.100
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.100[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub100 == dados$sub100[i]], na.rm=T)
    
  }
  
  
  # Subparcela 50 m
  
  #bal7.50
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.50[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub50 == dados$sub50[i]], na.rm=T)
    
  }
  
  
  # Subparcela 25 m
  
  #bal7.25
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.25[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub25 == dados$sub25[i]], na.rm=T)
    
  }
  
  
  # Subparcela 20 m
  
  #bal7.20
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.20[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub20 == dados$sub20[i]], na.rm=T)
    
  }
  
  
  # Subparcela 15 m
  
  #bal7.15
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.15[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub15 == dados$sub15[i]], na.rm=T)
    
  }
  
  
  # Subparcela 10 m
  
  #bal7.10
  
  for (i in 1:dim(dados)[1]) {
    
    dados$bal7.10[i] <- sum(dados$g7[dados$g7 > dados$g7[i] & dados$sub10 == dados$sub10[i]], na.rm=T)
    
  }
  
  
  ###################### 10.3 DAP medio por subcela ##########################
  
  # Subparcela 100 m
  
  #d7.100
  d7.100 <- tapply(dados$d7,dados$sub100,mean,na.rm=T)
  d7.100 <- data.frame("sub100"=rownames(d7.100),"d7.100"=as.vector(d7.100))
  
  
  dados <- left_join(x  = dados,
                     y  = d7.100,
                     by = "sub100")

  
  # Subparcela 50 m
  
  #d7.50
  d7.50 <- tapply(dados$d7,dados$sub50,mean,na.rm=T)
  d7.50 <- data.frame("sub50"=rownames(d7.50),"d7.50"=as.vector(d7.50))
  
  dados <- left_join(x  = dados,
                    y  = d7.50,
                    by= "sub50")

  
  # Subparcela 25 m
  
  #d7.25
  d7.25 <- tapply(dados$d7,dados$sub25,mean,na.rm=T)
  d7.25 <- data.frame("sub25"=rownames(d7.25),"d7.25"=as.vector(d7.25))

  dados <- left_join(x  = dados,
                    y  = d7.25,
                    by= "sub25")
  
  # Subparcela 20 m
  
  #d7.20
  d7.20 <- tapply(dados$d7,dados$sub20,mean,na.rm=T)
  d7.20 <- data.frame("sub20"=rownames(d7.20),"d7.20"=as.vector(d7.20))
  
  
  dados <- left_join(x  = dados,
                    y  = d7.20,
                    by= "sub20")

  # Subparcela 15 m
  
  #d7.15
  d7.15 <- tapply(dados$d7,dados$sub15,mean,na.rm=T)
  d7.15 <- data.frame("sub15"=rownames(d7.15),"d7.15"=as.vector(d7.15))
  
  dados <- left_join(x  = dados,
                    y  = d7.15,
                    by= "sub15")

  
  # Subparcela 10 m
  
  #d7.10
  d7.10 <- tapply(dados$d7,dados$sub10,mean,na.rm=T)
  d7.10 <- data.frame("sub10"=rownames(d7.10),"d7.10"=as.vector(d7.10))

  dados <- left_join(x  = dados,
                    y  = d7.10,
                    by= "sub10")


  rm(d7.100,d7.50,d7.25,d7.20,d7.15,d7.10)
  
  
  ###################### 10.3.1 DAP medio por subcela ##########################
  #Excluindo a árvore alvo
  
  # Subparcela 100 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.100[i] <- ifelse(test = !is.na(dados$d7[i]),
                                 yes = ((sum(dados$d7[dados$sub100 == dados$sub100[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub100 == dados$sub100[i] & !is.na(dados$d7)])-1)),
                                 no = NA)
  }
  
  
  # Subparcela 50 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.50[i] <- ifelse(test = !is.na(dados$d7[i]),
                                 yes = ((sum(dados$d7[dados$sub50 == dados$sub50[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub50 == dados$sub50[i] & !is.na(dados$d7)])-1)),
                                 no = NA)
  }
  
  
  
  # Subparcela 25 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.25[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = ((sum(dados$d7[dados$sub25 == dados$sub25[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub25 == dados$sub25[i] & !is.na(dados$d7)])-1)),
                                no = NA)
  }
  
  
  # Subparcela 20 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = ((sum(dados$d7[dados$sub20 == dados$sub20[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub20 == dados$sub20[i] & !is.na(dados$d7)])-1)),
                                no = NA)
  }
  
  
  # Subparcela 15 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = ((sum(dados$d7[dados$sub15 == dados$sub15[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub15 == dados$sub15[i] & !is.na(dados$d7)])-1)),
                                no = NA)
  }
  
  
  # Subparcela 10 m
  
  for (i in 1:dim(dados)[1]) {
    
    dados$dalt7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = ((sum(dados$d7[dados$sub10 == dados$sub10[i]],na.rm = T)-dados$d7[i])/(length(dados$d7[dados$sub10 == dados$sub10[i] & !is.na(dados$d7)])-1)),
                                no = NA)
  }
  
  
   
  ########## 10.4 (Glover & Hool - Igh) DAP da arvore / DAP medio da subcela ######
  
  #Subparcela 100 m
  
  #Igh7.100
  dados$Igh7.100 <- dados$d7/dados$d7.100
 
  #Subparcela 50 m
  
  #Igh7.50
  dados$Igh7.50 <- dados$d7/dados$d7.50

  #Subparcela 25 m
  
  #Igh7.25
  dados$Igh7.25 <- dados$d7/dados$d7.25

  #Subparcela 20 m
  
  #Igh7.20
  dados$Igh7.20 <- dados$d7/dados$d7.20

  #Subparcela 15 m
  
  #Igh7.15
  dados$Igh7.15 <- dados$d7/dados$d7.15

  #Subparcela 10 m
  
  #Igh7.10
  dados$Igh7.10 <- dados$d7/dados$d7.10

  
  ######################### 11. IDD Para varias Distancias ################
  
  
  ## Raios fixos
  # Sernao utilizados 3, 5, 7, 10, 12, 15, 17 e 20 metros da raio
  
  ## Raios variaveis, como em Orellana (2014), n�o ser�o testados
  
  
  ##################### Distancia euclidiana pura #########################
  #Distancia euclidiana entre a arvore alvo e as vizinhas, dentro de um raio especificado. O resultado sera o somatorio das distancias
  
  ##### Raio = 3 metros 
  
  R <- 3 #Raio
  
  #dist7.3 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.3"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }

  #viz7.3 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.3"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }

 
  # Raio = 5 metros 
  
  R <- 5 #Raio
  
  #dist7.5 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.5"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                no = NA)
    
  }
  
  
  #viz7.5 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.5"] <- ifelse(test = !is.na(dados[i,"d7"]),
                               yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                               no = NA)
  }
  
  
  # Raio = 7 metros 
  
  R <- 7 #Raio
  
  #dist7.7 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.7"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                no = NA)
    
  }
  
  
  #viz7.7 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.7"] <- ifelse(test = !is.na(dados[i,"d7"]),
                               yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                               no = NA)
  }
  
  
  # Raio = 10 metros 
  
  R <- 10 #Raio
  
  #dist7.10 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.10"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }
  
  
  #viz7.10 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.10"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }
  
  
  # Raio = 12 metros 
  
  R <- 12 #Raio
  
  #dist7.12 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.12"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }
  
  
  #viz7.12 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.12"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }
  
  
  # Raio = 15 metros 
  
  R <- 15 #Raio
  
  #dist7.15 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.15"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }
  
  
  #viz7.15 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.15"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }
  
  
  # Raio = 17 metros 
  
  R <- 17 #Raio
  
  #dist7.17 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.17"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }
  
  
  #viz7.17 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.17"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }
  
  # Raio = 20 metros 
  
  R <- 20 #Raio
  
  #dist7.20 - Distancia
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"dist7.20"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                 yes = (sum(sqrt((dados$coordx[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[sqrt((dados$coordx-dados$coordx[i])^2 + (dados$coordy-dados$coordy[i])^2) <= R & !is.na(dados$d7)]-dados$coordy[i])^2))),
                                 no = NA)
    
  }
  
  
  #viz7.20 - Numero de vizinhos
  
  for (i in 1:dim(dados)[1]) {
    
    dados[i,"viz7.20"] <- ifelse(test = !is.na(dados[i,"d7"]),
                                yes = (sum(sqrt((dados$coordx[!is.na(dados$d7)]-dados$coordx[i])^2 + (dados$coordy[!is.na(dados$d7)]-dados$coordy[i])^2) <= R))-1,
                                no = NA)
  }
  
  
  ######################### 11.1 Indice de Hegyi ###############################
  Hg7 <- function(x, R) { #x = indice da arvore alvo; R = Raio 
    alvo <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,d7,linha) %>% 
      filter(linha == x) %>% 
      column_to_rownames('linha')
    
    viz <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,d7,linha) %>% 
      filter(!is.na(dados$d7)) %>%  
      filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
      mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
      filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
      column_to_rownames('linha')
    
    
    razao <- (viz$d7/rep(alvo$d7,length(viz$d7)))
    invdist <- 1/viz$dist
    
    Hg <- sum(razao*invdist)
    
    return(Hg)
    
  }
  
  
  t1.Hg7 <- Sys.time()
  #Hg7.3
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.3[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hg7(x=i,R=3),
                               no = NA)
  }
  
  #Hg7.5
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.5[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hg7(x=i,R=5),
                               no = NA)
  }
  
  #Hg7.7
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.7[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hg7(x=i,R=7),
                               no = NA)
  }
  
  #Hg7.10
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hg7(x=i,R=10),
                                no = NA)
  }
  
  #Hg7.12
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.12[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hg7(x=i,R=12),
                                no = NA)
  }
  
  #Hg7.15
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hg7(x=i,R=15),
                                no = NA)
  }
  
  #Hg7.17
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.17[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hg7(x=i,R=17),
                                no = NA)
  }
  
  #Hg7.20
  for (i in 1:dim(dados)[1]) {
    
    dados$Hg7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hg7(x=i,R=20),
                                no = NA)
  }
  
  t2.Hg7 <- Sys.time()
 t.Hg7 <-  t2.Hg7-t1.Hg7
  
  
  
  ############## 11.2 Adaptacao Tome & Burkhart (1989) ############################
  
  # Usa diferenca entre as dimensoes, e nao a razao. Pommerening & Meador (2018) comentam que a utilizacao de diferenca pode ser mais adequado, visto que permite a ocorrencia de interacao benefica tambem, e nao so competicao.
  
  
  TB7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
    alvo <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,d7,linha) %>% 
      filter(linha == x) %>% 
      column_to_rownames('linha')
    
    viz <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,d7,linha) %>% 
      filter(!is.na(dados$d7)) %>%  
      filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
      mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
      filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
      column_to_rownames('linha')
    
    
    razao <- (viz$d7-rep(alvo$d7,length(viz$d7)))
    invdist <- 1/viz$dist
    
    TB <- sum(razao*invdist)
    
    return(TB)
    
  }
  
  t1.TB7 <- Sys.time()
  #TB7.3
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.3[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = TB7(x=i,R=3),
                            no = NA)
  }
  
  #TB7.5
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.5[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = TB7(x=i,R=5),
                            no = NA)
  }
  
  #TB7.7
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.7[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = TB7(x=i,R=7),
                            no = NA)
  }
  
  #TB7.10
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = TB7(x=i,R=10),
                             no = NA)
  }

  #TB7.12
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.12[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = TB7(x=i,R=12),
                             no = NA)
  }
  
  #TB7.15
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = TB7(x=i,R=15),
                             no = NA)
  }

  #TB7.17
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.17[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = TB7(x=i,R=17),
                             no = NA)
  }
  
  #TB7.20
  for (i in 1:dim(dados)[1]) {
    
    dados$TB7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = TB7(x=i,R=20),
                             no = NA)
  }
  
  t2.TB7 <- Sys.time()
t.TB7 <-   t2.TB7-t1.TB7
  
  
  
    ######################## 11.3 APA ################################
  #A Area potencialmente disponivel foi calculada com base nos poligonos de Voronoi.
library(ggvoronoi)
  par(mar=rep(1,4))
  
  
  pontos.7 <- with(dados[!is.na(dados$d7),], ppp(x = coordx, 
                      y = coordy, 
                      c(min(coordx),max(coordx)),
                      c(min(coordy),max(coordy)),
                      marks = dados[!is.na(dados$d7),]))
  
  
  tesselacao.7 <- dirichlet(pontos.7)

  par(mfrow=c(1,2))
  
  plot(tesselacao.7)
  plot(pontos.7, use.marks = FALSE,add=TRUE, col = "black",cex=0.5,pch=16)
  
  par(mfrow=c(1,1))
  
  poligonos.7 <- tiles(tesselacao.7) #Extraindo info de cada poligono da tessel.
  
  areas.7 <- sapply(poligonos.7, area) #Calculando Area de cada poligono
  
  areas.7 <- data.frame("poligonos.7" = attr(areas.7,"names"), areas.7)
  
  diametros.7 <- sapply(poligonos.7, diameter) #Calcula diametros de cada poligono
  
  diametros.7 <- data.frame("poligonos.7" = attr(diametros.7,"names"), diametros.7)
  
  info.7 <- cbind(areas.7,"diametros.7" = diametros.7[,2]) #Junta area e diametro
  
  #O nome de cada poligono ficou como "tile #". E preciso retirar os caracteres e manter somente os Numeros, a fim de posteriormente utiliza-los para extrair os poligonos vizinhos.
  require(readr)
  
  info.7$num.viz <- parse_number(info.7$poligonos.7)
  
  info.7 <- info.7[,-1]
  
  colnames(info.7) <- c("areas.7","diametros.7","poligonos.7")
  
  #Para calcular o Numero de poligonos vizinhos, basta contabilizar o Numero de vertices (ou par de coordenadas xy) de cada poligono. Na funcao abaixo, foi calculado o comprimento do vetor de coordenadas x de cada poligono
  length(poligonos.7[[5]][[4]][[1]][[1]])
  
  for (i in 1:dim(info.7)[1]) {
    info.7$num.viz.7[i] <- length(poligonos.7[[i]][[4]][[1]][[1]])
  }
  
  #Nos casos de borda, ha a superestimativa do Numero de vizinhos, uma vez que uma (ou duas nos cantos) das arestas e o limite da tesselacao.
  
  #Esse calculo foi realizado mais como prova para o calculo a seguir, que e de maior complexidade e necessitava de um valor correto para comparacao.
  
  #________________________________________________________________
  
  require(sp)
  require(rgeos)
  
  voroni_to_polys <- function(dd) {
    
    vor_desc <- tiles(dd) # Extrair os dados dos poligonos da tesselacao
    
    # ^^ nos fornece os pares de pontos para os poligonos, mas ainda precisamos fecha-los, por isso a necessidade do comando rbind abaixo.
    
    
    lapply(1:(length(vor_desc)), function(i) {
      
      tmp <- cbind(vor_desc[[i]][[4]][[1]][[1]], vor_desc[[i]][[4]][[1]][[2]])
      tmp <- rbind(tmp, tmp[1,])  #Replica a primeira linha novamente embaixo dos valores, para que o metodo gaussiano possa ser utilizado para calculo das Areas.
      
      Polygons(list(Polygon(tmp)), ID=i) # Agora podemos fazer os poligonos
      
    }) -> vor_polygons
    
    # Um data frame dummy para facilitar a identificacao dos poligonos
    xdf <- data.frame(id=sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID'))
    
    rownames(xdf) <- xdf$id
    
    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=xdf)
    
  }
  
  vpoly.7 <- voroni_to_polys(tesselacao.7)
  
  #a funcao gTouches avalia poligonos e informa se possuem algum limite em comum. Ela retorna uma matriz quadrada de valores logicos, indicando se ha ou nao coincidencia de limites entre os poligonos.
  matriz.7 <- gTouches(vpoly.7, byid=TRUE)
  
  matriz.7 <- as.data.frame(matriz.7)
  
  #Para saber o Numero de limites, basta somar a linha ou coluna da matriz. O resultado e o Numero de ocorrencias em que ha limite comum.
  sum(matriz.7[,500] == TRUE)
  #Para saber quais sao os vizinhos, basta utilizar a funcao "which()"
  
  rownames(matriz.7[which(matriz.7[,225] == TRUE),])
  
  #Esses comandos estao disponiveis em (https://stackoverflow.com/questions/47664041/find-elements-that-share-a-common-voronoi-boundary)
  
  #________________________________________________________________
  
  
  inters.7 <- cut(pontos.7,tesselacao.7) #Interseccao - Associa cada poligono com o respectivo ponto
  
  marcas.7 <- cbind(pontos.7$marks,"poligonos.7" = inters.7$marks) # Adiciona o respectivo poligono a cada observacao nas marcas do objeto pontos
  
  marcas.7 <- marcas.7[,c("id","poligonos.7")]
  
  
  marcas.7$num.poli <- parse_number(as.character(marcas.7$poligonos.7))
  
  marcas.7 <- marcas.7[,c(1,3)]
  
  colnames(marcas.7) <- c("id","poligonos.7")
  
  
  #calculo do Numero real de vizinhos
  
  for (i in 1:dim(info.7)[1]) {
     info.7$num.viz.7[i] <- sum(matriz.7[,i] == TRUE)}

  #Associa a Area e diametro de cada poligono para cada observacao da base de dados com base na coluna "poligonos"
  marcas.7 <- left_join(x = marcas.7,        
                      y = as.data.frame(info.7),
                      by = "poligonos.7")
  
  
  dados <- left_join(x = dados,
                    y = marcas.7,
                    by = "id")
  
  
  #Essas operacoes foram necessarias pois a funcao sapply retorna a lista de poligonos em ordem numerica, e nao necessariamente na ordem dos pontos do objeto "pontos". Por isso, foi necessaria a utilizacao da funcao "cut()", que associa cada poligono no seu devido ponto. Por fim, foram associadas a Area e diametro de cada poligono nas marcas do objeto "pontos".
  
  
  #Agora sera preciso adicionar uma coluna com os vizinhos de cada poligono. Ou seja, cada linha dessa nova coluna devera ser um vetor contendo os nomes dos vizinhos. Uma solucao para isso e a utilizacao de listas.
  
  
  poli.viz.7 <- data.frame("poligonos.7" = as.numeric(rownames(matriz.7)), "vizinhos.7" = NA)
  
  for (i in 1:dim(poli.viz.7)[1]) {
   poli.viz.7$vizinhos.7[i] <- list(as.numeric(rownames(matriz.7[which(matriz.7[,i] == TRUE),])))
  }
  

  dados <- left_join(x = dados,
                    y = poli.viz.7,
                    by = "poligonos.7")
  
  
  #Teste 7
  plot(tesselacao.7) #tesselacao
  plot(tesselacao.7$tiles[[225]], add = TRUE, col = "red") #poligono 225
  
  #Agora quero plotar os vizinhos do poligono 1000
  dados$vizinhos.7[[which(dados$poligonos.7 == 225)]]
  
  #Tentativa
  for (i in dados$vizinhos.7[[which(dados$poligonos.7 == 225)]]) {
    
    plot(tesselacao.7$tiles[[i]], add = TRUE, col = "blue")
    
  }
    
  
  # Criando funcoes para esses Graficos
  
  g.viz.7 <- function(poligono) {
    
    plot(tesselacao.7)
    plot(tesselacao.7$tiles[[poligono]], add = TRUE, col = "red")
    
    vizinhos <- dados$vizinhos.7[[which(dados$poligonos.7 == poligono)]]
    
    for (i in vizinhos) {
      plot(tesselacao.7$tiles[[i]], add = TRUE, col = "blue")
    }
    
  }
    
  g.viz.7(225)
  
  c <- ppp(x=dados[225,"coordx"],y=dados[225,"coordy"],c(min(dados$coordx),max(dados$coordx)),c(min(dados$coordy),max(dados$coordy)))
  
  plot(c,add = TRUE, pch=16, cex=0.5, col = "black")
  
  
  # 11.3.1 Relacao entre APA alvo e Media das vizinhas ################
  
  APA7m.f <- function(x) {
   
    tmp <- dados$vizinhos.7[[x]]
    areas <- dados$areas.7[dados$poligonos.7 %in% tmp]
    areas <- unique(areas) #necessario pois ha arvores bifurcadas que contariam como dois poligonos, e infelizmente nao foi possivel contornar esse problema.
    APA7.m <- dados$areas.7[x]/mean(areas)
    return(APA7.m) 
  }
  
  
  for (i in 1:dim(dados)[1]) {
    dados$APA7.m[i] <- APA7m.f(x = i)
  }
  
  
  # 11.3.2 APA alvo e cada uma das vizinhas
  
  
  APA7v.f <- function(x) {
    
    tmp <- dados$vizinhos.7[[x]]
    areas <- dados$areas.7[dados$poligonos.7 %in% tmp]
    areas <- unique(areas)
    APA7.v <- sum(rep(dados$areas.7[x],length(tmp))/areas)
    return(APA7.v)
  }
    
  
 
  for (i in 1:dim(dados)[1]) {
    
    dados$APA7.v[i] <- APA7v.f(x = i)
  }
  
  
  dados$APA7.v[dados$APA7.v == 0] <- NA
  
  
  # 12 Hg versao alternativa ####
  
  ##somente com os vizinhos de estrato igual ou superior ao da arvore alvo
  
  #funcao para descobrir o Numero de vizinhos, calcular Distancia e gerar o Indice
  
  
  #  Hgalt7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
  #    alvo <- dados %>% rownames_to_column('linha') %>% 
  #      dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
  #      filter(linha == x) %>% 
  #      column_to_rownames('linha')
  #    
  #     viz <- dados %>% rownames_to_column('linha') %>% 
  #       dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
  #       filter(!is.na(dados$d7)) %>%  
  #       filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
  #       mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
  #       filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
  #       filter(st7 >= alvo$st7) %>% 
  #       column_to_rownames('linha')
  #   
  #     
  #     razao <- (viz$d7/rep(alvo$d7,length(viz$d7)))
  #     invdist <- 1/viz$dist
  #     
  #     Hg <- sum(razao*invdist)
  #     
  #     return(Hg)
  #     
  # }
  #   
 
  
   Hgalt7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
    alvo <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
      filter(linha == x) %>% 
      column_to_rownames('linha')
    
    viz <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
      filter(!is.na(dados$d7)) %>%  
      filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
      mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
      filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
      filter(st7 >= alvo$st7) %>% 
      column_to_rownames('linha')
    
    
    razao <- (viz$d7/rep(alvo$d7,length(viz$d7)))
    invdist <- 1/viz$dist
    
    Hg <- sum(razao*invdist)
    
    return(Hg)
    
  }
  
  #Teste
  
  x <- 225
  R <- 10
   
  alvo <- dados %>% rownames_to_column('linha') %>% 
    dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
    slice(x) %>% 
    column_to_rownames('linha')
  
  viz <- dados %>% rownames_to_column('linha') %>% 
    dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
    filter(!is.na(dados$d7)) %>%  
    filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
    mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
    filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
    filter(st7 >= alvo$st7) %>% 
    column_to_rownames('linha')
  
  
  ggplot(dados[!is.na(dados$d7),]) + 
    geom_point(aes(coordx,coordy,color = as.factor(st7))) + 
    geom_point(data = alvo,aes(coordx,coordy, color = as.factor(st7))) + 
    coord_cartesian(xlim=c(alvo$coordx-2*R,alvo$coordx+2*R),ylim=c(alvo$coordy-2*R,alvo$coordy+2*R)) + 
    geom_circle(data = alvo,aes(x0 = coordx,y0 = coordy, r = R), stat = "circle",position="identity") + 
    labs(x="X",y = "Y", subtitle = "Banco de dados original") + 
    theme_bw() + 
    scale_color_brewer(palette = "Dark2", name = "Estratos")
  
  
  ggplot(viz) + 
    geom_point(aes(coordx,coordy,color = as.factor(st7))) + 
    geom_point(data = alvo,aes(coordx,coordy,color = as.factor(st7))) + 
    coord_cartesian(xlim=c(alvo$coordx-2*R,alvo$coordx+2*R),ylim=c(alvo$coordy-2*R,alvo$coordy+2*R)) + 
    geom_circle(data = alvo,aes(x0 = coordx,y0 = coordy, r = R), stat = "circle",position="identity") + 
    labs(x="X",y = "Y", subtitle = "Vizinhos selecionados") + 
    theme_bw() + 
    scale_color_brewer(palette = "Dark2", name = "Estratos")
  
  rm(viz,alvo,x,R)
  
  
  t1.Hgalt7 <- Sys.time()
  #Hgalt7.3
  for (i in 1:dim(dados)[1]) {
    
  dados$Hgalt7.3[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = Hgalt7(x=i,R=3),
                             no = NA)
  }
  
  #Hgalt7.5
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.5[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hgalt7(x=i,R=5),
                               no = NA)
  }
  
  
  #Hgalt7.7
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.7[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hgalt7(x=i,R=7),
                               no = NA)
  }

  #Hgalt7.10
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = Hgalt7(x=i,R=10),
                               no = NA)
  }
  
  #Hgalt7.12
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.12[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hgalt7(x=i,R=12),
                                no = NA)
  }
  
  
  #Hgalt7.15
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hgalt7(x=i,R=15),
                                no = NA)
  }
  
  #Hgalt7.17
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.17[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hgalt7(x=i,R=17),
                                no = NA)
  }
  
  
  #Hgalt7.20
  for (i in 1:dim(dados)[1]) {
    
    dados$Hgalt7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Hgalt7(x=i,R=20),
                                no = NA)
  }
  
  t2.Hgalt7 <- Sys.time()
t.Hgalt7 <-   t2.Hgalt7-t1.Hgalt7
  
  
#_______________________________________________________________________
### 13 TB Versaoo adaptada ###############################################
#_______________________________________________________________________

##somente com os vizinhos de estrato igual ou superior ao da arvore alvo

#funcao para descobrir o Numero de vizinhos, calcular Distancia e gerar o Indice


#  TBalt7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
#    alvo <- dados %>% rownames_to_column('linha') %>% 
#      dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
#      filter(linha == x) %>% 
#      column_to_rownames('linha')
#    
#     viz <- dados %>% rownames_to_column('linha') %>% 
#       dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
#       filter(!is.na(dados$d7)) %>%  
#       filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
#       mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
#       filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
#       filter(st7 >= alvo$st7) %>% 
#       column_to_rownames('linha')
#   
#     
#     subtracao <- (viz$d7-rep(alvo$d7,length(viz$d7)))
#     invdist <- 1/viz$dist
#     
#     TB <- sum(subtracao*invdist)
#     
#     return(TB)
#     
# }
#   


TBalt7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
  alvo <- dados %>% rownames_to_column('linha') %>% 
    dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
    filter(linha == x) %>% 
    column_to_rownames('linha')
  
  viz <- dados %>% rownames_to_column('linha') %>% 
    dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
    filter(!is.na(dados$d7)) %>%  
    filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
    mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
    filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
    filter(st7 >= alvo$st7) %>% 
    column_to_rownames('linha')
  
  
  subtracao <- (viz$d7-rep(alvo$d7,length(viz$d7)))
  invdist <- 1/viz$dist
  
  TB <- sum(subtracao*invdist)
  
  return(TB)
  
}

#Teste

x <- 225
R <- 10

alvo <- dados %>% rownames_to_column('linha') %>% 
  dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
  slice(x) %>% 
  column_to_rownames('linha')

viz <- dados %>% rownames_to_column('linha') %>% 
  dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
  filter(!is.na(dados$d7)) %>%  
  filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
  mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
  filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
  filter(st7 >= alvo$st7) %>% 
  column_to_rownames('linha')


ggplot(dados[!is.na(dados$d7),]) + 
  geom_point(aes(coordx,coordy,color = as.factor(st7))) + 
  geom_point(data = alvo,aes(coordx,coordy, color = as.factor(st7))) + 
  coord_cartesian(xlim=c(alvo$coordx-2*R,alvo$coordx+2*R),ylim=c(alvo$coordy-2*R,alvo$coordy+2*R)) + 
  geom_circle(data = alvo,aes(x0 = coordx,y0 = coordy, r = R), stat = "circle",position="identity") + 
  labs(x="X",y = "Y", subtitle = "Banco de dados original") + 
  theme_bw() + 
  scale_color_brewer(palette = "Dark2", name = "Estratos")


ggplot(viz) + 
  geom_point(aes(coordx,coordy,color = as.factor(st7))) + 
  geom_point(data = alvo,aes(coordx,coordy,color = as.factor(st7))) + 
  coord_cartesian(xlim=c(alvo$coordx-2*R,alvo$coordx+2*R),ylim=c(alvo$coordy-2*R,alvo$coordy+2*R)) + 
  geom_circle(data = alvo,aes(x0 = coordx,y0 = coordy, r = R), stat = "circle",position="identity") + 
  labs(x="X",y = "Y", subtitle = "Vizinhos selecionados") + 
  theme_bw() + 
  scale_color_brewer(palette = "Dark2", name = "Estratos")

rm(viz,alvo,x,R)


t1.TBalt7 <- Sys.time()
#Hgalt7.3
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.3[i] <- ifelse(test = !is.na(dados$d7[i]),
                              yes = TBalt7(x=i,R=3),
                              no = NA)
}

#TBalt7.5
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.5[i] <- ifelse(test = !is.na(dados$d7[i]),
                              yes = TBalt7(x=i,R=5),
                              no = NA)
}


#TBalt7.7
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.7[i] <- ifelse(test = !is.na(dados$d7[i]),
                              yes = TBalt7(x=i,R=7),
                              no = NA)
}

#TBalt7.10
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = TBalt7(x=i,R=10),
                               no = NA)
}

#TBalt7.12
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.12[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = TBalt7(x=i,R=12),
                               no = NA)
}


#TBalt7.15
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = TBalt7(x=i,R=15),
                               no = NA)
}

#TBalt7.17
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.17[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = TBalt7(x=i,R=17),
                               no = NA)
}


#TBalt7.20
for (i in 1:dim(dados)[1]) {
  
  dados$TBalt7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                               yes = TBalt7(x=i,R=20),
                               no = NA)
}

t2.TBalt7 <- Sys.time()
t.TBalt7 <-   t2.TBalt7-t1.TBalt7

  
  #__________________________________________________________________
  ### 14 Martin-EK ##################################
  #__________________________________________________________________
  
  
  Ek7 <- function(x, R) { #x = Indice da arvore alvo; R = Raio 
    alvo <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,coordx,coordy,st7,d7,linha) %>% 
      filter(linha == x) %>% 
      column_to_rownames('linha')
    
    viz <- dados %>% rownames_to_column('linha') %>% 
      dplyr::select(id,st7,coordx,coordy,d7,linha) %>% 
      filter(!is.na(dados$d7)) %>%  
      filter(sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2) <= R) %>%
      mutate(dist = sqrt((coordx-alvo$coordx)^2 + (coordy-alvo$coordy)^2)) %>%
      filter(coordx != alvo$coordx | coordy != alvo$coordy) %>% 
      filter(st7 >= alvo$st7) %>% 
      column_to_rownames('linha')
    
    alvod <- rep(alvo$d7,length(viz$d7))
    
    razao <- (viz$d7/alvod)
    exp <- exp(viz$dist/(alvo$d7+viz$d7))
    
    Ek <- sum(razao*exp)
    
    return(Ek)
    
  }


#Ek7.3
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.3[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=3),
                            no = NA)
}


#Ek7.5
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.5[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=5),
                            no = NA)
}



#Ek7.7
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.7[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=7),
                            no = NA)
}



#Ek7.10
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.10[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=10),
                            no = NA)
}


#Ek7.12
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.12[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=12),
                            no = NA)
}


 #Ek7.15
  for (i in 1:dim(dados)[1]) {
    
    dados$Ek7.15[i] <- ifelse(test = !is.na(dados$d7[i]),
                                yes = Ek7(x=i,R=15),
                                no = NA)
  }

#Ek7.17
for (i in 1:dim(dados)[1]) {
  
  dados$Ek7.17[i] <- ifelse(test = !is.na(dados$d7[i]),
                            yes = Ek7(x=i,R=17),
                            no = NA)
}
  
  #Ek7.20
  for (i in 1:dim(dados)[1]) {
    
    dados$Ek7.20[i] <- ifelse(test = !is.na(dados$d7[i]),
                             yes = Ek7(x=i,R=20),
                             no = NA)
  }
  
  
  ########################### EXPORTAR DADOS ##########################
 #removendo (vizinhos.7), que sao as listas de vizinhos. E preciso remove-las pois nao ha como criar um arquivo .csv tendo uma coluna com a classe lista.
  
dados <- dados %>% dplyr::select(-vizinhos.7)
  
   write.csv2(x = dados,
             file = "processed_data.csv",
             sep = ";",
             quote = c(51:56),
             dec = ",",
             row.names = FALSE)
  

   write.csv2(x = sp,
              file = "species.csv",
              sep = ";",
              dec = ",",
              row.names = FALSE)
      
    