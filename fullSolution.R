#Dane tylko dla dwustu pierwszych wierszy

library(data.table)
training <- fread("trainingData.csv", header = TRUE)
training1 <- data.matrix(training)
remove(training)
gc()

chunktr1 <- training1[1:5000,]
chunktr2 <- training1[5001:15000,]
chunktr3 <- training1[15001:19999,]
remove(training1)
gc()

test1 <- fread("testData.csv")
test <- data.matrix(test1)

remove(test1)
gc()

chunkte1 <- test[1:10000,]
chunkte2 <- test[10001:20000,]

remove(test)
gc()


#'Teraz tabela training zawiera te same dane, posortowane wzgledem pierwszej decyzji
#'Teraz bede chcial obliczyc statystyke
#'Pierwsze 42 kolumny zostawiam bez niczego, tam sa juz srednie wiec nic z tym nie zrobie
#'Potem wyciagam srednie z reszty

cols <- c(0:399)
m <- list()
for (k in 1:42) {
  m[[k]] <- chunktr1[,unlist(lapply(cols, function(i) {42+k+1+i*43}))]
}


#Średnie kroczące dla kazdego strazaka
#Dlugosc okienka dobrana na podstawie rzutu urojona moneta

windowList <- list(c(1:50), c(51:100), c(101:150), c(201:250), c(251:300), c(301:325), c(326:350), c(351:375),c(376:390), c(391:400))
srednieKroczace <- matrix(0, ncol=(length(windowList)+5)*(42), nrow=dim(chunktr1)[1])
result <- vector()

#Obliczanie sredniej kroczacej, statystyk i lokalnych maksimow
for(i in 1:dim(chunktr1)[1]) { 
  result <-vector()
  srednieDoWpisania <- vector()
  
  for(j in 1:42){
    sr <- as.vector(unlist(lapply(windowList, function(v){ mean(m[[j]][i,v])}))) #Ok
    dyn <- sum(unlist(lapply(2:9, function(i) {if( (sr[i]-sr[i-1])*(sr[i+1]-sr[i]) < 0 ) {1} else {0}})))
    kolejnaPorcja <- c(sr, max(m[[j]][i,]), min(m[[j]][i,]), mean(m[[j]][i,]), sqrt(var(m[[j]][i,])), dyn*3)
    result <- c(result, kolejnaPorcja)
  }
  srednieKroczace[i,] <- result
}

test <- srednieKroczace


test <- cbind(chunktr1[,1:42], test)
write.csv(test, "chunktr11.csv")








cols <- c(0:399)
m <- list()
for (k in 1:42) {
  m[[k]] <- chunktr2[,unlist(lapply(cols, function(i) {42+k+1+i*43}))]
}


#Średnie kroczące dla kazdego strazaka
#Dlugosc okienka dobrana na podstawie rzutu urojona moneta

windowList <- list(c(1:50), c(51:100), c(101:150), c(201:250), c(251:300), c(301:325), c(326:350), c(351:375),c(376:390), c(391:400))
srednieKroczace <- matrix(0, ncol=(length(windowList)+5)*(42), nrow=dim(chunktr2)[1])
result <- vector()

#Obliczanie sredniej kroczacej, statystyk i lokalnych maksimow
for(i in 1:dim(chunktr2)[1]) { 
  result <-vector()
  srednieDoWpisania <- vector()
  
  for(j in 1:42){
    sr <- as.vector(unlist(lapply(windowList, function(v){ mean(m[[j]][i,v])}))) #Ok
    dyn <- sum(unlist(lapply(2:9, function(i) {if( (sr[i]-sr[i-1])*(sr[i+1]-sr[i]) < 0 ) {1} else {0}})))
    kolejnaPorcja <- c(sr, max(m[[j]][i,]), min(m[[j]][i,]), mean(m[[j]][i,]), sqrt(var(m[[j]][i,])), dyn*6)
    result <- c(result, kolejnaPorcja)
  }
  srednieKroczace[i,] <- result
}

test <- srednieKroczace


test <- cbind(chunktr2[,1:42], test)
write.csv(test, "chunktr22.csv")









cols <- c(0:399)
m <- list()
for (k in 1:42) {
  m[[k]] <- chunktr3[,unlist(lapply(cols, function(i) {42+k+1+i*43}))]
}


#Średnie kroczące dla kazdego strazaka
#Dlugosc okienka dobrana na podstawie rzutu urojona moneta

windowList <- list(c(1:50), c(51:100), c(101:150), c(201:250), c(251:300), c(301:325), c(326:350), c(351:375),c(376:390), c(391:400))
srednieKroczace <- matrix(0, ncol=(length(windowList)+5)*(42), nrow=dim(chunktr3)[1])
result <- vector()

#Obliczanie sredniej kroczacej, statystyk i lokalnych maksimow
for(i in 1:dim(chunktr3)[1]) { 
  result <-vector()
  srednieDoWpisania <- vector()
  
  for(j in 1:42){
    sr <- as.vector(unlist(lapply(windowList, function(v){ mean(m[[j]][i,v])}))) #Ok
    dyn <- sum(unlist(lapply(2:9, function(i) {if( (sr[i]-sr[i-1])*(sr[i+1]-sr[i]) < 0 ) {1} else {0}})))
    kolejnaPorcja <- c(sr, max(m[[j]][i,]), min(m[[j]][i,]), mean(m[[j]][i,]), sqrt(var(m[[j]][i,])), dyn*7)
    result <- c(result, kolejnaPorcja)
  }
  srednieKroczace[i,] <- result
}

test <- srednieKroczace


test <- cbind(chunktr3[,1:42], test)
write.csv(test, "chunktr33.csv")





cols <- c(0:399)
m <- list()
for (k in 1:42) {
  m[[k]] <- chunkte1[,unlist(lapply(cols, function(i) {42+k+1+i*43}))]
}


#Średnie kroczące dla kazdego strazaka
#Dlugosc okienka dobrana na podstawie rzutu urojona moneta

windowList <- list(c(1:50), c(51:100), c(101:150), c(201:250), c(251:300), c(301:325), c(326:350), c(351:375),c(376:390), c(391:400))
srednieKroczace <- matrix(0, ncol=(length(windowList)+5)*(42), nrow=dim(chunkte1)[1])
result <- vector()

#Obliczanie sredniej kroczacej, statystyk i lokalnych maksimow
for(i in 1:dim(chunkte1)[1]) { 
  result <-vector()
  srednieDoWpisania <- vector()
  
  for(j in 1:42){
    sr <- as.vector(unlist(lapply(windowList, function(v){ mean(m[[j]][i,v])}))) #Ok
    dyn <- sum(unlist(lapply(2:9, function(i) {if( (sr[i]-sr[i-1])*(sr[i+1]-sr[i]) < 0 ) {1} else {0}})))
    kolejnaPorcja <- c(sr, max(m[[j]][i,]), min(m[[j]][i,]), mean(m[[j]][i,]), sqrt(var(m[[j]][i,])), dyn*10)
    result <- c(result, kolejnaPorcja)
  }
  srednieKroczace[i,] <- result
}

test <- srednieKroczace


test <- cbind(chunkte1[,1:42], test)
write.csv(test, "chunkte11.csv")





cols <- c(0:399)
m <- list()
for (k in 1:42) {
  m[[k]] <- chunkte2[,unlist(lapply(cols, function(i) {42+k+1+i*43}))]
}


#Średnie kroczące dla kazdego strazaka
#Dlugosc okienka dobrana na podstawie rzutu urojona moneta

windowList <- list(c(1:50), c(51:100), c(101:150), c(201:250), c(251:300), c(301:325), c(326:350), c(351:375),c(376:390), c(391:400))
srednieKroczace <- matrix(0, ncol=(length(windowList)+5)*(42), nrow=dim(chunkte2)[1])
result <- vector()

#Obliczanie sredniej kroczacej, statystyk i lokalnych maksimow
for(i in 1:dim(chunkte2)[1]) { 
  result <-vector()
  srednieDoWpisania <- vector()
  
  for(j in 1:42){
    sr <- as.vector(unlist(lapply(windowList, function(v){ mean(m[[j]][i,v])}))) #Ok
    dyn <- sum(unlist(lapply(2:9, function(i) {if( (sr[i]-sr[i-1])*(sr[i+1]-sr[i]) < 0 ) {1} else {0}})))
    kolejnaPorcja <- c(sr, max(m[[j]][i,]), min(m[[j]][i,]), mean(m[[j]][i,]), sqrt(var(m[[j]][i,])), dyn*10)
    result <- c(result, kolejnaPorcja)
  }
  srednieKroczace[i,] <- result
}

test <- srednieKroczace


test <- cbind(chunkte2[,1:42], test)
write.csv(test, "chunkte22.csv")







lab <- read.csv("trainingLabels.csv")


chunkTr1 <- read.csv("chunktr11.csv")
chunkTr2 <- read.csv("chunktr22.csv")
chunkTr3 <- read.csv("chunktr33.csv")

training <- rbind(chunkTr1, chunkTr2, chunkTr3)
training <- training[,-1]
train <- cbind(training,lab)

chunkTe1 <- read.csv("chunkte11.csv")
chunkTe2 <- read.csv("chunkte22.csv")

test <- rbind(chunkTe1, chunkTe2)
test <- test[,-1]

dec2 <- 674
dec1 <- 673

library(kknn)

colnames(train) <- c(colnames(test),"dec1","dec2")
trainSt <- train[,-dec2]
trainAct <- train[,-dec1]



predknn <- kknn(dec1~., trainSt, test, k=1, distance=1, kernel="triangular")$fit
od <- predknn
od <- as.character(od)
write.csv(predknn, "odpowiedziknn1.csv", row.names = FALSE)


predknn2 <- kknn(dec2~., trainAct, test, k=4, distance=1, kernel="triangular")$fit
od <- predknn2
od <- as.character(od)
write.csv(predknn2, "odpowiedziknnlabel2_3_rozne_wagi.csv", row.names = FALSE,col.names=FALSE)

y <- read.csv("odpowiedziknnlabel2_3_rozne_wagi.csv")
x <- read.csv("odpowiedziknn1.csv")
z <- cbind(x,y)

write.csv(z, "11.csv", col.names=FALSE, row.names=FALSE)
