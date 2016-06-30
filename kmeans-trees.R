library(cluster)
library(HSAUR)
library(rattle)
library(party)
data <- read.csv("minable.csv", header = TRUE,sep =",")
data$cIdentidad = NULL
data$sexo= NULL
data$cIdentidad = NULL
data$aResponsable = NULL
data$aEconomica = NULL
data$oSolicitudes = NULL
data$cDireccion = NULL
data$dHabitacion = NULL
data$beca = NULL
data$pReside = NULL
data$jReprobadas = NULL
data$sugerencias = NULL
data$fNacimiento = NULL
data$grOdontologicos = NULL
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

train <- na.omit(train)
kdata <- scale(train)

wss <- (nrow(kdata)-1)*sum(apply(kdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(kdata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(kdata, 3) # 5 cluster solution
# get cluster means 
aggregate(kdata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
kdata <- data.frame(kdata, fit$cluster)

fit <- kmeans(kdata, 3)
clusplot(kdata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
#arbol = rpart(kdata$eficiencia ~ ., data = train, method = 'class', control = rpart.control(minsplit = 20, cp = 0.01, maxdepth = 5))
arbol = ctree(kdata$eficiencia ~ ., data = train)
plot(arbol)
text(arbol,pretty = 0)
fancyRpartPlot(arbol)


