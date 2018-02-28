data1 <- read.csv2('ex1data1.txt', header=FALSE, sep=',')
colnames(data1) <- c('Poblacion', 'Ganancia')

sapply(data1, class)

data1$Poblacion <- as.numeric(as.character(data1$Poblacion))
data1$Ganancia <- as.numeric(as.character(data1$Ganancia))

sapply(data1, class)

plot(x=data1$Poblacion, y=data1$Ganancia, main='Poblacion vs Ganancia', xlab='Poblacion', ylab='Ganancia')

line = lm(data1$Ganancia ~ data1$Poblacion)
summary(line)

# In R the index of an array start on 1
predictGanancia <- function (poblacion, modelLine)
{
  # Converts dataframe to matrix for allow the acces to individual cells https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.matrix.html
  coefMat <- data.matrix(modelLine$coefficients)
  return(coefMat[1] + (poblacion * coefMat[2]))
}

predictGanancia(3.5, line)*10000

predictGanancia(7, line)*10000


abline(line)