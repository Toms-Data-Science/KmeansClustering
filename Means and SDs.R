MeanSDMatrix = matrix(data = NA, nrow = 0, ncol =8, byrow = TRUE)

SpeciesVect = iris$Species

SetSel = SpeciesVect == 'setosa'
Setosa = iris[SetSel,]
SetosaSL = c(mean(Setosa$Sepal.Length),sd(Setosa$Sepal.Length))
SetosaSW = c(mean(Setosa$Sepal.Width),sd(Setosa$Sepal.Width))
SetosaPL = c(mean(Setosa$Petal.Length),sd(Setosa$Petal.Length))
SetosaPW = c(mean(Setosa$Petal.Width),sd(Setosa$Petal.Width))
SetosaRow = c(SetosaSL,SetosaSW,SetosaPL,SetosaPW)

VersSel = SpeciesVect == 'versicolor'
Versicolor = iris[VersSel,]
VersicSL = c(mean(Versicolor$Sepal.Length),sd(Versicolor$Sepal.Length))
VersicSW = c(mean(Versicolor$Sepal.Width),sd(Versicolor$Sepal.Width))
VersicPL = c(mean(Versicolor$Petal.Length),sd(Versicolor$Petal.Length))
VersicPW = c(mean(Versicolor$Petal.Width),sd(Versicolor$Petal.Width))
VersicRow = c(VersicSL,VersicSW,VersicPL,VersicPW)

VirgSel = SpeciesVect == 'virginica'
Virginica = iris[VirgSel,]
VirginSL = c(mean(Virginica$Sepal.Length),sd(Virginica$Sepal.Length))
VirginSW = c(mean(Virginica$Sepal.Width),sd(Virginica$Sepal.Width))
VirginPL = c(mean(Virginica$Petal.Length),sd(Virginica$Petal.Length))
VirginPW = c(mean(Virginica$Petal.Width),sd(Virginica$Petal.Width))
VirginRow = c(VirginSL,VirginSW,VirginPL,VirginPW)

colnames(MeanSDMatrix) = c('SL Mean', 'SD','SW Mean','SD','PL Mean','SD','PW Mean','SD')
rbind(MeanSDMatrix,SetosaRow,VersicRow,VirginRow)
MeanSDMatrix

SVeSepL = t.test(Setosa$Sepal.Length,Versicolor$Sepal.Length)
SViSepL = t.test(Setosa$Sepal.Length,Virginica$Sepal.Length)
ViVeSepL = t.test(Versicolor$Sepal.Length,Virginica$Sepal.Length)

SVeSepW = t.test(Setosa$Sepal.Width,Versicolor$Sepal.Width)
SViSepW = t.test(Setosa$Sepal.Width,Virginica$Sepal.Width)
ViVeSepW = t.test(Versicolor$Sepal.Width,Virginica$Sepal.Width)

SVePetL = t.test(Setosa$Petal.Length,Versicolor$Petal.Length)
SViPetL = t.test(Setosa$Petal.Length,Virginica$Petal.Length)
ViVePetL = t.test(Versicolor$Petal.Length,Virginica$Petal.Length)

SVePetW = t.test(Setosa$Petal.Width,Versicolor$Petal.Width)
SViPetW = t.test(Setosa$Petal.Width,Virginica$Petal.Width)
ViVePetW = t.test(Versicolor$Petal.Width,Virginica$Petal.Width)

Sepal_length = c(SVeSepL$p.value,SViSepL$p.value,ViVeSepL$p.value)
Sepal_width = c(SVeSepW$p.value,SViSepW$p.value,ViVeSepW$p.value)
Petal_Length = c(SVePetL$p.value,SViPetL$p.value,ViVePetL$p.value)
Petal_Width = c(SVePetW$p.value,SViPetW$p.value,ViVePetW$p.value)

TTestMatrix = cbind(Sepal_length,Sepal_width,Petal_Length,Petal_Width)
rownames(TTestMatrix) = c("Setosa Versicolor","Setosa Virginica","Virginica Versicolor")
TTestMatrix

hist(Setosa$Sepal.Length, xlim= c(0,10),xlab = "Sepal Length",col = "red")
hist(Virginica$Sepal.Length, add=T,xlim= c(0,10), col="blue")
hist(Versicolor$Sepal.Length, add=T, xlim= c(0,10), col=rgb(0, 1, 0, 0.5))

hist(Setosa$Sepal.Width, xlim= c(0,10),xlab = "Sepal Width",col = "red")
hist(Virginica$Sepal.Width, add=T,xlim= c(0,10), col="blue")
hist(Versicolor$Sepal.Width, add=T, xlim= c(0,10), col=rgb(0, 1, 0, 0.5))

hist(Setosa$Petal.Length, xlim= c(0,10),xlab = "Petal Length",col = "red")
hist(Virginica$Petal.Length, add=T,xlim= c(0,10), col="blue")
hist(Versicolor$Petal.Length, add=T, xlim= c(0,10), col=rgb(0, 1, 0, 0.5))

hist(Setosa$Petal.Width, xlim= c(0,10),xlab = "Petal Width",col = "red")
hist(Virginica$Petal.Width, add=T,xlim= c(0,10), col="blue")
hist(Versicolor$Petal.Length, add=T, xlim= c(0,10), col=rgb(0, 1, 0, 0.5))

mean(as.matrix(Setosa[,1:4]))