Runs = 1:1000

SetosaDistVect = vector()
VersicDistVect = vector()
VirginiDistVect = vector()
CentPetLength = vector()
Iterations = vector()

for (i in Runs){

iris
newiris <- iris
newiris$Species <- NULL
newiris
kc <- kmeans(newiris, 3)
kc
output = table(iris$Species, kc$cluster)

set = output['setosa',]
setsel = set != 0
setosa = set[setsel]
SetosaDistVect = c(SetosaDistVect,setosa)

vers = output['versicolor',]
verssel = vers !=0
versicolor = vers[verssel]
versicolor
VersicDistVect = c(VersicDistVect, versicolor)

virg = output['virginica',]
virgsel = virg !=0
virginica = virg[virgsel]
virginica
VirginiDistVect = c(VirginiDistVect, virginica)

Cents = kc$centers
CPL = Cents[,'Petal.Length']
CentPetLength = c(CentPetLength,CPL)

Iterations = c(Iterations, kc$iter)

}

plot(table(SetosaDistVect),xlab = "Setosa Cluster Size",ylab = "Frequency")
plot(table(VersicDistVect),xlab = "Versicolor Cluster Size",ylab = "Frequency")
plot(table(VirginiDistVect),xlab = "Virginica Cluster Size",ylab = "Frequency")
CPL = format(round(CentPetLength,2))
plot(table(CPL),xlab = "Petal Length Centroid Value",ylab = "Frequency")
plot(table(Iterations))