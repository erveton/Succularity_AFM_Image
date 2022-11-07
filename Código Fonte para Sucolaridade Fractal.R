#Code for calculating fractal succularity of AFM topographic images created by Prof. Dr. Erveton Pinheiro Pinto, Federal University of Amapá, Department of Physics, Amapá, Brazil.

#Step 1: create a variable that contains the array of heights from the specific directory.

image1<-read.table (file = "sample.txt")


RMS <- apply (X = image1[3], MARGIN = 2, FUN = sd) # get RMS (usual method)
RMS

#Step 2: threshold.

limiar<-Threshold

#Transform height values into binary data, with values above or equal to the mean called 1 (inaccessible by water) and below the mean called 0 (accessible by water).

z<-ifelse (image1[3] >= limiar, "1","0")

#Create a binary matrix with the variable z.

mat<-matrix (z, nrow = 256, ncol = 256, byrow = TRUE)

#Create the values corresponding to the pressure centroid for each pixel.

n<-c (1:256)
pr<- 0.5+(n-1)

#Count the number of water access points in each row of the binary matrix.

x<-c(0)
for(i in 1:nrow (mat)){
  v = mat[i,]
  x[i] = sum (v==0) }

#Create the numerator of the fractal sucularity equation.

numerador<-sum (pr*x)

#Create the denominator of the fractal succularity equation.

denominador<-256 * sum (pr)

#Calculate fractal succularity.

Sucolaridade<-numerador/denominador
print(Sucolaridade)



