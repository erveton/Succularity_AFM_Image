#Código para o cálculo da sucolaridade fractal de imagens topográficas de AFM criado pelo Prof. Me. Erveton Pinheiro Pinto, Universidade Federal do Amapá, Departamento de Física, Amapá, Brazil.

#Passo 1: criar uma variável que contenha a matriz de alturas a partir do diretório específico.

image1<-read.table (file = "sample.txt")


RMS <- apply (X = image1[3], MARGIN = 2, FUN = sd) # obter RMS (método usual)
RMS

#Passo 2: limiar.

limiar<-Threshold

#Transformar os valores de alturas em dados binários, com valores acima ou iguais a média chamados de 1 (inacessíveis pela água) e abaixo da média chamados de 0 (acessíveis pela água).

z<-ifelse (image1[3] >= limiar, "1","0")

#Criar uma matriz binária com a variável z.

mat<-matrix (z, nrow = 256, ncol = 256, byrow = TRUE)

#Criar os valores correspondentes ao centroinde de pressão para cada pixel.

n<-c (1:256)
pr<- 0.5+(n-1)

#Contar o número de pontos de acesso para água em cada linha da matriz binária.

x<-c(0)
for(i in 1:nrow (mat)){
  v = mat[i,]
  x[i] = sum (v==0) }

#Criar o numerador da equação da sucolaridade fractal.

numerador<-sum (pr*x)

#Criar o denominador da equação da sucolaridade fractal.

denominador<-256 * sum (pr)

#Cálcular a sucolaridade fractal.

Sucolaridade<-numerador/denominador
print(Sucolaridade)



