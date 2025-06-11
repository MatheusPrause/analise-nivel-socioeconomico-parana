library(readxl)
dados <- read_excel("C:/Users/Perfil/Downloads/INSE_2021_municipios.xlsx")
summary(dados)
total<- subset(dados,TP_LOCALIZACAO==0)
total$SG_UF <- as.factor(total$SG_UF) 
summary(total$SG_UF)
total[total$SG_UF=="DF",]
require(dae)
total$rede <- fac.recode(as.factor(total$TP_TIPO_REDE),
                         c(1:5),
                         labels=c("Federal","Estadual",
                                  "Municipal","Est_Municipio",
                                  "Todos")
)

#### Comparando munípios no total das escolas
geral<- droplevels(subset(total,rede=="Todos"))
summary(geral)
table(geral$NO_UF)
table(table(geral$NO_UF))
for(i in c(10:19)){
  print(names(geral)[i])
  print("Média")
  print(round(tapply(geral[[i]],geral$SG_UF,mean,default = 0),1))
  print("Desvio padrão")
  print(round(tapply(geral[[i]],geral$SG_UF,sd,default = 0),1))
  
  }

boxplot(geral[,12:19],ylab="Percentaul de alunos nas escolas de acordo nível socioeconômico",
        xlab="Níveis",names = c(1:8))

# Padronizar os dados
geral_scaled <- scale(geral[,c(10,12:19)])
head(geral_scaled)

# PCA
names(geral)
resultado_pca <- prcomp(geral[,c(10,12:19)], scale = TRUE)  # Aplicar PCA
summary(resultado_pca)  # Verificar a import??ncia dos componentes

# Visualizar os componentes principais - Clusters do K-means
library(factoextra)
fviz_pca_ind(resultado_pca, label = "none", 
             habillage = geral$SG_UF,
             title = " ")

### Apenas o Parana

parana <- droplevels(subset(geral,SG_UF=="PR"))
# Padronizar os dados
parana_scaled <- scale(parana[,c(10,12:19)])
row.names(parana_scaled)<- parana$NO_MUNICIPIO

dist_matrix <- dist(parana_scaled, 
                    method = "euclidian") # Calcula a matriz de distancias
hc <- hclust(dist_matrix, method = "ward.D2") # contrucao do modelo hierarquico
par(mar=c(1,2,1,1))
plot(hc, main = "Dendrograma - Euclidiana", 
     xlab = "", sub = "", cex = 0.4,lwd=1) # dendograma
x<- rect.hclust(hc, k = 5, border = 2:4) # Adiciona retangulos para destacar os clusters
x
