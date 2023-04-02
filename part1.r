#Import the dataset
library(readxl)
whitewine_data<-read_excel("E:/ML & DL/ML with R/Cw old/Old Coursework/Machine-Learning-and-datamining-cw-2022/Whitewine_v2.xlsx")

#Number of data check
dim(whitewine_data)

#Check for there are any na values
sum(is.na(whitewine_data))

#Using boxplot method check outliers
boxplot(whitewine_data[,-12])

"hist(whitewine_data$`fixed acidity`,
     breaks = sqrt(nrow(whitewine_data))
)
hist(whitewine_data$`volatile acidity`,
     breaks = sqrt(nrow(whitewine_data))
)"


#First column outliers removal
IQR_fa<-IQR(whitewine_data$`fixed acidity`)
lower_fa<-quantile(whitewine_data$`fixed acidity`,.25)-1.5*IQR_fa
upper_fa<-quantile(whitewine_data$`fixed acidity`,.75)+1.5*IQR_fa

#second column outliers removal
IQR_va<-IQR(whitewine_data$`volatile acidity`)
lower_va<-quantile(whitewine_data$`volatile acidity`,.25)-1.5*IQR_va
upper_va<-quantile(whitewine_data$`volatile acidity`,.75)+1.5*IQR_va

# 3 column outliers removal
IQR_ca <- IQR(whitewine_data$`citric acid`)
lower_ca = quantile(whitewine_data$`citric acid`, .25) - 1.5*IQR_ca
upper_ca = quantile(whitewine_data$`citric acid`, .75) + 1.5*IQR_ca

# 4 column outliers removal
IQR_rsu <- IQR(whitewine_data$`residual sugar`)
lower_rsu = quantile(whitewine_data$`residual sugar`, .25) - 1.5*IQR_rsu
upper_rsu = quantile(whitewine_data$`residual sugar`, .75) + 1.5*IQR_rsu

# 5 column outliers removal
IQR_cl <- IQR(whitewine_data$chlorides)
lower_cl = quantile(whitewine_data$chlorides, .25) - 1.5*IQR_cl
upper_cl = quantile(whitewine_data$chlorides, .75) + 1.5*IQR_cl

# 6 column outliers removal
IQR_fsd <- IQR(whitewine_data$`free sulfur dioxide`)
lower_fsd = quantile(whitewine_data$`free sulfur dioxide`, .25) - 1.5*IQR_fsd
upper_fsd = quantile(whitewine_data$`free sulfur dioxide`, .75) + 1.5*IQR_fsd

# 7 column outliers removal
IQR_tsd <- IQR(whitewine_data$`total sulfur dioxide`)
lower_tsd = quantile(whitewine_data$`total sulfur dioxide`, .25) - 1.5*IQR_tsd
upper_tsd = quantile(whitewine_data$`total sulfur dioxide`, .75) + 1.5*IQR_tsd

# 8 column outliers removal
IQR_de <- IQR(whitewine_data$density)
lower_de = quantile(whitewine_data$density, .25) - 1.5*IQR_de
upper_de = quantile(whitewine_data$density, .75) + 1.5*IQR_de

# 9 column outliers removal
IQR_ph <- IQR(whitewine_data$pH)
lower_ph = quantile(whitewine_data$pH, .25) - 1.5*IQR_ph
upper_ph = quantile(whitewine_data$pH, .75) + 1.5*IQR_ph

# 10 column outliers removal
IQR_sul <- IQR(whitewine_data$sulphates)
lower_sul = quantile(whitewine_data$sulphates, .25) - 1.5*IQR_sul
upper_sul = quantile(whitewine_data$sulphates, .75) + 1.5*IQR_sul

# 11 column outliers removal
IQR_al <- IQR(whitewine_data$alcohol)
lower_al = quantile(whitewine_data$alcohol, .25) - 1.5*IQR_al
upper_al = quantile(whitewine_data$alcohol, .75) + 1.5*IQR_al

update_whitewine<-subset(whitewine_data,whitewine_data$`fixed acidity`>=lower_fa & whitewine_data$`fixed acidity`<=upper_fa
                         & whitewine_data$`volatile acidity`>=lower_va & whitewine_data$`volatile acidity`<=upper_va
                         & whitewine_data$`residual sugar` >= lower_rsu & whitewine_data$`residual sugar` <= upper_rsu
                         & whitewine_data$chlorides >= lower_cl & whitewine_data$chlorides <= upper_cl
                         & whitewine_data$`free sulfur dioxide` >= lower_fsd & whitewine_data$`free sulfur dioxide` <= upper_fsd
                         & whitewine_data$`total sulfur dioxide` >= lower_tsd & whitewine_data$`total sulfur dioxide` <= upper_tsd
                         & whitewine_data$density >= lower_de & whitewine_data$density <= upper_de
                         & whitewine_data$pH >= lower_ph & whitewine_data$pH <= upper_ph
                         & whitewine_data$sulphates >= lower_sul & whitewine_data$sulphates <= upper_sul
                         & whitewine_data$alcohol >= lower_al & whitewine_data$alcohol <= upper_al
                         )
#Boxplot
boxplot(update_whitewine)

# Plotting the box plot for before and after removing outliers
boxplot(whitewine_data[,1])
boxplot(updated_whitewine[,1])

boxplot(whitewine_data[,2])
boxplot(updated_whitewine[,2])

boxplot(whitewine_data[,3])
boxplot(updated_whitewine[,3])

boxplot(whitewine_data[,4])
boxplot(updated_whitewine[,4])

boxplot(whitewine_data[,5])
boxplot(updated_whitewine[,5])

boxplot(whitewine_data[,6])
boxplot(updated_whitewine[,6])

boxplot(whitewine_data[,7])
boxplot(updated_whitewine[,7])

boxplot(whitewine_data[,8])
boxplot(updated_whitewine[,8])

boxplot(whitewine_data[,9])
boxplot(updated_whitewine[,9])

boxplot(whitewine_data[,10])
boxplot(updated_whitewine[,10])

boxplot(whitewine_data[,11])
boxplot(updated_whitewine[,11])

#Normalizing the data
#Creating a new funcion called normalize
normalize<-function(x){
  return ((x-min(x))/max(x)-min(x))
}

whitewineNormalize<-as.data.frame(lapply(update_whitewine,normalize))
whiteNormUpadated <- whitewineNormalize[,-12]
boxplot(whiteNormUpadated)

#Elbow methd to find the number of clusters
library(factoextra)
#install.packages("ggsignif")
#install.packages("rstatix")
#install.packages("backports")

whiteNormUpadated %>%
  fviz_nbclust(          # From `factoextra`
    FUN = kmeans,        # Use k-means
    method = "wss"       # "within cluster sums of squares"
  ) +
  geom_vline(            # Reference line
    xintercept = 3, 
    color = "red", 
    linetype = "dotted"
  )                      # Look for "bend" in curve

# Silhouette method 
whiteNormUpadated %>%
  fviz_nbclust(          # From `factoextra`
    FUN = kmeans,        # Use k-means
    method = "silhouette"
  )

#Manual Method
#Create an empty method
kval<-list()
for(i in 1:10){
  kval[[i]]<-kmeans(whiteNormUpadated,i)
}
kval

#bss/tss Calcualtion
#List to store the val
betweenSS_totss<-list()
for(i in 1:10){
  betweenSS_totss[[i]]<-kval[[i]]$betweenss/kval[[i]]$totss
}

#Plotting the sum of suares ratio vs number of clusters
plot(1:10,betweenSS_totss,type = "b",
     ylab="Between SS/Total SS",xlab = "Clusters(k)")
