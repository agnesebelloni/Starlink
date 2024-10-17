#Homework Agnese Belloni 880126

library(tidyverse)
library(ggplot2)

#Analisi del dataset
dati<-read.csv("/Users/computer/Desktop/UNI/Terzo anno/Statitica Computazionale/homework/Belloni3.csv")

#elimino le colonne che non mi servono
starlink<-dati%>%select(Local.time,Download..Rx..bandwidth..bps.,Upload..Tx..bandwidth..bps.,RTT..ping...ms.)
#rinomino le colonne, per avere nomi più intuitivi
colnames(starlink)<-c("osservazioni","velocita_download","velocita_upload","RTT")



#creo una nuova colonna che indica se la velocità dell'unità statistica è pari o superiore alla soglia in upload, in download, in entrambi o in nessuna dei due

soglia_up<-20000000
soglia_down<-60000000


name<-c()

for (i in 1:nrow(starlink)) {
  if (starlink[i,]$velocita_download>=soglia_down & starlink[i,]$velocita_upload<soglia_up) {
    name[i]="down"
  }
  else if (starlink[i,]$velocita_download<soglia_down & starlink[i,]$velocita_upload>=soglia_up) {
    name[i]="up"
  }
  else if (starlink[i,]$velocita_download>=soglia_down & starlink[i,]$velocita_upload>=soglia_up) {
    name[i]="up and down"
  }
  else if (starlink[i,]$velocita_download<soglia_down & starlink[i,]$velocita_upload<soglia_up) {
    name[i]="none"
  }
  
} 

starlink<-starlink%>%mutate(velocita_superiori=name)


#creo una nuova colonna nel tibble che contiene i livelli dell'RTT (ottimo, buono, discreto, pessimo)

livelli<-c() 

for (i in 1:nrow(starlink)) {
  if (starlink[i,]$RTT<=60) {
    livelli[i]="ottimo"
  }
  else if (starlink[i,]$RTT<=80) {
    livelli[i]="buono"
  }
  else if (starlink[i,]$RTT<=100) {
    livelli[i]="discreto"
  }
  else if (starlink[i,]$RTT>=100) {
    livelli[i]="pessimo"
  }
  
} 

starlink<-starlink%>%mutate(livelli_RTT=livelli)



#rappresento graficamente le velocità in uplink e in downlink delle singole unità statistiche, 
#e indico le diverse unità statistiche per livello di RTT (ottimo,buono,discreto,pessimo)

ggplot(data=starlink, mapping = aes(x=velocita_upload,y=velocita_download,color=as.factor(livelli_RTT)))+geom_point(alpha=0.5)+
  geom_abline(intercept=soglia_down,slope=0,linetype = "dashed")+geom_abline(intercept=-(soglia_up)^2,slope=soglia_up,linetype = "dashed")+
  labs(x = "Velocità Upload", y = "Velocità Download", color = "Livello RTT") +
  theme_minimal()+ggtitle(label="Scatterplot Velocità upload e download in base ai livelli dell'RTT")

#per avere una rappresentzione più chiara faccio uno scatterplot 3d con la library plotly

library(plotly)
plot_ly(starlink, x = ~velocita_upload, y = ~velocita_download, z = ~RTT, color = ~livelli_RTT) %>%
  add_markers() %>%layout(scene = list(xaxis = list(title = 'Velocità upload'),
                                       yaxis = list(title = 'Velocità download'),
                                       zaxis = list(title = 'RTT')))


#creo un istogramma che confronta le 4 modalità di livelli_RTT e coloro le colonne in base ala variabile velocita_superiori
ggplot(data=starlink, mapping = aes(x=livelli_RTT,fill=as.factor(velocita_superiori)))+geom_bar()+
  labs(x = "Livelli RTT", y = "Conteggio", fill = "Velocità superiori") + theme_minimal() + 
  ggtitle(label="Distribuzione delle velocità che superano la soglia massima per Livello RTT")

#creo una nuova colonna, nei quali calcolo i jitter 

jit<-c()
r<-starlink$RTT 

for (i in 2:length(r)){
  jit[i-1]=r[i-1]-r[i]
} 

data<-tibble(num = (1:length(jit)),val = jit)
ggplot(data=data, mapping = aes(y=jit))+geom_boxplot(color="darkorchid3")+labs(y = "Jitter", title = "Boxplot dei Jitter")+theme_minimal()+theme(plot.title=element_text(hjust = 0.5))

#tutt gli outlier individuati sono potenzialmente intaccare alcune capacità di comunicazione dei satelliti
out<-boxplot.stats(data$val)$out 

outlier<-data%>%filter(val==out)


#essendo chiaro dall'istogramma che le classi sono sbilanciate, realizzo un undersalmpling
library(dplyr)
countRTT<-table(starlink$livelli_RTT)
classe.min<-min(countRTT)
set.seed(123)
under_star <- starlink %>%group_by(livelli_RTT) %>%sample_n(classe.min, replace = FALSE) %>%ungroup()

#verifico che le classi siano bilanciate
ggplot(data=under_star, mapping = aes(x=livelli_RTT,fill=as.factor(velocita_superiori)))+geom_bar()+
  labs(x = "Livelli RTT", y = "Conteggio", fill = "Velocità superiori") + theme_minimal() + 
  ggtitle(label="Distribuzione delle velocità che superano la soglia massima per Livello RTT")

#rappresento graficamente le velocità in uplink e in downlink delle singole unità statistiche, 
#e indico le diverse unità statistiche per livello di RTT (ottimo,buono,discreto,pessimo)

ggplot(data=under_star, mapping = aes(x=velocita_upload,y=velocita_download,color=as.factor(livelli_RTT)))+geom_point(alpha=0.5)+
  geom_abline(intercept=soglia_down,slope=0,linetype = "dashed")+geom_abline(intercept=-(soglia_up)^2,slope=soglia_up,linetype = "dashed")+
  labs(x = "Velocità Upload", y = "Velocità Download", color = "Livello RTT") +
  theme_minimal()+ggtitle(label="Scatterplot Velocità upload e download in base ai livelli dell'RTT")

#faccio dei grafici che esprimano la correlazione
star_data<-under_star%>%select(velocita_download,velocita_upload,RTT)
correlazioni <- round(cor(star_data),3)
library(ggcorrplot)
ggcorrplot(correlazioni,lab=T,color=c("darkorchid3","steelblue2","lightgreen"))
#l'RTT e la velocità download sono correlati negativamente, mentre l'RTT e la velocità upload e 
#la velocutà upload e la velocità download sono più o meno incorrelati


star_data_label<-under_star%>%select(velocita_download,velocita_upload,RTT,livelli_RTT) 
library(GGally)
ggpairs(under_star[,c(2:4)],aes(color=as.factor(under_star$livelli_RTT)))

#model-based clustering fitting and plotting

library(mclust)
star.Mclust<-Mclust(star_data)
summary(star.Mclust)
#Mclust VVI (diagonal, varying volume and shape) model with 7 components


star.mclustICL<-mclustICL(star_data)

par(mfrow=c(1,2))
plot(star.Mclust,what="BIC")
legendArgs=list("bottom", text='My values')
plot (star.mclustICL)

star.mclustICL

mclustICL(star_data,G=4)

mod <- Mclust(star_data,G=4, modelNames = 'VVV')

plot(mod , what = "classification")

#calcoliamo il CER
star.type.cluster<-mod$classification
star.type.true <- star_data_label$livelli_RTT
classError(star.type.cluster, class=star.type.true) 

# Plot uncertainty
par(mfrow=c(1,1))
coordProj (data=star_data , dimens=c(1,2), what="uncertainty",
           parameters=mod$parameters , z=mod$z)

#Classification with EDDA models

library(Rmixmod)
star.class <- as.factor(unlist(under_star[,6]))


res<-mixmodLearn(star_data, star.class, 
                 models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                 criterion=c('CV','BIC'))

res@results[[1]]@model   #"Gaussian_pk_Lk_CK"
res@results[[2]]@model   #"Gaussian_pk_L_Ck"

res@results[[1]]@criterionValue [1]   #0.05487805 #questo è il CV più basso 
res@results[[2]]@criterionValue [1]   #0.05640244
res@results[[1]]@criterionValue [2]  #51919.37 #questo BIC è più basso
res@results[[2]]@criterionValue [2]  #52075.96


BIC = CV = modello = rep(NA ,length(res@models@listModels) )
for (i in 1: length(res@models@listModels)){
  ind = which(res@results [[i]] @model == res@models@listModels)
  CV[ind] = res@results [[i]] @criterionValue [1]
  BIC[ind] = res@results [[i]] @criterionValue [2]
  modello[ind] = res@results[[i]]@model
}

#metto a confronto due grafici rappresentanti i valori del BIC e del CV per ogni modello, 
#e traccio una linea verticare che indica il valore minore per entrambi e il corrispondente modello
library(gridExtra)

dataframe<-tibble(index=1:length(BIC),model=modello ,BIC=BIC)
dataframe<-arrange(dataframe,desc(BIC))
g1<-ggplot(data=dataframe,mapping=aes(x=model,y=BIC,group=1))+geom_point(size=3) + 
  geom_line()+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_vline(aes(xintercept=modello[which.min(BIC)]),col="red",linetype="dashed")
g1

dataframe2<-tibble(index=1:length(CV),model=modello ,CV=CV)
dataframe2<-arrange(dataframe2,desc(CV))
g2<-ggplot(data=dataframe2,mapping=aes(x=model,y=CV,group=1))+geom_point(size=3) + 
  geom_line()+theme_minimal()+theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_vline(aes(xintercept=modello[which.min(CV)]),col="red",linetype="dashed")
g2
grid.arrange(g1,g2,ncol=1)  

#sia in base al BIC che all'ICL scegliamo il modello "Gaussian_pk_Lk_CK"

#ora applichiamo la regola di classificazione sviluppata utilizzando il training set,
#assegno le rimanenti osservazioni (test set) ai gruppi e infine calcolo l'accuratezza ell'allocazione nei gruppi
set.seed(211)
n<-nrow(star_data) 

test.set.labels<-sample(1:n,131) #estraggo il 20% delle osservazioni totali, che costituiranno il test set


CLASSIF<-mixmodLearn(star_data[-test.set.labels,], star.class[-test.set.labels], 
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE)) #sviluppo il modello in base al training set

CLASSIF@bestResult   #4 cluster, il modello è Gaussian_pk_Lk_Ck 

PREDICTION<- mixmodPredict(data=star_data[test.set.labels,], classificationRule=CLASSIF["bestResult"])
PREDICTION

mean(as.integer(star.class[test.set.labels]) == PREDICTION["partition"]) #0.9236641

#mixture of experts model
library(flexmix)

set.seed(312)  
fit <- flexmix(RTT~velocita_upload+velocita_download ,data=under_star,k=4,
               concomitant = FLXPmultinom(~velocita_upload+velocita_download ))
#Cluster sizes:
#1   2   3   4 
#172  71 360  53 
fit #l'algoritmo E-M non giunge a convergenza

#analizziamo la parte del modello Explore the Expert
parameters(fit)   
ICL(fit)          #6368.56
KLdiv(fit)        
#[,1]      [,2]      [,3]      [,4]
#[1,]    0.000 21599.524 3557.5875 1207.0177
#[2,] 7857.829     0.000  805.1551 1264.2659
#[3,] 5009.217  2849.073    0.0000  250.3284
#[4,] 3648.905  9161.770  516.2470    0.0000

#Inferenza sui parametri GLM
asymp.inf<-refit(fit)
summary(asymp.inf)
plot(asymp.inf)

# Explore the gating network part of the model
summary(fit)  
#        prior size post>0  ratio
#Comp.1 0.2917  172    535 0.3215
#Comp.2 0.0989   71    304 0.2336
#Comp.3 0.4873  360    597 0.6030
#Comp.4 0.1221   53    635 0.0835
posterior(fit)

apply(posterior(fit),2,sum)/131   #0.1980123 0.3314725 0.4705152

str(fit)
labs<-fit@cluster

parameters(fit ,which="concomitant") 

plot(fit)

p1 <- ggplot(data=star_data, mapping = aes(x=velocita_upload, y=RTT,color=factor(labs)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)
p2 <- ggplot(data=under_star, mapping = aes(x=velocita_upload, y=RTT,color=factor(livelli_RTT)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)

grid.arrange(p1, p2)

p3 <- ggplot(data=star_data, mapping = aes(x=velocita_download, y=RTT,color=factor(labs)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)
p4 <- ggplot(data=starlink, mapping = aes(x=velocita_download, y=RTT,color=factor(livelli_RTT)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)

grid.arrange(p3, p4)


final<-stepFlexmix(RTT~velocita_upload+velocita_download,data=under_star, k=1:5, concomitant = FLXPmultinom(~velocita_upload+velocita_download),
                   nrep = 5,verbose = TRUE, drop = F, unique = FALSE)
final
#iter converged k k0    logLik      AIC      BIC      ICL
#1    2      TRUE 1  1 -3113.580 6235.159 6253.104 6253.104
#2   67      TRUE 2  2 -2999.268 6020.536 6069.884 6095.863
#3  199      TRUE 3  3 -2963.312 5962.625 6043.375 6473.022
#4  114      TRUE 4  4 -2934.686 5919.372 6031.526 6339.966
#5  107      TRUE 5  5 -2925.824 5915.649 6059.206 6615.115
> 
str(final)

labs <- final@models$`4`@cluster

classError(labs, under_star$livelli_RTT)
   
ICL(final)   #6253.104 6095.863 6473.022 6339.966 6615.115       
plot(final)

p1 <- ggplot(data=star_data, mapping = aes(x=velocita_upload, y=RTT,color=factor(labs)))+
  geom_point()+geom_smooth(method="lm", se=F, size=1)+labs(x = "Velocità Upload", y = "RTT", color = "Gruppi") +
  theme_minimal()
p2 <- ggplot(data=under_star, mapping = aes(x=velocita_upload, y=RTT,color=factor(livelli_RTT)))+
  geom_point()+geom_smooth(method="lm", se=F, size=1)+labs(x = "Velocità Upload", y = "RTT", color = "Livelli RTT") +
  theme_minimal()

grid.arrange(p1, p2)

p3 <- ggplot(data=star_data, mapping = aes(x=velocita_download, y=RTT,color=factor(labs)))+
  geom_point()+geom_smooth(method="lm", se=F, size=1)+labs(x = "Velocità Download", y = "RTT", color = "Gruppi") +
  theme_minimal()
p4 <- ggplot(data=under_star, mapping = aes(x=velocita_download, y=RTT,color=factor(livelli_RTT)))+
  geom_point()+geom_smooth(method="lm", se=F, size=1)+labs(x = "Velocità Download", y = "RTT", color = "Livelli RTT") +
  theme_minimal()

grid.arrange(p3, p4)

par(mfrow=c(1,1))
plot(BIC(final),type='b',ylab='BIC')
points(x = which.min(BIC(final)),min(BIC(final)),col='red',pch=20)

plot(ICL(final),type='b',ylab='ICL')
points(x = which.min(ICL(final)),min(ICL(final)),col='red',pch=20)

############################################################################
#esseno i risultati non soddisfacenti, ripeto l'analisi con tutti i dati (senza undersamplin)

star_data2<-starlink%>%select(velocita_download,velocita_upload,RTT)
star_data_label2<-starlink%>%select(velocita_download,velocita_upload,RTT,livelli_RTT) 

library(mclust)
star.Mclust2<-Mclust(star_data2)
summary(star.Mclust2)
#Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 9
?Mclust
star.mclustICL2<-mclustICL(star_data2)

par(mfrow=c(1,2))
plot(star.Mclust2,what="BIC")
legendArgs=list("bottom", text='My values')
plot (star.mclustICL2)

star.mclustICL2$modelNames

mclustICL(star_data2,G=4)

mod2 <- Mclust(star_data2,G=4, modelNames = 'EVE')


#calcoliamo il CER
star.type.cluster2<-mod2$classification
star.type.true2 <- star_data_label2$livelli_RTT
classError(star.type.cluster2, class=star.type.true2) #0.2654662


library(Rmixmod)
star.class2 <- as.factor(unlist(starlink[,6]))


res2<-mixmodLearn(star_data2, star.class2, 
                 models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                 criterion=c('CV','BIC'))

res2@results[[1]]@model   #"Gaussian_pk_Lk_CK"
res2@results[[2]]@model   #"Gaussian_pk_Lk_Bk"

res2@results[[1]]@criterionValue [1]   #0.02529384 #questo è il CV più basso 
res2@results[[2]]@criterionValue [1]   #0.03369203
res2@results[[1]]@criterionValue [2]  #3958189 #questo BIC è più basso
res2@results[[2]]@criterionValue [2]  #3967799

set.seed(211)
n<-nrow(star_data2) 

test.set.labels2<-sample(1:n,10073) #estraggo il 20% delle osservazioni totali, che costituiranno il test set

CLASSIF2<-mixmodLearn(star_data2[-test.set.labels2,], star.class2[-test.set.labels2], 
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE)) #sviluppo il modello in base al training set

CLASSIF2@bestResult   #4 cluster, il modello è Gaussian_pk_Lk_Ck 

PREDICTION2<- mixmodPredict(data=star_data2[test.set.labels2,], classificationRule=CLASSIF2["bestResult"])
PREDICTION2

mean(as.integer(star.class2[test.set.labels2]) == PREDICTION2["partition"]) # 0.9768689


