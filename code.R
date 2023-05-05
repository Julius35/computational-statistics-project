#Caricamento librerie
#-------------------------------------------------
library(readxl)
library(tidyverse)
library(mclust)
library(GGally)
library(ggcorrplot)
library(gridExtra)
library(grid)
library(caret)
library(factoextra)
library(Rmixmod)

rm(list=ls())
# Caricamento dati
#-----------------------------------
basket <- read_excel("TIDY.xlsx")
View(basket)


# Rendiamo alcune variabili fattori e le ricodifichiamo
#------------------------------
basket$Pos <- as.factor(basket$Pos)
basket$YEAR <- as.factor(basket$YEAR)
levels(basket$YEAR) <- c(as.factor(2001:2023),as.factor(1997:2000)) #cambio modalità
basket$YEAR <- factor(basket$YEAR,levels = as.factor(1997:2023)) #cambio ordine modalità


# Filtriamo giocatori con più di 1000 minuti giocati
#-------------------------------------------------------
basket <- basket%>%
  filter(MP>=1000)

# Aggiungiamo due variabili per ricodificare in maniera più classica i ruoli
#--------------------------------------------------------------------------
basket <- basket%>%
  mutate(POS2=Pos)
levels(basket$POS2) <- c('C','C','F','C','F','G','G','G','F','F','F','F','G','F','G','G','G')
rbind(levels(basket$Pos),c('C','C','F','C','F','G','G','G','F','F','F','F','G','F','G','G','G'),as.numeric(table(basket$Pos)))
table(basket$POS2)



# Verifichiamo presenza NA
round(colSums(is.na(basket))/dim(basket)[1]*100,2)
# eliminiamo la variabile di 3P assisted, troppi NA e poco significativa per analisi
basket <- basket[,-10]




# Verifichiamo come varia altra variabile con NA
basket%>% #ali, come ci si apetterebbe, tirano più dall'angolo
  group_by(POS2)%>%
  summarize(mediana_corner3=median(`%3PA_CORNER`,na.rm = T))

basket%>% #incremento tiri dall'angolo post 2000
  group_by(YEAR)%>%
  summarize(mediana_corner3=median(`%3PA_CORNER`,na.rm = T))

# Preparazione dataset che verrà utilizzato per dati da imputare
dati_per_imputazione <- basket%>%
  group_by(POS2,YEAR)%>%
  summarize(media_corner3=median(`%3PA_CORNER`,na.rm = T))

# Elimino alcune variabili non utili
basket1 <- basket[,-c(1,2,3)]



# Imputazione mediana condizionata a posizione e anno
for (i in 1:dim(basket1)[1]){
  if (is.na(basket1$`%3PA_CORNER`[i])==T){
    pos_temp <- basket1$POS2[i]
    anno_temp <- basket1$YEAR[i]
    imp_temp <- dati_per_imputazione[(dati_per_imputazione$POS2==pos_temp & dati_per_imputazione$YEAR==anno_temp),3]
    basket1[i,8] <- imp_temp
  }
}

# Verifico non vi siano più NA
round(colSums(is.na(basket1))/dim(basket1)[1]*100,2)





# Analisi delle correlazioni
#--------------------------------------------------------------
cor_basket1 <- cor(basket1[-c(9,10)])
ggcorrplot(cor_basket1, hc.order = TRUE, type = "lower",
           colors = c("#6D9EC1", "white", "#E46726"), 
           lab = TRUE, lab_size = 3, ggtheme = ggplot2::theme_minimal())
# Vi è una forte correlazione positiva tra schiacciate e tiri ravvicinati, come ci si potrebbe aspettare.



ggplot(basket1)+
  geom_point(aes(x=`0\\3`,y=DUNKS,col=POS2),alpha=0.6)+
  theme_classic()
# Tuttavia decidiamo di conservare entrambe le variabili siccome hanno significati diversi.
# Anche tra tiri da 3/10 piedi e 0/3, quindi in generale ravvicinati, e tiri da 3 vi è correlazione negativa.
# Anche questo è logico.





# Analisi distribuzione covariate
#-------------------------------------------------------------
ggpairs(basket1[-c(9,10)], aes(col = basket1$POS2))

p1 <- ggplot(basket1)+
  geom_density(aes(x=`0\\3`,fill=POS2),alpha=0.4)+
  theme_classic()

p2 <- ggplot(basket1)+
  geom_density(aes(x=`3\\10`,fill=POS2),alpha=0.4)+
  theme_classic()

p3 <- ggplot(basket1)+
  geom_density(aes(x=`10\\16`,fill=POS2),alpha=0.4)+
  theme_classic()

p4 <- ggplot(basket1)+
  geom_density(aes(x=`16\\3P`,fill=POS2),alpha=0.4)+
  theme_classic()

p5 <- ggplot(basket1)+
  geom_density(aes(x=`3P`,fill=POS2),alpha=0.4)+
  ylim(0,10)+
  theme_classic()

p6 <- ggplot(basket1)+
  geom_density(aes(x=`%_ASTD_2P`,fill=POS2),alpha=0.4)+
  theme_classic()

p7 <- ggplot(basket1)+
  geom_density(aes(x=`DUNKS`,fill=POS2),alpha=0.4)+
  ylim(0,30)+
  theme_classic()

p8 <- ggplot(basket1)+
  geom_density(aes(x=`%3PA_CORNER`,fill=POS2),alpha=0.4)+
  theme_classic()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
# Si può notare dai primi 4 grafici come la dfistribuzione di densità delle variabili 
# condizionatamente al ruolo mostrino effettivamente alcune differenze nelle distribuzioni:
# le guardie tirano tendenzialmente da più lontano
# i centri tirano spesso da distanze ravvicinate
# le ali hanno sempre una posizione intermedia
# Inoltre nel quinto grafico si nota un picco vicino allo zero nella distribuzione per i centri
# per il tiro da 3, che è invece più utilizzato da guardie e ali
# Il sesto grafico mostra come la percentuale di tiri assistiti tra quelli da 2 sia tendenzialmente
# più alta per centri e ali, che usufruiscono delle trame di gioco create dalle guardie
# Il grafico 7, come ci si aspetta, mostra che sono i centri e solo marginalmente le ali
# a schiacciare di più, siccome giocano più vicino a canestro e sono tendenzialmente più alti.
# Infine la distribuzione dei tiri dall'angolo sui tiri da 3 totali sembra mostrare una preferenza
# delle ali che giocano infatti spesso in quelle zone del campo.






# Analisi dei boxplot delle covariate
#---------------------------------------------------------------------
q1 <- ggplot(basket1)+
  geom_boxplot(aes(y=`0\\3`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q2 <- ggplot(basket1)+
  geom_boxplot(aes(y=`3\\10`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q3 <- ggplot(basket1)+
  geom_boxplot(aes(y=`10\\16`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q4 <- ggplot(basket1)+
  geom_boxplot(aes(y=`16\\3P`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q5 <- ggplot(basket1)+
  geom_boxplot(aes(y=`3P`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q6 <- ggplot(basket1)+
  geom_boxplot(aes(y=`%_ASTD_2P`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q7 <- ggplot(basket1)+
  geom_boxplot(aes(y=`DUNKS`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

q8 <- ggplot(basket1)+
  geom_boxplot(aes(y=`%3PA_CORNER`,x=POS2,fill=POS2),show.legend = F)+
  theme_classic()+
  labs(x=NULL)

grid.arrange(q1,q2,q3,q4,q5,q6,q7,q8,ncol=2)
# Le considerazioni già fatte sono confermate

qq1 <- grid.arrange(q1,q5,ncol=2)
qq2 <- grid.arrange(q6,q7,q8,ncol=3)
grid.arrange(qq1,qq2,ncol=1)


#Mediana 3P nel tempo
#---------------------------------------------------------------------------
ts_3P <- basket%>%
  group_by(YEAR)%>%
  summarize(mediana_3P=median(`3P`,na.rm = T))
ts_3P$YEAR <- as.numeric(levels(ts_3P$YEAR))

ggplot(data = ts_3P, aes(x = YEAR, y = mediana_3P)) + 
  geom_line(col='#6D9EC1',linewidth=1.4) +
  geom_smooth(method = 'lm',se = F,col='#E46726',linetype=2)+
  theme_classic() +
  labs(title = "Mediana 3P nel tempo",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(1997,2023,by=2),)+
  ylim(0,0.5)+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))
# si nota che la percentuale dei tiri tentati da 3 punti sui tiri totali è in costante crescita,
# dal 2013 in poi, in particolare la crescita è più ripida e veloce



#Mediana 3P nel tempo per ruolo
#------------------------------------------------------------------------------
ts_3p_pos <- basket%>%
  group_by(YEAR,POS2)%>%
  summarize(mediana_3P=median(`3P`,na.rm = T))
ts_3p_pos$YEAR <- as.numeric(ts_3p_pos$YEAR)+1996

ggplot(data = ts_3p_pos, aes(x = YEAR, y = mediana_3P,col=POS2)) + 
  geom_line(linewidth=1.4) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1997,2023,by=2),)+
  labs(title = "Mediana 3P nel tempo per ruolo",
       x = NULL,
       y = NULL,col=NULL) +
  ylim(0,0.5)+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))
# La crescita dell'incidenza del tiro da 3 punti è eterogenea per ruoli:
# i centri inseriscono il tiro da 3 tra le conclusioni a partire dal 2017
# le ali fino al 2013 tirano meno da 3 rispetto alle guardie, dal 2013 in poi però,
# la differenza tra i due ruoli si affievolisce sempre più. La tendenza per il tiro
# da 3 è però in forte crescita per entrambi gli ruoli.

# Analisi variabile di classe
table(basket1$POS2)/sum(table(basket1$POS2))
# le tre classi sono bilanciate, sebbene i centri siano meno



#----------------------------------------------------------
# Analisi esplorativa: clustering
#----------------------------------------------------------



# K-means algorithm
#--------------------------------------------------------------------
set.seed(323)
k_medie <- kmeans(x = basket1[-c(9,10)],centers = 3,nstart = 50)
k_medie$centers
# 1=C, 2=F, 3=G
class_km <- as.factor(k_medie$cluster)
levels(class_km) <- c('C','F','G')
confusionMatrix(class_km,reference = basket1$POS2)
fviz_cluster(k_medie, data = basket1[,-c(9,10)],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()) #le due componenti principali non sono sufficienti a spiegare variabilità
adjustedRandIndex(class_km,basket1$POS2)
# sembrano effettivamente esserci 3 cluster




# Hierarchical clustering
#--------------------------------------------------------------------
dist_M <- dist(basket1[,-c(9,10)],method = 'euclidean')
hcl1 <- hclust(dist_M,method = 'ward.D')
cl1 <- cutree(hcl1,k = 3)
table(cl1)
class_hie <- as.factor(cl1)
table(basket1$POS2,class_hie)
levels(class_hie) <- c('F','C','G')
confusionMatrix(data = class_hie,reference = basket1$POS2)
adjustedRandIndex(class_hie,basket1$POS2)
# anche se il clustering gerarchico performa peggio sembrano poterci essere 3 gruppi





# Model-based Clustering
#---------------------------------------------------------------------------
mclustICL(basket1[,-c(9,10)],G = 3)
# modello VVV è il più adatto secondo il criterio ICL

mb_clust <- Mclust(basket1[,-c(9,10)],G = 3,modelNames = 'VVV')
summary(mb_clust,parameters = T)
# dalla interpretazione dei parametri sulla base delle osservazioni già fatte possiamo dire che:
# 1=F, 2=C, 3=G
mb_class <- mb_clust$classification
mb_class <- as.factor(mb_class)
levels(mb_class) <- c('F','C','G')
confusionMatrix(data = mb_class,reference = basket1$POS2)
# 66% di accuracy, ci sono alcuni problemi per la classificazione delle ali, ha senso poichè classe intermedia
adjustedRandIndex(mb_class,basket1$POS2)
summary(mb_clust$uncertainty)
median(mb_clust$uncertainty)
# incertezza bassa in mediana



# KL distance
#-------------------------------------------------------------------------
(mu1<-as.matrix(mb_clust$parameters$mean[,1]))
(mu2<-as.matrix(mb_clust$parameters$mean[,2]))
(mu3<-as.matrix(mb_clust$parameters$mean[,3]))

(sigma1<-mb_clust$parameters$variance$sigma[, ,1])
(sigma2<-mb_clust$parameters$variance$sigma[, ,2])
(sigma3<-mb_clust$parameters$variance$sigma[, ,3])

KL_S<-function(mu1,mu2,sigma1,sigma2){ 
  0.5*t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)+0.5*sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))-length(mu1)[1]
}

drop(KL_S(mu1,mu2,sigma1,sigma2)) #distanza F-C, distanti
drop(KL_S(mu1,mu3,sigma1,sigma3)) #distanza F-G, molto vicini
drop(KL_S(mu2,mu3,sigma2,sigma3)) #distanza C-G, come ci si aspetta sono molto distanti


prob <- mb_clust$parameters$pro
mu <- as.matrix(colMeans(mb_clust$data))
sigma_mix <- prob[1]*sigma1+prob[2]*sigma2+prob[3]*sigma3+prob[1]*(mu1-mu)%*%t(mu1-mu)+prob[2]*(mu2-mu)%*%t(mu2-mu)+prob[3]*(mu3-mu)%*%t(mu3-mu)
num_r2 <- prob[1]*sigma1+prob[2]*sigma2+prob[3]*sigma3

(R2_t <- 1-(sum(diag(num_r2))/sum(diag(sigma_mix))))
(R2_d <- 1-(det(num_r2)/det(sigma_mix))) # valore molto buono

EN <- -sum(rowSums(mb_clust$z*log(mb_clust$z+10^-300)))
(EN_rel <- EN/(dim(basket1)[1]*log(3))) #valore accettabile
# sembra quindi che l'operazione di clustering sia generalmente discreta

db <- basket1[,-c(9,10)]
db <- as.data.frame(db)
coordProj (data=db, dimens=c(1,6), what="uncertainty",
           parameters=mb_clust$parameters , z=mb_clust$z)
# Come già visto dalle distanze tra gruppi, c'è un po' di difficolta nel
# discriminare guardie e ali (in realtà le variabili dall'angolo e assisted aiutano)
# Siccome le osservazioni sono tante la visualizzazione è difficile.




#Creo training e test set
#-------------------------------------------------------------------
set.seed(323)
role <- basket1$POS2
train_size <- floor(0.80*nrow(db))
train_index <- sample(1:nrow(db), train_size, replace = F)
X_train <- db[train_index, ]
y_train <- role[train_index ]

X_test <- db[-train_index, ]
y_test <- role[-train_index]




# Classification con modello EDDA
#-------------------------------------------
set.seed(323)
mod_edda<-mixmodLearn(X_train, y_train,   
                      models=mixmodGaussianModel(family='all', equal.proportions = F),
                      criterion=c('CV','BIC')) 
summary(mod_edda)
mod_edda@bestResult

# si verifica quale è modello migliore con più passi di CV
set.seed(323)
# si può provare con diversi valori di V
V <- 5 #10 #15 #20
mat <- matrix(NA,20,2)
for (r in 1:20){
  learn <- mixmodLearn(X_train, y_train, 
                       models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                       criterion=c('CV'),nbCVBlocks = V)
  mat[r,1] <- learn@bestResult@model
  mat[r,2] <- learn@bestResult@criterionValue[1]
}
mat
#Sceglie sempre gaussian pk L C
# viene scelto sempre il modello EEE, qualunque sia il valore di V


#Prediction EDDA
#--------------------------------
set.seed(323)
pred_edda<- mixmodPredict(data=X_test, classificationRule=mod_edda["bestResult"])
classError(pred_edda["partition"], y_test) # 0.2870709

pred_edda_partition <- pred_edda["partition"]

# fase di testing del classifier
pred_edda_partition <- as.factor(pred_edda_partition)
levels(pred_edda_partition) <- c('C','F','G')
confusionMatrix(data = pred_edda_partition,reference = y_test)

adjustedRandIndex(pred_edda_partition,y_test)
z <- pred_edda@proba
uncert <- apply(z,1,function(x) 1-max(x))
median(uncert)

# KL distance
media1 <- pred_edda@classificationRule@parameters@mean[1,]
media2 <- pred_edda@classificationRule@parameters@mean[2,]
media3 <- pred_edda@classificationRule@parameters@mean[3,]

var1 <- pred_edda@classificationRule@parameters@variance[[1]]
var2 <- pred_edda@classificationRule@parameters@variance[[2]]
var3 <- pred_edda@classificationRule@parameters@variance[[3]]

drop(KL_S(media1,media2,var1,var2)) #distanza F-C, vicini
drop(KL_S(media1,media3,var1,var3)) #distanza C-G, distanti
drop(KL_S(media2,media3,var2,var3)) #distanza F-G, molto vicini

probe <- pred_edda@classificationRule@parameters@proportions
media <- probe[1]*media1+probe[2]*media2+probe[3]*media3
var_mix <- probe[1]*var1+probe[2]*var2+probe[3]*var3+probe[1]*(media1-media)%*%t(media1-media)+probe[2]*(media2-media)%*%t(media2-media)+probe[3]*(media3-media)%*%t(media3-media)
num_r2e <- probe[1]*var1+probe[2]*var2+probe[3]*var3

(R2_te <- 1-(sum(diag(num_r2e))/sum(diag(var_mix))))
(R2_de <- 1-(det(num_r2e)/det(var_mix))) 

ENe <- -sum(rowSums(z*log(z)))
(EN_rele <- EN/(dim(pred_edda@data)[1]*log(3)))

# visualizzazione grafica classificazione
ggpairs(data = X_test,aes(col=pred_edda_partition),lower = 'blank',diag='blank',upper=list(continuous = "points", combo = "dot_no_facet"))

# %astd2P e DUNKS sembra la proiezione più utile a visualizzare classificazione
classError(pred_edda_partition,y_test)
mis_class_edda <- classError(pred_edda_partition,y_test)$misclassified
dbg <- X_test
ggplot()+
  geom_point(data = dbg,aes(x=`%_ASTD_2P`,y = `DUNKS`,col=pred_edda_partition),size=2,alpha=0.7)+
  geom_point(data = dbg[mis_class_edda,],aes(x=`%_ASTD_2P`,y = `DUNKS`),pch='x',size=2.5)+
  labs(color=NULL)+
  theme_classic()+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))

ggplot()+
  geom_point(data = dbg,aes(x=`3P`,y = `0\\3`,col=pred_edda_partition),size=2,alpha=0.7)+
  geom_point(data = dbg[mis_class_edda,],aes(x=`3P`,y = `0\\3`),pch='x',size=2.5)+
  labs(color=NULL)+
  theme_classic()+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))


# MDA
#-------------------------------------------------
set.seed(323)
mod_mda = MclustDA(X_train, y_train)
summary(mod_mda)

pred <- predict(mod_mda, X_test)$class

sum(pred != y_test) / length(y_test) # 0.2987582

confusionMatrix(pred,y_test)
# problema è che scelta di G e modello viene basata su valore di BIC


#CV per MDA
#----------------------------------------------
set.seed(323)
G = 10; V = 10; n <- nrow(X_train); perm = sample(n)
B = (n/V); err = matrix(NA ,G,V)



for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[round((B*(v-1)+1)):round((B*v))]
    mod = MclustDA(X_train[-test.set.labels ,], y_train[-test.set.labels],G=g, modelNames='EEE')
    err[g,v] = sum(predict(mod ,X_train[test.set.labels,])$class != y_train[test.set.labels]) / B
  }
}


err
round(rowMeans(err),4)

which.min(rowMeans(err)) #g = 7

par(mfrow=c(1,1))
plot(1:G,rowMeans(err),type='b',ylab='Classification error ',xlab='G',col='#E46726')

db_dis <- as.data.frame(cbind(1:G,rowMeans(err)))
colnames(db_dis) <- c('G','Errore')
ggplot(db_dis)+
  geom_line(aes(x=G,y=Errore),linewidth=1,col='#6D9EC1')+
  geom_point(aes(x=G,y=Errore),size=4,col='grey')+
  geom_vline(xintercept = 7,linetype=2,col='grey30')+
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(n.breaks = 10)+
  theme_classic()+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))


mod_mda_cv <- MclustDA(data = X_train,class = y_train,G = 7,modelNames = 'EEE')
mda_cv_class <- predict(mod_mda_cv,X_test)


pred_cv <- mda_cv_class$class
sum(pred_cv != y_test) / length(y_test) 



# criteri bontà classificazione
confusionMatrix(pred_cv,y_test)

adjustedRandIndex(pred_cv,y_test)

z_m <- mda_cv_class$z
uncert_m <- apply(z_m,1,function(x) 1-max(x))
median(uncert_m)
mean(uncert_m)

EN_m <- -sum(rowSums(z_m*log(z_m)))
(EN_rel_m <- EN_m/(dim(X_test)[1]*log(3)))


# %astd2P e DUNKS sembra la proiezione più utile a visualizzare classificazione
ggpairs(data = X_test,aes(col=pred_cv),lower = 'blank',diag='blank',upper=list(continuous = "points", combo = "dot_no_facet"))

classError(pred_cv,y_test)
mis_class_mda <- classError(pred_cv,y_test)$misclassified
dbg <- X_test
ggplot()+
  geom_point(data = dbg,aes(x=`%_ASTD_2P`,y = `DUNKS`,col=pred_cv),size=2,alpha=0.7)+
  geom_point(data = dbg[mis_class_mda,],aes(x=`%_ASTD_2P`,y = `DUNKS`),pch='x',size=2.5)+
  labs(color=NULL)+
  theme_classic()+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))
# come si può notare vi è una maggiore difficoltà a discriminare le ali




#Domanda 2
#---------------------------------------------------
(unique(basket$YEAR))

#Divido 9 anni
#era1 <- c("96\\97" ,"97\\98" ,"98\\99" ,"99\\00" ,"00\\01" ,"01\\02", "02\\03", "03\\04", "04\\05")
#era2 <- c("05\\06", "06\\07" ,"07\\08" ,"08\\09" ,"09\\10" ,"10\\11" ,"11\\12", "12\\13" ,"13\\14")
#era3 <- c("14\\15", "15\\16", "16\\17" ,"17\\18" ,"18\\19", "19\\20" ,"20\\21" ,"21\\22", "22\\23")

era1 <- factor(1997:2005)
era2 <- factor(2006:2014)
era3 <- factor(2015:2023)


is.era1 <- basket$YEAR %in% era1
X_era1 <- db[is.era1, ]
y_era1 <- role[is.era1]

is.era2 <- basket$YEAR %in% era2
X_era2 <- db[is.era2, ]
y_era2 <- role[is.era2]

is.era3 <- basket$YEAR %in% era3
X_era3 <- db[is.era3, ]
y_era3 <- role[is.era3]




#Alleno MDA era1 senza CV
#---------------------------------------------------
set.seed(323)
mod_mda_era1 = MclustDA(X_era1, y_era1)


#Pred su era2
pred_era1_era2 <- predict(mod_mda_era1, X_era2)$class
er12 <- sum(pred_era1_era2 != y_era2) / length(y_era2) #0.3066141
classError (pred_era1_era2 , y_era2)

#Pred su era3
pred_era1_era3 <- predict(mod_mda_era1, X_era3)$class
er13 <- sum(pred_era1_era3 != y_era3) / length(y_era3) #0.3348529
classError (pred_era1_era3 , y_era3)





#Alleno MDA era1 con CV
#------------------------------------------------------------
set.seed(323)
G = 10; V = 10; n <- nrow(X_era1); perm = sample(n)
B = round(n/V); err = matrix(NA,G,V)


for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[(B*(v-1)+1):(B*v)]
    mod = MclustDA(X_era1[-test.set.labels ,], y_era1[-test.set.labels],G=g, modelNames='EEE')
    err[g,v] = sum(predict(mod ,X_era1[test.set.labels,])$class != y_era1[test.set.labels]) / B
  }
}
err
round(rowMeans(err),4)
which.min(rowMeans(err)) #g = 9

mod_mda_cv_era1 <- MclustDA(data = X_era1,class = y_era1,G = 9,modelNames = 'EEE')

#Pred su era2
pred_cv_era1_era2 <- predict(mod_mda_cv_era1, X_era2)$class
er12_cv <- sum(pred_cv_era1_era2 != y_era2) / length(y_era2) # 0.2934735
classError (pred_cv_era1_era2 , y_era2)

#Pred su era3
pred_cv_era1_era3 <- predict(mod_mda_cv_era1, X_era3)$class
er13_cv <- sum(pred_cv_era1_era3 != y_era3) / length(y_era3) 
classError (pred_cv_era1_era3 , y_era3) #0.3166183





#Alleno MDA era2 senza CV
#------------------------------------------------------
set.seed(323)
mod_mda_era2 = MclustDA(X_era2, y_era2)


#Pred su era1
pred_era2_era1 <- predict(mod_mda_era2, X_era1)$class
er21 <- sum(pred_era2_era1 != y_era1) / length(y_era1) # 0.3235431
classError (pred_era2_era1 , y_era1)

#Pred su era3
pred_era2_era3 <- predict(mod_mda_era2, X_era3)$class
er23 <- sum(pred_era2_era3 != y_era3) / length(y_era3) #0.3099876
classError (pred_era2_era3 , y_era3)




#Alleno MDA era2 con CV
#------------------------------------------------------------------------
set.seed(323)
G = 10; V = 10; n <- nrow(X_era2); perm = sample(n)
B = (n/V); err = matrix(NA ,G,V)


for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[round((B*(v-1)+1)):round((B*v))]
    mod = MclustDA(X_era2[-test.set.labels ,], y_era2[-test.set.labels],G=g, modelNames='EEE')
    err[g,v] = sum(predict(mod ,X_era2[test.set.labels,])$class != y_era2[test.set.labels]) / B
  }
}
err
round(rowMeans(err),4)
which.min(rowMeans(err)) #g = 8

mod_mda_cv_era2 <- MclustDA(data = X_era2,class = y_era2,G = 8,modelNames = 'EEE')

#Pred su era1
pred_cv_era2_era1 <- predict(mod_mda_cv_era2, X_era1)$class
er21_cv <- sum(pred_cv_era2_era1 != y_era1) / length(y_era1) #0.3501166
classError (pred_cv_era2_era1 , y_era1)

#Pred su era3
pred_cv_era2_era3 <- predict(mod_mda_cv_era2, X_era3)$class
er23_cv <- sum(pred_cv_era2_era3 != y_era3) / length(y_era3) #0.2834646
classError (pred_cv_era2_era3 , y_era3)







#Alleno MDA era3
#--------------------------------------------------------------
set.seed(323)
mod_mda_era3 = MclustDA(X_era3, y_era3)


#Pred su era1
pred_era3_era1 <- predict(mod_mda_era3, X_era1)$class
er31 <- sum(pred_era3_era1 != y_era1) / length(y_era1) #0.4284382
classError (pred_era3_era1 , y_era1)

#Pred su era2
pred_era3_era2 <- predict(mod_mda_era3, X_era2)$class
er32 <- sum(pred_era3_era2 != y_era2) / length(y_era2) #0.3140604
classError (pred_era3_era2 , y_era2)



#Alleno MDA era3 con CV
#----------------------------------------------------------------
set.seed(323)
G = 10; V = 10; n <- nrow(X_era3); perm = sample(n)
B = (n/V); err = matrix(NA,G,V)



for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[round((B*(v-1)+1)):round((B*v))]
    mod = MclustDA(X_era3[-test.set.labels ,], y_era3[-test.set.labels],G=g, modelNames='EEE')
    err[g,v] = sum(predict(mod ,X_era3[test.set.labels,])$class != y_era3[test.set.labels]) / B
  }
}
err
round(rowMeans(err),4)
which.min(rowMeans(err)) #10

mod_mda_cv_era3 <- MclustDA(data = X_era3,class = y_era3,G = 10,modelNames = 'EEE')

#Pred su era1
pred_cv_era3_era1 <- predict(mod_mda_cv_era3, X_era1)$class
er31_cv <- sum(pred_cv_era3_era1 != y_era1) / length(y_era1) #0.4167832
classError (pred_cv_era3_era1 , y_era1)

#Pred su era2
pred_cv_era3_era2 <- predict(mod_mda_cv_era3, X_era2)$class
er32_cv <- sum(pred_cv_era3_era2 != y_era2) / length(y_era2) #0.3293912
classError (pred_cv_era3_era2 , y_era2)





risultati <- matrix(c(0, er12_cv, er13_cv,
                      er21_cv, 0, er23_cv,
                      er31_cv, er32_cv, 0), byrow = T, ncol = 3, nrow = 3)
colnames(risultati) <- c("era1", "era2", "era3")
rownames(risultati) <- c("era1", "era2", "era3")

risultati <- 1-risultati
diag(risultati) <- 0
round(risultati, 3)


#       era1  era2  era3
# era1 0.000 0.293 0.317
# era2 0.350 0.000 0.283
# era3 0.417 0.329 0.000







####################################################################
#
#              Codice per campo da basket
#
#########################################################################
#-------------------------------------------------------------------

# Per lo svolgimento di questa parte ci siamo avvalsi dell'aiuto della risorsa: Spatial Analysis of Basketball Shots in R
# E in particolare del codice github fornito da Olivier Chabot

## --------------------------------------------------------------------------
# Load the libraries
library(sf) # Work with polygons
library(tidyverse) # ggplot2 and dplyr 

# Load some predefined court themes for the plot_court() functions
court_themes = list(
  light = list(
    court = '#ffffff',
    lines = '#000000',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0.3,
    hex_border_color = "#cccccc"
  ),
  dark = list(
    court = '#000004',
    lines = '#ffffff',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  )
)
# All lengths will be in metres
line_thick = 0.05
width = 15
height = 28 / 2
key_height = 5.8
key_width = 4.9
key_radius = 1.8
backboard_width = 1.8
backboard_thick = 0.1
backboard_offset = 1.2
hoop_radius = 0.45 / 2
hoop_center_y = 1.575
rim_thick = 0.02
neck_length = hoop_center_y - (backboard_offset + hoop_radius + rim_thick)
three_point_radius = 6.75
three_point_side_offset = 0.9
three_point_side_height = sqrt(
  three_point_radius^2 - (three_point_side_offset - width/2)^2
) + hoop_center_y
restricted_area_radius = 1.25


## --------------------------------------------------------------------------
# Draw a rectangle that defines the half-court interior
half_court_int <- rbind(
  c(0, 0),
  c(0, height),
  c(width, height),
  c(width, 0),
  c(0,0)
)

# Draw a rectangle that defines the half-court exterior
half_court_ext <- rbind(
  c(0-line_thick, 0-line_thick),
  c(0-line_thick, height + line_thick),
  c(width + line_thick, height + line_thick),
  c(width + line_thick, 0-line_thick),
  c(0-line_thick, 0-line_thick)
)

# Define a sfg polygon object in sf by subtracting interior from exterior
half_court <- st_polygon(list(half_court_ext, half_court_int))

## --------------------------------------------------------------------------
# Draw a rectangle for the key
key_int <- rbind(
  c(width/2 - key_width/2 + line_thick, 0),
  c(width/2 - key_width/2 + line_thick, key_height - line_thick),
  c(width/2 + key_width/2 - line_thick, key_height - line_thick),
  c(width/2 + key_width/2 - line_thick, 0),
  c(width/2 - key_width/2 + line_thick, 0)
)

key_ext <- rbind(
  c(width/2 - key_width/2, 0),
  c(width/2 - key_width/2, key_height),
  c(width/2 + key_width/2, key_height),
  c(width/2 + key_width/2, 0),
  c(width/2 - key_width/2, 0)
)

key <- st_polygon(list(key_ext, key_int))

# Draw a rectangle for the backboard
backboard_points <- rbind(
  c(width/2 - backboard_width/2, backboard_offset - backboard_thick),
  c(width/2 - backboard_width/2, backboard_offset),
  c(width/2 + backboard_width/2, backboard_offset),
  c(width/2 + backboard_width/2, backboard_offset - backboard_thick),
  c(width/2 - backboard_width/2, backboard_offset - backboard_thick)
)

backboard <- st_polygon(list(backboard_points))

# Neck
neck_points <- rbind(
  c(width/2 - line_thick/2, backboard_offset),
  c(width/2 - line_thick/2, backboard_offset + neck_length),
  c(width/2 + line_thick/2, backboard_offset + neck_length),
  c(width/2 + line_thick/2, backboard_offset),
  c(width/2 - line_thick/2, backboard_offset)
)

neck <- st_polygon(list(neck_points))


## --------------------------------------------------------------------------
# Define a point sfg object for the center of the hoop
hoop_center <- st_point(c(width/2, hoop_center_y))

# Check class of hoop_center
class(hoop_center)

# Interior of the rim
# Buffer the point by the radius of the hoop to create a circle
hoop_int <- hoop_center %>%
  st_buffer(dist = hoop_radius)

# Exterior of the rim
hoop_ext <- hoop_center %>%
  st_buffer(dist = hoop_radius + rim_thick)

# Subtract interior from exterior to get the rim
hoop <- st_polygon(list(
  # Only kepp the X, Y columns of the coordinates
  st_coordinates(hoop_ext)[ , 1:2],
  st_coordinates(hoop_int)[ , 1:2]
))


## --------------------------------------------------------------------------
# Draw the half-circle at the top of the key
key_center <- st_point(c(width/2, key_height))

key_circle_int <- st_crop(
  st_sfc(st_buffer(key_center, dist = key_radius - line_thick)),
  # Only keep the part of the circle above the top of the key
  xmin = 0, ymin = key_height, xmax = width, ymax = height
)

key_circle_ext <- st_crop(
  st_sfc(st_buffer(key_center, dist = key_radius)),
  xmin = 0, ymin = key_height, xmax = width, ymax = height
)

key_circle <- st_polygon(list(
  st_coordinates(key_circle_ext)[ , 1:2],
  st_coordinates(key_circle_int)[ , 1:2]
))

# Draw the half-circle at the bottom of half-court
half_center <- st_point(c(width/2, height))

half_circle_int <- st_crop(
  st_sfc(st_buffer(half_center, dist = key_radius - line_thick)),
  # only keep the bottom half below the half-court line
  xmin = 0, ymin = 0, xmax = width, ymax = height
)

half_circle_ext <- st_crop(
  st_sfc(st_buffer(half_center, dist = key_radius)),
  xmin = 0, ymin = 0, xmax = width, ymax = height
)

half_circle <- st_polygon(list(
  st_coordinates(half_circle_ext)[ , 1:2],
  st_coordinates(half_circle_int)[ , 1:2]
))



## --------------------------------------------------------------------------
# Define a point sfg object for the center of the hoop
three_center <- st_point(c(width/2, hoop_center_y))

# Buffer the point to create a circle & crop it at 2.99 meters
three_int <- st_crop(
  st_sfc(st_buffer(three_center, dist = three_point_radius - line_thick)),
  xmin = three_point_side_offset + line_thick, ymin = three_point_side_height,
  xmax = width - (three_point_side_offset + line_thick), ymax = height
)

# Get the number of rows of coordinates of the three_int object
n <- nrow(st_coordinates(three_int))

# Bind the straight line points to the arc
three_int <- rbind(
  c(three_point_side_offset + line_thick, 0),
  c(three_point_side_offset + line_thick, three_point_side_height),
  # Remove the last two rows and only keep the X,Y columns
  st_coordinates(three_int)[1:(n-2), 1:2],
  c(width - (three_point_side_offset + line_thick), three_point_side_height),
  c(width - (three_point_side_offset + line_thick), 0),
  c(three_point_side_offset + line_thick, 0)
)

# Do the same for the exterior
three_ext <- st_crop(
  st_sfc(st_buffer(three_center, dist = three_point_radius)),
  xmin = three_point_side_offset, ymin = three_point_side_height,
  xmax = width - three_point_side_offset, ymax = height
)

three_ext <- rbind(
  c(three_point_side_offset, 0),
  c(three_point_side_offset, three_point_side_height),
  st_coordinates(three_ext)[1:(n-2), 1:2],
  c(width - three_point_side_offset, three_point_side_height),
  c(width - three_point_side_offset, 0),
  c(three_point_side_offset, 0)
)

# Create a three-point line sfg polygon object
three_point_line <- st_polygon(list(three_int, three_ext))



## --------------------------------------------------------------------------
# Restricted area
ra_center <- st_point(c(width/2, hoop_center_y))

ra_ext <- st_crop(
  st_sfc(st_buffer(ra_center, dist = restricted_area_radius + line_thick)),
  xmin = 0, ymin = hoop_center_y,
  xmax = width, ymax = height
)

n <- nrow(st_coordinates(ra_ext))

ra_ext <- tibble(
  x = st_coordinates(ra_ext)[1:(n-2), 1],
  y = st_coordinates(ra_ext)[1:(n-2), 2]
)


ra_ext <- rbind(
  c(width/2 - restricted_area_radius - line_thick, backboard_offset),
  c(width/2 - restricted_area_radius - line_thick, hoop_center_y),
  ra_ext,
  c(width/2 + restricted_area_radius + line_thick, hoop_center_y),
  c(width/2 + restricted_area_radius + line_thick, backboard_offset)
)

ra_int <- st_crop(
  st_sfc(st_buffer(ra_center, dist = restricted_area_radius)),
  xmin = 0, ymin = hoop_center_y,
  xmax = width, ymax = height
)

# Reverse the direction of the interior arc points
ra_int_flip <- tibble(
  x = st_coordinates(ra_int)[1:(n-2), 1],
  y = st_coordinates(ra_int)[1:(n-2), 2]
) %>%
  arrange(desc(x))

ra_int <- rbind(
  c(width/2 + restricted_area_radius, backboard_offset),
  c(width/2 + restricted_area_radius, hoop_center_y),
  ra_int_flip,
  c(width/2 - restricted_area_radius, hoop_center_y),
  c(width/2 - restricted_area_radius, backboard_offset),
  c(width/2 - restricted_area_radius - line_thick, backboard_offset)
)

# Bind all the points together
ra_points <- as.matrix(rbind(ra_ext, ra_int))

restricted_area <- st_polygon(list(ra_points))


## --------------------------------------------------------------------------
# Create sf object with 9 features and 1 field
court_sf <- st_sf(
  description = c("half_court", "key", "hoop", "backboard",
                  "neck", "key_circle", "three_point_line",
                  "half_circle", "restricted_area"), 
  geom = c(st_geometry(half_court), st_geometry(key), st_geometry(hoop),
           st_geometry(backboard), st_geometry(neck), st_geometry(key_circle), 
           st_geometry(three_point_line), st_geometry(half_circle),
           st_geometry(restricted_area))
)



## --------------------------------------------------------------------------
plot_court = function(court_theme = court_themes$light) {
  ggplot() +
    geom_sf(data = court_sf,
            fill = court_theme$lines, col = court_theme$lines) +
    theme_void() +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(
        fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(
        fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(
        fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

# Light Theme (Default)
plot_court()





############################### ZONE COLORATE ###################################
# Oltre la linea da 3
three_ext <- st_crop(
  st_sfc(st_buffer(three_center, dist = three_point_radius)),
  xmin = three_point_side_offset, ymin = three_point_side_height,
  xmax = width - three_point_side_offset, ymax = height
)
n <- dim(st_coordinates(three_ext))[1]

# Mid-Range
three_ext <- tibble(
  x = st_coordinates(three_ext)[1:(n-2), 1],
  y = st_coordinates(three_ext)[1:(n-2), 2]
)


half_court <- st_polygon(list(half_court_int))

two_point_area <- rbind(
  c(three_point_side_offset, 0),
  c(three_point_side_offset, three_point_side_height),
  three_ext,
  c(width - three_point_side_offset, three_point_side_height),
  c(width - three_point_side_offset, 0),
  c(three_point_side_offset, 0)
)

two_point_area <- st_polygon(list(as.matrix(two_point_area)))

three_point_area <- st_difference(half_court, two_point_area)


################################ Distanze ###################################
# 0-3 ft
zero_three_ft <- hoop_center %>%
  st_buffer(dist = 3 * 0.3048)

zero_three_ft <- st_polygon(list(st_coordinates(zero_three_ft)[ , 1:2]))

zero_three_ft <- st_intersection(half_court, zero_three_ft)

# 3-10 ft
three_ten_ft <- hoop_center %>%
  st_buffer(dist = 10 * 0.3048)

three_ten_ft <- st_polygon(list(st_coordinates(three_ten_ft)[ , 1:2]))

three_ten_ft <- st_intersection(half_court, three_ten_ft)

three_ten_ft <- st_difference(three_ten_ft, zero_three_ft)




# 10-16 ft
ten_sixteen_ft <- hoop_center %>%
  st_buffer(dist = 16 * 0.3048)

ten_sixteen_ft <- st_polygon(list(st_coordinates(ten_sixteen_ft)[ , 1:2]))

ten_sixteen_ft <- st_intersection(half_court, ten_sixteen_ft)

ten_sixteen_ft <- st_difference(ten_sixteen_ft,
                                st_union(three_ten_ft,zero_three_ft))

# 16-24 ft
sixteen_twentyfour_ft <- st_difference(
  two_point_area,
  st_union(ten_sixteen_ft, three_ten_ft)
)

sixteen_twentyfour_ft <- st_difference(
  sixteen_twentyfour_ft,
  zero_three_ft
)

# 24+ ft
twentyfour_plus_ft <- three_point_area

distance_polys <- st_sf(
  description = c(
    "0-3 ft",
    "3-10 ft",
    "10-16 ft",
    "16-3P",
    "3P"
  ), 
  geom = c(
    st_geometry(zero_three_ft),
    st_geometry(three_ten_ft),
    st_geometry(ten_sixteen_ft),
    st_geometry(sixteen_twentyfour_ft),
    st_geometry(twentyfour_plus_ft)
  )
) %>%
  transmute(
    shot_zone_range = factor(
      description,
      levels = c("0-3 ft","3-10 ft","10-16 ft","16-3P","3P")
    ),
    geom
  )

medians <- basket %>%
  group_by(POS2) %>%
  summarize_at(vars('0\\3':'3P'), median, na.rm = TRUE) %>%
  ungroup() %>%
  t() 

medianC<-as.vector(as.numeric(medians[-1,1]))
medianF<-as.vector(as.numeric(medians[-1,2]))
medianG<-as.vector(as.numeric(medians[-1,3]))


distance_data_G <- data.frame(
  geom = distance_polys$geom,
  percent = medianG
) %>% 
  st_as_sf()

# disegnare le zone di tiro con i colori delle percentuali e la legenda
pg1 <- plot_court() +
  geom_sf(data = distance_data_G,
          aes(fill = percent),
          alpha = 0.9,show.legend = F) +
  scale_fill_gradient(name = "Percentuale",
                      limits = c(0, 1),
                      low = "white",
                      high = "red")+
  geom_sf_text(data = distance_data_G,
               aes(label = paste0(percent * 100, "%")),
               size = 3,
               color = "black")+
  labs(title = "Guardie")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

distance_data_F <- data.frame(
  geom = distance_polys$geom,
  percent = medianF
) %>% 
  st_as_sf()

# disegnare le zone di tiro con i colori delle percentuali e la legenda
pf1 <- plot_court() +
  geom_sf(data = distance_data_F,
          aes(fill = percent),
          alpha = 0.9,show.legend = F) +
  scale_fill_gradient(name = "Percentuale",
                      limits = c(0, 1),
                      low = "white",
                      high = "red")+
  geom_sf_text(data = distance_data_F,
               aes(label = paste0(percent * 100, "%")),
               size = 3,
               color = "black")+
  labs(title = "Ali")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

distance_data_C <- data.frame(
  geom = distance_polys$geom,
  percent = medianC
) %>% 
  st_as_sf()

# disegnare le zone di tiro con i colori delle percentuali e la legenda
pc1 <- plot_court() +
  geom_sf(data = distance_data_C,
          aes(fill = percent),
          alpha = 0.9,show.legend = F) +
  scale_fill_gradient(name = "Percentuale",
                      limits = c(0, 1),
                      low = "white",
                      high = "red")+
  geom_sf_text(data = distance_data_C,
               aes(label = paste0(percent * 100, "%")),
               size = 3,
               color = "black")+
  labs(title = "Centri")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

grid.arrange(pg1,pf1,pc1,nrow=1,top=textGrob("Distribuzione dei tiri tentati per ruolo",gp=gpar(fontsize=18,font=2)))



