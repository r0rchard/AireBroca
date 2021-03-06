#========================Lancement========================
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
data <- readRDS("H:/2 Ann�e/Semestre 1/Statistiques/activation.Rdata", refhook = NULL)
colnames(data)
data <- data[2:17]
attach(data) #Acc�s aux composantes directement par son nom
dataTriSexe <- split(data, Sexe)#D�coupage des donn�es en 2 (1�re partie : F, 2�me partie : H)
dataTriSexe
detach(data) 
attach(dataTriSexe) #On n'a plus acc�s aux composantes par nom de "data" mais de "dataTriSexe"
dataF <- F #Stockage des valeurs F
dataH <- H
dataF<-dataF[2:16]
dataH<-dataH[2:16]
dataF
dataH
#========================
mean(data$Index_Lateralisation_Hemispherique) #moyenne des index de lat�ralisation h�misph�rique
indexPos <- data$Index_Lateralisation_Hemispherique>0
indexPos #100 % des sujets ont leur h�misph�re gauche dominant

#========================ACP======================================
res <- PCA(data[3:17], graph = FALSE) #on applique l'ACP aux donn�es quatitatives

round(res$ind$cos2,digit=3)
eig.val <- get_eigenvalue(res) #Valeurs propres / variances
eig.val #on garde les premi�res
fviz_eig(res, addlabels = TRUE, ylim = c(0, 50)) #visualisation

var <- get_pca_var(res)
var

head(var$coord, 4)
fviz_pca_var(res, col.var = "black") #pas de corr�lation 

head(var$cos2, 4) #dim pas repr�sentatives

#Il n'y a pas de corr�lations fortes entre les diff�rentes variables (i.e aires du cerveau)

#========================Regression Lin�aire========================
res <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L+PROD_G_Occipital_Lat_1_L
+PROD_G_Rolandic_Oper_1_L+PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
+PROD_G_Angular_2_R+PROD_G_Occipital_Lat_1_R+PROD_G_Rolandic_Oper_1_R          
+PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=data)

summary(res) #regarder la p-value, H0 : beta1=beta2=betap=0 ; H1 il existe betap tq betap =!0 
#ici p-value <0.05 -> on rejette H0, mod�le utile
#R� = 60% explique 60% de la variance, pas terrible
#R� ajust� = 0.5517 
#regarder aussi le residual standart error : �cart-type
#Pr(>|t|) : p-value 

#v�rification qu'il y a pas de lien entre r�sidus
par(mfrow=c(1,2))
plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,data$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(res$residuals) #pvalue < 0.5, mais pas  trop loin

#========================AIC(drop)========================
drop1(res) #on enl�ve : PROD_G_Occipital_Lat_1_L, PROD_G_Rolandic_Oper_1_L,PROD_G_Angular_2_R,PROD_G_Rolandic_Oper_1_R    

#========================MODELE2 -> data ========================
resDrop <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L
              +PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
                +PROD_G_Occipital_Lat_1_R          
              +PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=data[,6:17])

summary(resDrop) #regarder la p-value, H0 : beta1=beta2=betap=0 ; H1 il existe betap tq betap =!0 
#ici p-value <0.05 -> on rejette H0, mod�le utile
#m�me R� qu'avant, mais avec moins de variables, R� ajust�  = 0.5636

#v�rification qu'il y a pas de lien entre r�sidus
par(mfrow=c(1,2))
plot(resDrop$fitted,resDrop$residuals)
abline(h=0,col=2)
plot(resDrop$fitted,data$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resDrop$residuals) #p-value encore pire
#========================FEMMES========================

resF <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L+PROD_G_Occipital_Lat_1_L
          +PROD_G_Rolandic_Oper_1_L+PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
            +PROD_G_Angular_2_R+PROD_G_Occipital_Lat_1_R+PROD_G_Rolandic_Oper_1_R          
          +PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=dataF)
summary(resF)
par(mfrow=c(1,2))
plot(resF$fitted,resF$residuals)
abline(h=0,col=2)
plot(resF$fitted,dataF$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resF$residuals) #pvalue < 0.5, mais pas  trop loin
#========================AIC(step ascendant)========================
resF2 <- lm(PROD_G_Frontal_Inf_Tri_1_L~1, data=dataF)
resF2 <- step(resF2, as.formula(paste("~",paste(colnames(dataF)[-1],collapse="+"), sep="")), trace =TRUE)

#========================MODELE3 : Femmes ========================
resFStep <- lm(PROD_G_Frontal_Inf_Tri_1_L ~ PROD_G_Frontal_Inf_Tri_1_R + PROD_S_Sup_Temporal_4_L + 
                Index_Lateralisation_Hemispherique + PROD_G_Angular_2_L,data=dataF)

summary(resFStep) #R� ajust� = 0.5551
#v�rification qu'il y a pas de lien entre r�sidus
par(mfrow=c(1,2))
plot(resFStep$fitted,resFStep$residuals)
abline(h=0,col=2)
plot(resFStep$fitted,dataF$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resFStep$residuals)
#========================HOMMES========================

resH <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L+PROD_G_Occipital_Lat_1_L
           +PROD_G_Rolandic_Oper_1_L+PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
             +PROD_G_Angular_2_R+PROD_G_Occipital_Lat_1_R+PROD_G_Rolandic_Oper_1_R          
           +PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=dataH)
summary(resH)
par(mfrow=c(1,2))
plot(resH$fitted,resH$residuals)
abline(h=0,col=2)
plot(resH$fitted,dataH$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resH$residuals) #pvalue < 0.5, mais pas  trop loin
#========================AIC(step descendant)========================
resHStep <- step(resH)
#========================MODELE4 : Hommes========================
summary(resHStep) #R� ajust� =  0.5636
#v�rification qu'il y a pas de lien entre r�sidus
par(mfrow=c(1,2))
plot(resHStep$fitted,resHStep$residuals)
abline(h=0,col=2)
plot(resHStep$fitted,dataH$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resHStep$residuals) #p-value = 0.1767  m�me mod�le que le 2
#======================ANOVA sans interaction===========================
#Entr�e des donn�es � partir du modele4
data.aov <-data[,6:17]
res.aov1 <- aov(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L + 
                 PROD_S_Sup_Temporal_4_L + PROD_G_Hippocampus_1_L + PROD_G_Frontal_Inf_Tri_1_R + 
                 PROD_G_Occipital_Lat_1_R + PROD_S_Sup_Temporal_4_R + PROD_G_Hippocampus_1_R,data=data.aov)
summary(res.aov1)
#On enl�ve PROD_G_Hippocampus_1_L et PROD_G_Occipital_Lat_1_R

#======================MODELE5===========================
res.aov1 <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L + 
                 PROD_S_Sup_Temporal_4_L + PROD_G_Frontal_Inf_Tri_1_R + 
                 PROD_S_Sup_Temporal_4_R + PROD_G_Hippocampus_1_R, data=data.aov)
summary(res.aov1) #R� ajust� = 0.4936, mauvaise id�e

#======================EN FONCTION DE L'AGE ET DU SEXE======
brocaG <- data[,5]
sexe <- data[,2]
age <- data[,3]

plot(brocaG~sexe)#boxplot : moyennes proches mais var plus grande pour les H
#moyenne de chaque sexe
f <- data$PROD_G_Frontal_Inf_Tri_1_L[which(data$Sexe=="F")] 
h <- data$PROD_G_Frontal_Inf_Tri_1_L[which(data$Sexe=="H")] 
t.test(h,f) #test de student, p-value = 0.01409 ; diff�rence de moyennes significative
var.test(f,h) #p-value < 0.5 ; variances significativement diff�rentes

plot(brocaG~age) #pas de forme dans le nuage de pts -> on peut supposer que l'�ge n'a pas d'impact


data.aov2 <-data[,2:17]
res.aov2 <- aov(PROD_G_Frontal_Inf_Tri_1_L~Sexe+Age, data=data.aov2) #sans interaction : age pas utile
summary(res.aov2)

data.aov3 <-data[,2:17]
res.aov3 <- aov(PROD_G_Frontal_Inf_Tri_1_L~Sexe*Age, data=data.aov3) #avec interaction : age et sexe:age pas utile
summary(res.aov3) #le sexe a bien une influence

fligner.test(data[,5],data[,2],data[,3])
#p-value = 0.0345<0.5, var diff�rentes : ok
shapiro.test(res.aov4$residuals)
#p-value = 0.6154 > 5% -> r�sidus suivent une loi normale


