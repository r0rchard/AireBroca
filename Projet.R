#========================Lancement========================
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
data <- readRDS("H:/2 Année/Semestre 1/Statistiques/activation.Rdata", refhook = NULL)
colnames(data)
data <- data[2:17]
attach(data) #Accès aux composantes directement par son nom
dataTriSexe <- split(data, Sexe)#Découpage des données en 2 (1ère partie : F, 2ème partie : H)
dataTriSexe
detach(data) 
attach(dataTriSexe) #On n'a plus accès aux composantes par nom de "data" mais de "dataTriSexe"
dataF <- F #Stockage des valeurs F
dataH <- H
dataF<-dataF[2:16]
dataH<-dataH[2:16]
dataF
dataH
#========================
mean(data$Index_Lateralisation_Hemispherique) #moyenne des index de latéralisation hémisphérique
indexPos <- data$Index_Lateralisation_Hemispherique>0
indexPos #100 % des sujets ont leur hémisphère gauche dominant

#========================ACP======================================
res <- PCA(data[3:17], graph = FALSE) #on applique l'ACP aux données quatitatives

round(res$ind$cos2,digit=3)
eig.val <- get_eigenvalue(res) #Valeurs propres / variances
eig.val #on garde les premières
fviz_eig(res, addlabels = TRUE, ylim = c(0, 50)) #visualisation

var <- get_pca_var(res)
var

head(var$coord, 4)
fviz_pca_var(res, col.var = "black") #pas de corrélation 

head(var$cos2, 4) #dim pas représentatives

#Il n'y a pas de corrélations fortes entre les différentes variables (i.e aires du cerveau)

#========================Regression Linéaire========================
res <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L+PROD_G_Occipital_Lat_1_L
+PROD_G_Rolandic_Oper_1_L+PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
+PROD_G_Angular_2_R+PROD_G_Occipital_Lat_1_R+PROD_G_Rolandic_Oper_1_R          
+PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=data)

summary(res) #regarder la p-value, H0 : beta1=beta2=betap=0 ; H1 il existe betap tq betap =!0 
#ici p-value <0.05 -> on rejette H0, modèle utile
#R² = 60% explique 60% de la variance, pas terrible
#R² ajusté = 0.5517 
#regarder aussi le residual standart error : écart-type
#Pr(>|t|) : p-value 

#vérification qu'il y a pas de lien entre résidus
par(mfrow=c(1,2))
plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,data$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(res$residuals) #pvalue < 0.5, mais pas  trop loin

#========================AIC(drop)========================
drop1(res) #on enlève : PROD_G_Occipital_Lat_1_L, PROD_G_Rolandic_Oper_1_L,PROD_G_Angular_2_R,PROD_G_Rolandic_Oper_1_R    

#========================MODELE2 -> data ========================
resDrop <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L
              +PROD_S_Sup_Temporal_4_L+PROD_G_Hippocampus_1_L+PROD_G_Frontal_Inf_Tri_1_R+        
                +PROD_G_Occipital_Lat_1_R          
              +PROD_S_Sup_Temporal_4_R+PROD_G_Hippocampus_1_R, data=data[,6:17])

summary(resDrop) #regarder la p-value, H0 : beta1=beta2=betap=0 ; H1 il existe betap tq betap =!0 
#ici p-value <0.05 -> on rejette H0, modèle utile
#même R² qu'avant, mais avec moins de variables, R² ajusté  = 0.5636

#vérification qu'il y a pas de lien entre résidus
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

summary(resFStep) #R² ajusté = 0.5551
#vérification qu'il y a pas de lien entre résidus
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
summary(resHStep) #R² ajusté =  0.5636
#vérification qu'il y a pas de lien entre résidus
par(mfrow=c(1,2))
plot(resHStep$fitted,resHStep$residuals)
abline(h=0,col=2)
plot(resHStep$fitted,dataH$PROD_G_Frontal_Inf_Tri_1_L)
abline(0,1,col=2)
par(mfrow=c(1,1))
#pas de figure
shapiro.test(resHStep$residuals) #p-value = 0.1767  même modèle que le 2
#======================ANOVA sans interaction===========================
#Entrée des données à partir du modele4
data.aov <-data[,6:17]
res.aov1 <- aov(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L + 
                 PROD_S_Sup_Temporal_4_L + PROD_G_Hippocampus_1_L + PROD_G_Frontal_Inf_Tri_1_R + 
                 PROD_G_Occipital_Lat_1_R + PROD_S_Sup_Temporal_4_R + PROD_G_Hippocampus_1_R,data=data.aov)
summary(res.aov1)
#On enlève PROD_G_Hippocampus_1_L et PROD_G_Occipital_Lat_1_R

#======================MODELE5===========================
res.aov1 <- lm(PROD_G_Frontal_Inf_Tri_1_L~PROD_G_Angular_2_L + 
                 PROD_S_Sup_Temporal_4_L + PROD_G_Frontal_Inf_Tri_1_R + 
                 PROD_S_Sup_Temporal_4_R + PROD_G_Hippocampus_1_R, data=data.aov)
summary(res.aov1) #R² ajusté = 0.4936, mauvaise idée

#======================EN FONCTION DE L'AGE ET DU SEXE======
brocaG <- data[,5]
sexe <- data[,2]
age <- data[,3]

plot(brocaG~sexe)#boxplot : moyennes proches mais var plus grande pour les H
#moyenne de chaque sexe
f <- data$PROD_G_Frontal_Inf_Tri_1_L[which(data$Sexe=="F")] 
h <- data$PROD_G_Frontal_Inf_Tri_1_L[which(data$Sexe=="H")] 
t.test(h,f) #test de student, p-value = 0.01409 ; différence de moyennes significative
var.test(f,h) #p-value < 0.5 ; variances significativement différentes

plot(brocaG~age) #pas de forme dans le nuage de pts -> on peut supposer que l'âge n'a pas d'impact


data.aov2 <-data[,2:17]
res.aov2 <- aov(PROD_G_Frontal_Inf_Tri_1_L~Sexe+Age, data=data.aov2) #sans interaction : age pas utile
summary(res.aov2)

data.aov3 <-data[,2:17]
res.aov3 <- aov(PROD_G_Frontal_Inf_Tri_1_L~Sexe*Age, data=data.aov3) #avec interaction : age et sexe:age pas utile
summary(res.aov3) #le sexe a bien une influence

fligner.test(data[,5],data[,2],data[,3])
#p-value = 0.0345<0.5, var différentes : ok
shapiro.test(res.aov4$residuals)
#p-value = 0.6154 > 5% -> résidus suivent une loi normale


