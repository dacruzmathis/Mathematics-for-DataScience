library(FactoMineR)
library(factoextra)

#Importation csv dans var prostate

prostate <- read.table(file = "/Users/sarankansivananthan/Desktop/M1/Maths/TP_R/prostate.txt", header = TRUE , sep = " ", dec = ".")

#Premieres stats sur prostate

#2.2

#1

#Nb rows

nrow(prostate)
nrow(na.omit(prostate))

#nb lignes égales avant et après suppression des valeurs na => pas de valeurs na

#2

summary(prostate["psa"])

# le taux spécifique d'antigène de la prostate au-dessous duquel se situe 25% des taux est de 6.125 (1st Qu)
# le taux spécifique d'antigène de la prostate au-dessous duquel se situe 50% des taux est de 14.400 (Median)
# le taux spécifique d'antigène de la prostate au-dessous duquel se situe 75% des taux est de 21.350 (3rd Qu)
# le taux spécifique d'antigène de la prostate moyen est de 25.473 (Mean)
#le max est bien supérieur au troisième quertile. On peut le considérer comme une valeur extrême

#3

cor(prostate["psa"] , prostate["vol"]) # 0.6664723 => coef proche de 1 ie quand les valeurs d'une var augmentent alors celles de l'autre aussi
cor(prostate["psa"] , prostate["wht"]) # 0.1662757 => coef proche de 0 ie corrélation positive faible 
cor(prostate["psa"] , prostate["age"]) # 0.01304884 => coef proche de 0 ie corrélation  positive faible A CHANGER
cor(prostate["psa"] , prostate["bh"]) # -0.02203714 => corr nég ie quand les val d'une var augmentent les valeurs de l'autre var diminuent A CHANGER
cor(prostate["psa"] , prostate["pc"]) # 0.5957111 => coef proche de 1 ie quand les valeurs d'une var augmentent alors celles de l'autre aussi

# => vol est plus corrélée avec psa
# => on peut supposer que les var psa, vol, pc permettraient de representer un max d'information? (à vérifier avec l'ACP)

plot(prostate) # graphes en fonction de chaques variables

#4
attach(prostate)

lvol = log10(vol)
lwht = log10(wht)
age = age
lbh = log10(bh)
lpc = log10(pc)
lpsa = log10(psa)

lprostate = data.frame("lvol" = lvol , "lwht" = lwht , age , "lbh" = lbh , "lpc" = lpc , "lpsa" = lpsa)

plot(lprostate) #graphes en fonction de chaques varibles

# Interpretations
  # corr linéaire positive entre vol et psa
  # aussi entre vol et pc
  # moins visible pour psa / pc



#III) ACP


#3.1

# Oui car à eux deux, elles représenteraient 100% de l'information

#3.2

apply(lprostate , 2 ,var) # plus variance est petite moins la variable a des valeurs dispersées ie moins elle varie par rapport à la moyenne

apply(prostate , 2 ,var)

# on voit bien que le log a permis de réduire la dispersion, ex: psa: 1963 -> 1,44...

#3.3

#oui pour réduire la variance de l'age (variance trop élevée), faire en sorte que l'age soit comparable aux autres variables

prostate.cr = scale(lprostate , center = TRUE , scale = TRUE)

#3.4
resultat.PCA = PCA(prostate.cr , graph = T)

names(resultat.PCA)

resultat.PCA$eig

summary(resultat.PCA)


#3.5

# avec les deux premiers axes on arrive à représenter 69.46201% de l'information
resultat.PCA$eig 


barplot(resultat.PCA$eig[,2])
fviz_eig(resultat.PCA , addlabels=TRUE)

# On voit que les 3 premiers axes permettent de représenter le max d'info et que les 3 derniers ne rajooutent pas plus d'infos par rapport aux 3 premiers



#3.6
PVE <- resultat.PCA$eig[,2]/ sum(resultat.PCA$eig[,2])

qplot(c(1:6), PVE) + geom_line() + ylim(0, 1)
qplot(c(1:6), cumsum(PVE)) + geom_line() + ylim(0, 1)


#3.7

#on en garde 3



#4.1

#coef deter = coef corr ^2

#plage de R : [-1 , 1]

#4.2



cor(lpsa , lvol) # 0.7858116 => coef proche de 1 ie quand les valeurs d'une var augmentent alors celles de l'autre aussi
cor(lpsa , lwht) # 0.4558655 => coef proche de 0 ie corrélation positive faible 
cor(lpsa , prostate["age"]) # 0.1755921 => coef proche de 0 ie corrélation  positive faible A CHANGER
cor(lpsa , lbh) # 0.1927745 => corr nég ie quand les val d'une var augmentent les valeurs de l'autre var diminuent A CHANGER
cor(lpsa , lpc) # 0.5545791 => coef proche de 1 ie quand les valeurs d'une var augmentent alors celles de l'autre aussi


X = lvol

model = lm(lpsa ~ X , data = lprostate)

summary(model)

plot(X, lpsa)
abline(model)


#4.3
model$coefficients

# le coeff directeur dit qu'il y  aune correlation positive faible
# Quand on a pas de cancer, le taux normal de psa est de ...
# Quand le volume de cancer augmente de 1%, la quantité de psa augmente de 0,8%


sum = summary(model)

sum$coefficients[c(3)]



#4.4
t.val.calc = sum$coefficients[c(6)] # = sum$coefficients[c(2)] / sum$coefficients[c(4)]

t.val.theo = qt(0.975, df = 78) # niveau d'erreur de 5%. Test de Student bilatéral => 2.5% de part et d'autres

#4.5

# Interprétation du test H0: b1_estimate = 0
                        #H1: b1_estimate != 0

# Comme t.val.calc > t.val.theo, on est dans la zone de rejet ie on rejette H0
# Donc une relaation entre plsa et X.b1_estimate n'est pas significativement nul avec un risque d'erreur de 5%

#4.6

#R2 = 0.6126, plus proche de 0.5(weak) que de 0.9(strong)

# le R2 pourrait etre plus strong avec lbh...

#61% dela variance est expliqué par le modèle de régression
#linéaire, le reste ne peut pas être expliqué