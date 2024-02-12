library("readxl")
library("here")
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("lubridate")
library("dplyr")
library("tidyverse")
#Loading the datasets


#Il faut peut-être changer les chemins ...

press_release <- read_excel(here("Statapp-BDF/Dataset_EA-MPD.xlsx"), sheet = "Press Release Window")
press_conference <- read_excel(here("Statapp-BDF/Dataset_EA-MPD.xlsx"), sheet = "Press Conference Window")
monetary_event <- read_excel(here("Statapp-BDF/Dataset_EA-MPD.xlsx"), sheet = "Monetary Event Window")


#-------------------
#Je fais l'ACP sur la press_conference juste pour essayer
#-------------------

#Création de variables de dates utilisables
press_conference["date"] <- ymd(press_conference$date)
press_conference["year"] <- year(press_conference$date)
press_conference["month"] <- month(press_conference$date)
press_conference["day"] <- day(press_conference$date)

#Je garde que les valeurs après 2009 car il y a des NA avant
#Il faudra rerefléchir à ce filtre
#Vérifier si on a pas d'autres missings values
press_conference_2009 <- press_conference %>%
  filter(year >= 2009)



#On garde tout sauf les dates
press_conference_work <- press_conference_2009[2:46]

#------- Avec Facto extra, première visualisation --------#
#On récupère les valeurs propres et on plot pour avoir un aperçu
PC.pca <- PCA(press_conference_work, graph = FALSE) 
#print(PC.pca)
eig.val <- get_eigenvalue(PC.pca)
#print(eig.val)
fviz_eig(PC.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(PC.pca)
fviz_pca_var(PC.pca, col.var = "black")

#------ Récupération de la surprise ------

#On utilise la méthode de https://www.ijcb.org/journal/ijcb05q2a2.pdf
#Voir partie 2, surtout 2.2 et 2.3 + annexe
#L'idée est de faire l'ACP, de récupérer les deux composantes principales F1 et F2
#Et de faire une "rotation" pour récupérer Z1 et Z2 où Z1 représente la surprise

#On utilise le package psych pour la rotation
install.packages("psych")
library("psych")

#On rescale (pour avoir mean = 0, sd = 1)
press_conference_work_scaled <- scale(press_conference_work)
#On fait l'ACP
PC.pca_2 <- prcomp(press_conference_work_scaled)
#On récupère les deux vecteurs F1 et F2
F1 <- PC.pca_2$x[, 1]
F2 <- PC.pca_2$x[, 2]


#On rescale F1 et F2
F1_scaled <- F1/sd(F1)
F2_scaled <- F2/sd(F2)

#On les fusionne
F <- cbind(F1, F2)

#On fait la rotation
rotation <- varimax(F)

# On récupère  Z1 et Z2
Z1 <- rotation$loadings[, 1]
Z2 <- rotation$loadings[, 2]

Z1_scaled <- Z1/sd(Z1)
Z2_scaled <- Z2/sd(Z2)

#On obtient Z1 un vecteur de longueur 20 qui correspond à la surprise aux 20 dates.
#Verifier si cette méthode correspond bien à celle de l'article


#-----Tentative autre méthode ------

#Visiblement dans psych il existe une commande qui fait tout
acp.varimax <- principal(r=press_conference_work, nfactors=2, rotate="varimax")

Z1_bis <- acp.varimax$loadings[,1]
Z2_bis <- acp.varimax$loadings[,2]

#Mais le résultat est incohérent car on a des vecteurs de taille 46 
#(le nombre de colonnes et les colonnes correspondent aux instruments OIS_3M OIS_1Y etc.)
#Or la surprise doit dépendre de la date de la conférence pas de l'instrument je pense


#Test