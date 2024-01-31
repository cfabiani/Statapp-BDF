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

press_release <- read_excel(here("Dataset_EA-MPD.xlsx"), sheet = "Press Release Window")
press_conference <- read_excel(here("Dataset_EA-MPD.xlsx"), sheet = "Press Conference Window")
monetary_event <- read_excel(here("Dataset_EA-MPD.xlsx"), sheet = "Monetary Event Window")


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

#------ Tentative de rotation --------
head(var$coord, 4)
F1 <- var$coord[, 1]
F2 <- var$coord[, 2]
F <- cbind(F1, F2)

# On veut récupérer le vecteur Z tel que Z=FU où U est la matrice de rotation adaptée
# Pour construire U on se réfère à "The impact of ECB monetary policy decisions and
# communication on the yield curve"

gamma1=F[2,1]
gamma2=F[2,2]
alpha1=gamma1/(gamma1+gamma2)
alpha2=gamma1/(gamma1+gamma2)
beta1=-alpha2*var(F2)/(alpha1*var(F1)-alpha2*var(F2))
beta2=alpha1*var(F1)/(alpha1*var(F1)-alpha2*var(F2))

# ATTENTION : il faut avant standardiser les colonnes de U
U=cbind(c(alpha1,alpha2),c(beta1,beta2))
z1=alpha1*F1+alpha2*F2
z2=beta1*F1+beta2*F2

Z=cbind(z1,z2)

# Il s'agit enfin de standardiser Z. Pour ce, les auteurs de l'article effectuent deux régressions



