library("readxl")
library("here")
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
monetary_event["date"] <- ymd(monetary_event$date)
monetary_event["year"] <- year(monetary_event$date)
monetary_event["month"] <- month(monetary_event$date)
monetary_event["day"] <- day(monetary_event$date)

#Je garde que les valeurs après 2009 car il y a des NA avant
#Il faudra rerefléchir à ce filtre
#Vérifier si on a pas d'autres missings values
monetary_event_2012 <- monetary_event %>%
  filter(year >= 2012)



#On garde tout sauf les dates
monetary_event_2012_viz <- pivot_longer(monetary_event[1:15], cols = c("OIS_1Y","OIS_2Y"), names_to = 'instruments')

monetary_event_work <- monetary_event_2012[3:15]


#------- Avec Facto extra, première visualisation --------#
#On récupère les valeurs propres et on plot pour avoir un aperçu
PC.pca <- PCA(monetary_event_work, graph = FALSE) 
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
monetary_event_work <- scale(monetary_event_work)
#On fait l'ACP
PC.pca_2 <- prcomp(monetary_event_work_scaled)
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


#--------------

F1 <- PC.pca_2$x[, 1]
F2 <- PC.pca_2$x[, 2]

loadings_matrix  <- PC.pca_2$rotation
mp1_loadings <- loadings_matrix[,c("PC1")]

gamma1 <- mp1_loadings[1] 
gamma2 <- mp1_loadings[2]

F1_scaled <- F1/sd(F1)
F2_scaled <- F2/sd(F2)

alpha1=gamma1/(gamma1+gamma2)
alpha2=gamma2/(gamma1+gamma2)
beta1=-alpha2*var(F2_scaled)/(alpha1*var(F1_scaled)-alpha2*var(F2_scaled))
beta2=alpha1*var(F1_scaled)/(alpha1*var(F1_scaled)-alpha2*var(F2_scaled))

z1=alpha1*F1_scaled+alpha2*F2_scaled
z2=beta1*F1_scaled+beta2*F2_scaled

#-----Tentative autre méthode ------

#------ Tentative de rotation --------#
head(var$coord, 4)
loadings_matrix=var$contrib[,1:2]
F1 <- var$coord[, 1]
F2 <- var$coord[, 2]
F <- cbind(F1, F2)

# On veut récupérer le vecteur Z tel que Z=FU où U est la matrice de rotation adaptée
# Pour construire U on se réfère à "The impact of ECB monetary policy decisions and
# communication on the yield curve"

gamma1=loadings_matrix[2,1]
gamma2=loadings_matrix[2,2]
alpha1=gamma1/(gamma1+gamma2)
alpha2=gamma2/(gamma1+gamma2)
beta1=-alpha2*var(F2)/(alpha1*var(F1)-alpha2*var(F2))
beta2=alpha1*var(F1)/(alpha1*var(F1)-alpha2*var(F2))

# ATTENTION : il faut avant standardiser les colonnes de U
U=cbind(c(alpha1,alpha2),c(beta1,beta2))


z1=alpha1*F1+alpha2*F2
z2=beta1*F1+beta2*F2

Z=cbind(z1,z2)

# Il s'agit enfin de standardiser Z. Pour ce, les auteurs de l'article effectuent deux régressions

#------ Matrice de rotation d'après mes calculs à la main ------#
gamma=sqrt(gamma1^2+gamma2^2)
a=gamma1/gamma
b=gamma2/gamma
c=-b
d=a
U_main=cbind(c(a,b),c(c,d))
z1_main=a*F1+b*F2
z2_main=c*F1+d*F2
Z_main=cbind(date,z1_main,z2_main) 

df_final=merge(monetary_event_work, Z_main, by="date")

# ------ Scaling ------ #
# Selon l'article "Measuring Euro Area Monetary Policy", on pondère les vecteurs trouvés, pour que
# une augmentation de un point de z1 soit associée à une augmentation de 1% du 1 month OIS
# une augmentation de un point de z2 soit associée à une augmentation de 1% du 2 year OIS.

#scaling_vector1= press_conference_2009$OIS_1M 
#scaling_vector2=press_conference_2009$OIS_2Y 

mod_lin1=lm(OIS_1M~z1_main, data=df_final)

summary(mod_lin1)


