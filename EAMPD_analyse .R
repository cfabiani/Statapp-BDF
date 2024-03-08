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
  filter(year >= 2016)


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

#-----------------
# Contributions of variables to PC1
fviz_contrib(PC.pca, choice = "var", axes = 1, top = 15)
# Contributions of variables to PC2
fviz_contrib(PC.pca, choice = "var", axes = 2, top = 15)

#------ Récupération de la surprise ------

#On utilise la méthode de https://www.ijcb.org/journal/ijcb05q2a2.pdf
#Voir partie 2, surtout 2.2 et 2.3 + annexe
#L'idée est de faire l'ACP, de récupérer les deux composantes principales F1 et F2
#Et de faire une "rotation" pour récupérer Z1 et Z2 où Z1 représente la surprise

#On utilise le package psych pour la rotation
library("psych")

#On rescale (pour avoir mean = 0, sd = 1)
monetary_event_work_scaled <- scale(monetary_event_work)
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


#-------------- New

F1 <- PC.pca_2$x[, 1]
F2 <- PC.pca_2$x[, 2]
F1_scaled <- F1/sd(F1)
F2_scaled <- F2/sd(F2)



press_release["date"] <- ymd(press_release$date)
press_release["year"] <- year(press_release$date)
press_release["month"] <- month(press_release$date)
press_release["day"] <- day(press_release$date)

#Je garde que les valeurs après 2009 car il y a des NA avant
#Il faudra rerefléchir à ce filtre
#Vérifier si on a pas d'autres missings values
press_release <- press_release %>%
  filter(year >= 2016)

press_release$F1 <- F1_scaled
press_release$F2 <- F2_scaled

F_bis <- cbind(F1_scaled,F2_scaled)


model <- lm(OIS_1M ~ F1, data = press_release)
summary(model)



#-------------------------


monetary_event_long <- monetary_event[1:15] %>% 
  pivot_longer(cols = -date, names_to = "instrument", values_to = "interest")


 

table_graph <- monetary_event_long %>%
  filter(date %in% c( "2022-10-27","2022-12-15","2023-03-16"))

table_graph$date <- as.factor(table_graph$date)


table_graph$instrument <- factor(table_graph$instrument, levels = colnames(monetary_event)[-1])

ggplot(table_graph, aes(x = instrument, y = interest, group = date, color = date)) +
  geom_line() +
  scale_color_manual(values = c( "2022-10-27" = "green","2022-12-15" = "red", "2023-03-16" = "blue"),
                     name = "Date") +
  labs(x = "Maturité de l'OIS", y = "Variation du taux de l'instrument") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


monetary_event_2020 <- monetary_event %>%
  filter(year >=2020)

ggplot(monetary_event_2020, aes(x = date )) +
  geom_line(aes(y = OIS_1M, color = "OIS_1M"))+
  geom_line(aes(y = OIS_1Y, color = "OIS_1Y"))+
  geom_line(aes(y = OIS_10Y, color = "OIS_10Y"))+
  labs(x = "Date des annonces monétaires", y = "Variation du taux des instruments") +
  scale_color_manual(values = c(OIS_1M = "coral4", OIS_1Y = "dimgray", OIS_10Y = "greenyellow"), 
                     labels = c("OIS_1M", "OIS_1Y", "OIS_10Y"),
                     name = "Instrument") + 
  theme_bw()


#-----------------

#Bon fit de press release

vars <- colnames(press_release)[2:15]  

get_regression_results <- function(dependent_var, independent_var) {
  model <- lm(formula(paste(dependent_var, "~", independent_var)), data = press_release)
  coef <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  r_squared <- summary(model)$r.squared
  if (p_value <= 0.01) {
    significance <- "***"
  } else if (p_value <= 0.05) {
    significance <- "**"
  } else if (p_value <= 0.10) {
    significance <- "*"
  } else {
    significance <- ""
  }
  return(c(Coefficient = coef, `P-value` = p_value, `R-squared` = r_squared, Significance = significance))
}

results_F1 <- sapply(vars, function(var) get_regression_results(var, "F1"))
results_F2 <- sapply(vars, function(var) get_regression_results(var, "F2"))
results_table_F1 <- t(as.data.frame(results_F1))
results_table_F2 <- t(as.data.frame(results_F2))
print(results_table_F1)
print(results_table_F2)

#Bon fit de press conference

press_conference["date"] <- ymd(press_conference$date)
press_conference["year"] <- year(press_conference$date)
press_conference["month"] <- month(press_conference$date)
press_conference["day"] <- day(press_conference$date)

press_conference <- press_conference %>%
  filter(year >= 2016)
press_conference$F1 <- F1_scaled
press_conference$F2 <- F2_scaled

vars_pc <- colnames(press_conference)[2:15]  

get_regression_results_pc <- function(dependent_var, independent_var) {
  model <- lm(formula(paste(dependent_var, "~", independent_var)), data = press_conference)
  coef <- coef(model)[2] 
  p_value <- summary(model)$coefficients[2, 4]
  r_squared <- summary(model)$r.squared
  if (p_value <= 0.01) {
    significance <- "***"
  } else if (p_value <= 0.05) {
    significance <- "**"
  } else if (p_value <= 0.10) {
    significance <- "*"
  } else {
    significance <- ""
  }
  return(c(Coefficient = coef, `P-value` = p_value, `R-squared` = r_squared, Significance = significance))
}

results_F1_pc <- sapply(vars_pc, function(var) get_regression_results_pc(var, "F1"))
results_F2_pc <- sapply(vars_pc, function(var) get_regression_results_pc(var, "F2"))
results_table_F1_pc <- t(as.data.frame(results_F1_pc))
results_table_F2_pc <- t(as.data.frame(results_F2_pc))
print(results_table_F1_pc)
print(results_table_F2_pc)
library(xtable)
latex_table <- xtable(results_table_F2_pc)
print(latex_table, include.rownames = FALSE)


