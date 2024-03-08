library("readxl")
library("here")
library("FactoMineR")
library("factoextra")
library("lubridate")
library("dplyr")
library("tidyverse")
library("readr")


anacredit <- read_csv("D:/final_df_anonymous.csv")

#unique(anacredit$TYP_INSTRMNT)
#[1] 1004   20 1002 1001   71   80   51 1000 1003
#Les types d'instruments de la base

#Mis en format des Inception date
anacredit <- anacredit %>%
  mutate(DT_INCPTN = as.Date(as.character(DT_INCPTN), format = "%Y%m%d"))


#-----Première représentation des taux par jour-----
rate_by_day <- anacredit %>%
  group_by(DT_INCPTN) %>%
  summarise(mean_rate = mean(ANNLSD_AGRD_RT, na.rm = TRUE))

ggplot(data = rate_by_day,  aes(x = DT_INCPTN, y = mean_rate)) +
  geom_line() +
  theme_bw()


#-----Tentative de débruitage-----
#Vérifier ces choix 
anacredit_reduce <- anacredit %>%
  mutate(percentage_protection = ifelse(is.na(percentage_protection), 0, percentage_protection)) %>%
  filter(percentage_protection <= 1 & percentage_protection != Inf) %>%
  mutate(LTV_INSTRMNT = ifelse(is.na(LTV_INSTRMNT), 0, LTV_INSTRMNT)) %>%
  filter(!is.na(TYP_INSTRMNT)) %>%
  filter(!is.na(ANNLSD_AGRD_RT))

#On regrésse
reg_test <- lm(ANNLSD_AGRD_RT ~ LTV_INSTRMNT + TYP_INSTRMNT +  
                 percentage_protection + OTSTNDNG_NMNL_AMNT_CV + ORGNL_MTRTY
                 , data = anacredit_reduce)
summary(reg_test)
#Mais on trouve un R² très faible ...

#On récupère les résidus
anacredit_reduce$ANNLSD_AGRD_RT_clean = reg_test$residuals

#Représentation graphique
rate_by_day_clean <- anacredit_reduce %>%
  group_by(DT_INCPTN) %>%
  summarise(mean_rate = mean(ANNLSD_AGRD_RT_clean, na.rm = TRUE))


ggplot(data = rate_by_day_clean,  aes(x = DT_INCPTN, y = mean_rate)) +
  geom_line() +
  theme_bw()
#Pas d'amélioration particulière.
#On peut tenter d'ajouter à la regression l'oustanding amount et la maturité mais il 
#faut clean ces variables d'abord



#-----Ajout des dates--------

date_event = as.Date(c("2022-02-03", "2022-03-10", "2022-04-14", "2022-06-09",
               "2022-07-21", "2022-09-08", "2022-10-27", "2022-12-15",
               "2023-02-02", "2023-03-16", "2023-05-04", "2023-06-15"), format = "%Y%m%d") 



anacredit <- anacredit %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-02-03",
                              floor(difftime(DT_INCPTN,"2022-02-03",  units = "days")),
                                             -1
                              )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-02-03",
                              mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-02-02"], na.rm = TRUE),
                              0
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-03-10",
                              floor(difftime(DT_INCPTN,"2022-03-10",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-03-10",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-03-09"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-04-14",
                              floor(difftime(DT_INCPTN,"2022-04-14",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-04-14",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-04-13"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-06-09",
                              floor(difftime(DT_INCPTN,"2022-06-09",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-06-09",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-06-08"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-07-21",
                              floor(difftime(DT_INCPTN,"2022-07-21",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-07-21",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-07-20"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-09-08",
                              floor(difftime(DT_INCPTN,"2022-09-08",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-09-08",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-09-07"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-10-27",
                              floor(difftime(DT_INCPTN,"2022-10-27",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-10-27",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-10-26"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2022-12-15",
                              floor(difftime(DT_INCPTN,"2022-12-15",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2022-12-15",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2022-12-14"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2023-02-02",
                              floor(difftime(DT_INCPTN,"2023-02-02",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2023-02-02",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2023-02-01"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2023-03-16",
                              floor(difftime(DT_INCPTN,"2023-03-16",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2023-03-16",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2023-03-15"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2023-05-04",
                              floor(difftime(DT_INCPTN,"2023-05-04",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2023-05-04",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2023-05-03"], na.rm = TRUE),
                             diff_rate
  )) %>%
  mutate( diff_event = ifelse(DT_INCPTN >= "2023-06-15",
                              floor(difftime(DT_INCPTN,"2023-06-15",  units = "days")),
                              diff_event
  )) %>%
  mutate( diff_rate = ifelse(DT_INCPTN >= "2023-06-15",
                             mean(ANNLSD_AGRD_RT[DT_INCPTN == "2023-06-14"], na.rm = TRUE),
                             diff_rate
  )) 
  

#Vérification par représentation graphique

diff_date_df <- anacredit %>%
  group_by(DT_INCPTN) %>%
  summarise(diff = mean(diff_event, na.rm = TRUE))

ggplot(data = diff_date_df,  aes(x = DT_INCPTN, y = diff)) +
  geom_line() +
  theme_bw()


diff_rate_df <- anacredit %>%
  group_by(DT_INCPTN) %>%
  summarise(diff = mean(diff_rate, na.rm = TRUE))

#Graph inutile
ggplot(data = diff_rate_df,  aes(x = DT_INCPTN, y = diff)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-03")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-10")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-04-14")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-06-09")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-07-21")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-08")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-27")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-15")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-16")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-04")), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2023-06-15")), color = "red", linetype = "dashed") +
  theme_bw()

diff_rate_diff_date <- anacredit %>%
  group_by(diff_event) %>%
  summarise(diff = mean(diff_rate, na.rm = TRUE))
diff_rate_diff_date <- diff_rate_diff_date %>%
  filter(diff_event <= 30)

ggplot(data = diff_rate_diff_date,  aes(x = diff_event, y = diff)) +
  geom_line() +
  theme_bw()
#--------
#On garde uniquement les créances commerciales
anacredit_cc <- anacredit %>%
  filter(TYP_INSTRMNT == 71)
