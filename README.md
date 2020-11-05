# teste1
library(plyr)
library(dplyr)
base_5ano <- read.csv2("base final_5ano.csv")
base_teste <- base_5ano %>% filter(!is.na(PROFICIENCIA_LP) & !is.na(PROFICIENCIA_MT)) %>% 
                           select("PROFICIENCIA_LP", "PROFICIENCIA_MT", "ID_LOCALIZACAO", "q1", "q2",                                     "q17", "q20", "distancia")
base_teste <- round(base_teste$PROFICIENCIA_LP, 4)
base_teste <- round(base_teste$PROFICIENCIA_MT, 4)
#definir um nível de escolaridade para os pais
#se completou a 8 série será considerado educado
base_teste$q17 <- as.numeric(mapvalues(as.character(base_teste$q17), from = c("A","B","C","D"), to = c(1,2,3,4)))
base_teste <- mutate(base_teste, educ_mae = ifelse(q17 >= 3, 1,0))
base_teste$q20 <- as.numeric(mapvalues(as.character(base_teste$q20), from = c("A","B","C","D"), to = c(1,2,3,4)))
base_teste <- mutate(base_teste, educ_pai = ifelse(q17 >= 3, 1,0))
#fazer uma dammy para as escolas que estão até 1000m de distância de áreas de risco
base_teste <- mutate(base_teste, dist_1000 = ifelse(distancia <= 1000, 1, 0))
#fazer uma dammy para as escolas que estão até 5000m de distância de áreas de risco
base_teste <- mutate(base_teste, dist_5000 = ifelse(distancia <= 5000, 1, 0))
#modelo com controle para sexo, raça, educação dos pais e localização da escola
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_mae + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_pai + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_mae + educ_pai + educ_mae.educ_pai + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_mae + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_pai + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_mae + educ_pai + educ_mae.educ_pai + ID_LOCALIZACAO + dist_1000, base_teste))
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_mae + ID_LOCALIZACAO + dist_5000, base_teste))
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_pai + ID_LOCALIZACAO + dist_5000, base_teste))
summary(lm(PROFICIENCIA_LP ~ q1 + q2 + educ_mae + educ_pai + educ_mae.educ_pai + ID_LOCALIZACAO + dist_5000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_mae + ID_LOCALIZACAO + dist_5000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_pai + ID_LOCALIZACAO + dist_5000, base_teste))
summary(lm(PROFICIENCIA_MT ~ q1 + q2 + educ_mae + educ_pai + educ_mae.educ_pai + ID_LOCALIZACAO + dist_5000, base_teste))
