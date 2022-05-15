
# Prática com pacote tidyverse -------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 14/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ---------------------------------------------------------------------------------------------------------------------------

library(dados)
library(tidyverse)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

ping <- dados::pinguins
View(ping)

# Análises descritivas ---------------------------------------------------------------------------------------------------------------------

# Por sexo ---------------------------------------------------------------------------------------------------------------------------------

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### sexo de pinguins no ano de 2009

names(ping) # Verificar nomes das variáveis
ping$ano # Verificar quais anos incluídos nos dados

ping1 <- ping %>%
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira,
         massa_corporal, sexo, ano) %>%
  filter(ano == "2009") %>%
  view()
ping1

# Comprimento do bico

ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))

# Profundidade do bico

ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))

# Comprimento da nadadeira

ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))

# Massa corporal

ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))


### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### espécie de pinguins no ano de 2009

# Por espécie ------------------------------------------------------------------------------------------------------------------------------

ping2 <- ping %>%
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira,
         massa_corporal, especie, ano) %>%
  filter(ano == "2009") %>%
  view()
ping2

# Comprimento do bico

ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))

# Profundidade do bico

ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))

# Comprimento da nadadeira

ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))

# Massa corporal

ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### ilha no ano de 2009

# Por ilha ---------------------------------------------------------------------------------------------------------------------------------

ping3 <- ping %>%
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira,
         massa_corporal, ilha, ano) %>%
  filter(ano == "2009") %>%
  view()
ping3

# Comprimento do bico

ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))

# Profundidade do bico

ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))

# Comprimento da nadadeira

ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))

# Massa corporal

ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))
