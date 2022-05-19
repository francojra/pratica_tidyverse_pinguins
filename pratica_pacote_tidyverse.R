
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

cbs <- ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))
view(cbs)

# Profundidade do bico

pbs <- ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))
view(pbs)

# Comprimento da nadadeira

cns <- ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))
view(cns)

# Massa corporal

mcs <- ping1 %>%
  group_by(sexo) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))
view(mcs)

# Por espécie ------------------------------------------------------------------------------------------------------------------------------

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### espécie de pinguins no ano de 2009

ping2 <- ping %>%
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira,
         massa_corporal, especie, ano) %>%
  filter(ano == "2009") %>%
  view()
ping2

# Comprimento do bico

cbe <- ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))

view(cbe)

# Profundidade do bico

pbe <- ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))

view(pbe)

# Comprimento da nadadeira

cne <- ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))

view(cne)

# Massa corporal

mce <- ping2 %>%
  group_by(especie) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))

view(mce)

# Por ilha ---------------------------------------------------------------------------------------------------------------------------------

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### ilha no ano de 2009

ping3 <- ping %>%
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira,
         massa_corporal, ilha, ano) %>%
  filter(ano == "2009") %>%
  view()
ping3

# Comprimento do bico

cbi <- ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_comp_bico = mean(comprimento_bico), 
            sd_comp_bico = sd(comprimento_bico),
            medin_comp_bico = median(comprimento_bico))

view(cbi)

# Profundidade do bico

pbi <- ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_prof_bico = mean(profundidade_bico), 
            sd_prof_bico = sd(profundidade_bico),
            medin_prof_bico = median(profundidade_bico))

view(pbi)

# Comprimento da nadadeira

cni <- ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_comp_nad = mean(comprimento_nadadeira), 
            sd_comp_nad = sd(comprimento_nadadeira),
            medin_comp_nad = median(comprimento_nadadeira))

view(cni)

# Massa corporal

mci <- ping3 %>%
  group_by(ilha) %>%
  drop_na() %>%
  summarise(med_massa_cop = mean(massa_corporal), 
            sd_massa_cop = sd(massa_corporal),
            medin_massa_cop = median(massa_corporal))

view(mci)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### sexo de pinguins no ano de 2009

g1s <- ggplot(cbs) +
  geom_col(aes(x = sexo, y = med_comp_bico, fill = sexo)) +
  geom_errorbar(aes(x = sexo, y = med_comp_bico,
                    ymin = med_comp_bico - sd_comp_bico,
                    ymax = med_comp_bico + sd_comp_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_discrete(labels = c("Fêmea", "Macho"),
                   name = "Sexo") +
  labs(y = "Comprimento do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g1s

g2s <- ggplot(pbs) +
  geom_col(aes(x = sexo, y = med_prof_bico, fill = sexo)) +
  geom_errorbar(aes(x = sexo, y = med_prof_bico,
                    ymin = med_prof_bico - sd_prof_bico,
                    ymax = med_prof_bico + sd_prof_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_discrete(labels = c("Fêmea", "Macho"),
                   name = "Sexo") +
  labs(y = "Profundidade do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g2s

g3s <- ggplot(cns) +
  geom_col(aes(x = sexo, y = med_comp_nad, fill = sexo)) +
  geom_errorbar(aes(x = sexo, y = med_comp_nad,
                    ymin = med_comp_nad - sd_comp_nad,
                    ymax = med_comp_nad + sd_comp_nad),
                    width = 0.2) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_discrete(labels = c("Fêmea", "Macho"),
                   name = "Sexo") +
  labs(y = "Comprimento das nadadeiras (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g3s

g4s <- ggplot(mcs) +
  geom_col(aes(x = sexo, y = med_massa_cop, fill = sexo)) +
  geom_errorbar(aes(x = sexo, y = med_massa_cop,
                    ymin = med_massa_cop - sd_massa_cop,
                    ymax = med_massa_cop + sd_massa_cop),
                    width = 0.2) +
  scale_fill_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_discrete(labels = c("Fêmea", "Macho"),
                   name = "Sexo") +
  labs(y = "Massa corporal (g)") +
  theme_minimal() +
  theme(legend.position = "none")
g4s

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### espécie de pinguins no ano de 2009

g1e <- ggplot(cbe) +
  geom_col(aes(x = especie, y = med_comp_bico, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_comp_bico,
                    ymin = med_comp_bico - sd_comp_bico,
                    ymax = med_comp_bico + sd_comp_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Comprimento do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g1e

g2e <- ggplot(pbe) +
  geom_col(aes(x = especie, y = med_prof_bico, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_prof_bico,
                    ymin = med_prof_bico - sd_prof_bico,
                    ymax = med_prof_bico + sd_prof_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Profundidade do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g2e

g3e <- ggplot(cne) +
  geom_col(aes(x = especie, y = med_comp_nad, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_comp_nad,
                    ymin = med_comp_nad - sd_comp_nad,
                    ymax = med_comp_nad + sd_comp_nad),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Comprimento das nadadeiras (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g3e

g4e <- ggplot(mce) +
  geom_col(aes(x = especie, y = med_massa_cop, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_massa_cop,
                    ymin = med_massa_cop - sd_massa_cop,
                    ymax = med_massa_cop + sd_massa_cop),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Massa corporal (g)") +
  theme_minimal() +
  theme(legend.position = "none")
g4e

### Medidas de comprimento da nadadeira, massa corporal e dimensões do bico por
### ilha no ano de 2009

g1i <- ggplot(cbe) +
  geom_col(aes(x = especie, y = med_comp_bico, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_comp_bico,
                    ymin = med_comp_bico - sd_comp_bico,
                    ymax = med_comp_bico + sd_comp_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Comprimento do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g1i

g2i <- ggplot(pbe) +
  geom_col(aes(x = especie, y = med_prof_bico, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_prof_bico,
                    ymin = med_prof_bico - sd_prof_bico,
                    ymax = med_prof_bico + sd_prof_bico),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Profundidade do bico (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g2i

g3i <- ggplot(cne) +
  geom_col(aes(x = especie, y = med_comp_nad, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_comp_nad,
                    ymin = med_comp_nad - sd_comp_nad,
                    ymax = med_comp_nad + sd_comp_nad),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Comprimento das nadadeiras (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
g3i

g4i <- ggplot(mce) +
  geom_col(aes(x = especie, y = med_massa_cop, fill = especie)) +
  geom_errorbar(aes(x = especie, y = med_massa_cop,
                    ymin = med_massa_cop - sd_massa_cop,
                    ymax = med_massa_cop + sd_massa_cop),
                    width = 0.2) +
  scale_fill_manual(values = c("#d53e4f", "#99d594", "#fee08b")) +
  scale_x_discrete(name = "Espécies") +
  labs(y = "Massa corporal (g)") +
  theme_minimal() +
  theme(legend.position = "none")
g4i