
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

