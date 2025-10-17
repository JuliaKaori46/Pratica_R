getwd()
dados <- read.csv("D:/OneDrive/1. Pós-graduação/2. Doutorado/9. Disciplinas/4. Ferramentas computacionais de modelagem/Pokemon_full.csv")
head(dados) #olha as primeiras linhas
tail(dados) #olha as últimas linhas

#pacote para manipulacao
library(tidyverse)

#seleciona nomes da primeira linha 
names(dados)

#seleciona coluna
select(dados, name, hp, speed, attack)

#filtragem
filter(dados, attack > 50)

#operacoes
mutate(dados, x = attack+speed) #cria nova variável
mutate(dados, attack = attack/2) #modifica variavel

#exemplo operacao
dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack > 50) %>%
  mutate(x = attack+speed)
  




