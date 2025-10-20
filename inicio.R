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
mutate(dados, IMC = weight/(height*height))

dados <- mutate(dados, IMC = weight/(height*height))

#exemplo operacao
dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack > 50) %>%
  mutate(x = attack+speed) 
  
#gsub modifica strings
x <- c("Thomas", "Fernando", "Thais")

#Se colocar o pipe ele rastreia e substitui. 
x %>%
  gsub("Th", "th", .)

dados %>%
  filter(height > 10) %>%
  select(name, height, weight) %>%
  mutate(IMC = weight/(height*height)) %>%
  ggplot() +
  geom_density(aes(x = IMC))

#comando interessante

glimpse(dados)
summary(dados)
str(dados)

dados %>%
  pull(IMC) #retorna como vetor

dados %>%
  select(IMC) #retorna a coluna

dados %>%
  mutate (media = mean(IMC)) # cria e preenche uma coluna com o mesmo valor

dados %>%
  summarise (media = mean(IMC)) # retorna o resumo do valor

dados %>%
  group_by(type) %>%
  summarise (media = mean(IMC)) 

dados %>%
  group_by(type) %>%
  mutate (media = mean(IMC)) %>%
  View

dados %>%
  group_by(type) %>%
  mutate (media = mean(IMC)) %>%
  filter(IMC > media) %>%
  View

#busca padrões
# aceita Regular Expression (ReGex)
grep("saur|fly", dados$name)
grepl("saur|fly", dados$name)

grep("[Ss]aur", dados$name)

dados %>%
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

###### trabalhando juntando dados

# bind row

df1 <- dados %>%
  select(attack, speed, weight) %>%
  filter(attack > 70)

df2 <- dados %>%
  select(attack, weight, height, hp) %>%
  filter(attack <= 70)

rbind(df1, df2) #juntar linhas - não aceita dimensoes e nomes diferentes

bind_rows(df1, df2) #juntar linhas - completa se não bater

#juntar colunas

df1<- dados %>% head(100)
df2<- dados %>% tail(100)
cbind(df1, df2) %>% names
bind_cols(df1, df2)

#############

df_resumo<-dados %>%
  group_by(type) %>%
  summarise(media = mean(IMC), desvio = sd(IMC))

# Fazendo join
# left, right, full, inner

left_join(dados, df_resumo, by = c("type")) %>% View
df_resumo_mis<-df_resumo %>% filter(type != "grass")

left_join(dados, df_resumo_mis, by = c("type")) 
right_join(dados, df_resumo_mis, by = c("type"))

full_join(dados, df_resumo_mis, by = c("type"))
inner_join(dados, df_resumo_mis, by = c("type"))
