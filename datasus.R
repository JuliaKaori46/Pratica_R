################
dengue_sp <- read.csv2("dengue_sp.csv")
library(tidyverse)
dengue_sp <- dengue_sp %>%
  rename(municipio = 1)

dengue_sp <- dengue_sp %>%
  filter(municipio != "Total",
         municipio != "&",
         !grepl("IGNORADO", municipio)) # ! significa não

## dados do IBGE

dados_ibge <- read.csv2("tabela4709.csv", skip = 3)

dados_ibge <- dados_ibge %>%
  rename(Cod = 1)

## manippulando dados

# separar o código do município

dengue_sp <- dengue_sp %>%
  mutate(codigo = str_extract(municipio, "\\d{6}"))

dados_ibge <- dados_ibge %>%
  mutate(
    Cod2 = str_extract(Cod, "^\\d{6}")
  )

# esse serve tbm
dados_ibge %>%
  mutate(
    Cod2 = str_remove(Cod, "^\\d$")
  )

## manipulando dengue
glimpse(dengue_sp)

dengue_sp <- dengue_sp %>%
  mutate(across(starts_with("X"), as.integer)) %>% # transformei chr em inteiro. 
  replace(is.na(.), 0) # coloca zero no lugar

dengue_sp <- dengue_sp %>%
  select(-Total) %>%
  pivot_longer(2:12, names_to = "Ano", values_to = "Casos")

dengue_sp <- dengue_sp %>%
  mutate(Ano = str_remove_all(Ano, "\\D")) %>%
  mutate(Ano = as.integer(Ano))

df_final <- dados_ibge %>%
  select(Cod = Cod2, Inhab = X2022) %>%
  right_join(dengue_sp, by = c("Cod" = "codigo"))

df_final <- df_final %>%
  mutate( 
    incidencia = Casos/Inhab*100000)

ggplot(df_final) +
  geom_boxplot(aes(x = factor(Ano,), y = Casos)) 

ggplot(df_final) +
  geom_boxplot(aes(x = factor(Ano), y = incidencia))

##lendo DBC
library(tidyverse)
#install.packages("read.dbc")

devtools::install_github("danicat/read.dbc")


dados <- read.dbc::read.dbc("DENGBR24.dbc")

nrow(dados)

names(dados)


glimpse(dados)

dados_sp <- dados %>%
  filter(grepl("^35", ID_MN_RESI))

nrow(dados_sp)

rm(dados)

glimpse(dados_sp)

write.csv(dados_sp, "dados_dengue_sp.csv")

######trabalhar com conjunto de dados muito grande

library(tidyverse)
library("duckdb")

con <- dbConnect(duckdb::duckdb(), dbdir = "meu_banco.duckdb")


##### dados de mortalidade

library(tidyverse)
#criei uma função 
roda_arquivo <- function(arquivo){
  
dadosimdo <- read.dbc::read.dbc(arquivo)

dados <- dadosimdo %>%
  select(DTOBITO, CAUSABAS_O)%>%
  mutate(
    dataobito = as.Date(DTOBITO, "%d%m%Y")
  )
#ctrl+shift+C seleciona vários e #
# as.Date("2015-10-12")-as.Date("2015-10-10")
# 
# x <- c(as.Date("2015-10-12"), as.Date("2015-07-12"), as.Date("2015-05-12"))
# 
# floor(as.integer(x-as.Date("2015-01-01"))/7+1)

dados <- dados %>%
  mutate(
    semana = floor(as.integer(dataobito-as.Date("2015-01-01"))/7+1),
    diasemana = as.Date("2015-01-01")+7*(semana-1),
    estado = str_remove(arquivo, ".*DO"), #. (ponto apenas) significa qualquer caracter
    estado = str_remove(estado, "\\d{4}\\.dbc") #\\ ponto
  ) 
return(dados)
}

roda_arquivo("DATASUS/DOAC2015.dbc")

arquivos <- list.files("DATASUS/", full.names = T)

resultados <- map(arquivos, roda_arquivo)

resultados[[15]]

dados <- Reduce(rbind, resultados)

nrow(dados)
head(dados)

dados %>%
  group_by(diasemana, estado) %>%
  summarise (
    n = n()
  ) %>%
  ungroup() %>%
  ggplot(aes(x = diasemana, y = n, color = estado)) +
  geom_line()+
  theme_bw()
#02/03/2015 <- %d%m%Y

#lubridate

# rastreando neoplasias CID

dados_cancer <- dadossimdo %>%
  filter(
    grepl("^C|^D[1234]", CAUSABAS)
  )
nrow(dados_cancer)