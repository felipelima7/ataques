#installing packages and loading data


install.packages("brazilmaps")

install.packages("readxl")
library(readxl)

setwd("C:/Users/Felipe/Downloads/Curso/Ataques")
library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
library(leaflet)
library(DT)
library(tigris)
library(readr)
library(brazilmaps)
library(ggplot2)


agr_fis <- read_csv("ABRAJI.AgrFis.csv", skip = 1)
agr_virt <- read_csv("ABRAJI.AgrVirt.csv", skip = 1)
br_estados <- read_excel("BRestados.xls", sheet=1)

#cleaning data

colnames(agr_virt)[8] <- "Agressor"
colnames(agr_virt)[10] <- "Data"

#merging both virtual and real life datasets

agr_jor <- full_join(agr_fis, agr_virt)

#loading Brazilian shapefiles

brmap <- get_brmap(geo = "State",
          geo.filter = NULL,
          class = c("sf", "SpatialPolygonsDataFrame", "data.frame"))

#cleaning data a bit more

colnames(br_estados)[1] <- "nome"

br_dados <- full_join(br_estados, brmap, by = "nome")

agressoes <- agr_jor %>%
  group_by(UF) %>%
  summarize(total=n())

#fazendo uma tabela com agress�es por estados

br_merge <- full_join(agressoes, br_dados, by = "UF")

#mapa de agress�es por estados

ggplot(br_merge) +
  geom_sf(aes(fill=total)) +
  scale_fill_distiller(direction=1, name="N�mero de ataques", na.value="gray") +
  labs(title="Ataques a jornalistas no per�odo pr�-eleitoral",
       subtitle = "Por Estado",
       caption="Fonte: ABRAJI") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

#demais gr�ficos, and cleaning columns

colnames(agr_jor)[5] <- "veiculo"
colnames(agr_jor)[6] <- "metodo"
colnames(agr_jor)[7] <- "motivo"

#por ve�culo

veiculo <- agr_jor %>%
  group_by(veiculo) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

library(forcats)
library(tidyr)

veiculo <- veiculo %>% drop_na()

ggplot(veiculo,
       aes(x=total, y=fct_reorder(veiculo, total, desc=TRUE))) +
  geom_segment(
    aes(x = 0,
        y=fct_reorder(veiculo, total, desc=TRUE),
        xend = total,
        yend = fct_reorder(veiculo, total, desc=TRUE)),
    color = "gray50") +
  geom_point() +
  labs(x="N�mero de agress�es", y="Ve�culo no qual trabalham", 
       title = "Jornalistas agredidos, por ve�culo",
       subtitle = "Houve tr�s casos envolvendo profissionais sem empresa definida",
       caption = "Fonte: ABRAJI") +
  theme_minimal()

# Motivo

motivo <- agr_jor %>%
  group_by(motivo) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

library(DT)

datatable(motivo, options = list(
  dom = 'Bfrtip', columnDefs = list(list(className = 'dt-center')),
    pageLength = 28))

# linha do tempo

library(lubridate)

agr_jor$quando <- dmy(agr_jor$Data)

quando <- agr_jor %>%
  group_by(quando) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

#criando fun��o com sequ�ncia de dias para colocar dados naqueles em que n�o houve agress�es

start_date <- mdy("01-01-18")
end_date <- mdy("10-03-18")
#calculate how many days in this time interval
n_days <- interval(start_date,end_date)/days(1)
date <- start_date + days(0:n_days)
date <- format(start_date + days(0:n_days), format="%m-%d-%y")

df <- data.frame(date)
colnames(df)[1] <- "quando"
df$quando <- mdy(df$quando)

#juntando a fun��o que eu criei com os dados da Abraji

quando_all <- full_join(quando, df, by = "quando")

#substituindo NA por zero

quando_all[is.na(quando_all)] <- 0

ggplot(quando_all, aes(x=quando, y=total)) +
  geom_line() +
  theme_minimal() +
  labs(x="", y="n�mero de ocorr�ncias", 
       title="Agress�es no per�odo pr�-eleitoral a jornalistas",
       caption="Fonte: Abraji")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = as.Date("2018-04-14"), y = 29, label = "tiros contra �nibus da Caravana Lula", size = 4) +
  annotate("text", x = as.Date("2018-05-28"), y = 26, label = "parceria Facebook, ag�ncias de checagem", size = 4)

# metodo

Metodo <- agr_jor %>%
  group_by(metodo) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

# tentativa de corrigir o problema na separa��o de coluna

library(stringr)

#retirando as aspas da tabelas, porque vou precisar de aspas depois

Metodo$metodo <- str_replace_all(string = Metodo$metodo, pattern = '"', replacement = "")

# os dois casos abaixo davam problema ao separar por v�rgula ou por "e"

Metodo <- Metodo %>% 
  mutate(metodo=case_when(
  metodo=="Amea�ado f�sica e virtualmente" ~ paste0("Amea�a"),
  metodo=="Ofensas" ~ paste0("Ofensa"),
  metodo=="Atribui��o de informa��o falsa (edi��o de �udio de podcast, fazendo parecer que ele disse que a Folha pretendia prejudicar Bolsonaro fingindo fazer jornalismo)" ~ paste0("Atribui��o de informa��o falsa"),
  TRUE ~ metodo))

# separando por v�rgula, ; ponto e "e"

Metodo2 <- Metodo %>%
  separate(metodo, c("metodo", "metodo1"), sep = ",", extra = "merge", fill = "right")
  
Metodo3 <- Metodo2 %>%
  separate(metodo1, c("metodo1", "metodo2"), sep = ",", extra = "merge", fill = "right")

Metodo4 <- Metodo3 %>%
  separate(metodo, c("metodo", "metodo3"), sep = ";", extra = "merge", fill = "right")

Metodo5 <- Metodo4 %>%
  separate(metodo3, c("metodo3", "metodo4"), sep = ";", extra = "merge", fill = "right")

# este abaixo diz que vai separar sempre que houver um ponto precedido de letra min�scula

Metodo6 <- Metodo5 %>% 
  separate(metodo, 
           into = c("metodo","metodo5"), 
           sep = "(?<=[a-z])\\.")

Metodo7 <- Metodo6 %>%
  separate(metodo, c("metodo", "metodo6"), sep = " e ", extra = "merge", fill = "right")

#collpse de colunas, fundindo a 1 a 7 e mantendo o total

library(tidyr)

Metodo_final <- gather(Metodo7, "code", "value", 1:7)
Metodo_final <- Metodo_final %>% drop_na()

Metodo_final <- select(Metodo_final,
                     caso=value,
                     total)

#colocando a tabela toda em min�scula

Metodo_final$caso <- tolower(Metodo_final$caso)

#um dos muitos casos de limpeza que precisei fazer

Metodo_final2 <- Metodo_final %>%
  mutate(caso=case_when(
    caso=="agredida verbalmente" ~ paste0("agress�o verbal"),
    caso=="agredido verbalmente" ~ paste0("agress�o verbal"),
    TRUE ~ caso))

#remover espa�o na frente

Metodo_final2$caso <- trimws(Metodo_final2$caso, "l")

#corrigir xingada e ofensas

Metodo_final2 <- Metodo_final2 %>%
  mutate(caso=case_when(
    caso=="xingada" ~ paste0("xingamento"),
    caso=="ofensas" ~ paste0("ofensa"),
    caso=="agredida com um tapa" ~ paste0("atingido com um tapa"),
    caso=="atingido com ovos" ~ paste0("agress�o com ovos"),
    caso=="atingida com ovos" ~ paste0("agress�o com ovos"),
    TRUE ~ caso))

#somando tudo

Metodo_tidy <- Metodo_final2 %>%
  group_by(caso) %>%
  summarize(numeros=sum(total)) %>%
  arrange(desc(numeros))

#gr�fico

ggplot(Metodo_tidy,
       aes(x=numeros, y=fct_reorder(caso, numeros, desc=TRUE))) +
  geom_segment(
    aes(x = 0,
        y=fct_reorder(caso, numeros, desc=TRUE),
        xend = numeros,
        yend = fct_reorder(caso, numeros, desc=TRUE)),
    color = "gray50") +
  geom_point() +
  labs(x="Total", y="Tipo de agress�es", 
       title = "Como os jornalistas foram agredidos",
       caption = "Fonte: ABRAJI") +
  theme_minimal() +
  geom_text(aes(label=numeros), hjust=-.5) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        axis.text.x = element_blank())
