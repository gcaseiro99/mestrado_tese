library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(stringr)
library(readr)

tab <- read_csv("Projetos_R/tese_elei_mun/tab_cod_mun.csv", 
                     col_types = cols(...1 = col_skip(), CD_LOCALIDADE_IBGE = col_skip(), 
                                               NM_LOCALIDADE = col_skip()))

tab<-tab%>%rename(SG_UE=CD_LOCALIDADE_TSE)


dt_coli_w <- readRDS("~/Projetos_R/tese_elei_mun/dt_coli_w.rds")

dt_coli_w<-dt_coli_w%>%mutate(QT_VOTOS_LEGENDA_ver_p=QT_VOTOS_LEGENDA_ver/(QT_VOTOS_LEGENDA_ver+QT_VOTOS_NOMINAIS_ver))


temp<-dt_coli_w[!is.na(QT_VOTOS_LEGENDA_ver_p),]%>%left_join(tab)%>%group_by(ANO_ELEICAO,NR_PARTIDO,SG_UF)%>%
  summarise(vot_legenda=mean(QT_VOTOS_LEGENDA_ver_p))



for (i in unique(temp$ANO_ELEICAO)) {


fig<-ggplot(temp[temp$ANO_ELEICAO==i,],aes(x=SG_UF,y=vot_legenda,fill=as.factor(NR_PARTIDO)))+
  geom_bar(stat="identity",position = "identity", alpha=.3)

fig<-ggplotly(fig)%>%layout(title=paste0("Votos de Legenda - ",i))

saveWidget(fig,paste0("vot_legenda_",i,".html"))

}



temp<-dt_coli_w%>%group_by(ANO_ELEICAO,NR_PARTIDO)%>%summarise(Np=sum(!is.na(DS_COMPOSICAO_COLIGACAO_pre)),
                                                               Nv=sum(!is.na(DS_COMPOSICAO_COLIGACAO_ver)))%>%
  mutate(NR_PARTIDO=as.character(NR_PARTIDO))


fig<-ggplot(temp,aes(x=ANO_ELEICAO,y=Np,color=NR_PARTIDO))+
  geom_line()

fig<-ggplotly(fig)




temp<-dt_coli_w%>%subset(!is.na(DS_COMPOSICAO_COLIGACAO_pre))%>%
  select(ANO_ELEICAO,SG_UE,DS_COMPOSICAO_COLIGACAO_pre)%>%unique()%>%
  mutate(N=str_count(DS_COMPOSICAO_COLIGACAO_pre,"/")+1,ano_tipo=paste0(ANO_ELEICAO,"_pre"))

temp<-dt_coli_w%>%subset(!is.na(DS_COMPOSICAO_COLIGACAO_ver))%>%
  select(ANO_ELEICAO,SG_UE,DS_COMPOSICAO_COLIGACAO_ver)%>%unique()%>%
  mutate(N=str_count(DS_COMPOSICAO_COLIGACAO_ver,"/")+1,ano_tipo=paste0(ANO_ELEICAO,"_ver"))%>%
  bind_rows(temp)
  
fig<-ggplot(temp,aes(x=N,fill=ano_tipo))+
  geom_histogram( color='#e9ecef', alpha=0.6,position='identity',bins = 24)

fig<-ggplotly(fig)%>%layout(title="Nº. de Partidos nas Coligações")
fig

saveWidget(fig,'n_colig.html')



temp<-dt_coli_w%>%subset(!is.na(DS_COMPOSICAO_COLIGACAO_pre)&ANO_ELEICAO==2020)%>%
  select(ANO_ELEICAO,SG_UE,DS_COMPOSICAO_COLIGACAO_pre)%>%unique()%>%left_join(tab)%>%
  mutate(N=str_count(DS_COMPOSICAO_COLIGACAO_pre,"/")+1,ano_tipo=paste0(ANO_ELEICAO,"_pre"))

fig<-ggplot(temp%>%
              group_by(SG_UF) %>%
              mutate(weight = 1 / n()),aes(x=N,fill=SG_UF))+
  geom_histogram( aes(weight = weight),color='#e9ecef', alpha=0.6,position='identity',bins = 15)

fig<-ggplotly(fig)%>%layout(title="Nº. de Partidos nas Coligações Majoritárias")
fig

saveWidget(fig,'n_colig_2020.html')




temp<-dt_coli_w%>%subset(!is.na(DS_COMPOSICAO_COLIGACAO_pre)&N_PREFEITO==1)%>%
  select(ANO_ELEICAO,SG_UE,DS_COMPOSICAO_COLIGACAO_pre,NR_PARTIDO)%>%unique()%>%left_join(tab)%>%
  mutate(N=str_count(DS_COMPOSICAO_COLIGACAO_pre,"/")+1,ano_tipo=paste0(ANO_ELEICAO,"_pre"))%>%
  group_by(ANO_ELEICAO,NR_PARTIDO)%>%summarise(N=mean(N==1))


fig<-ggplot(temp,aes(x=ANO_ELEICAO,y=N,fill=as.factor(NR_PARTIDO)))+
  geom_bar(stat="identity",position = "identity", alpha=.3)

fig<-ggplotly(fig)%>%layout(title="Partido Isolado - Prefeito")
fig

saveWidget(fig,'part_isol.html')





##################################


files<-list.files("~/Projetos_R/tese_elei_mun/filiados","SP")

dt<-NULL

for (i in files) {
  temp<-fread(paste0("~/Projetos_R/tese_elei_mun/filiados/",i),select = 1:4,colClasses = rep("character",6))
  temp$Data<-gsub("_RJ.csv","",i)
  dt<-bind_rows(dt,temp)
}

fun_data <- function(x) {
  x<-gsub("Dezembro","12_01",x)
  x<-gsub("Novembro","11_01",x)
  x<-gsub("Outubro","10_01",x)
  x<-gsub("Setembro","09_01",x)
  x<-gsub("Agosto","08_01",x)
  x<-gsub("Julho","07_01",x)
  x<-gsub("Junho","06_01",x)
  x<-gsub("Maio","05_01",x)
  x<-gsub("Abril","04_01",x)
  x<-gsub("Março","03_01",x)
  x<-gsub("Fevereiro","02_01",x)
  x<-gsub("Janeiro","01_01",x)
  
  return(x)
}

dt$Data<-ymd(fun_data(dt$Data))
dt$Eleitores<-gsub("[.]|[,]","",dt$Eleitores)
dt$Eleitores<-as.numeric(dt$Eleitores)


temp<-dt%>%subset(!Partido==""&!is.na(Eleitores))%>% group_by(Data,Partido)%>%summarise(N=sum(Eleitores))


fig<-ggplot(temp[month(temp$Data)%in%c(2,8),],aes(x=Data,y=N,color=Partido))+
  geom_line()

fig<-ggplotly(fig)
fig  
