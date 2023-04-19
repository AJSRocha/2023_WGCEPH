## Landings para WGCEPH2022

# Ganda javardice porque nao havia desembarques no formato dos anos anteriores. Por esse motivo, foram usados vendas-dia agregados

library(dplyr)
source('C://repos/path.R'); path('local')

ano = 2022

# Importa dados
land = read.csv(paste0(dados, 'desembarques//desemb_2022.csv'),
               sep = ",", dec = ".")

# load("C:/dados_pnab/vendas-dia/vd_2022.Rdata")

# land = vd_2022 %>%
#   mutate(ANO = year_sale,
#             MES = month_sale,
#             zona = zona,
#             PORTO_PNAB = IPORTO,
#             PORTO_SLV = IPORTO,
#             PORTO_NOME = PORTO,
#             ARTE_EU = EGRUPART,
#             COD_FAO = EESPECIE,
#             DESEMBARQUE = QVENDA) %>% 
#   group_by(ANO, MES, zona, PORTO, ARTE_EU, COD_FAO) %>% 
#   summarise(QESTIMADA = sum(DESEMBARQUE))


slv = read.csv(paste0(dados, 'especies_slv/slv.csv'))
portos = read.csv(paste0(dados, 'portos_slv//codigos_portos.csv'))
portos[182,] = c(40950, "CAIS DO BICO", "csbic", "NW","AVEIRO", "PTCDB", "CDB",NA,NA)


# acrescenta codigos fao da tabela SLV 
land = merge(land,slv[,c("ESPECIE_SLV","COD_FAO","FAMILIA")],all.x=T,all.y=F,
             by.x = 'IESPECIE',
             by.y ="ESPECIE_SLV")

# acrescenta portos slv
land =merge(land,portos[,c("codporto","nome","zona")],
      all.x = T,
      all.y = F,
      by.x = "IPORTO",
      by.y = "codporto")

# restringe desembarques a codigos fao tirados do ASFIS
## no dia 26.04 foi acrescentado o SQU, que foi alterado para OMZ
fao = c("SQC","OCT","OMZ","CTC",
        "OCC","EOI","SQF","OFJ",
        "OUL","OUW","SQI","SQR",
        "OUM","OCM","OCZ","SQE",
        "SQM","YHT","OQD","SQZ",
        "CTL","TDQ","EDT","SQA",
        "OJJ","SQL","CTR","SQU")

# transforma tabela
land_export =
land %>%
  select(nome, zona, COD_FAO, IANO, IMES, EARTE, QESTIMADA) %>%
  filter(IANO == ano) %>%
  # remove artes espanholas
  filter(EARTE %in% unique(land$EARTE)[!grepl("SP_", unique(land$EARTE))]) %>%
  filter(COD_FAO %in% fao) %>%
  mutate(zona = factor(case_when(zona == "NW" ~ "27.9.a.c.n",
                                 zona == "SW" ~ "27.9.a.c.s",
                                 zona == 'S' ~ "27.9.a.s.a",
                                 zona == 'ACORES' ~ '27.10',
                                 T ~ 'O')),
         # Acerta niveis com formato intercatch
         EARTE = factor(case_when(EARTE == 3 ~ "OTB",
                                    EARTE == 5 ~ 'PS_SPF_0_0_0',
                                    T ~ 'MIS_MIS_0_0_0'))) %>%
  group_by(COD_FAO, zona, IMES, EARTE) %>%
  # desembarques à zona, em kg
  summarise(QESTIMADA = sum(QESTIMADA, na.rm = T)) %>% 
  filter(zona != 'O')

# land_export = land %>% filter(COD_FAO %in% fao)

# altera 'SQU' para 'OMZ' para ficar de acordo com o nome do stock do ICES
land_export[land_export$COD_FAO == 'SQU',]$COD_FAO = 'OMZ'
land_export$zona = as.character(land_export$zona)

# save(land, file="C://Google Drive//Polvices//WGCEPH 2020//desemb_mes_2019.Rdata")

for(j in unique(land_export$COD_FAO)){
  teste_9 = data.frame()
  teste_10 = data.frame()
  occ = land_export[land_export$COD_FAO==j,]
  for(i in 1:nrow(occ)){
    linha =
      paste("HI,","PT,",ano,",","Month,",
        occ$IMES[i],",",
        occ$EARTE[i],",",
        ifelse(occ$zona[i] == '27.10',"SubArea,","AreaUnit,"),
        occ$zona[i],",",
        "NA,","NA,","-9,","NA",
        "\n",
        "SI,","PT,",ano,",","Month,",
        occ$IMES[i],",",
        occ$EARTE[i],",",
        ifelse(occ$zona[i] == '27.10',"SubArea,","AreaUnit,"),
        occ$zona[i],",",
        "NA,",
        occ$COD_FAO[i],",",
        "NA,","L,","R,","NA,","H,","U,","NA,","t,",
        occ$QESTIMADA[i]/1000,",",
        occ$QESTIMADA[i]/1000,",",
        "-9,",",,",
        sep="")
    if(occ$zona[i] != '27.10'){teste_9[nrow(teste_9)+1,1] = linha}
    else if(occ$zona[i] == '27.10'){teste_10[nrow(teste_10)+1,1] = linha}
  }
  
if(nrow(teste_9>0)){  
write.table(teste_9,
            file=paste0('Intercatch/IC',ano,j,"27_9a_PT_landings.dat",sep="_")
            ,sep="",row.names = F,col.names = F,quote=F)}

if(nrow(teste_10>0)){  
write.table(teste_10,
            file=paste0('Intercatch/IC',ano,j,"27_10_PT_landings.dat",sep="_")
            ,sep="",row.names = F,col.names = F,quote=F)}
}

# resumo de desembarques OTB
land_export %>% 
  filter(ARTE_EU == 'MIS_MIS_0_0_0') %>% 
  group_by(COD_FAO) %>%
  summarise(tons = sum(QESTIMADA/1000))
