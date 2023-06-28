library(dplyr)
library(ggplot2)

bio16 = read.csv('CatDyn/data//allspp_19972016_bio.csv', sep = ';', dec = ',')

bio16 = 
  bio16 %>%
  filter(COD_FAO == "OCC") %>%
  # filter()
  transmute(id_viagem = factor(ID_AMOSTRA),
            DATA = as.POSIXct(DATA_AM, format = '%d-%m-%Y'),
            ANO = factor(ANO),
            PORTO = droplevels(factor(PORTO_NOME)),
            REGIAO = case_when(REGIAO == "N" ~ "27.9.a.c.n",
                               REGIAO == "C" ~ "27.9.a.c.s",
                               TRUE ~ "27.9.a.s.a") %>% factor,
            ARTE_EU = factor(ARTE_EU),
            especie_am = factor("OCC"),
            land_kg = DESEMBARQUE, # desembarque da denominação na viagem
            PESO_AM = PESO_AM,
            N_OBS = N_OBS,
            C_CLASSE = C_CLASSE,
            C_INDIVIDUAL = C_INDIVIDUAL,
            P_INDIVIDUAL = P_INDIVIDUAL,
            P_EVISCERADO = P_EVISCERADO,
            SEXO = factor(case_when(SEXO == "F" ~ "F",
                                    SEXO == "f" ~ "F",
                                    SEXO == "M" ~ "M",
                                    SEXO == "m" ~ "M",
                                    TRUE ~ "I")),
            EST_MATURACAO = factor(case_when(EST_MATURACAO == "2         " ~ "2",
                                             EST_MATURACAO == "4         " ~ "4",
                                             EST_MATURACAO == "3         " ~ "3",
                                             EST_MATURACAO == "1         " ~ "1",
                                             EST_MATURACAO == "5         " ~ "5",
                                             EST_MATURACAO == "0         " ~ "0",
                                             EST_MATURACAO == "1" ~ "1",
                                             EST_MATURACAO == "2/3       " ~ "2/3",
                                             EST_MATURACAO == "" ~ "NA",
                                             EST_MATURACAO == "1       1 " ~ "1",
                                             EST_MATURACAO == "2" ~ "2",
                                             EST_MATURACAO == "3" ~ "3",
                                             EST_MATURACAO == "5" ~ "5",
                                             EST_MATURACAO == "4" ~ "4")),
            # C_PENIS = C_PENIS,
            # C_TEST = C_TEST,
            P_TEST = P_TEST,
            P_CESPERM = P_CESPERM,
            P_OVARIO = P_OVARIO,
            P_GOVID = P_GOVID,
            D_GOVID = D_GOVID,
            # C_GASS = C_GASS,
            # P_GASS = P_GASS,
            D_GNID = D_GNID,
            # P_GNID = P_GNID,
            # P_COVID = P_COVID,
            E_ESTOM = E_ESTOM,
            P_GDIGEST = P_GDIGEST,
            # FECUNDADA = factor(FECUNDADA)
  ) 


# Limpezas de bio16

cm_mm = which(bio16$C_INDIVIDUAL < 40)
#table(bio16[cm_mm,]$PORTO, bio16[cm_mm,]$ANO)
#table(bio16[-cm_mm,]$PORTO, bio16[-cm_mm,]$ANO)

bio16$C_INDIVIDUAL_corr = bio16$C_INDIVIDUAL
bio16[cm_mm,]$C_INDIVIDUAL_corr = bio16[cm_mm,]$C_INDIVIDUAL_corr*10

# corrigir pesos registados em kg - 2 obs
#bio16[is.na(bio16$P_INDIVIDUAL),]$P_INDIVIDUAL = 0
#bio16[bio16$P_INDIVIDUAL > 0 & bio16$P_INDIVIDUAL < 20,]$P_INDIVIDUAL = bio16[bio16$P_INDIVIDUAL > 0 & bio16$P_INDIVIDUAL < 20,]$P_INDIVIDUAL * 1000

bio16[is.na(bio16$C_INDIVIDUAL_corr),]$C_INDIVIDUAL_corr = 0
bio16[bio16$C_INDIVIDUAL_corr == 12.2,]$C_INDIVIDUAL_corr = 122


########## NAUTILUS ###################

load('CatDyn/data/nautilus_occ.Rdata')


