#TODO
#' integrar dados SIC



# Junção das duas tabelas

bio_tmp =
  data.frame(regiao = c(bio16$REGIAO, bio21$REGIAO),
             comp_manto = c(bio16$C_INDIVIDUAL_corr, bio21$comp_manto * 10),
             peso = c(bio16$P_INDIVIDUAL, bio21$peso_total),
             ano = c(bio16$ANO, bio21$ANO),
             mes = c(format(bio16$DATA, format = '%m'),
                     format(bio21$DATA, format = '%m')),
             sexo = c(bio16$SEXO, bio21$SEXO),
             mat = c(bio16$EST_MATURACAO, bio21$EST_MATURACAO)) %>% 
  mutate(mes = gsub('(?<=\\b|-)0',
                    '',
                    format(mes, format = '%m'),
                    perl=TRUE))

bio_tmp =
  bio_tmp %>%
  filter(ano %in% c(2007:2022)) %>% 
  filter(peso > 0) %>%
  filter(comp_manto < 600) %>%
  filter(comp_manto > 0) %>% 
  filter(sexo %in% c('M', 'F'))

# Tabela de modelos W = aL^b por ano e regiao:

bio_reg = 
  bio_tmp %>% 
  group_by(ano, regiao) %>% 
  summarise(a_0 = lm(log(peso) ~ log(comp_manto))$coefficients[1],
            b_0 = lm(log(peso) ~ log(comp_manto))$coefficients[2],
            a = coefficients(nls(peso ~ a*comp_manto^b, start = c(a = 1, b = 3)))[1],
            b = coefficients(nls(peso ~ a*comp_manto^b, start = c(a = 1, b = 3)))[2])

