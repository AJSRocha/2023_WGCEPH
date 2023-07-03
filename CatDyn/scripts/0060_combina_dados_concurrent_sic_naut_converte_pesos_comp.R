# Preparar dados lota para conversao:
# dados em Lt (lota16 e lota_naut_1) têm de ser agrupados

## Lota_naut_1: comprimentos (que irao ser passado a pesos)

lota_naut_1 =
  data.frame(id_viagem = c(lota16$ID_VIAGEM  %>% as.character(), lota_naut_1$id_viagem %>% as.character()),
             DATA = c(lota16$DATA , lota_naut_1$DATA),
             ANO = c(lota16$ANO %>% as.character(), lota_naut_1$ANO %>% as.character()),
             MES = c(lota16$MES %>% as.character(), lota_naut_1$MES %>% as.character()),
             PORTO_NOME = c(lota16$PORTO_NOME %>% as.character(), as.character(lota_naut_1$PORTO)),
             REGIAO = c(lota16$REGIAO, as.character(lota_naut_1$REGIAO)),
             GEAR = c(lota16$GEAR %>% as.character(), lota_naut_1$GEAR %>% as.character()),
             cat_com = c(lota16$cat %>% as.character(), lota_naut_1$cat_com %>% as.character()),
             classe_comp = c(lota16$C_CLASSE, lota_naut_1$classe_comp),
             # artificio para bater certo, as variaveis nao sao equivalentes
             # serve só para ambos os df passarem pelo mesmo numero de passos de ampliaçao
             land_kg = c(lota16$land_kg, lota_naut_1$land_kg),
             peso_amostrado_dom = c(lota16$PESO_AM, lota_naut_1$peso_amostrado_dom),
             peso_total_caixa = c(lota16$PESO_D_C, lota_naut_1$peso_total_caixa),
             peso_am_caixa = c(lota16$PESO_A_C, lota_naut_1$peso_am_caixa),
             peso_total_spp = c(rep(1, nrow(lota16)), lota_naut_1$peso_total_spp),
             peso_am_spp = c(rep(1, nrow(lota16)), lota_naut_1$peso_am_spp),
             n_nao_observados = c(lota16$INDIF, lota_naut_1$n_nao_observados)) %>% 
  filter(ANO %in% c(2009:2022))


lota_naut_1 =
  lota_naut_1 %>% left_join(., bio_reg[,c('ano','regiao', 'a', 'b')],
                            by = c('ANO' = 'ano',
                                   'REGIAO' = 'regiao'))

for(i in 1:nrow(lota_naut_1)){
  if(lota_naut_1$REGIAO[i] == '27.9.a.c.n'){
    lota_naut_1$a[i] =
      bio_reg[bio_reg$ano == as.character(lota_naut_1$ANO[i]) & bio_reg$regiao == '27.9.a.c.s',]$a
    lota_naut_1$b[i] =
      bio_reg[bio_reg$ano == as.character(lota_naut_1$ANO[i]) & bio_reg$regiao == '27.9.a.c.s',]$b
  }
}


lota_naut_2 =
  lota_naut_2 %>% left_join(., bio_reg[,c('ano','regiao', 'a', 'b')],
                            by = c('ANO' = 'ano',
                                   'REGIAO' = 'regiao'))

for(i in 1:nrow(lota_naut_2)){
  if(lota_naut_2$REGIAO[i] == '27.9.a.c.n'){
    lota_naut_2$a[i] =
      bio_reg[bio_reg$ano == lota_naut_2$ANO[i] & bio_reg$regiao == '27.9.a.c.s',]$a
    lota_naut_2$b[i] =
      bio_reg[bio_reg$ano == lota_naut_2$ANO[i] & bio_reg$regiao == '27.9.a.c.s',]$b
  }
}


# agrupar tabela 2 (pesos) para classes

lota_naut_2 =
  lota_naut_2 %>% 
  mutate(comp_manto = (peso_total/a)^(1/b))


# lota_naut_2_temp %>% 
#   ggplot + 
#   geom_point(aes(x = comp_manto,
#                  y = peso_total,
#                  col = REGIAO)) +
#   geom_point(aes(x = comp_manto_alt,
#                  y = peso_total, col = 'pink'
#   )) + 
#   facet_grid(ANO ~ REGIAO) +
#   theme_light()

# agregar o naut_2_temp a classes de comprimentos


# classes de comprimento

lota_naut_2_cmp = 
  lota_naut_2 %>% 
  mutate(classe_comp = trunc(comp_manto/10)) %>% 
  group_by(codporto, id_viagem, id_denominacao, id_caixa,
           id_spp, REGIAO, DATA, ANO, MES,PORTO, GEAR,
           cat_com, especie_am, land_kg, peso_amostrado_dom,
           peso_total_caixa, peso_am_caixa,
           peso_total_spp, peso_am_spp,
           n_total_caixa, n_amostrados_caixa,
           n_total_spp, n_amostrado_comprimentos,
           n_nao_observados_tot, classe_comp,
           nome) %>% 
  summarise(n_nao_observados = length(classe_comp))


naut_cmp = data.frame(
  id_viagem = c(lota_naut_1$id_viagem, lota_naut_2_cmp$id_viagem %>% as.character()),
  DATA = c(lota_naut_1$DATA, lota_naut_2_cmp$DATA),
  ANO = c(lota_naut_1$ANO, lota_naut_2_cmp$ANO %>% as.character()),
  MES = c(lota_naut_1$MES, lota_naut_2_cmp$MES %>% as.character()),
  PORTO_NOME = c(lota_naut_1$PORTO_NOME, as.character(lota_naut_2_cmp$PORTO)),
  REGIAO = c(lota_naut_1$REGIAO, as.character(lota_naut_2_cmp$REGIAO)),
  GEAR = c(lota_naut_1$GEAR, lota_naut_2_cmp$GEAR %>% as.character()),
  cat_com = c(lota_naut_1$cat_com, lota_naut_2_cmp$cat_com %>% as.character()),
  classe_comp = c(trunc(lota_naut_1$classe_comp), lota_naut_2_cmp$classe_comp),
  land_kg = c(lota_naut_1$land_kg, lota_naut_2_cmp$land_kg),
  peso_amostrado_dom = c(lota_naut_1$peso_amostrado_dom, lota_naut_2_cmp$peso_amostrado_dom),
  peso_total_caixa = c(lota_naut_1$peso_total_caixa, lota_naut_2_cmp$peso_total_caixa),
  peso_am_caixa = c(lota_naut_1$peso_am_caixa, lota_naut_2_cmp$peso_am_caixa),
  peso_total_spp = c(lota_naut_1$peso_total_spp, lota_naut_2_cmp$peso_total_spp),
  peso_am_spp = c(lota_naut_1$peso_am_spp, lota_naut_2_cmp$peso_am_spp),
  n_nao_observados = c(lota_naut_1$n_nao_observados, lota_naut_2_cmp$n_nao_observados)
) 

# Rever estas medidas: isto é problematico!!

# %>%
#   mutate(peso_am_spp = case_when(is.na(peso_am_spp) ~ peso_total_spp,
#                                  T ~ peso_am_spp),
#          peso_total_spp = case_when(is.na(peso_total_spp) ~ peso_am_spp,
#                                  T ~ peso_total_spp)
#          # COMPLETAR COM OUTRAS MEDIDAS
#          )



# table(naut_cmp$classe_comp)
# hist(naut_cmp$land_kg)
# 
# 
# naut_cmp = bind_rows(lota_naut_1 %>% mutate(classe_comp = trunc(classe_comp)),
#                      lota_naut_2_cmp) #yay

# naut_cmp %>% 
#   filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
#   filter(GEAR != 'PS') %>% 
#   group_by(ANO, REGIAO, GEAR, cat_com, classe_comp) %>% 
#   summarise(n = sum(n_nao_observados)) %>% 
#   ggplot +
#   geom_bar(aes(
#     x = classe_comp,
#     y = n,
#     fill = GEAR),
#     stat = 'identity') + 
#   facet_grid(cat_com ~ REGIAO) +
#   theme_light()

# naut_cmp %>% 
#   filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
#   group_by(ANO, REGIAO, GEAR, cat_com, classe_comp) %>% 
#   summarise(n = sum(n_nao_observados))


lota_naut_2_peso = 
  lota_naut_2 %>% 
  mutate(classe_peso = plyr::round_any(peso_total, 50, f = floor)) %>% 
  group_by(codporto, id_viagem, id_denominacao, id_caixa,
           id_spp, REGIAO, DATA, ANO, MES, PORTO, GEAR,
           cat_com, especie_am, land_kg, peso_amostrado_dom,
           peso_total_caixa, peso_am_caixa,
           peso_total_spp, peso_am_spp,
           n_total_caixa, n_amostrados_caixa,
           n_total_spp, n_amostrado_comprimentos,
           n_nao_observados_tot, classe_peso,
           nome) %>% 
  summarise(n_nao_observados = length(classe_peso))


## classes de peso
naut_peso = bind_rows(lota_naut_1 %>% mutate(peso_total = a*(10*classe_comp)^b,
                                             classe_peso = plyr::round_any(peso_total, 50, f = floor)),
                      lota_naut_2_peso)

# naut_peso %>%
#   filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>%
#   filter(GEAR != 'PS') %>%
#   filter(classe_peso < 5000) %>% 
#   group_by(ANO, REGIAO, GEAR, cat_com, classe_peso) %>%
#   summarise(n = sum(n_nao_observados), na.rm =T) %>%
#   ggplot +
#   geom_bar(aes(
#     x = classe_peso,
#     y = n,
#     fill = GEAR),
#     stat = 'identity') +
#   facet_grid(cat_com ~ REGIAO) +
#   theme_light()
