vd12 = vd %>%
  filter(year_sale %in% c(2012)) %>% 
  arranjador()

vd12_frota = vd12 %>%
  mutate(polvicidade = QVENDA/QVENDA_total) %>% 
  group_by(IEMBARCA) %>% 
  summarise(ratio = mean(polvicidade),
            total = sum(QVENDA)) %>% 
  arrange(desc(ratio))

vd21_frota %>% 
  filter(ratio >= 0.90) %>% 
  summarise(res = sum(total)/sum(vd12_frota$total))

cd12 = vd12 %>%  
  # filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2012') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))


cd12_ref = vd12 %>%
  filter(IEMBARCA %in% unique(vd12_frota$IEMBARCA[vd12_frota$ratio >= 0.90])) %>% 
  filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2012') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))

df12 = cd12[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

df12_ref = cd12_ref[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

occ_cat_12 = as.CatDynData(x=df12,
                           step="week",
                           fleet.name="Artisanal-S-2012",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2012-01-01"),
                                          as.Date("2012-12-23")))

occ_cat_12_ref = as.CatDynData(x=df12_ref,
                           step="week",
                           fleet.name="Artisanal-S-2012-Targeted",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2012-01-01"),
                                          as.Date("2012-12-23")))
