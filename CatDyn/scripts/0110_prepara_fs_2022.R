vd22 = vd %>%
  filter(year_sale %in% c(2022)) %>% 
  arranjador()

vd22_frota = vd22 %>%
  mutate(polvicidade = QVENDA/QVENDA_total) %>% 
  group_by(IEMBARCA) %>% 
  summarise(ratio = mean(polvicidade),
            total = sum(QVENDA)) %>% 
  arrange(desc(ratio))

vd22_frota %>% 
  filter(ratio >= 0.90) %>% 
  summarise(res = sum(total)/sum(vd22_frota$total))

cd22 = vd22 %>%  
  # filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2022') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))


cd22_ref = vd22 %>%
  filter(IEMBARCA %in% unique(vd22_frota$IEMBARCA[vd22_frota$ratio >= 0.90])) %>% 
  filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2022') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))

df22 = cd22[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

df22_ref = cd22_ref[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

occ_cat_22 = as.CatDynData(x=df22,
                           step="week",
                           fleet.name="Artisanal-S-2022",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2022-01-01"),
                                          as.Date("2022-12-25")))

occ_cat_22_ref = as.CatDynData(x=df22_ref,
                           step="week",
                           fleet.name="Artisanal-S-2022-Targeted",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2022-01-01"),
                                          as.Date("2022-12-25")))