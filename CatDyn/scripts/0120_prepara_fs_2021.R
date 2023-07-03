vd21 = vd %>%
  filter(year_sale %in% c(2021)) %>% 
  arranjador()

vd21_frota = vd21 %>%
  mutate(polvicidade = QVENDA/QVENDA_total) %>% 
  group_by(IEMBARCA) %>% 
  summarise(ratio = mean(polvicidade),
            total = sum(QVENDA)) %>% 
  arrange(desc(ratio))

vd21_frota %>% 
  filter(ratio >= 0.90) %>% 
  summarise(res = sum(total)/sum(vd21_frota$total))

cd21 = vd21 %>%  
  # filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2021') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))


cd21_ref = vd21 %>%
  filter(IEMBARCA %in% unique(vd21_frota$IEMBARCA[vd21_frota$ratio >= 0.90])) %>% 
  filter(regiao == 'Costa Sul') %>% 
  group_by(IDATVEND, semana) %>% 
  summarise(QVENDA_s = sum(QVENDA),
            effort_s = n_distinct(IEMBARCA)) %>% 
  group_by(semana) %>% 
  summarise(QVENDA = sum(QVENDA_s),
            effort = sum(effort_s)) %>% 
  left_join(.,
            contabilidade %>% 
              filter(ANO == '2021') %>% 
              select(semana, mean_weight_estim),
            by = c('semana' = 'semana'))

df21 = cd21[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

df21_ref = cd21_ref[1:52,] %>%
  transmute(obscat = QVENDA,
            obseff = effort,
            mw = mean_weight_estim,
            week = semana)

occ_cat_21 = as.CatDynData(x=df21,
                           step="week",
                           fleet.name="Artisanal-S-2021",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2021-01-01"),
                                          as.Date("2021-12-25")))

occ_cat_21_ref = as.CatDynData(x=df21_ref,
                           step="week",
                           fleet.name="Artisanal-S-2021-Targeted",
                           coleff=2,
                           colcat=1,
                           colmbw=3,
                           unitseff="trips",
                           unitscat="kg",
                           unitsmbw="kg",
                           nmult="thou",
                           season.dates=c(as.Date("2021-01-01"),
                                          as.Date("2021-12-25")))