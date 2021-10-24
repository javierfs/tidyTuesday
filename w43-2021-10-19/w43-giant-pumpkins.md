TidyTuesday 2021/w43 - Giant Pumpkins
================
@javierfs
19th of October 2021

# Preprocessing & Cleaning

``` r
#Cleaning a bit

pumpkins %>% elucidate::copies(filter = "dupes", sort_by_copies = TRUE) # Duplicated rows detected! 75 of 28065 rows
```

    ## # A tibble: 75 × 15
    ##    id     place weight_lbs grower_name      city   state_prov country  gpc_site 
    ##    <chr>  <chr> <chr>      <chr>            <chr>  <chr>      <chr>    <chr>    
    ##  1 2019-T EXH   2.00       Boutain, Richard Linco… Michigan   United … Dundees …
    ##  2 2019-T EXH   2.00       Boutain, Richard Linco… Michigan   United … Dundees …
    ##  3 2019-T EXH   2.00       Boutain, Richard Linco… Michigan   United … Dundees …
    ##  4 2013-F 101   91.00      Gantner, Garry   Menom… Wisconsin  United … Stillwat…
    ##  5 2013-F 101   91.00      Gantner, Garry   Menom… Wisconsin  United … Stillwat…
    ##  6 2013-P 412   948.00     Macri, Robert    Spoka… Washington United … Ronan Ha…
    ##  7 2013-P 412   948.00     Macri, Robert    Spoka… Washington United … Ronan Ha…
    ##  8 2013-T 185   2.20       Jaser, Robert    Schoe… Other      Germany  Hofgut S…
    ##  9 2013-T 185   2.20       Jaser, Robert    Schoe… Other      Germany  Hofgut S…
    ## 10 2014-F 163   80.00      Harper, Peter    West … Other      United … Royal Vi…
    ## # … with 65 more rows, and 7 more variables: seed_mother <chr>,
    ## #   pollinator_father <chr>, ott <chr>, est_weight <chr>, pct_chart <chr>,
    ## #   variety <chr>, n_copies <int>

``` r
nodup_pumpkins <- pumpkins %>% 
  elucidate::copies(filter = "first") %>% #only keep the 1st detected copy of each row
  elucidate::wash_df()
 
#There is some weird names in country and other fields such as -> "1900 Entries.\r\n\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t(128 exhibition only,\r\n               29 damaged)"
#remove them by filtering out place/ranking != [1,500]


cln_pumpkins <- nodup_pumpkins %>%
  separate(id, c("year", "type"), sep = "-") %>%
  mutate(type = fct_recode(type,
                           "Field Pumpkin" = "F",
                           "Giant Pumpkin" = "P",
                           "Giant Squash" = "S",
                           "Giant Watermelon" = "W",
                           "Long Gourd" = "L",
                           "Tomato" = "T")) %>%
  mutate(weight_lbs = if_else(is.na(weight_lbs), est_weight, weight_lbs)) %>% 
  mutate_at(c("year", "place", "ott", "est_weight", "pct_chart", "weight_lbs"), as.numeric) %>% 
  #filter(type != "Long Gourd") %>% #Long Gourd are measured in length not in kg
  mutate(weight_kg = round(weight_lbs * 0.4535924, 1)) %>%
  filter(is.na(type)==FALSE,
         is.na(place)==FALSE,
         place %in% c(1:500)) %>%  #removed weird entries EXH, DMG and mentioned prev 
  mutate_if(is_character, as_factor) %>% 
  select(year,type,place,grower_name,city,country,gpc_site,seed_mother, pollinator_father,weight_kg)




#getting grower total number of participations per year per type of a grower
grower_n_participations <- cln_pumpkins %>% 
  select(year, grower_name, type) %>% 
  count(year, grower_name, type) %>% 
  rename(n_participations_year=n)

#Cumulative participations
pumpkins_npartipations_cum <- grower_n_participations %>%
distinct(year,grower_name, type, n_participations_year) %>% 
group_by(grower_name, type) %>% 
mutate(cum_n_part = cumsum(n_participations_year)) %>%  ungroup()  

pumpkins_npartipations <- cln_pumpkins %>% merge(pumpkins_npartipations_cum, by = c('grower_name', 'type', 'year'))



  
#Getting the top 20 ranking 
cln_pumpkins_top_by_type_year <- pumpkins_npartipations %>% 
  group_by(type, year) %>% 
  top_n(25, -place) %>%
  arrange(place, year, type)
  
  
# Discoveries: 
# 1. growers can participate more than 1 time per year
# 2. growers can participate in different types: 
#            MacDonald, Christopher -> partipated in giant squad and tomatoes
#            Vial, Andrew -> partipated in Giant Pumpkin and Giant Watermelon


cln_pumpkins_top_by_type_year %>%
  distinct(grower_name,type, year, place) %>% # a grower can be participating >1 in the same year and still be in the top 
  select(grower_name, type, year, place) %>% 
  left_join(pumpkins_npartipations, by = c('grower_name', 'type', 'year', 'place')) -> pumpkings_2plot
```

``` r
# Q: Is it recurrent partipication a good predictor of achieving a higher ranking in the Festivals?
#ragg::agg_png

bg <- '#fff7f2'

pumpkings_2plot %>% 
  ggplot(aes(x = cum_n_part, y = place)) + 
  geom_point(alpha = 0.6, color = "#bb4b00") + 
  stat_smooth(method = "loess",color = '#ff7518') +
  labs(title= "Is it recurrent participation assuring a higher ranking?",
       subtitle = 'Top 20 places (2013-2020) on the Great Pumpkins Commonwealth',
       y="Ranking / Place", 
       x = "Yearly cumulative participation",
       caption = "Viz: @javierfs | Data: BigPumpkins.com") +
  theme_minimal()+
  theme(text = element_text(family= 'IM_Fell_DW_Pica',
                                  hjust = 0.5),
        plot.background = element_rect(fill = bg, color = NA),
        plot.title = element_text(size = rel(2), 
                                  color = 'black',
                                  family= 'IM_Fell_DW_Pica',
                                  hjust = 0.5),
        axis.title = element_text(hjust=0.95),
        plot.subtitle = element_text(size = rel(1), 
                                  color = 'black',
                                  family= 'IM_Fell_DW_Pica',
                                  hjust = 0.5))+
  facet_wrap( ~ type, ncol=3, scales = "free") +
  scale_y_reverse(lim=c(25,1), breaks = c(25,20,15,10,5,1))
```

![](w43-giant-pumpkins_files/figure-gfm/showing%20viz-1.png)<!-- -->
