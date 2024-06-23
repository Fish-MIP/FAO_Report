

#### 03b_plotting_yearly_global_biomass

# Check NAs by model ----

# by country - either countries on the Caspian Sea for which data is avaialble only from a few IPSL-forced models or  countries with small coasts for which data is avaialble only from a few GFDL-forced models
a<-filter(count_bio_by_model, is.na(mean_bio))
unique(a$fao_official)

# [1] "Turkmenistan"                    "the Hashemite Kingdom of Jordan" "the Kingdom of Bahrain"
# [4] "the Republic of Albania"         "the Republic of Azerbaijan"      "the Republic of Djibouti"
# [7] "the Republic of Iraq"            "the Republic of Kazakhstan"      "the Republic of the Sudan"
# [10] "the State of Kuwait"             "the Kingdom of Belgium"          "the Republic of Cameroon"

a<-filter(count_bio_by_model, fao_official == "Turkmenistan", !is.na(mean_bio))
a$id<-paste(a$esm, a$mem)
unique(a$id) # only models with Caspian Sea

a<-filter(count_bio_by_model, fao_official == "the Hashemite Kingdom of Jordan", !is.na(mean_bio))
a$id<-paste(a$esm, a$mem)
unique(a$id) # only gfdl model (but boats) # # almost landlocked

a<-filter(count_bio_by_model, fao_official == "the Kingdom of Bahrain", !is.na(mean_bio))
a$id<-paste(a$esm, a$mem)
unique(a$id) # only gfdl model (including boats)

# for global
a<-filter(global_bio_by_model, is.na(mean_bio))
a$id<-paste(a$mem, a$esm)
unique(a$id)

# for fao regions
a<-filter(fao_bio_by_model, is.na(mean_bio))
a$id<-paste(a$mem, a$esm)
unique(a$id)

# for top 7
a<-filter(top_7_data_by_model, is.na(mean_bio))
a$id<-paste(a$mem, a$esm)
unique(a$id)

# CHECK correct way of calculating % changes between periods for stat table ----

# calculate percentage change between 2 periods
# Calculating reference period
ref_bio <- count_bio_by_model |>
  filter(year >= 2005 & year <= 2014) |>
  #Calculate mean per ensemble member
  group_by(mem, esm, fao_official) |>
  dplyr::summarise(ref_bio = mean(mean_bio, na.rm = T))

# between decades
last_decade_bio <- count_bio_by_model |>
  filter(year >= 2041 & year <= 2050) |>
  #Calculate mean per ensemble member
  group_by(mem, esm, scenario, fao_official) |>
  dplyr::summarise(last_decade_bio = mean(mean_bio, na.rm = T))

change_between_decades<-last_decade_bio %>%
  full_join(ref_bio) %>%
  arrange(fao_official) %>%
  mutate(perc_change = ((last_decade_bio-ref_bio)/ref_bio)*100)

# check differences between averaging % changes across last decade and option above - No difference
trial<-count_bio_by_model %>% filter(year %in%(2041:2050)) %>% group_by(mem, esm, scenario, fao_official) %>% summarise(mean = mean(perc_change)) %>% arrange(fao_official)
change_between_decades

setdiff(round(trial$mean,3), round(change_between_decades$perc_change,3))

# check the 2 approaches with fake data - no difference
# mean biomass across ref decade for each model, assuming one country
ref<-data.frame(model = c("mod1", "mod2"), ref_bio = c(10,12))
# biomass per year per model
year<-data.frame(model= c("mod1", "mod1", "mod2", "mod2"), year = c(1,2,1,2), bio = c(5,10, 11,13))

# approach 1
# A % change between mean biomass across ref decade per model and biomass per future years per model
all<-ref %>%
  full_join(year) %>%
  mutate(per_change = ((bio-ref_bio)/ref_bio)*100) %>%
  group_by(model) %>%
  summarise(mean_change = mean(per_change)) # B mean percentage change across years

# approach 2
# A mean biomass across ref decade and mean biomass across future years
all2<-ref %>%
  full_join(year) %>%
  group_by(model, ref_bio) %>%
  summarise(bio = mean(bio)) %>%
  mutate(mean_change = ((bio-ref_bio)/ref_bio)*100) # B % change across decades

# check table of stats ----

# test on model agreement ----
data.frame(n_model = 10,
           n_positive = 5,
           n_negative = 5) %>%
  mutate(same_sign = max(n_positive, n_negative),
         agreement = (same_sign/n_model)*100)

# check by plotting top 7 countries - OK
to_plot<-top_7_data_by_model %>%
  filter(year %in% c(2041:2050),
         scenario == "ssp126") %>%
  mutate(id = paste(esm, mem, sep ="_")) %>%
  group_by(mem, esm, scenario, figure_name) %>%
  summarise(perc_change = mean(perc_change)) %>%
  ungroup()

unique(stats$spatial_scale)
filter(stats, decade == "2041-2050", scenario == "ssp126", spatial_scale == "top_7_countries")

ggplot(to_plot, aes(x = mem, y = perc_change))+
  # geom_line()+
  geom_point()+
  geom_hline(yintercept = 0, color = "black")+
  facet_wrap(~figure_name)

# wilcox test ----
# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
# from example: The p-value of the test is 0.02712, which is less than the significance level alpha = 0.05. We can conclude that men’s median weight is significantly different from women’s median weight with a p-value = 0.02712.

w_test<-all_scale_data_by_model %>% 
  select(!c(mean_bio, ref_bio)) %>% 
  spread(scenario, perc_change) %>% 
  group_by(figure_name, decade, spatial_scale) %>% 
  summarise(wilcox_p = wilcox.test(ssp126, ssp585, alternative = "two.sided")$p.value) %>% 
  ungroup()

# check warnings
dplyr::last_dplyr_warnings() # warning for 4 countries

filter(w_test, figure_name == "Bahrain", decade == "2041-2050", spatial_scale == "countries") # values are provided but might not be trustable is my interpretation.

# check single test
w_test_warnings<-all_scale_data_by_model %>%
  select(!c(mean_bio, ref_bio)) %>%
  spread(scenario, perc_change) %>%
  filter(figure_name == "Bahrain",
         decade == "2041-2050",
         spatial_scale == "countries")

wilcox.test(w_test_warnings$ssp126, w_test_warnings$ssp585, alternative = "two.sided")$p.value
# Warning message:
# In wilcox.test.default(w_test_warnings$ssp126, w_test_warnings$ssp585,  :
#   cannot compute exact p-value with ties
# where ties is when both set have the same value.

# NAs non-influent
a<-c(1,4,6,NA, 2,7,NA,8,9,20,NA)
b<-c(NA,3,26, NA,28,40,33,NA,44,55,80)
wilcox.test(a, b, alternative = "two.sided")$p.value

# check test
# check if values from the table are the same as the below - yes
filter(w_test, figure_name == "Viet Nam")

# test to understand wilcox
# between scenarios in 2050 (test not significant)
trial<-top_7_data_by_model %>%
  filter(figure_name == "Viet Nam",
         year >= 2041 & year <= 2050)

# boxplot
ggplot(trial, aes(x=scenario, y=perc_change)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9)

trial_test<-trial %>%
  spread(scenario, perc_change)

wilcox.test(trial_test$ssp126, trial_test$ssp585, alternative = "two.sided")$p.value # 0.0590907 not significant

# between scenarios in 2100 (test significant)
trial<-top_7_data_by_model %>%
  filter(figure_name == "Viet Nam",
         year >= 2091 & year <= 2100)

# boxplot
ggplot(trial, aes(x=scenario, y=perc_change)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9)

trial_test<-trial %>%
  spread(scenario, perc_change)

wilcox.test(trial_test$ssp126, trial_test$ssp585, alternative = "two.sided")$p.value # 1.967129e-20 with raw values this is highly significant although sd of values overlap in trend figure and values overlap in boxplot.

# losses ----

losses<-stats %>% 
  select(!c(median, sd, min, max, n_model, agreement)) %>% 
  mutate(mean = round(mean, 1)) %>% # calculate with one decimal values. Results are sensitive to decimal places, these will be consistent with the ones that can be calcualted from the final, published table (where mean is rounded to 1 decimal) 
  pivot_wider(names_from = scenario, values_from = mean) %>% 
  group_by(figure_name, decade, spatial_scale) %>% 
  summarise(losses = (1-(ssp126/ssp585))*100,
            losses_direction = ifelse(ssp126 > ssp585, "reduction of losses under SSP1-2.6", "increase of losses under SSP1-2.6")) %>% 
  ungroup() %>%
  filter(decade == "2091-2100")

# check losses - added column on direction (i.e. reduced or increased losses under ssp1) to understand signs
filter(stats, figure_name == "Algeria")
(1-(-4.5/-0.722))*100

#### mitigation higher than high emission (they should all be negative)
a<-7 # mitigation 7% increase
b<-3 # high emission 3% increase
(1-(a/b))*100 # -133% # high emission results in more losses

a<--1
b<--3
(1-(a/b))*100 # 66 # this should be negative

a<-3
b<--1
(1-(a/b))*100 # 400 # this should be negative

#### mitigation lower than high emission
a<-3 # mitigation 7% increase
b<-7 # high emission 3% increase
(1-(a/b))*100 # 57% # high emission results in less losses

a<--3
b<-1
(1-(a/b))*100 # 400

a<--3
b<--1
(1-(a/b))*100 # -200 # this should be positive

# e.g global
a<--7
b<--24
(1-(a/b))*100 # 70

# differences between versions -----
old<-read_csv("data/table_stats_with_continent.csv") # older version - saved with different name in google drive, then uploaded from Julia after she calculated some stat and worked on it in excel.
new<-read_csv(file.path(base_folder,"table_stats.csv"))

old<-old %>%
  arrange(fa_ffcl_shrt, scenario, decade, contnnt, spatial_scale)
new<-new %>%
  arrange(figure_name, scenario, decade, continent, spatial_scale)

# check countries included - only difference is Bouvet Island - to add and Colombia (Serranilla) to remove. Colombia (Serranilla) not even in the formatted table below so was deleted in previous rounds.
setdiff(sort(unique(old$fa_ffcl_shrt)), sort(unique(new$figure_name))) # plus russia that should have been russian federation
setdiff(sort(unique(new$figure_name)), sort(unique(old$fa_ffcl_shrt)))

# difference in values
old<-old %>%
  filter(!fa_ffcl_shrt == "Colombia (Serranilla)") %>%
  mutate(fa_ffcl_shrt = case_when(fa_ffcl_shrt == "C√¥te d'Ivoire" ~ "Côte d'Ivoire",
                                  fa_ffcl_shrt == "Cura√ßao" ~ "Curaçao",
                                  fa_ffcl_shrt == "R√©union" ~ "Réunion",
                                  fa_ffcl_shrt == "Russia" ~ "Russian Federation",
                                  fa_ffcl_shrt == "T√ºrkiye" ~ "Türkiye",
                                  .default = as.character(fa_ffcl_shrt)))
new<-new %>%
  filter(!figure_name == "Bouvet Island")

old<-old %>%
  arrange(fa_ffcl_shrt, scenario, decade, contnnt, spatial_scale) %>%
  select(fa_ffcl_shrt, scenario, decade, contnnt, spatial_scale, mean)

new<-new %>%
  arrange(figure_name, scenario, decade, continent, spatial_scale) %>%
  rename(mean_new = mean, fa_ffcl_shrt = figure_name, contnnt = continent) %>%
  select(fa_ffcl_shrt, scenario, decade, contnnt, spatial_scale, mean_new)

comapre_values<- old %>%
  full_join(new) %>%
  mutate(mean = round(mean, 1),
         mean_new = round(mean_new, 1)) %>%
  mutate(compare = ifelse(mean == mean_new, "ok", "wrong")) %>%
  filter(compare == "wrong") %>%
  mutate(diff = abs(abs(mean) - abs(mean_new))) %>%
  arrange(-diff)

View(comapre_values)
# different = Canary Island, Mauritania, Puerto Rico, Romania, Turkia
# different for some scenarios ~0.5 = Comoros, Cuba, Fiji

# check Romania - all models present and none showing huge changes fro ssp126 as previously done.
rom<-count_bio_by_model %>%
  filter(figure_name == "Romania") %>%
  mutate(id = paste(esm, mem, sep = "-"))

ggplot(rom, aes(x = year, y = perc_change, group = id, color = id))+
  geom_line()+
  facet_wrap(~scenario)

rom<-rom %>%
  group_by(scenario, year) %>%
  summarise(mean = mean(perc_change),
            sd = sd(perc_change))

ggplot(rom, aes(x = year, y = mean, group = scenario, color = scenario))+
  geom_line()+
  geom_ribbon(aes(ymin = mean-sd,
                  ymax = mean+sd,
                  fill = scenario), alpha = 0.3)

# print the diffences and update the report.
# fwrite(comapre_values, "data/data_products/table_stats_differences.csv")
