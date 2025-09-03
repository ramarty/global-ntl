# Analysis

# Load data --------------------------------------------------------------------
ntl_df <- readRDS(file.path(db_dir, "data", "ntl", "global_annual_ntl.Rds"))

gadm_sf <- read_sf(file.path(db_dir, "data", "gadm", "gadm_410-levels.gpkg"),
                   "ADM_0")

gadm_sf <- gadm_sf %>%
  clean_names()

# VIIRS vs Black Marble --------------------------------------------------------
cntry_viirs <- ntl_df %>%
  filter(!is.na(ntl_viirs_sum)) %>%
  distinct(country) %>%
  pull(country)

cntry_bm <- ntl_df %>%
  filter(!is.na(ntl_bm_sum)) %>%
  distinct(country) %>%
  pull(country)

length(cntry_viirs)
length(cntry_bm)

length(cntry_viirs[!(cntry_viirs %in% cntry_bm)])
cntry_viirs[!(cntry_viirs %in% cntry_bm)]

## All
ntl_df %>%
  mutate(ntl_bm_sum = log(ntl_bm_sum),
         ntl_viirs_sum = log(ntl_viirs_sum)) %>%
  filter(!is.na(ntl_bm_sum),
         !is.na(ntl_viirs_sum)) %>%
  ggplot(aes(x = ntl_bm_sum,
             y = ntl_viirs_sum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  stat_cor(method = "pearson", label.x = 0.5, label.y = 16) +
  theme_classic2() +
  labs(x = "VIIRS: Black Marble, Logged",
       y = "VIIRS: Colorado School of Mines, Logged",
       title = "Comparing sources of VIIRS data")

ggsave(filename = file.path(figures_dir,
                            "viirs_vs_bm.png"),
       height = 4, width = 5)

## By country
ntl_df %>%
  filter(!is.na(ntl_bm_sum),
         !is.na(ntl_viirs_sum)) %>%
  group_by(gid_0) %>%
  mutate(gid_n = n()) %>%
  ungroup() %>%
  filter(gid_n >= 11) %>%
  
  group_by(gid_0) %>%
  summarise(ntl_cor = cor(ntl_bm_sum, ntl_viirs_sum)) %>%
  ungroup() %>%
  
  ggplot() +
  geom_histogram(aes(x = ntl_cor),
                 fill = "dodgerblue",
                 color = "black") +
  labs(x = "Correlation",
       y = "N Countries",
       title = "Distribution of within country correlation") +
  theme_classic2()

ggsave(filename = file.path(figures_dir,
                            "viirs_vs_bm_within_country.png"),
       height = 4, width = 5)


## By country
# ntl_df %>%
#   filter(!is.na(ntl_bm_sum),
#          !is.na(ntl_viirs_sum)) %>%
#   group_by(gid_0) %>%
#   mutate(gid_n = n()) %>%
#   ungroup() %>%
#   filter(gid_n >= 11) %>%
#   
#   group_by(gid_0) %>%
#   summarise(ntl_cor = cor(ntl_bm_sum, ntl_viirs_sum),
#             ntl_viirs_sum = mean(ntl_viirs_sum)) %>%
#   ungroup() %>%
#   
#   ggplot(aes(x = log(ntl_viirs_sum),
#              y = ntl_cor)) +
#   geom_point() +
#   theme_classic2()

# Global -----------------------------------------------------------------------
#### DMSP
ntl_df %>%
  filter(year <= 2013) %>%
  group_by(year) %>%
  summarise(ntl_dmsp_sum = sum(ntl_dmsp_sum)) %>%
  ungroup() %>%
  mutate(ntl_dmsp_sum = ntl_dmsp_sum / 1000000000,
         ntl_dmsp_sum_rnd = round(ntl_dmsp_sum, 2)) %>%
  
  ggplot(aes(x = year,
             y = ntl_dmsp_sum)) +
  geom_col() +
  geom_text(aes(label = ntl_dmsp_sum_rnd),
            nudge_y = 0.025,
            size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights",
       caption = "Values from DMSP-OLS")

ggsave(filename = file.path(figures_dir,
                            "global_dmsp_sum.png"),
       height = 3, width = 7)

#### DMSP, GF
ntl_df %>%
  filter(year <= 2013) %>%
  group_by(year) %>%
  summarise(ntl_dmsp_sum = sum(ntl_gf_10km_dmsp_sum, na.rm = T)) %>%
  ungroup() %>%
  mutate(ntl_dmsp_sum = ntl_dmsp_sum / 1000000000,
         ntl_dmsp_sum_rnd = round(ntl_dmsp_sum, 4)) %>%
  
  ggplot(aes(x = year,
             y = ntl_dmsp_sum)) +
  geom_col() +
  geom_text(aes(label = ntl_dmsp_sum_rnd),
            nudge_y = 0.001,
            size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights near gas flaring locations",
       caption = "Values from DMSP-OLS")

ggsave(filename = file.path(figures_dir,
                            "global_dmsp_gf_sum.png"),
       height = 3, width = 9)

#### VIIRS
ntl_df %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(ntl_viirs_sum = sum(ntl_viirs_sum)) %>%
  ungroup() %>%
  mutate(ntl_viirs_sum = ntl_viirs_sum / 1000000000,
         ntl_viirs_sum_rnd = round(ntl_viirs_sum, 2)) %>%
  
  ggplot(aes(x = year,
             y = ntl_viirs_sum)) +
  geom_col() +
  geom_text(aes(label = ntl_viirs_sum_rnd),
            nudge_y = 0.025,
            size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights",
       caption = "Values from VIIRS")

ggsave(filename = file.path(figures_dir,
                            "global_viirs_sum.png"),
       height = 3, width = 6)

#### VIIRS: GF
ntl_df %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(ntl_viirs_sum = sum(ntl_gf_10km_viirs_sum, na.rm = T)) %>%
  ungroup() %>%
  mutate(ntl_viirs_sum = ntl_viirs_sum / 1000000000,
         ntl_viirs_sum_rnd = round(ntl_viirs_sum, 4)) %>%
  
  ggplot(aes(x = year,
             y = ntl_viirs_sum)) +
  geom_col() +
  geom_text(aes(label = ntl_viirs_sum_rnd),
            nudge_y = 0.002,
            size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights near gas flaring locations",
       caption = "Values from VIIRS")

ggsave(filename = file.path(figures_dir,
                            "global_viirs_gf_sum.png"),
       height = 3, width = 6)

# Trends by continent ----------------------------------------------------------

#### DMSP
ntl_df %>%
  filter(year <= 2013,
         !is.na(continent),
         continent != "Antarctica") %>%
  group_by(year, continent) %>%
  summarise(ntl_dmsp_sum = sum(ntl_dmsp_sum)) %>%
  ungroup() %>%
  mutate(ntl_dmsp_sum = ntl_dmsp_sum / 1000000000,
         ntl_dmsp_sum_rnd = round(ntl_dmsp_sum, 2)) %>%
  
  ggplot(aes(x = year,
             y = ntl_dmsp_sum)) +
  geom_col() +
  # geom_text(aes(label = ntl_dmsp_sum_rnd),
  # nudge_y = 0.025,
  # size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights",
       caption = "Values from DMSP-OLS") +
  facet_wrap(~continent)

ggsave(filename = file.path(figures_dir,
                            "global_dmsp_continent_sum.png"),
       height = 3, width = 7)

#### VIIRS
ntl_df %>%
  filter(year >= 2012,
         !is.na(continent),
         continent != "Antarctica") %>%
  group_by(year, continent) %>%
  summarise(ntl_viirs_sum = sum(ntl_viirs_sum)) %>%
  ungroup() %>%
  mutate(ntl_viirs_sum = ntl_viirs_sum / 1000000000,
         ntl_viirs_sum_rnd = round(ntl_viirs_sum, 2)) %>%
  
  ggplot(aes(x = year,
             y = ntl_viirs_sum)) +
  geom_col() +
  # geom_text(aes(label = ntl_viirs_sum_rnd),
  #           nudge_y = 0.025,
  #           size = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme_classic2() +
  labs(x = NULL,
       y = "Nighttime lights radiance\n(Values in billions)",
       title = "Global annual nighttime lights",
       caption = "Values from VIIRS") + 
  facet_wrap(~continent)

ggsave(filename = file.path(figures_dir,
                            "global_viirs_continent_sum.png"),
       height = 3, width = 6)

# Change in NTL ----------------------------------------------------------------
#### Prep changes
change_df <- ntl_df %>%
  filter(year %in% c(2012, 2024)) %>%
  pivot_wider(id_cols = c(country, gid_0, continent),
              names_from = year,
              values_from = ntl_viirs_sum) %>%
  mutate(change = `2024` - `2012`,
         pchange = (`2024` - `2012`) / `2012` * 100) %>%
  mutate(change_win = Winsorize(change, probs = c(0.01, 0.99)),
         pchange_win = Winsorize(pchange, probs = c(0.01, 0.99))) %>%
  mutate(growth = log(`2024`) - log(`2012`),
         ntl2012log = log(`2012`))

#### Boxplot
change_df %>%
  filter(!is.na(continent),
         continent != "Antarctica") %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray80") +
  geom_boxplot(aes(x = pchange_win,
                   y = continent),
               fill = "gray70") +
  labs(x = "Percent change",
       y = NULL,
       title = "Percent change in nighttime lights, 2012 to 2024",
       subtitle = "Distribution of country changes across continents",
       caption = "Values Winsorized at 1% and 99% level") +
  theme_classic2()

ggsave(filename = file.path(figures_dir,
                            "viirs_change_boxplot.png"),
       height = 3, width = 6)

#### Boxplot, growth rate
change_df %>%
  filter(!is.na(continent),
         continent != "Antarctica") %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray80") +
  geom_boxplot(aes(x = growth,
                   y = continent),
               fill = "gray70") +
  labs(x = "Growth Rate",
       y = NULL,
       title = "Growth rate of nighttime lights, 2012 to 2024",
       subtitle = "Distribution of country changes across continents") +
  theme_classic2()

ggsave(filename = file.path(figures_dir,
                            "viirs_growth_boxplot.png"),
       height = 3, width = 6)

#### Map, percent change
gadm_ntl_sf <- gadm_sf %>%
  left_join(change_df, by = c("country", "gid_0")) %>%
  filter(!(country %in% c("Antarctica")))

gadm_ntl_sf$pchange_win[gadm_ntl_sf$pchange_win >= 1000]  <- 1000
gadm_ntl_sf$pchange_win[gadm_ntl_sf$pchange_win <= -1000] <- -1000

p <- ggplot() +
  geom_sf(data = gadm_ntl_sf,
          aes(fill = pchange_win)) +
  theme_void() +
  scale_fill_gradient2(labels = c("< -1000",
                                  "-500",
                                  "0",
                                  "500",
                                  "> 1000")) +
  labs(fill = "% Change",
       title = "Percent change in nighttime lights, 2012 to 2024")

ggsave(p,
       filename = file.path(figures_dir,
                            "viirs_change_map.png"),
       height = 2.8, width = 6)

#### Map, growth
gadm_ntl_sf <- gadm_sf %>%
  left_join(change_df, by = c("country", "gid_0")) %>%
  filter(!(country %in% c("Antarctica")))

p <- ggplot() +
  geom_sf(data = gadm_ntl_sf,
          aes(fill = growth)) +
  theme_void() +
  # scale_fill_gradient2(labels = c("< -1000",
  #                                 "-500",
  #                                 "0",
  #                                 "500",
  #                                 "> 1000")) +
  labs(fill = "Growth\nRate",
       title = "Percent change in nighttime lights, 2012 to 2024")

ggsave(p,
       filename = file.path(figures_dir,
                            "viirs_growth_map.png"),
       height = 2.8, width = 6)

#### Change vs initial
change_df %>%
  ggplot(aes(x = ntl2012log,
             y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  stat_cor(method = "pearson", label.x = 13, label.y = 3) +
  theme_classic2() +
  labs(x = "Nighttime lights, 2012 (logged)",
       y = "Nighttime lights growth\nrate, 2012 to 2024",
       title = "Growth rate versus initial nighttime lights",
       caption = "Growth rate calcualted as the difference between log of nighttime lights in 2024 and 2012")

ggsave(filename = file.path(figures_dir,
                            "viirs_change_vs_baseline.png"),
       height = 3, width = 6)
