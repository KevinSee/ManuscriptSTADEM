#------------------------------------------------------------------------------
# Script loads STADEM model runs, and creates time-series plots of model estimates,
# window counts and trap estimates for each species / year combination
#
# Author: Kevin See
#------------------------------------------------------------------------------

# load needed packages
library(tidyverse)
library(magrittr)

#------------------------------------------------------------------------------
# get plotting data from multiple years
plot_df = crossing(Species = c("Chinook",
                               "Steelhead"),
                   Year = 2010:2019) %>%
  mutate(plot_title = paste(Species, Year)) %>%
  rowwise() %>%
  mutate(plot_data = map2(Species,
                          Year,
                          .f = function(x, y) {
                            load(paste0('analysis/data/derived_data/STADEM_results/LGR_STADEM_', Species, '_', Year, '.rda'))
                            plot_df = stadem_mod$summary[grep('^X.all', rownames(stadem_mod$summary)),] %>%
                              as_tibble(rownames = 'var') %>%
                              mutate(week = as.integer(str_extract(var, "[0-9]+")),
                                     param = str_extract_all(var, "[:alpha:]+", simplify = T)[,3],
                                     param = ifelse(param == '', 'all', param)) %>%
                              select(var, param, week, everything()) %>%
                              left_join(stadem_list$weeklyData %>%
                                          rename(week = week_num) %>%
                                          rowwise() %>%
                                          mutate(trap_lowCI = max(0, trap_est + trap_est_se * qnorm(0.025)),
                                                 trap_uppCI = trap_est + trap_est_se * qnorm(0.975)) %>%
                                          ungroup())%>%
                              mutate_at(vars(trap_est),
                                        list(~ if_else(. == Inf,
                                                       as.numeric(NA),
                                                       .))) %>%
                              select(-Species)
                            return(plot_df)
                          })) %>%
  unnest(cols = plot_data) %>%
  select(Species, Year, plot_title, everything()) %>%
  filter(param == "all")

# drop initial weeks when window counts == 0
plot_df %<>%
  group_by(Species, Year) %>%
  filter(Start_Date >= min(Start_Date[win_cnt > 0]))

# scale some variables for plotting
plot_df %<>%
  mutate_at(vars(trap_lowCI,
                 trap_uppCI,
                 `2.5%`,
                 `97.5%`,
                 win_cnt,
                 trap_est,
                 `50%`),
            list(~ . / 10000))


#------------------------------------------------------------------------------
# create figures
# set size of points
pt_size = 4

for(spp in unique(plot_df$Species)) {

  # color version
  ts_col = plot_df %>%
    filter(Species == spp) %>%
    ggplot(aes(x = Start_Date)) +
    geom_ribbon(aes(ymin = trap_lowCI,
                    ymax = trap_uppCI,
                    fill = "Trap Est."),
                alpha = 0.4) +
    geom_ribbon(aes(ymin = `2.5%`,
                    ymax = `97.5%`,
                    fill = "Model"),
                alpha = 0.4) +
    geom_point(aes(y = win_cnt,
                   shape = 'Window Count',
                   color = 'Window Count'),
               size = pt_size) +
    geom_point(aes(y = trap_est,
                   shape = 'Trap Est.',
                   color = 'Trap Est.'),
               size = pt_size) +
    geom_line(aes(y = `50%`,
                  color = 'Model')) +
    geom_point(aes(y = `50%`,
                   shape = 'Model',
                   color = 'Model'),
               size = pt_size) +
    scale_color_manual(values = c('Model' = 'black',
                                  'Window Count' = 'blue',
                                  'Trap Est.' = 'red'),
                       name = "Source") +
    scale_fill_manual(values = c('Model' = 'black',
                                 'Window Count' = 'blue',
                                 'Trap Est.' = 'red'),
                      guide = "none") +
    scale_shape_manual(values = c("Model" = 18,
                                  "Window Count" = 1,
                                  "Trap Est." = 8),
                       name = "Source") +
    theme_pubr() +
    theme(legend.position = 'bottom') +
    facet_wrap(~ plot_title,
               scales = "free",
               ncol = 2) +
    labs(x = 'Date',
         y = 'Total Escapement (x 10,000)')

  # black and white version
  ts_bw = ts_col +
    scale_color_grey(start = 0,
                     end = 0,
                     name = "Source") +
    scale_fill_manual(values = c('Model' = 'gray20',
                                 'Trap Est.' = 'gray70'),
                      guide = "none")

  # save
  ggsave(paste0('analysis/figures/time_series_', spp, ".pdf"),
         ts_col,
         width = 8,
         height = 11)

  rm(ts_col, ts_bw)
}
