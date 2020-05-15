## This file produces the plots for the hydra paper.
library(ggplot2)
library(dplyr)
library(forcats)
## install.packages("viridis")
library(viridis)
theme_set(theme_bw())


readData <- function(fp) {
  renameNodes <- function(d) {
    local <- d %>% filter(regions == 'Local') %>%
      mutate(node = fct_recode(
               node, 'Frankfurt' = 'NodeId 0',
               'Frankfurt' = 'NodeId 1',
               'Frankfurt' = 'NodeId 2',
               'Frankfurt' = 'FrankfurtAWS'))
    continental <- d %>% filter(regions == 'Continental') %>%
      mutate(node = fct_recode(
               node,
               'Frankfurt/Ireland' = 'FrankfurtAWS',
               'Frankfurt/Ireland' = 'IrelandAWS',
               'London' = 'LondonAWS',
               'Frankfurt/Ireland' = 'NodeId 0',
               'Frankfurt/Ireland' = 'NodeId 1',
               'London' = 'NodeId 2'))
    global <- d %>% filter(regions == 'Global') %>%
      mutate(node = fct_recode(
               node,
               'Oregon' = 'NodeId 0',
               'Frankfurt/Tokyo' = 'NodeId 1',
               'Frankfurt/Tokyo' = 'NodeId 2',
               'Oregon' = 'OregonAWS',
               'Frankfurt/Tokyo' = 'FrankfurtAWS',
               'Frankfurt/Tokyo' = 'TokyoAWS'))
    bind_rows(local, continental, global)
  }
  d <- read.csv(fp) %>%
    mutate(regions = fct_recode(
             regions,
             'Local' = 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS',
             'Continental' = 'IrelandAWS-FrankfurtAWS-LondonAWS',
             'Global' = 'OregonAWS-FrankfurtAWS-TokyoAWS'),
           concLabel = fct_recode(
             as.factor(conc),
             'Concurrency 1' = '1',
             'Concurrency 2' = '2',
             'Concurrency 5' = '5',
             'Concurrency 10' = '10',
             'Concurrency 20' = '20'),
           blType = fct_recode(
             object,
             'infinite concurrency' = 'hydra-unlimited-infinte-conc-tps',
             'infinite concurrency' = 'full-trust-infinte-conc-tps',
             'infinite concurrency' = 'sprites-unlimited-infinte-conc-tps',
             'finite concurrency' = 'hydra-unlimited-tps',
             'finite concurrency' = 'full-trust-tps',
             'finite concurrency' = 'sprites-unlimited-tps'
           )
           )
  renameNodes(d) %>% filter(conc %in% c(1, 5, 10))
}

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
baseline_hu_inf <- function(d) {
  geom_line(data = subset(d, object=='hydra-unlimited-infinte-conc-tps'), size=1,
            aes(colour = 'Hydra Unlimited', linetype=snapsize))
}
baseline_hu <- function(d) {
  geom_line(data = subset(d, object=='hydra-unlimited-tps'), size=1,
            aes(colour = 'Hydra Unlimited', linetype=snapsize))
}

baseline_ft_inf <- function(d) {
  geom_line(data = subset(d, object=='full-trust-infinte-conc-tps'), size=1,
            aes(colour = 'Universal'))
}
baseline_ft <- function(d) {
  geom_line(data = subset(d, object=='full-trust-tps'), size=1,
            aes(colour = 'Universal'))
}

baseline_sp_inf <- function(d) {
  geom_line(data = subset(d, object=='sprites-unlimited-infinte-conc-tps'), size=1,
            aes(colour = 'Sprites Unlimited'))
}
baseline_sp <- function(d) {
  geom_line(data = subset(d, object=='sprites-unlimited-tps'), size=1,
            aes(colour = 'Sprites Unlimited'))
}

points <- function(d) {
  geom_point(data = subset(d, object=='tps'), size = 2.5)
}

linescale <- scale_linetype_manual('Snapshot size', limits = c('1', '2', '5', '10', 'infinite'), values = c('dotted', 'dashed', 'dotdash', 'longdash', 'solid'))

themeSettings <- theme(legend.position = 'bottom',
                       legend.box = 'vertical',
                       text = element_text(size=32),
                       legend.key.width = unit(2,"cm")
                       )

tpsPlot <- function(d) {
  ggplot(d, aes(x = bandwidth/1024, y = value)) +
    scale_x_log10(name = 'bandwidth [Mbit/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    scale_y_log10(name = 'transaction throughput [tx/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    scale_color_viridis('Baseline', discrete=TRUE,
                        option="magma",
                        ## begin = 0.2,
                        end = 0.8)
    ## scale_color_hue('Baseline')
}



dSimple = readData('csv/simple.csv')
dPlutus = readData('csv/plutus.csv')


## Comparison of the baselines, without latency
tpsPlot(dSimple) +
  baseline_ft_inf(dSimple) +
  baseline_hu_inf(dSimple) +
  baseline_sp_inf(dSimple) +
  linescale + themeSettings + theme(legend.position = 'none') +
  ggtitle('Universal and Hydra Unlimited Baselines',
          subtitle = 'Simple Transactions, Zero Latency')
ggsave('pdf/baselines-nolat-simple.pdf')

## example of finite concurrency
d <- dSimple %>% filter(conc == 5) %>% filter(regions == 'Continental') %>% filter(blType %in% c('finite concurrency', 'infinite concurrency'))
tpsPlot(d) +
  baseline_ft_inf(dSimple) + baseline_hu_inf(dSimple) + baseline_sp_inf(dSimple) +
  baseline_ft(d) + baseline_hu(d) + baseline_sp(d) +
  linescale + themeSettings +
  facet_grid(. ~ blType)
ggsave('pdf/baselines.pdf', width = 16, height = 9)

## +
##   theme(legend.position = c(.95, .05),
##         legend.box = 'vertical',
##         legend.justification = c('right', 'bottom'))




tpsPlot(dPlutus) +
  baseline_ft_inf(dPlutus) +
  baseline_hu_inf(dPlutus) +
  linescale + themeSettings +
  ggtitle('Universal and Hydra Unlimited Baselines',
          subtitle = 'Plutus Transactions, Zero Latency')
ggsave('pdf/baselines-nolat-plutus.pdf')

## Comparison of the baselines, including finite latency
tpsPlot(dSimple) +
  baseline_ft(dSimple) + baseline_hu(dSimple) +
  baseline_sp(dSimple) +
  linescale + themeSettings +
  facet_grid(regions ~ concLabel) +
  ggtitle('Universal and Protocol Specific Baselines',
          subtitle = 'Simple Transactions')
ggsave('pdf/baselines-simple.pdf')

tpsPlot(dPlutus) +
  baseline_ft(dPlutus) + baseline_hu(dPlutus) +
  linescale + themeSettings +
  baseline_sp(dPlutus) +
  facet_grid(regions ~ concLabel) +
  ggtitle('Universal and Protocol Specific Baselines',
          subtitle = 'Script Transactions')
ggsave('pdf/baselines-plutus.pdf')

## Experimental Evaluation
tpsPlot(dSimple) +
  baseline_ft(dSimple) + baseline_hu(dSimple %>% filter(snapsize %in% c('1', '2', 'infinite'))) +
  baseline_sp(dSimple) +
  points(dSimple) +
  linescale + themeSettings +
  facet_grid(regions ~ concLabel, scales = 'free_y') +
  scale_linetype_manual('Snapshot size', limits = c('1', '2', 'infinite'), values = c('dotted', 'dashed', 'solid'))
##  +
  ## ggtitle('Experimental Evaluation',
  ##         subtitle = 'Simple Transactions')
ggsave('pdf/tps-simple.pdf', width = 16, height = 18)

tpsPlot(dPlutus) +
  baseline_ft(dPlutus) + baseline_hu(dPlutus %>% filter(snapsize %in% c('1', '2', 'infinite'))) +
  baseline_sp(dPlutus) +
  points(dPlutus) +
  linescale + themeSettings +
  facet_grid(regions ~ concLabel, scales = 'free_y') +
  scale_linetype_manual('Snapshot size', limits = c('1', '2', 'infinite'), values = c('dotted', 'dashed', 'solid'))##  +
  ## ggtitle('Experimental Evaluation',
  ##         subtitle = 'Script Transactions')
ggsave('pdf/tps-plutus.pdf', width = 16, height = 18)


conftimePlot <- function(d) {
ggplot(d, aes(x = bandwidth/1024, y = value)) +
  geom_point(data = subset(d, object == 'conftime-tx'), aes(colour = node), alpha = 0.1) +
  geom_line(data = subset(d, object == 'min-conftime'), aes(colour = node)) +
  scale_x_log10(name = 'bandwidth [Mbits/s]'
              , breaks = breaks, minor_breaks = minor_breaks) +
  scale_y_log10(name = 'transaction confirmation time [s]',
                breaks = 10^(-2:1), minor_breaks = rep(1:9, 4)*(10^rep(-2:1, each=9))) +
  themeSettings +
  scale_color_viridis('Node Location', discrete=TRUE,
                      option="magma",
                      begin = 0.2,
                      end = 0.8) +
  ## scale_colour_hue('Node Location') +
  guides(color = guide_legend(override.aes = list(linetype = 1, alpha = 1)))##  +
  ## ggtitle('Transaction Confirmation Time')
}

conftimePlot(dSimple %>% filter(regions == 'Local')) +
  ## ggtitle(waiver(), subtitle = 'Simple Transactions, Local Cluster') +
  scale_y_log10(name = 'transaction confirmation time [s]',
                breaks = 10^(-2:1), minor_breaks = rep(1:9, 4)*(10^rep(-2:1, each=9)),
                limits = c(0.01,1)) +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-local-simple.pdf', width = 16, height = 9)

conftimePlot(dSimple %>% filter(regions == 'Continental')) +
  ggtitle(waiver(), subtitle = 'Simple Transactions, Continental Cluster') +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-continental-simple.pdf')

conftimePlot(dSimple %>% filter(regions == 'Global')) +
  ggtitle(waiver(), subtitle = 'Simple Transactions, Global Cluster') +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-global-simple.pdf')



conftimePlot(dPlutus %>% filter(regions == 'Local')) +
  ggtitle(waiver(), subtitle = 'Plutus Transactions, Local Cluster') +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-local-plutus.pdf')

conftimePlot(dPlutus %>% filter(regions == 'Continental')) +
  ggtitle(waiver(), subtitle = 'Plutus Transactions, Continental Cluster') +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-continental-plutus.pdf')

conftimePlot(dPlutus %>% filter(regions == 'Global')) +
  ## ggtitle(waiver(), subtitle = 'Plutus Transactions, Global Cluster') +
  scale_y_log10(name = 'transaction confirmation time [s]',
                breaks = 10^(-2:1), minor_breaks = rep(1:9, 4)*(10^rep(-2:1, each=9)),
                limits = c(0.1,1)) +
  facet_wrap(~ concLabel)
ggsave('pdf/conftime-global-plutus.pdf', width = 16, height = 9)
