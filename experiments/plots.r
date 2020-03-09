## This file produces the plots for the hydra paper.
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(forcats)

theme_set(theme_bw())

blues = brewer.pal(n = 7, "Blues")[3:7]
oranges = brewer.pal(n = 7, "Oranges")[3:7]

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

baseline1 <- 'Hydra Unlimited'
baseline2 <- 'Full Trust'

plot_tps_bandwidth <- function(d, subtitle) {
  p <- ggplot(d, aes(x = bandwidth, y = tps, colour = group## , shape = group
                     ))
  p +
    scale_x_log10(name = 'bandwidth [kbits/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    scale_y_log10(name = 'transaction throughput [tx/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx'), aes(shape = group), size = 3) +
    geom_line(data = subset(d, object=='tx-baseline'), aes(linetype=baseline1), colour = 'black') +
    geom_line(data = subset(d, object=='tx-baseline2'), aes(colour = group, linetype=baseline2)) +
    scale_colour_manual('Concurrency per Node', values = blues[1:4], limits = c('1', '2', '10', '20')) +
    scale_shape_discrete('Concurrency per Node') +
    scale_linetype_manual('Baseline Scenario', values = c('solid', 'dashed')) +
    theme(legend.position = 'bottom', legend.box = 'vertical')+
    theme(text = element_text(size=28)) +
    guides(color = guide_legend(override.aes = list(linetype = 0)))
}

readData <- function(fp) {
  data <- read.csv(fp) %>%
    mutate(regions = fct_recode(
             regions,
             'Local' = 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS',
             'Continental' = 'IrelandAWS-FrankfurtAWS-LondonAWS',
             'Global' = 'OregonAWS-FrankfurtAWS-TokyoAWS'),
           concLabel = fct_recode(
             as.factor(conc),
             'Concurrency 1' = '1',
             'Concurrency 2' = '2',
             'Concurrency 10' = '10',
             'Concurrency 20' = '20'))
  data$group <- factor(ifelse(data$object=='tx-baseline', baseline1, data$conc),
                       levels = c('1', '2', '10', '20', baseline1))
  data
}

dataBWSimple <- readData('csv/bandwidth-simple.csv')
plot_tps_bandwidth(subset(dataBWSimple, regions %in% c('Local', 'Continental', 'Global')),
                   subtitle = 'Comparison of geographic distribution of nodes') +
  facet_wrap(~ regions)
ggsave(filename = 'throughput-simple.png', width = 16, height = 9)


plot_tps_bandwidthMBitS <- function(d, subtitle) {
  p <- ggplot(d, aes(x = bandwidth/1024, y = tps, colour = group))
  p +
    scale_x_log10(name = 'bandwidth [Mbits/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    scale_y_log10(name = 'transaction throughput [tx/s]'
                , breaks = breaks, minor_breaks = minor_breaks) +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx'), aes(shape = group), size = 3) +
    geom_line(data = subset(d, object=='tx-baseline'), aes(linetype=baseline1), colour = 'black') +
    geom_line(data = subset(d, object=='tx-baseline2'), aes(colour = group, linetype=baseline2)) +
    scale_colour_manual('Concurrency per Node', values = blues[1:4], limits = c('1', '2', '10', '20')) +
    scale_shape_discrete('Concurrency per Node') +
    scale_linetype_manual('Baseline Scenario', values = c('solid', 'dashed')) +
    theme(legend.position = 'bottom', legend.box = 'vertical')+
    theme(text = element_text(size=28)) +
    guides(color = guide_legend(override.aes = list(linetype = 0)))
}

dataBWPlutus <- readData('csv/bandwidth-plutus.csv')
plot_tps_bandwidthMBitS(subset(dataBWPlutus, regions %in% c('Local', 'Continental', 'Global')),
                   subtitle = 'Comparison of geographic distribution of nodes') +
  facet_wrap(~ regions) +
  coord_cartesian(xlim = c(4e-1, 2000))
ggsave(filename = 'throughput-plutus.png', width = 16, height = 9)



plot_conftime_bandwidth <- function(d) {
  p <- ggplot(d, aes(x = bandwidth, y = conftime)) +
    scale_x_log10(name = 'bandwidth [kbits/s]'
                , breaks = 10^(-1:10), minor_breaks = minor_breaks) +
    scale_y_continuous(name = 'transaction confirmation time [s]') +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx'), alpha = 0.1) +
    geom_line(data = subset(d, object=='tx-baseline'), aes(group = node), linetype = 'dashed') +
    scale_linetype_manual('Baseline', values = c('dashed')) +
    facet_wrap(~ concLabel) +
    theme(text = element_text(size=28)) +
    theme(legend.position = 'bottom')
  p
}

plot_conftime_bandwidth(subset(dataBWSimple, regions == 'Local' & conftime < 1))
ggsave(filename = 'conftime-simple-local.png', width = 16, height = 9)

plot_conftime_bandwidth_col <- function(d) {
  p <- ggplot(d, aes(x = bandwidth/1024, y = conftime)) +
    scale_x_log10(name = 'bandwidth [Mbits/s]'
                , breaks = 10^(-1:10), minor_breaks = minor_breaks) +
    scale_y_continuous(name = 'transaction confirmation time [s]') +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx'), alpha = 0.1, aes(colour = node), position = position_jitter(width = 0.01, height = 0)) +
    geom_line(data = subset(d, object=='tx-baseline'), aes(group = node, colour = node), linetype = 'dashed') +
    scale_color_discrete('Node Location') +
    guides(color = guide_legend(override.aes = list(linetype = 'dashed', alpha = 1)))
  p + facet_wrap(~ concLabel) +
    theme(text = element_text(size=28)) +
    theme(legend.position = 'bottom')
}

dataGlobalPlutus <- subset(dataBWPlutus, regions == 'Global') %>% 
  mutate(node = fct_recode(
           node,
           'Oregon' = 'NodeId 0',
           'Frankfurt/Tokyo' = 'NodeId 1',
           'Frankfurt/Tokyo' = 'NodeId 2',
           'Oregon' = 'OregonAWS',
           'Frankfurt/Tokyo' = 'FrankfurtAWS',
           'Frankfurt/Tokyo' = 'TokyoAWS'
         ))

plot_conftime_bandwidth_col(subset(dataGlobalPlutus, conftime < 1)) + coord_cartesian(xlim = c(4e-1, 2000))
ggsave(filename = 'conftime-plutus-global.png', width = 16, height = 9)
