
library(ggplot2)
library(RColorBrewer)

theme_set(theme_bw())

blues = brewer.pal(n = 7, "Blues")[3:7]
oranges = brewer.pal(n = 7, "Oranges")[3:7]

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

plot_tps_bandwidth <- function(d, subtitle) {
  p <- ggplot(d, aes(x = bandwidth, y = tps, colour = group)) +
    scale_x_log10(name = 'bandwidth [kb/s]'
                , breaks = 10^(-1:10), minor_breaks = minor_breaks) +
    scale_y_log10(name = 'transaction throughput [transactions/s]'
                , breaks = minor_breaks, minor_breaks = NULL) 
  p +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx')) +
    geom_line(data = subset(d, object=='tx-baseline')) +
    ggtitle('Transaction Throughput',
            sub = subtitle) +
    scale_colour_manual('Concurrency per Node', values = blues)
}

plot_conftime_bandwidth <- function(d, subtitle) {
  p <- ggplot(d, aes(x = bandwidth, y = 100*conftime, colour = group)) +
    scale_x_log10(name = 'bandwidth [kb/s]'
                , breaks = 10^(-1:10), minor_breaks = minor_breaks) +
    scale_y_log10(name = 'transaction confirmation time [ms]'
                , breaks = minor_breaks, minor_breaks = NULL) +
    geom_point(alpha = 0) +
    geom_point(data = subset(d, object=='tx')) +
    geom_line(data = subset(d, object=='tx-baseline'), aes(group = node)) +
    ggtitle('Transaction Confirmation Time',
            sub = subtitle) +
    scale_colour_manual('Concurrency per Node', values = oranges)
  p + facet_wrap(~ conc)
}

plot_conc <- function(d, subtitle, rescale) {

  ggplot(d, aes(x = conc)) +
    geom_point(data = subset(d, object=='tx'),
               aes(y = tps, colour = 'Throughput')) +
    geom_line(data = subset(d, object=='tx-baseline'),
              aes(y = tps, colour = 'Optimal Throughput')) +
    geom_point(data = subset(d, object=='tx'),
               aes(y = rescale * conftime, colour = 'Confirmation Time')) +
    geom_line(data = subset(d, object=='tx-baseline'),
               aes(y = rescale * conftime, colour = 'Optimal Confirmation Time', group = node)) +
    scale_y_continuous(name = 'transaction throughput [transactions/s]',
                       sec.axis = sec_axis(~.*1000/rescale,
                                           name = 'Confirmation Time [ms]')) +
    scale_colour_manual("", values = c(oranges[3], oranges[5], blues[5], blues[3])) +
    ggtitle('Confirmation Time and Throughput over Concurrency per node',
            sub = subtitle) +
    xlab('concurrency per node')

}


data <- read.csv('csv/bandwidth-simple.csv')
data$group <- factor(ifelse(data$object=='tx-baseline', 'limit', data$conc),
                     levels = c('1', '2', '10', '20', 'limit'))

plot_tps_bandwidth(subset(data, regions == 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS'),
                   subtitle = 'Three nodes in same AWS region')
ggsave(filename = 'pdf/throughput-simple-local.pdf', width = 16, height = 9)

plot_tps_bandwidth(subset(data, regions == 'IrelandAWS-FrankfurtAWS-LondonAWS'),
                   subtitle = 'Three nodes in Europe (Ireland, Frankfurt, London)')
ggsave(filename = 'pdf/throughput-simple-europe.pdf', width = 16, height = 9)

plot_tps_bandwidth(subset(data, regions == 'OregonAWS-FrankfurtAWS-TokyoAWS'),
                   subtitle = 'Three nodes, globally distributed (Oregon, Frankfurt, TokyoAWS)')
ggsave(filename = 'pdf/throughput-simple-global.pdf', width = 16, height = 9)

plot_tps_bandwidth(data,
                   subtitle = 'Comparison of geographic distribution of nodes') +
  facet_wrap(~ regions)
ggsave(filename = 'pdf/throughput-simple.pdf', width = 16, height = 9)


plot_conftime_bandwidth(subset(data, regions == 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS'),
                        subtitle = 'Three nodes in same AWS region')
ggsave(filename = 'pdf/conftime-simple-local.pdf', width = 16, height = 9)

plot_conftime_bandwidth(subset(data, regions == 'IrelandAWS-FrankfurtAWS-LondonAWS'),
                   subtitle = 'Three nodes in Europe (Ireland, Frankfurt, London)')
ggsave(filename = 'pdf/conftime-simple-europe.pdf', width = 16, height = 9)

plot_conftime_bandwidth(subset(data, regions == 'OregonAWS-FrankfurtAWS-TokyoAWS'),
                   subtitle = 'Three nodes, globally distributed (Oregon, Frankfurt, TokyoAWS)')
ggsave(filename = 'pdf/conftime-simple-global.pdf', width = 16, height = 9)






plutusdata <- read.csv('csv/bandwidth-plutus.csv')
plutusdata$group <- factor(ifelse(plutusdata$object=='tx-baseline', 'limit', plutusdata$conc),
                     levels = c('1', '2', '10', '20', 'limit'))

plot_tps_bandwidth(subset(plutusdata, regions == 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS'),
                   subtitle = 'Three nodes in same AWS region -- Plutus Transactions')
ggsave(filename = 'pdf/throughput-plutus-local.pdf', width = 16, height = 9)

plot_tps_bandwidth(subset(plutusdata, regions == 'IrelandAWS-FrankfurtAWS-LondonAWS'),
                   subtitle = 'Three nodes in Europe (Ireland, Frankfurt, London) -- Plutus Transactions')
ggsave(filename = 'pdf/throughput-plutus-europe.pdf', width = 16, height = 9)

plot_tps_bandwidth(subset(plutusdata, regions == 'OregonAWS-FrankfurtAWS-TokyoAWS'),
                   subtitle = 'Three nodes, globally distributed (Oregon, Frankfurt, TokyoAWS) -- Plutus Transactions')
ggsave(filename = 'pdf/throughput-plutus-global.pdf', width = 16, height = 9)

plot_tps_bandwidth(plutusdata,
                   subtitle = 'Comparison of geographic distribution of nodes -- Plutus Transactions') +
  facet_wrap(~ regions)
ggsave(filename = 'pdf/throughput-plutus.pdf', width = 16, height = 9)


plot_conftime_bandwidth(subset(plutusdata, regions == 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS'),
                        subtitle = 'Three nodes in same AWS region -- Plutus Transactions')
ggsave(filename = 'pdf/conftime-plutus-local.pdf', width = 16, height = 9)

plot_conftime_bandwidth(subset(plutusdata, regions == 'IrelandAWS-FrankfurtAWS-LondonAWS'),
                        subtitle = 'Three nodes in Europe (Ireland, Frankfurt, London) -- Plutus Transactions')
ggsave(filename = 'pdf/conftime-plutus-europe.pdf', width = 16, height = 9)

plot_conftime_bandwidth(subset(plutusdata, regions == 'OregonAWS-FrankfurtAWS-TokyoAWS'),
                        subtitle = 'Three nodes, globally distributed (Oregon, Frankfurt, TokyoAWS) -- Plutus Transactions')
ggsave(filename = 'pdf/conftime-plutus-global.pdf', width = 16, height = 9)




concdata <- read.csv('csv/conc-simple.csv')
concdata$group <- factor(ifelse(concdata$object=='tx-baseline', 'limit', concdata$conc),
                         levels = c('1', '2', '5', '10', '20', '40', 'limit'))

plot_conc(subset(concdata, regions == 'FrankfurtAWS-FrankfurtAWS-FrankfurtAWS'),
          subtitle = 'Three nodes in same AWS region',
          rescale = 100)
ggsave(filename = 'pdf/tradeoff-simple-local.pdf', width = 16, height = 9)

plot_conc(concdata,
          subtitle = 'Comparison of geographic distribution of nodes',
          rescale = 100) +
  facet_wrap(~ regions)
ggsave(filename = 'pdf/tradeoff-simple.pdf', width = 16, height = 9)


