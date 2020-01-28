basicplot <- function () {

  plot <- function(d, rescale) {

    breaks <- 10^(-10:10)
    minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

    ggplot(d, aes(x = bandwidth, shape = conc)) +
      scale_x_log10(name = 'bandwidth [kb/s]', breaks = 10^(-1:10), minor_breaks = minor_breaks) +
      geom_point(aes(y = tps, colour = 'Throughput')) +
      geom_point(aes(y = rescale*conftime,
                     colour = 'Confirmation Time [ms]')) +
      scale_y_continuous(name = 'Throughput [tx/s]', sec.axis = sec_axis(~.*1000/rescale, name = "Confirmation Time [ms]")) +
      facet_grid(txtype ~ 'Three nodes in one AWS region, limit of one inflight tx per node', scales = 'free_y')
  }

  data <- read.csv('csv/basic.csv')
  data$conc <- as.factor(data$conc)

  plot(subset(data, (object=='tx') & (txtype=='Simple')), 1000)
  ggsave(filename = 'pdf/basic-simple.pdf', width = 16, height = 9)

  plot(subset(data, (object=='tx') & (txtype=='Plutus')), 10)
  ggsave(filename = 'pdf/basic-simple.pdf', width = 16, height = 9)
}

concplot <- function () {

  plot <- function (d, rescale) {
    ggplot(d, aes(x = conc)) +
      scale_x_continuous(name = 'limit on in-flight txs per node', breaks = seq(0,10,1)) +
      geom_point(aes(y = tps, colour = 'Throughput')) +
      geom_point(aes(y = rescale*conftime,
                     colour = 'Confirmation Time [ms]')) +
      scale_y_continuous(name = 'Throughput [tx/s]', sec.axis = sec_axis(~.*1000/rescale, name = "Confirmation Time [ms]")) +
      facet_grid(bandwidth ~ regions, scales = 'free_y')
  }

  data <- read.csv('csv/conc.csv')
  data$bandwidth <- as.factor(data$bandwidth)

  plot(subset(data, (object=='tx') & (regions=='FrankfurtAWS-FrankfurtAWS-FrankfurtAWS') & (txtype=='Simple')), 1000)
  ggsave('pdf/conc-local-simple.pdf')
  plot(subset(data, (object=='tx') & (regions!='FrankfurtAWS-FrankfurtAWS-FrankfurtAWS') & (txtype=='Simple')), 1000)
  ggsave('pdf/conc-global-simple.pdf')
  plot(subset(data, (object=='tx') & (regions=='FrankfurtAWS-FrankfurtAWS-FrankfurtAWS') & (txtype=='Plutus')), 500)
  ggsave('pdf/conc-local-plutus.pdf')
  plot(subset(data, (object=='tx') & (regions!='FrankfurtAWS-FrankfurtAWS-FrankfurtAWS') & (txtype=='Plutus')), 100)
  ggsave('pdf/conc-global-plutus.pdf')
}



basicplot()
concplot()
