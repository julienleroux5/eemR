rm(list = ls())

file <- system.file("extdata/cary/scans_day_1", "sample1.csv", package = "eemR")
eem <- eem_read(file) %>%
  eem_remove_scattering(type = "raman", order = 1, width = 20) %>%
  eem_remove_scattering(type = "rayleigh", order = 1, width = 20)

plot(eem)

eem2 <- eem_interpolate_scattering(eem)
plot(eem2)
