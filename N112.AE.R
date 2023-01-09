library(AquaEnv)
library(ggplot2)

df <- read.table("N112_subset_110909.txt", header=TRUE, na.strings=c(".", "NA"))

df <- df[, c(1, 10:11, 13:16, 22)]
names(df) <- c("ID", "pCO2", "CO2", "DIC", "pH", "ALK", "TOC", "temp")
df$DIC <- df$DIC / 12.01 # To umol/L

plot(df[, 2:6])

ggplot(df, aes(x=DIC, y=CO2, col=pH)) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()
ggplot(df, aes(x=DIC, y=CO2, col=log10(TOC))) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()

ae.DIC <- with(df, aquaenv(S=0, t=temp, SumCO2=DIC / 1e6, pH=pH))
df$fCO2.DIC <- ae.DIC$fCO2 * 1e6

ae.ALK <- with(df, aquaenv(S=0, t=temp, SumCO2=NULL, TA=ALK / 1e6, pH=pH))
df$fCO2.ALK <- ae.ALK$fCO2 * 1e6

ggplot(df, aes(x=pCO2, y=fCO2.ALK, col=pH)) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()
ggplot(df, aes(x=pCO2, y=fCO2.ALK, col=log10(TOC))) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()
ggplot(df, aes(x=pCO2, y=fCO2.ALK, col=(ALK == 0))) +
  geom_point() + geom_abline(slope=1, intercept=0) 

ggplot(df, aes(x=pCO2, y=fCO2.DIC, col=pH)) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()
ggplot(df, aes(x=pCO2, y=fCO2.DIC, col=log10(TOC))) +
  geom_point() + geom_abline(slope=1, intercept=0) + scale_color_viridis_c()
ggplot(df, aes(x=pCO2, y=fCO2.DIC, col=(ALK == 0))) +
  geom_point() + geom_abline(slope=1, intercept=0) 


