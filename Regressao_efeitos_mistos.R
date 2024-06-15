library(lme4)
library(ggplot2)
library(gridExtra)
library(grid)

# dados fictícios
cultivares <- rep(1:5, each = 5)
locais <- rep(1:5, times = 5)
medias <- c(95, 86, 73, 97, 83, 71, 59, 77, 95, 85, 68, 91, 82, 85, 64, 72, 81, 65, 81, 98, 75, 66, 71, 65, 63)
dados <- data.frame(cultivares, locais, medias)

# ajustar o modelo de regressão linear (Intercepto fixo)
M1 <- lm(medias ~ locais, data=dados)

plot1=ggplot(dados, aes(locais, medias)) +
  geom_point(size=0.7) +
  geom_line(aes(y=predict(M1)), size=0.7) +
  labs(x="Plot A", y="", title="Fixed intercept") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))
#------------------------------------------------------------#
# Modelo Misto - Intercepto aleatório e inclinação fixa
M2 <- lmer(medias ~ locais + (1|cultivares), data=dados) 

intercepto = fixef(M2)["(Intercept)"]
inclinacao = fixef(M2)["locais"]

plot2=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M2), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot B", y="", title="Random intercept and fixed slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))
#------------------------------------------------------------#
# Modelo Misto - Intercepto aleatório e inclinação aleatória
M3 <- lmer(medias ~ locais + (1 + locais|cultivares), data=dados) 

intercepto = fixef(M3)["(Intercept)"]
inclinacao = fixef(M3)["locais"]

plot4=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M3), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot D", y="", title="Random intercept and random slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))

#------------------------------------------------------------#
# Modelo Misto - Intercepto fixo e inclinação aleatória
M4 <- lmer(medias ~ locais + (0 + locais|cultivares), data=dados)

intercepto = fixef(M4)["(Intercept)"]
inclinacao = fixef(M4)["locais"]

plot3=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M4), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot C", y="", title="Fixed intercept and random slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
