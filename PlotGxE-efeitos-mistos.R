library(lme4)
library(ggplot2)
library(gridExtra)
library(grid)

# dados fictícios
cultivares <- rep(1:5, each = 5)
locais <- rep(1:5, times = 5)
medias <- c(95, 86, 73, 97, 83, 71, 59, 77, 95, 85, 68, 91, 82, 85, 64, 72, 81, 65, 81, 98, 75, 66, 71, 65, 63)
dados <- data.frame(cultivares, locais, medias) 

#------------------------------------------------------------#
# Modelo Misto - Intercepto aleatório e inclinação fixa
M1 <- lmer(medias ~ locais + (1|cultivares), data=dados)          # Tipos de interação: Ausência

intercepto = fixef(M1)["(Intercept)"]
inclinacao = fixef(M1)["locais"]

plot1=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M1), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot A", y="", title="Random intercept and fixed slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))
#------------------------------------------------------------#
# Modelo Misto - Intercepto aleatório e inclinação aleatória
M2 <- lmer(medias ~ locais + (1 + locais|cultivares), data=dados)       # Tipos de interação: simples

intercepto = fixef(M2)["(Intercept)"]
inclinacao = fixef(M2)["locais"]

plot2=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M2), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot B", y="", title="Random intercept and random slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))

#------------------------------------------------------------#
# Modelo Misto - Intercepto fixo e inclinação aleatória
M3 <- lmer(medias ~ locais + (0 + locais|cultivares), data=dados)      # Tipos de interação: Complexa

intercepto = fixef(M3)["(Intercept)"]
inclinacao = fixef(M3)["locais"]

plot3=ggplot(dados, aes(locais, medias, color=cultivares)) +
  geom_point(aes(color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_line(aes(y=predict(M3), group=cultivares, color=factor(cultivares)), size=0.7, show.legend=F) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size=0.7) +
  labs(x="Plot C", y="", title="Fixed intercept and random slope") +
  theme(plot.title=element_text(size=10)) +
  theme(axis.title = element_text(size=10))

grid.arrange(plot1, plot2, plot3, ncol=2)
