#Bibliotecas
library(dplyr)
library(ggplot2)
#Lectura de archivos con información

#Total de defunciones
td = read.csv(file = 'https://raw.githubusercontent.com/earandir/AnalisisInterpretacionDatos/main/Actividad04/TotalDefunciones.csv')
#Defunciones Femeninas
df = read.csv(file = 'https://raw.githubusercontent.com/earandir/AnalisisInterpretacionDatos/main/Actividad04/DefuncionesFemeninas.csv')
#DefuncionesMasculinas
dm = read.csv(file = 'https://raw.githubusercontent.com/earandir/AnalisisInterpretacionDatos/main/Actividad04/DefuncionesMasculinas.csv')

#Datos Estadísticos del total de defunciones
summarise(td, sum = sum(TotalDef), mean= mean(TotalDef), 
          mediana = median(TotalDef), variance = var(TotalDef), 
          standardeviation = sd(TotalDef))


value <- as.numeric(summarise(td, median(TotalDef)))
standardeviation <- as.numeric(summarise(td, sd(TotalDef)))
samplesize <- as.numeric(nrow(td))
samplemean <- as.numeric(summarise(td, mean(TotalDef)))

z <- (samplemean-value)/(standardeviation/sqrt(samplesize))
z

2*pnorm(-abs(z))

2*(1-pnorm(samplemean,mean=value,sd=standardeviation/sqrt(samplesize)))

t.test(df, dm)


# Kernel Density Plot
d <- density(td$TotalDef) # returns the density data
plot(d) # plots the results

# Density curve
ggplot(td, aes(x=TotalDef)) + geom_density()

# Density plots with semi-transparent fill
ggplot(td, aes(x=TotalDef, fill='Defunciones')) + geom_density(alpha=.3)

# Density plots with means
ggplot(td, aes(x=TotalDef, fill='Def')) +
  geom_density(alpha=.3) +
  geom_vline(data=td, aes(xintercept=as.numeric(summarise(td, mean(TotalDef)))),
             linetype="dashed", size=1)


dfm <- data.frame(x = df, y = dm)
dfm <- stack(dfm)

dx <- density(df$DefFem)
dy <- density(dm$DefMas)

ggplot(dfm, aes(x = values, fill = ind)) + 
  geom_density(alpha = 0.5) + # Densities with transparency
  xlim(c(min(dx$x, dy$x), # X-axis limits
         c(max(dx$x, dy$x)))) +
  scale_fill_discrete(name = "Defunciones", # Change legend title
                      labels = c("Femeninas", "Masculinas")) # + # Change default legend labels
# theme(legend.position = "none") # Delete legend


# Equivalent
ggplot(dfm, aes(x = values)) +
  geom_density(aes(group = ind, fill = ind), alpha = 0.5) + 
  xlim(c(min(dx$x, dy$x), c(max(dx$x, dy$x)))) +
  scale_fill_discrete(name = "Defunciones", 
                      labels = c("Femeninas", "Masculinas"))+
  geom_vline(data=dfm, aes(xintercept=as.numeric(summarise(dfm, mean(values))),  colour=ind),
             linetype="dashed", size=1)