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

#Datos Estadísticos del total de defunciones en Mujeres
summarise(df, sum = sum(DefFem), mean= mean(DefFem), 
          mediana = median(DefFem), variance = var(DefFem), 
          standardeviation = sd(DefFem))

#Datos Estadísticos del total de defunciones en Hombres
summarise(dm, sum = sum(DefMas), mean= mean(DefMas), 
          mediana = median(DefMas), variance = var(DefMas), 
          standardeviation = sd(DefMas))


value <- as.numeric(summarise(dm, median(DefMas)))
standardeviation <- as.numeric(summarise(dm, sd(DefMas)))
samplesize <- as.numeric(nrow(dm))
samplemean <- as.numeric(summarise(dm, mean(DefMas)))

z <- (samplemean-value)/(standardeviation/sqrt(samplesize))
z

2*pnorm(-abs(z))

2*(1-pnorm(samplemean,mean=value,sd=standardeviation/sqrt(samplesize)))

t.test(df, dm)

t.test(df, dm, var.equal = TRUE)

#Contraste de hipótesis de dos muestras aleatorias:
sdf <- sample_n(df, 10)
sdm <- sample_n(dm, 10)

t.test(sdf, sdm, var.equal = TRUE)

#Cálculo de datos para ambas muestras
s1 <- as.numeric(summarise(df, var(DefFem)))
n1 <- as.numeric(nrow(df))
x1 <- as.numeric(summarise(df, mean(DefFem)))

s2 <- as.numeric(summarise(dm, var(DefMas)))
n2 <- as.numeric(nrow(dm))
x2 <- as.numeric(summarise(dm, mean(DefMas)))



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
  ggtitle("Total de Defunciones") +
  labs(y = "Densidad", x = "Defunciones") +  
  geom_vline(data=td, aes(xintercept=as.numeric(summarise(td, mean(TotalDef)))),
             linetype="dashed", size=1)


dfm <- data.frame(x = df, y = dm)
dfm <- stack(dfm)

dx <- density(df$DefFem)
dy <- density(dm$DefMas)

ggplot(dfm, aes(x = values, fill = ind)) + 
  ggtitle("Número de defunciones por sexo") +
  labs(y = "Densidad", x = "Defunciones") +  
  geom_density(alpha = 0.5) + # Densities with transparency
  xlim(c(min(dx$x, dy$x), # X-axis limits
         c(max(dx$x, dy$x)))) +
  scale_fill_discrete(name = "Sexo", # Change legend title
                      labels = c("Femeninas", "Masculinas")) # + # Change default legend labels
# theme(legend.position = "none") # Delete legend


# Equivalent
ggplot(dfm, aes(x = values)) +
  geom_density(aes(group = ind, fill = ind), alpha = 0.5) + 
  xlim(c(min(dx$x, dy$x), c(max(dx$x, dy$x)))) +
  ggtitle("Número de defunciones por sexo") +
  labs(y = "Densidad", x = "Defunciones") +
  scale_fill_discrete(name = "Sexo", 
                      labels = c("Femeninas", "Masculinas"))+
  geom_vline(data=dfm, aes(xintercept=as.numeric(summarise(dfm, mean(values))),  colour=ind),
             linetype="dashed", size=1)