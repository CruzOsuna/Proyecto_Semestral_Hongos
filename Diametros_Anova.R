###Diametros

Diametros=read.csv(file.choose(),head=T)
Diametros
ls(Diametros)
str(Diametros)

tapply(Diametros$Diametros,Diametros$Tratamiento,mean)
tapply(Diametros$Diametros,Diametros$Tratamiento,var)
tapply(Diametros$Diametros,Diametros$Tratamiento,length)


boxplot(Diametros$Diametros~Diametros$Tratamiento)
library(ggplot2)
ggplot(data = Diametros, aes(x = Tratamiento, y = Diametros, color = Tratamiento)) +
        geom_boxplot() +
        theme_bw()

aov.Diametros=aov(Diametros$Diametros~Diametros$Tratamiento)
summary(aov.Diametros) 

plot(aov.Diametros)

tukey<-TukeyHSD(aov.Diametros)
tukey

plot(TukeyHSD(aov.Diametros))

library(ggplot2)
ggplot(data = Diametros, aes(x = Tratamiento, y = Diametros, color = Tratamiento)) +
        geom_boxplot() +
        theme_bw()

library(multcompView)
library(dplyr)
library(datasets)

cld <- multcompLetters4(aov.Diametros, tukey)
print(cld)

ggplot(Diametros, aes(Tratamiento, Diametros)) + 
        geom_boxplot(aes(fill = Tratamiento)) +
        labs(x="Tratamiento", y="Diametros (mm)") +
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 




#######################
ggplot(data = Diametros, aes(x = Tratamiento, y = Diametros, color = Tratamiento)) +
        geom_boxplot() +
        theme_classic()
