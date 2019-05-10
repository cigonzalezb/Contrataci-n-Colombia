
if (!require("data.table")) install.packages("data.table")
library(data.table)

if (!require("bit64")) install.packages("bit64")
library(bit64)

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if (!require("devtools")) install.packages("devtools")
library(devtools)

install_github("easyGgplot2", "kassambara")

if(!require(igraph)) install.packages("igraph",repos = "http://cran.us.r-project.org")
library("igraph")


rm(list = ls())
gc()

setwd("/home/smartinez/Dropbox/0. UoG/Projects")

#cont <- read.csv("file:///home/smartinez/Downloads/redes-de-contratacion.csv", header = TRUE)
#head(cont)

#secopI <- data.table::fread("file:///home/smartinez/Downloads/SECOP_I.csv", header = TRUE)
#save(secopI, file = "secopI.RData")
load(file = "secopI.RData")
names(secopI)



# Exploring the variables

# Nombre de la entidad
length(unique(secopI$`Nombre de la Entidad`))

# Nombre de la razon social del contratista
length(unique(secopI$`Nom Raz Social Contratista`))

# Cross Nivel de la Entidad X Orden Entidad
secopI%>%
  group_by(`Nivel Entidad`, `Orden Entidad`)%>%
  summarize(n=n())

# Tipo de proceso
unique(secopI$`Tipo de Proceso`)

# Estado del Proceso
unique(secopI$`Estado del Proceso`)

# Regimen de contratacion
unique(secopI$`Regimen de Contratacion`)

# Objeto a contratar
unique(secopI$`Objeto a Contratar`)
  
# Quantia

summary(secopI$`Cuantia Contrato`)
#normcontratos <- secopI[secopI$`Cuantia Contrato`<1000000000000]
#rm(normcontratos)

#normcontratos <- normcontratos[normcontratos$`Cuantia Contrato`<1000000000]
#hist(normcontratos$`Cuantia Contrato`, breaks = 100)
#summary(secopI$`Cuantia Contrato`)
#boxplot(secopI$`Cuantia Contrato`)
#supercontratos <- secopI[secopI$`Cuantia Contrato`>1000000000000]
#supercontratos[16,]

# 
unique(secopI$`Calificacion Definitiva`)


####################################

# Solo los contratos celebrados
secopI <-secopI[secopI$`Estado del Proceso` == "Celebrado"]



# Division por regimen de contratacion

# Regimen de contratacion
#[X] "Estatuto General de Contratación"
#[X] "Ley 80 de 1993"
#[] "Régimen Especial"
#[] ""    

# 16 proyectos misteriosos, donde el regimen de contratacion no es del todo claro
secopI <- secopI[!(secopI$`Regimen de Contratacion` == "")] 

# Secop - Regimen de contratacion: Regimen Especial
secopespecial <- secopI[secopI$`Regimen de Contratacion` == "Régimen Especial"]
hist(log(secopespecial$`Cuantia Contrato`))
quant_secopespecial <- quantile(secopespecial$`Cuantia Contrato`, probs = seq(from = 0, to = 1, by = 0.1))
save(secopespecial, file = "SECOPespecial.RData")

# Secop -  Regimen de contratacion: Estatuto General de contratacion, y Ley 80
secopestatuto80 <- secopI[(secopI$`Regimen de Contratacion` == "Estatuto General de Contratación") | (secopI$`Regimen de Contratacion` == "Ley 80 de 1993")]
hist(log(secopestatuto80$`Cuantia Contrato`))
save(secopestatuto80, file = "SECOPestatuto80.RData")

####################################
# SECOP ESPECIAL
####################################

rm(list = ls())
gc()
load("../SECOPespecial.RData")
# Cargando bienes y servicios
bienesyservicios <- read.delim("bienesyservicios.txt", 
                               header = TRUE, sep = "/")

bienesyservicios$`Objeto a Contratar` <- as.character(bienesyservicios$Nombre)

bienesyservicios <- bienesyservicios[, c("Objeto a Contratar", "Grupo", "Sector")]


# Seleccionando nivel de entidad nacional y nacional descentralizado
secopespecial %>%
  group_by(`Nivel Entidad`, `Orden Entidad`)%>%
  summarize(n=n())

secopespecial <- secopespecial[secopespecial$`Nivel Entidad` == "NACIONAL"]
secopespecial <- secopespecial[(secopespecial$`Orden Entidad` == "NACIONAL DESCENTRALIZADO") | (secopespecial$`Orden Entidad` == "NACIONAL CENTRALIZADO")]

descentral <-   secopespecial[(secopespecial$`Orden Entidad` == "NACIONAL DESCENTRALIZADO") ]
central <-   secopespecial[(secopespecial$`Orden Entidad` == "NACIONAL CENTRALIZADO") ]
rm(secopespecial)
gc()


# Juntar ambas bases de datos con la base de bienes y servicios
central$`Objeto a Contratar` <- as.character(central$`Objeto a Contratar`) 
descentral$`Objeto a Contratar` <- as.character(descentral$`Objeto a Contratar`) 

central <- merge(x = central, 
                y = bienesyservicios, 
                by = "Objeto a Contratar", 
                all.x =  TRUE)

descentral <- merge(x = descentral, 
                 y = bienesyservicios, 
                 by = "Objeto a Contratar", 
                 all.x =  TRUE)


# Eliminar contratos por menos de 1.000.000 pesos
central <- central[central$`Cuantia Contrato` >= 1000000]
descentral <- descentral[descentral$`Cuantia Contrato` >= 1000000]

# Agrupacion cuantias
quant_cuantias <- c(999999, 
                    10000000, 
                    50000000, 
                    100000000, 
                    500000000, 
                    1000000000, 
                    max(c(central$`Cuantia Contrato`, descentral$`Cuantia Contrato`) ) )

central$grupocuantia <- cut(central$`Cuantia Contrato`, breaks = quant_cuantias)
descentral$grupocuantia <- cut(descentral$`Cuantia Contrato`, breaks = quant_cuantias)

levels(central$grupocuantia) = c("1. 1' - 10'", 
                                 "2. 10' - 50'",
                                 "3. 50' - 100'",
                                 "4. 100' - 500'",
                                 "5. 500' - 1000'",
                                 "6. +1000'")
levels(descentral$grupocuantia) = c("1. 1' - 10'", 
                                    "2. 10' - 50'",
                                    "3. 50' - 100'",
                                    "4. 100' - 500'",
                                    "5. 500' - 1000'",
                                    "6. +1000'")
central$grupocuantia <- as.character(central$grupocuantia)
descentral$grupocuantia <- as.character(descentral$grupocuantia)

central %>%
  group_by(grupocuantia)%>%
  summarize(n=n())

descentral %>%
  group_by(grupocuantia)%>%
  summarize(n=n())

central %>%
  group_by(grupocuantia, Grupo)%>%
  summarize(n=n())

descentral %>%
  group_by(grupocuantia, Grupo)%>%
  summarize(n=n())


# Fechas

central$fechaini <- as.Date(central$`Fecha Ini Ejec Contrato`, format = "%d/%m/%Y")
central$fechafin <- as.Date(central$`Fecha Fin Ejec Contrato`, format = "%d/%m/%Y")

central$fechadif <- central$fechafin - central$fechaini
central$fechadif_0 <- ifelse(test = central$fechadif == 0, yes = 0, no = 1)

descentral$fechaini <- as.Date(descentral$`Fecha Ini Ejec Contrato`, format = "%d/%m/%Y")
descentral$fechafin <- as.Date(descentral$`Fecha Fin Ejec Contrato`, format = "%d/%m/%Y")

descentral$fechadif <- descentral$fechafin - descentral$fechaini
descentral$fechadif_0 <- ifelse(test = descentral$fechadif == 0, yes = 0, no = 1)



# Juntando Central y Descentral

central$orden <- "C"
descentral$orden <- "D"

ordendf <- rbind(central, descentral)

ordendf %>%
  group_by(orden) %>%
  summarize(n = n())

####################################################################################

ordendf2017 <- ordendf[year(ordendf$fechaini) == 2017]
  
ordendf2017 <- ordendf2017 %>%
  group_by(Sector, orden) %>%
  summarize(n = n())

# Two Mode Network
g <- graph.data.frame(d = ordendf2017, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2017$n/max(ordendf2017$n)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
#plot(g, vertex.label.cex = 1.8, vertex.label.color = "black")

pdf('Contratos CvsD - 2017.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2017",
      cex.main=2,
      col.main="Black")
dev.off()


unique(ordendf2017$Sector)

####################################################################################
ordendf2016 <- ordendf[year(ordendf$fechaini) == 2016]

ordendf2016 <- ordendf2016 %>%
  group_by(Sector, orden) %>%
  summarize(n = n())

# Two Mode Network
g <- graph.data.frame(d = ordendf2016, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2016$n/max(ordendf2016$n)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

pdf('Contratos CvsD - 2016.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2016",
      cex.main=2,
      col.main="Black")
dev.off()

####################################################################################
ordendf2015 <- ordendf[year(ordendf$fechaini) == 2015]

ordendf2015 <- ordendf2015 %>%
  group_by(Sector, orden) %>%
  summarize(n = n())

# Two Mode Network
g <- graph.data.frame(d = ordendf2015, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2015$n/max(ordendf2015$n)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
#plot(g, vertex.label.cex = 1.8, vertex.label.color = "black")

pdf('Contratos CvsD - 2015.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2015",
      cex.main=2,
      col.main="Black")
dev.off()

####################################################################################




####################################################################################

ordendf2017 <- ordendf[year(ordendf$fechaini) == 2017]

ordendf2017 <- ordendf2017 %>%
  group_by(Sector, orden) %>%
  summarize(cuantias = sum(`Cuantia Contrato`))

# Two Mode Network
g <- graph.data.frame(d = ordendf2017, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2017$cuantias/max(ordendf2017$cuantias)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
#plot(g, vertex.label.cex = 1.8, vertex.label.color = "black")

pdf('Cuantias CvsD - 2017.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2017",
      cex.main=2,
      col.main="Black")
dev.off()


unique(ordendf2017$Sector)

####################################################################################
ordendf2016 <- ordendf[year(ordendf$fechaini) == 2016]

ordendf2016 <- ordendf2016 %>%
  group_by(Sector, orden) %>%
  summarize(cuantias = sum(`Cuantia Contrato`))

# Two Mode Network
g <- graph.data.frame(d = ordendf2016, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2016$cuantias/max(ordendf2016$cuantias)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

pdf('Cuantias CvsD - 2016.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2016",
      cex.main=2,
      col.main="Black")
dev.off()

####################################################################################
ordendf2015 <- ordendf[year(ordendf$fechaini) == 2015]

ordendf2015 <- ordendf2015 %>%
  group_by(Sector, orden) %>%
  summarize(cuantias = sum(`Cuantia Contrato`))


# Two Mode Network
g <- graph.data.frame(d = ordendf2015, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- ordendf2015$cuantias/max(ordendf2015$cuantias)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
#plot(g, vertex.label.cex = 1.8, vertex.label.color = "black")

pdf('Cuantias CvsD - 2015.pdf')
plot(g, 
     layout=layout.bipartite, 
     vertex.size=8, 
     vertex.label.cex=1, 
     edge.width = E(g)$weight)
title("Central vs. Descentral: \n 2015",
      cex.main=2,
      col.main="Black")
dev.off()

####################################################################################






# Two Mode Network

central2017 <- central[year(central$fechaini) == 2017]
centralnet <- central2017 %>%
                group_by(`Nombre de la Entidad`, Grupo) %>%
                summarize(n = n())

centralnet$Grupo <- as.character(centralnet$Grupo)

g <- graph.data.frame(d = centralnet, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type


V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)



# Analisis tiempo ejecucion contratos

central$fechaini <- as.Date(central$`Fecha Ini Ejec Contrato`, format = "%d/%m/%Y")
central$fechafin <- as.Date(central$`Fecha Fin Ejec Contrato`, format = "%d/%m/%Y")


central$fechadif <- central$fechafin - central$fechaini
central$fechadif_0 <- ifelse(test = central$fechadif == 0, yes = 0, no = 1)

# "Fecha Ini Ejec Contrato"
central$`Fecha Ini Ejec Contrato`
# "Fecha Fin Ejec Contrato"
central$`Fecha Fin Ejec Contrato`


central %>%
  group_by(grupocuantia, fechadif_0)%>%
  summarize(n=n())

View(central[is.na(central$fechadif_0),])

# "Plazo de Ejec del Contrato"
fechas <- data_frame(plazo = central$`Plazo de Ejec del Contrato`, 
                      rango = central$`Rango de Ejec del Contrato`, 
                      finic = central$`Fecha Ini Ejec Contrato`,
                      ffina = central$`Fecha Fin Ejec Contrato`, 
                      fdiff = central$fechadif)
fechas <- fechas[order(fechas$rango),]

diffechas <- central %>%
        group_by(`Rango de Ejec del Contrato`, fechadif)%>%
        summarize(n=n())

write.table(x = diffechas, file = "diffechas.csv", sep = ";")


central %>%
  group_by(`Rango de Ejec del Contrato`, fechadif)%>%
  summarize(n=n())



# "Rango de Ejec del Contrato"
unique(central$`Rango de Ejec del Contrato`)
"Tiempo Adiciones en Dias"
"Tiempo Adiciones en Meses"







hist(log(central$`Cuantia Contrato`))
hist((central$`Cuantia Contrato`))
hist(log(descentral$`Cuantia Contrato`))

a <- sort(central$`Cuantia Contrato`)
View(a)


secopespecial %>%
  group_by(`Nivel Entidad`, `Orden Entidad`)%>%
  summarize(n=n())

namesdescentral <- sort(unique(descentral$`Nombre de la Entidad`))
namescentral <- sort(unique(central$`Nombre de la Entidad`))

# No hay sobrelape entre las entidades que contratan en los dos sitios
which(namescentral %in% namesdescentral)
which(namesdescentral %in% namescentral)





secopI <- secopI[secopI$`Tipo de Contrato` == "Suministro"] 

secopI <- secopI[secopI$`Tipo de Proceso` == "Contratación Directa (Ley 1150 de 2007)"]

secopI <- secopI[secopI$`Estado del Proceso` == "Celebrado"]

secopI <- secopI[secopI$`Nivel Entidad` == "NACIONAL"]

log(secopI$`Cuantia Contrato`)
secopI$`Objeto a Contratar`
svesecop <- secopI

bienesyservicios <- read.delim("/home/smartinez/Dropbox/0. UoG/Projects/Contrataci-n-Colombia/bienesyservicios.txt", 
           header = TRUE, sep = "/")

bienesyservicios$`Objeto a Contratar` <- as.character(bienesyservicios$Nombre)

bienesyservicios <- bienesyservicios[, c("Objeto a Contratar", "Grupo")]

secopI$`Objeto a Contratar` <- as.character(secopI$`Objeto a Contratar`) 

secopI <- merge(x = secopI, 
                y = bienesyservicios, 
                by = "Objeto a Contratar", 
                all.x =  TRUE)

ordenes <- read.delim("/home/smartinez/Dropbox/0. UoG/Projects/Contrataci-n-Colombia/ordenes.txt", 
                               header = TRUE, sep = "/")

ordenes$`Orden Entidad` <- as.character(ordenes$Orden)
ordenes <- ordenes[, c("Orden Entidad", "OEnt")]

secopI$`Orden Entidad` <- as.character(secopI$`Orden Entidad`)

secopI <- merge(x = secopI, 
                y = ordenes, 
                by = "Orden Entidad", 
                all.x =  TRUE)

secopI$logcuantia <- log(secopI$`Cuantia Contrato`)

g <- ggplot(data = secopI, aes(x = OEnt, y = logcuantia))
g + geom_point(aes(colour = Grupo) )
            

hist(secopI[secopI$Grupo == "BIENES"]$logcuantia, col='blue', breaks = 50)
hist(secopI[secopI$Grupo == "SERVICIOS"]$logcuantia, col='red', breaks = 50, add = TRUE)

ggplot2.h 

secopI$`Orden Entidad`

