###### ------------------------------------------------------------ ###
###### ------------------------------------------------------------ ###
###### NAME: Creating a network measure
###### DATE: May 2019
###### Version: 1
###### ------------------------------------------------------------ ###
###### ------------------------------------------------------------ ###
###### NOTES ######
###### Coming from the Initial Exploration

if (!require("data.table")) install.packages("data.table")
library(data.table)

if (!require("bit64")) install.packages("bit64")
library(bit64)

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if (!require("devtools")) install.packages("devtools")
library(devtools)

if(!require(easyGgplot2)) install_github("easyGgplot2")
library("easyGgplot2")

if(!require(kassambara)) install_github("kassambara")
library("kassambara")

if(!require(igraph)) install.packages("igraph",repos = "http://cran.us.r-project.org")
library("igraph")

if(!require(ade4)) install.packages("ade4",repos = "http://cran.us.r-project.org")
library("ade4")




rm(list = ls())
gc()

# Cargando entidades nivel nacional, orden centralizado
load("../../SECOPespecial.RData")
secopespecial <- secopespecial[secopespecial$`Nivel Entidad` == "NACIONAL", ]
secopespecial <- secopespecial[(secopespecial$`Orden Entidad` == "NACIONAL DESCENTRALIZADO") | 
                                 (secopespecial$`Orden Entidad` == "NACIONAL CENTRALIZADO"), ]
# Dejar solo las entidades del nivel nacional y orden centralizado
central <-   secopespecial[(secopespecial$`Orden Entidad` == "NACIONAL CENTRALIZADO") ,]

# Limpiar memoria
rm(secopespecial)
gc()


# Cargando bienes y servicios
bienesyservicios <- read.delim("bienesyservicios.txt", 
                               header = TRUE, sep = "/")

bienesyservicios$`Objeto a Contratar` <- as.character(bienesyservicios$Nombre)
bienesyservicios <- bienesyservicios[, c("Objeto a Contratar", "Grupo", "Sector")]


# Cargando niveles entidades
nivelesentidadesnalrel <- read.delim("entidades_nal-rel_ministerios.txt", 
                               header = TRUE, sep = "/")

nivelesentidadesnalrel$`Nombre de la Entidad` <- as.character(nivelesentidadesnalrel$Nombre)
nivelesentidadesnalrel <- nivelesentidadesnalrel[, c("Nombre de la Entidad", "Nivel", "Nombrecorto")]

# Juntar bases de datos con la base de bienes y servicios, y con la de los niveles de las entidades
central$`Objeto a Contratar` <- as.character(central$`Objeto a Contratar`) 

central <- merge(x = central, 
                 y = bienesyservicios, 
                 by = "Objeto a Contratar", 
                 all.x =  TRUE)

central <- merge(x = central, 
                 y = nivelesentidadesnalrel, 
                 by = "Nombre de la Entidad", 
                 all.x =  TRUE)

# Dejar solo las "Nacionales"
central <- central[central$Nivel == "Nacional", ]


# Eliminar contratos por menos de 1.000.000 pesos
central <- central[central$`Cuantia Contrato` >= 1000000, ]

# Agrupacion cuantias
quant_cuantias <- c(999999, 
                    10000000, 
                    50000000, 
                    100000000, 
                    500000000, 
                    1000000000, 
                    max(c(central$`Cuantia Contrato`) ) )

central$grupocuantia <- cut(central$`Cuantia Contrato`, breaks = quant_cuantias)

levels(central$grupocuantia) = c("1. 1' - 10'", 
                                 "2. 10' - 50'",
                                 "3. 50' - 100'",
                                 "4. 100' - 500'",
                                 "5. 500' - 1000'",
                                 "6. +1000'")

central$grupocuantia <- as.character(central$grupocuantia)


central %>%
  group_by(grupocuantia)%>%
  summarize(n=n())

central %>%
  group_by(grupocuantia, Grupo)%>%
  summarize(n=n())


# Fechas

central$fechaini <- as.Date(central$`Fecha Ini Ejec Contrato`, format = "%d/%m/%Y")
central$fechafin <- as.Date(central$`Fecha Fin Ejec Contrato`, format = "%d/%m/%Y")

central$fechadif <- central$fechafin - central$fechaini
central$fechadif_0 <- ifelse(test = central$fechadif == 0, yes = 0, no = 1)

####################################################################################
# Save secure
centralsecure <- central
####################################################################################



centralarge <- central[central$grupocuantia == "6. +1000'", ]

centralarge <- centralarge %>%
  group_by(Sector, Nombrecorto) %>%
  summarize(n = n())


# Two Mode Network
g <- graph.data.frame(d = centralarge, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- centralarge$n/max(centralarge$n)*(10)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
#plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")
degree(g)



ag <- as_incidence_matrix(g)
ag <- t(ag) %*% ag

ming <- graph.adjacency(ag, weighted = TRUE)

 
plot(ming,  vertex.label.cex = 0.8, vertex.label.color = "black")
degree(ming)

# Simple Matching
ag_alt <- as.matrix(dist.binary(t(ag), method=2, upper=TRUE, diag = FALSE))
ag_alt <- ifelse(ag_alt > 0.9, 1, 0)

ag_alt_g <- graph_from_adjacency_matrix(ag_alt, 
                                           mode = "undirected")
plot(ag_alt_g)
degree(ag_alt_g)

# Jaccard Similarity

ag <- as_incidence_matrix(g)
ag_jac <- as.matrix(dist.binary(t(ag), method=1, upper=TRUE, diag = FALSE) )

diag(ag_jac)<-0

# women_jaccard          # Look at the matrix before you binarize
ag_jac <- ifelse(ag_jac > 0.95, 1, 0)     # Binarize

# jaccard_women      # Take a look at the matrix if you like.

ag_jac_g <- graph_from_adjacency_matrix(ag_jac,    # Create an igraph network
                                          mode = "undirected")
plot(ag_jac_g)










View(centralsmall[centralsmall$`Nombre de la Entidad` == "MINISTERIO DE TRANSPORTE (MINTRANSPORTE)",]) 


central2017 <- central[year(central$fechaini) == 2017, ]

central2017 <- central2017 %>%
  group_by(Sector, `Nombre de la Entidad`) %>%
  summarize(n = n())

View

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

