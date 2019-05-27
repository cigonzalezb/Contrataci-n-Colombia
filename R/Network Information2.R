###### ------------------------------------------------------------ ###
###### ------------------------------------------------------------ ###
###### NAME: Creating a network measure
###### DATE: May 2019
###### Version: 2
###### ------------------------------------------------------------ ###
###### ------------------------------------------------------------ ###
###### NOTES ######
###### Coming from the Initial Exploration
###### https://study.sagepub.com/borgatti2e/student-resources/chapter-5/chapter-13-analyzing-two-mode-data
###### https://rpubs.com/pjmurphy/317838

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

if(!require(igraph)) install.packages("igraph",repos = "http://cran.us.r-project.org")
library("igraph")

if(!require(ade4)) install.packages("ade4",repos = "http://cran.us.r-project.org")
library("ade4")

if(!require(network)) install.packages("network",repos = "http://cran.us.r-project.org")
library("network")

if(!require(GGally)) install.packages("GGally",repos = "http://cran.us.r-project.org")
library("GGally")




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

centralsecure <- central

# Jaccard Similarity Threshold
jaccthresh <- 0.90

####################################################################################

# Contract sizes
#  "1. 1' - 10'"
#  "2. 10' - 50'"
#  "3. 50' - 100'"
#  "4. 100' - 500'"
#  "5. 500' - 1000'"
#  "6. +1000'")

# Separate into large and small contracts
centralarge <- central[((central$grupocuantia == "4. 100' - 500'")|
                          (central$grupocuantia == "5. 500' - 1000'")|
                            (central$grupocuantia == "6. +1000'")), ]
#centralarge <- centralarge[centralarge$`Anno Firma del Contrato` == 2015]

centralarge <- centralarge %>%
  group_by(Sector, Nombrecorto) %>%
  summarize(n = n())


# Two Mode Network
g <- igraph::graph.data.frame(d = centralarge, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- centralarge$n/max(centralarge$n)*(10)


# Jaccard Similarity 
prejaccard_large <- as_incidence_matrix(g)

# Calculating the binary distance for the Jaccard Coefficient
jaccardmat_large <- as.matrix(dist.binary(t(prejaccard_large), 
                                          method = 1, 
                                          upper = TRUE, 
                                          diag = FALSE) )
# Making the diagonal equal to zero
diag(jaccardmat_large) <- 0

# Binarise the matrix according to the threshold
jaccardmat_large <- ifelse(jaccardmat_large > jaccthresh, 1, 0)     

# Make a network objecto for plotting
jaccardgra_large <- network(jaccardmat_large, 
                            directed = FALSE)


ggnet2(jaccardgra_large, 
       node.size = 6, 
       node.color = "grey", 
       edge.size = 1, 
       edge.color = "black", 
       label = TRUE, 
       label.size = 3,
       layout.exp = 0.5) + 
  ggsave("Contratos grandes.pdf", 
         width = 15, 
         height = 10, 
         units = 'cm')




####################################################################################

#  "1. 1' - 10'"
#  "2. 10' - 50'"
#  "3. 50' - 100'"
#  "4. 100' - 500'"
#  "5. 500' - 1000'"
#  "6. +1000'")


centrasmall <- central[((central$grupocuantia == "1. 1' - 10'")|
                          (central$grupocuantia == "2. 10' - 50''")|
                             (central$grupocuantia == "3. 50' - 100'")), ]
#centrasmall <- centrasmall[centrasmall$`Anno Firma del Contrato` == 2015]


centrasmall <- centrasmall %>%
  group_by(Sector, Nombrecorto) %>%
  summarize(n = n())


# Two Mode Network
g <- igraph::graph.data.frame(d = centrasmall, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- centrasmall$n/max(centrasmall$n)*(10)


# Jaccard Similarity 
prejaccard_small <- as_incidence_matrix(g)

# Calculating the binary distance for the Jaccard Coefficient
jaccardmat_small <- as.matrix(dist.binary(t(prejaccard_small), 
                                            method = 1, 
                                            upper = TRUE, 
                                            diag = FALSE) )
# Making the diagonal equal to zero
diag(jaccardmat_small) <- 0



# Binarise the matrix according to the threshold
jaccardmat_small <- ifelse(jaccardmat_small > jaccthresh, 1, 0)     

# Make a network objecto for plotting
jaccardgra_small <- network(jaccardmat_small, 
                            directed = FALSE)


ggnet2(jaccardgra_small, 
       node.size = 6, 
       node.color = "grey", 
       edge.size = 1, 
       edge.color = "black", 
       label = TRUE, 
       label.size = 3,
       layout.exp = 0.5) + 
  ggsave("Contratos pequeños.pdf", 
         width = 15, 
         height = 10, 
         units = 'cm')


####################################################################################
####################################################################################
####################################################################################

rm(list = ls())
gc()
# Cargando entidades nivel nacional, orden centralizado
load("../../SECOPespecial.RData")

# Cargando Gobernaciones
gobernaciones <- read.delim("nombres_secop_especial.txt", 
                               header = TRUE, sep = "/")

names(gobernaciones)
gobernaciones$`Nombre de la Entidad` <- as.character(gobernaciones$Nombre.de.la.Entidad)
gobernaciones <- gobernaciones[, c("Gobernacion", "Nombre de la Entidad")]

# Cargando bienes y servicios
bienesyservicios <- read.delim("bienesyservicios.txt", 
                               header = TRUE, sep = "/")

bienesyservicios$`Objeto a Contratar` <- as.character(bienesyservicios$Nombre)
bienesyservicios <- bienesyservicios[, c("Objeto a Contratar", "Grupo", "Sector")]

# Cargando CALIFICACIONES DNP 
dnp_departamentos <- read.delim("../Bases DNP/Calificacion departamental 2000-2017.txt", 
                               header = TRUE, sep = "/", fileEncoding = "WINDOWS-1252")

dnp_departamentos$`Nombre de la Entidad` <- as.character(dnp_departamentos$Nombre.de.la.Entidad)
dnp_departamentos$`Indicador` <- as.numeric(dnp_departamentos$Indicador.fiscal)

dnp_departamentos <- dnp_departamentos[, c("Año", "Nombre de la Entidad", "Indicador")]


dnp_departamentos <- dnp_departamentos[dnp_departamentos$Año >= 2011, ]


dnp_departamentos <- dnp_departamentos %>%
  group_by(`Nombre de la Entidad`) %>%
  summarize(ind_promedio = mean(Indicador))

quant_indicador <- c(0,
                     39, 
                     60, 
                     70, 
                     80, 
                     100)

dnp_departamentos$calificacion <- cut(dnp_departamentos$ind_promedio, breaks = quant_indicador)

levels(dnp_departamentos$calificacion) = c("1. Deterioro", 
                                           "2. Riesgo",
                                           "3. Vulnerable'",
                                           "4. Sostenible",
                                           "5. Solvente")
dnp_departamentos$calificacion <- as.character(dnp_departamentos$calificacion)


## MERGE
gob <- merge(x = secopespecial, 
                 y = gobernaciones, 
                 by = "Nombre de la Entidad", 
                 all =  TRUE)

rm(secopespecial)
gc()


gob <- gob[gob$Gobernacion == "GOBERNACION", ]


gob <- merge(x = gob, 
                 y = bienesyservicios, 
                 by = "Objeto a Contratar", 
                 all.x =  TRUE)

gob <- merge(x = gob, 
             y = dnp_departamentos, 
             by = "Nombre de la Entidad", 
             all.x = TRUE)

unique(sort(matrix(data = gob$`Nombre de la Entidad`, ncol = 1, nrow = 32)))



# Agrupacion cuantias
quant_cuantias <- c(999999, 
                    10000000, 
                    50000000, 
                    100000000, 
                    500000000, 
                    1000000000, 
                    max(c(gob$`Cuantia Contrato`) ) )

gob$grupocuantia <- cut(gob$`Cuantia Contrato`, breaks = quant_cuantias)

levels(gob$grupocuantia) = c("1. 1' - 10'", 
                                 "2. 10' - 50'",
                                 "3. 50' - 100'",
                                 "4. 100' - 500'",
                                 "5. 500' - 1000'",
                                 "6. +1000'")

gob$grupocuantia <- as.character(gob$grupocuantia)

cuantias_origen <-gob %>%
  group_by(`Origen de los Recursos`, grupocuantia) %>%
  summarize(n = n())


write_delim(x = cuantias_origen, path = "cuantias y origenes.txt", delim = "/")




####################################################################################
jaccthresh <- 0.9

propios <- gob[gob$`Origen de los Recursos` == "Recursos propios", ]
#centrasmall <- centrasmall[centrasmall$`Anno Firma del Contrato` == 2015]


propios <- propios %>%
  group_by(Sector, `Nombre de la Entidad`) %>%
  summarize(n = n())


# Two Mode Network
g <- igraph::graph.data.frame(d = propios, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- propios$n/max(propios$n)*(10)


# Jaccard Similarity 
prejaccard_propios <- as_incidence_matrix(g)

# Calculating the binary distance for the Jaccard Coefficient
jaccardmat_propios <- as.matrix(dist.binary(t(prejaccard_propios), 
                                          method = 1, 
                                          upper = TRUE, 
                                          diag = FALSE) )
# Making the diagonal equal to zero
diag(jaccardmat_propios) <- 0



# Binarise the matrix according to the threshold
jaccardmat_propios <- ifelse(jaccardmat_propios > jaccthresh, 1, 0)     

# Make a network objecto for plotting
jaccardgra_propios <- network(jaccardmat_propios, 
                            directed = FALSE)

pre_names <- data.frame("Nombre de la Entidad" = network::get.vertex.attribute(x = jaccardgra_propios, 
                                           attrname = "vertex.names"))

pre_names <- merge(x = pre_names, 
                   y = dnp_departamentos, 
                   by.x = "Nombre.de.la.Entidad", 
                   by.y = "Nombre de la Entidad", all.x = TRUE)

network::set.vertex.attribute(x = jaccardgra_propios, attrname = "Calificacion", value = pre_names$calificacion)



ggnet2(jaccardgra_propios, 
       node.size = 6, 
       node.color = "Calificacion", 
       edge.size = 0.5, 
       edge.color = "black", 
       label = TRUE, 
       label.size = 2,
       layout.exp = 0.5) + 
  ggsave("Recursos Propios.pdf", 
         width = 15, 
         height = 10, 
         units = 'cm')



####################################################################################

#"No definido"                      
#"Recursos propios"
#"Regalías"                         
#"SGP"
#"Presupuesto Nacional/Territorial" 
#"Recursos de crédito"
#"SGR" 

regalias <- gob[gob$`Origen de los Recursos` == "Regalías", ]



regalias <- regalias %>%
  group_by(Sector, `Nombre de la Entidad`) %>%
  summarize(n = n())


# Two Mode Network
g <- igraph::graph.data.frame(d = regalias, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- regalias$n/max(regalias$n)*(10)


# Jaccard Similarity 
prejaccardregalias <- as_incidence_matrix(g)

# Calculating the binary distance for the Jaccard Coefficient
jaccardmatregalias <- as.matrix(dist.binary(t(prejaccardregalias), 
                                            method = 1, 
                                            upper = TRUE, 
                                            diag = FALSE) )
# Making the diagonal equal to zero
diag(jaccardmatregalias) <- 0



# Binarise the matrix according to the threshold
jaccardmatregalias <- ifelse(jaccardmatregalias > jaccthresh, 1, 0)     

# Make a network objecto for plotting
jaccardgraregalias <- network(jaccardmatregalias, 
                              directed = FALSE)

pre_names <- data.frame("Nombre de la Entidad" = network::get.vertex.attribute(x = jaccardgraregalias, 
                                                                               attrname = "vertex.names"))

pre_names <- merge(x = pre_names, 
                   y = dnp_departamentos, 
                   by.x = "Nombre.de.la.Entidad", 
                   by.y = "Nombre de la Entidad", all.x = TRUE)

network::set.vertex.attribute(x = jaccardgraregalias, attrname = "Calificacion", value = pre_names$calificacion)

ggnet2(jaccardgraregalias, 
       node.size = 6, 
       node.color = "Calificacion", 
       edge.size = 1, 
       edge.color = "black", 
       label = TRUE, 
       label.size = 3,
       layout.exp = 0.5) + 
  ggsave("Recursos Regalias.pdf", 
         width = 15, 
         height = 10, 
         units = 'cm')



####################################################################################

#"No definido"                      
#"Recursos propios"
#"Regalías"                         
#"SGP"
#"Presupuesto Nacional/Territorial" 
#"Recursos de crédito"
#"SGR" 

presupuesto <- gob[gob$`Origen de los Recursos` == "Presupuesto Nacional/Territorial", ]



presupuesto <- presupuesto %>%
  group_by(Sector, `Nombre de la Entidad`) %>%
  summarize(n = n())


# Two Mode Network
g <- igraph::graph.data.frame(d = presupuesto, directed = FALSE)
bipartite_mapping(g)

V(g)$type <- bipartite_mapping(g)$type
E(g)$weight <- presupuesto$n/max(presupuesto$n)*(10)


# Jaccard Similarity 
prejaccardpresupuesto <- as_incidence_matrix(g)

# Calculating the binary distance for the Jaccard Coefficient
jaccardmatpresupuesto <- as.matrix(dist.binary(t(prejaccardpresupuesto), 
                                            method = 1, 
                                            upper = TRUE, 
                                            diag = FALSE) )
# Making the diagonal equal to zero
diag(jaccardmatpresupuesto) <- 0



# Binarise the matrix according to the threshold
jaccardmatpresupuesto <- ifelse(jaccardmatpresupuesto > jaccthresh, 1, 0)     

# Make a network objecto for plotting
jaccardgrapresupuesto <- network(jaccardmatpresupuesto, 
                              directed = FALSE)

pre_names <- data.frame("Nombre de la Entidad" = network::get.vertex.attribute(x = jaccardgrapresupuesto, 
                                                                               attrname = "vertex.names"))

pre_names <- merge(x = pre_names, 
                   y = dnp_departamentos, 
                   by.x = "Nombre.de.la.Entidad", 
                   by.y = "Nombre de la Entidad", all.x = TRUE)

network::set.vertex.attribute(x = jaccardgrapresupuesto, attrname = "Calificacion", value = pre_names$calificacion)


ggnet2(jaccardgrapresupuesto, 
       node.size = 6, 
       node.color = "Calificacion", 
       edge.size = 0.5, 
       edge.color = "black", 
       label = TRUE, 
       label.size = 2,
       layout.exp = 0.5) + 
  ggsave("Recursos Recursos Nacionales.pdf", 
         width = 15, 
         height = 10, 
         units = 'cm')
