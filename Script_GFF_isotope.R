#### Visualisation résultats isotopie #### 

library(readxl)
library(ggplot2)
library(dplyr)

#### Carbone ####
GFF_isotopie_C <- read_excel("~/Desktop/GFF_isotopie_C.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "numeric", "numeric", "text", "text", 
                                           "numeric", "numeric"))
# Transformation en data frame
GFF_isotopie_C <- as.data.frame(GFF_isotopie_C)

# Conversion en facteurs
GFF_isotopie_C$site <- as.factor(GFF_isotopie_C$site)
GFF_isotopie_C$Prov <- as.factor(GFF_isotopie_C$Prov)
GFF_isotopie_C$years <- as.factor(GFF_isotopie_C$years)

str(GFF_isotopie_C)


ggplot(GFF_isotopie_C, aes(x = years, y = d13C, color = Prov)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = Prov), linetype = "solid", size = 0.8) +
  facet_wrap(~site, labeller = labeller(site = function(x) paste("site :", x))) +
  theme_minimal() +
  labs(
    title = "Chronologie de δ13C par site et provenance",
    x = "Année",
    y = expression(delta^{13}*C~("\u2030")),
    color = "Provenance"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

# Pour ploter Carbone avec la même chronologie que l'oxygène 

#Créer une copie du jeu de données avec years en numérique
GFF_isotopie_C$years_num <- as.numeric(as.character(GFF_isotopie_C$years))

# Filtrage de la plage 2000–2009
GFF_isotopie_C_filt <- GFF_isotopie_C %>%
  filter(years_num >= 2000 & years_num <= 2009)

ggplot(GFF_isotopie_C_filt, aes(x = years, y = d13C, color = Prov)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = Prov), linetype = "solid", size = 0.8) +
  facet_wrap(~site, labeller = labeller(site = function(x) paste("site :", x))) +
  theme_minimal() +
  labs(
    title = "Chronologie de δ13C (2000–2009) par site et provenance",
    x = "Année",
    y = expression(delta^{13}*C~("\u2030")),
    color = "Provenance"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )


#### Oxygène ####

GFF_isotopie_O <- read_excel("~/Desktop/GFF_isotopie_O.xlsx", 
                             col_types = c("text", "numeric", "text", 
                                           "text", "numeric", "numeric", "text", 
                                           "text", "numeric", "numeric"))

GFF_isotopie_O <- as.data.frame(GFF_isotopie_O)

GFF_isotopie_O <- GFF_isotopie_O %>%
  mutate(
    site = factor(site),
    Prov = factor(Prov),
    years = factor(years)
  )

str(GFF_isotopie_O)

### Visualisation ### 

ggplot(GFF_isotopie_O, aes(x = years, y = `d18O - IRMS (‰)`, color = Prov)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = Prov), linetype = "solid", size = 0.8) +
  facet_wrap(~site, labeller = labeller(site = function(x) paste("site :", x))) +
  theme_minimal() +
  labs(
    title = "Chronologie δ18O par site et provenance",
    x = "Année",
    y = expression(delta^{18}*O~("\u2030")),
    color = "Provenance"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
