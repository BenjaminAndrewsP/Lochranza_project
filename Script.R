getwd()
rm(list = ls())
graphics.off()
citation()

##### I.- Libraries #####

library(tidyverse)
library(gridExtra)

##### II.- Data #####

data <- read.csv("Loch_data.csv")
View(data)
dim(data)

##### III.- Desk Work #####
# NBN Atlas data
dw <- read.csv("Docs/Desk work/Desk_work.csv")
View(dw)

###### a.- Identification of priority and invasive species ######
dw <- dw[dw$Kingdom == "SPECIES",]
dim(dw)
#dw$Species.Name <- trimws(dw$Species.Name)
#dw$Species.Name <- gsub(" ", "_", dw$Species.Name)

exp.taxa <- dw
dim(exp.taxa)

colnames(exp.taxa)
colnames(exp.taxa)[colnames(exp.taxa) == "Kingdom"] <- "Taxonomic level"
colnames(exp.taxa)[colnames(exp.taxa) == "Phylum"] <- "Kingdom"
colnames(exp.taxa)[colnames(exp.taxa) == "Class"] <- "Phylum"
colnames(exp.taxa)[colnames(exp.taxa) == "Order"] <- "Class"
colnames(exp.taxa)[colnames(exp.taxa) == "Family"] <- "Order"
colnames(exp.taxa)[colnames(exp.taxa) == "Genus"] <- "Family"
colnames(exp.taxa)

exp.taxa <- exp.taxa[!duplicated(dw$Species.Name),]
dim(exp.taxa)

exp.taxa <- exp.taxa %>%
  mutate(across(everything(), ~ na_if(str_squish(as.character(.)), ""))) %>%
  drop_na()

ggplot(exp.taxa, aes(x = Kingdom)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Taxonomic groups") +
  ylab("Number of Species") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Identification and exclusion of irrelevant groups (eg. algae, fish)
####### Animalia
exp.animalia <- exp.taxa[exp.taxa$Kingdom == "Animalia",]

ggplot(exp.animalia, aes(x = Class)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_bw() +
  facet_wrap(~Phylum)

exp.animalia <- exp.animalia[exp.animalia$Phylum != "Mollusca",]

ggplot(exp.animalia, aes(x = Order)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_bw() +
  facet_wrap(~Class)

exp.animalia <- exp.animalia[exp.animalia$Class != "Actinopterygii",]
exp.animalia <- exp.animalia[exp.animalia$Class != "Elasmobranchii",]

exp.ani.prio <- exp.animalia[exp.animalia$Category == "concern",]
exp.ani.inv <- exp.animalia[exp.animalia$Category == "invasive",]

exp.ani.plot <- ggplot(exp.ani.prio, aes(x = Order, fill = Order)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 3), hjust = 0, vjust = 0.2, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  coord_polar(theta = "y") +
  ylab("") +
  labs(title = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "cm"),  # smaller boxes
        legend.text = element_text(size = 8), # smaller text
        legend.title = element_text(size = 9)) +
  facet_wrap(~Class, drop = T, scale = "free")

####### Plantae
exp.plantae <- exp.taxa[exp.taxa$Kingdom == "Plantae",]
unique(exp.plantae$Phylum)

exp.plantae <- exp.plantae[exp.plantae$Phylum != "Rhodophyta",]

unique(exp.plantae$Phylum)

exp.pla.prio <- exp.plantae[exp.plantae$Category == "concern",]
exp.pla.inv <- exp.plantae[exp.plantae$Category == "invasive",]

ggplot(exp.pla.prio, aes(x = Class, fill = Order)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 3), hjust = 0, vjust = 0.2, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  coord_polar(theta = "y") +
  ylab("") +
  labs(title = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "cm"),  # smaller boxes
        legend.text = element_text(size = 8), # smaller text
        legend.title = element_text(size = 9)) +
  facet_wrap(~Phylum, drop = T, scale = "free")

### Fungi
exp.fungi <- exp.taxa[exp.taxa$Kingdom == "Fungi",]
unique(exp.fungi$Phylum)

# All
all.exp <- as.data.frame(rbind(exp.fungi,exp.plantae,exp.animalia))
all.exp.prio <- all.exp[all.exp$Category == "concern",]
all.exp.inv <- all.exp[all.exp$Category == "invasive",]

# All concern - invasive
ggplot(all.exp.prio, aes(x = Class, fill = Class)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 3), hjust = -0.3, vjust = 0, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  coord_polar(theta = "y") +
  ylab("") +
  labs(title = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.8, "cm"),  # smaller boxes
        legend.text = element_text(size = 8), # smaller text
        legend.title = element_text(size = 10)) +
  facet_wrap(~Phylum, drop = T, scale = "free")

###### b.- Match of priority and invasive species with obs ######
# Intersect at species general
invasive.sp <- dw$Species.Name[dw$Category == "invasive"]
concern.sp <- dw$Species.Name[dw$Category == "concern"]

intersect(data$scientificName, invasive.sp) # 0 invasive recorded
length(intersect(data$scientificName, concern.sp)) # 42 sp of concern recorded

# intersect by slope
south.sp <- data[data$Slope == "South",]
south.sp <- south.sp[!duplicated(south.sp$scientificName),]

s.inter.sp <- intersect(south.sp$scientificName,concern.sp)
length(s.inter.sp) # 27 sp of concern in south slope

north.sp <- data[data$Slope == "North",]
north.sp <- north.sp[!duplicated(north.sp$scientificName),]
n.inter.sp <- intersect(north.sp$scientificName,concern.sp)
length(n.inter.sp) # 32 sp of concern in north slope

# Intersect by Class or phylium (in case of inverts)
mam.s <- south.sp[south.sp$Class == "Mammalia",]
aves.s <- south.sp[south.sp$Class == "Aves",]
ins.s <- south.sp[south.sp$Class == "Insecta",]
ins.s$Class <- c("Arthropoda")
fwi.s <- south.sp[south.sp$Class == "FWI",]
fwi.s$Class <- c("Arthropoda")
art.s <- rbind(ins.s,fwi.s)
pla.s <- south.sp[south.sp$Class == "Plantae",]

mam.n <- north.sp[north.sp$Class == "Mammalia",]
aves.n <- north.sp[north.sp$Class == "Aves",]
ins.n <- north.sp[north.sp$Class == "Insecta",]
ins.n$Class <- c("Arthropoda")
fwi.n <- north.sp[north.sp$Class == "FWI",]
fwi.n$Class <- c("Arthropoda")
art.n <- rbind(ins.n,fwi.n)
pla.n <- north.sp[north.sp$Class == "Plantae",]

new_south <- as.data.frame(rbind(mam.s,aves.s,art.s))
new_north <- as.data.frame(rbind(mam.n,aves.n,art.n))

# Getting the numbers
mam.int.s <- length(intersect(mam.s$scientificName,concern.sp))
mam.tot.s <- length(mam.s$scientificName)
ave.int.s <- length(intersect(aves.s$scientificName,concern.sp))
ave.tot.s <- length(aves.s$scientificName)
art.int.s <- length(intersect(art.s$scientificName,concern.sp))
art.tot.s <- length(art.s$scientificName)
pla.int.s <- length(intersect(pla.s$scientificName,concern.sp))
pla.tot.s <- length(pla.s$scientificName)
tot.s <- sum(mam.int.s,ave.int.s,art.int.s,pla.int.s)

mam.int.n <- length(intersect(mam.n$scientificName,concern.sp))
mam.tot.n <- length(mam.n$scientificName)
ave.int.n <- length(intersect(aves.n$scientificName,concern.sp))
ave.tot.n <- length(aves.n$scientificName)
art.int.n <- length(intersect(art.n$scientificName,concern.sp))
art.tot.n <- length(art.n$scientificName)
pla.int.n <- length(intersect(pla.n$scientificName,concern.sp))
pla.tot.n <- length(pla.n$scientificName)
tot.n <- sum(mam.int.n,ave.int.n,art.int.n,pla.int.n)

# Plot concern exp vs obs and slope richness
div.con <- c(mam.int.s,ave.int.s,art.int.s,pla.int.s,
             mam.tot.s,ave.tot.s,art.tot.s,pla.tot.s,
             mam.int.n,ave.int.n,art.int.n,pla.int.n,
             mam.tot.n,ave.tot.n,art.tot.n,pla.tot.n)
class <- c("Mammalia", "Aves", "Arthropoda", "Plantae",
           "Mammalia", "Aves", "Arthropoda", "Plantae",
           "Mammalia", "Aves", "Arthropoda", "Plantae",
           "Mammalia", "Aves", "Arthropoda", "Plantae")
slope <- c("South","South","South","South",
           "South","South","South","South",
           "North","North","North","North",
           "North","North","North","North")
Category <- c("Concern","Concern","Concern","Concern",
         "Observed","Observed","Observed","Observed",
         "Concern","Concern","Concern","Concern",
         "Observed","Observed","Observed","Observed")
div.concer.df <- as.data.frame(cbind(div.con,class,slope,Category))
str(div.concer.df)
div.concer.df$div.con <- as.numeric(div.concer.df$div.con)
str(div.concer.df)

ggplot(div.concer.df, aes(x = class, y = div.con, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = div.con),vjust = -0.2,
            position = position_dodge(width = 0.9),size = 3) +
  scale_fill_manual(values = c("Concern" = "firebrick", "Observed" = "bisque4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  xlab("Taxonomic groups") +
  ylab("Number of Species") +
  theme_bw() +
  labs(title = "") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  facet_wrap(~slope, drop = T, scale = "free")

# Appendix priority
north.sp
south.sp
concern.sp

# mams
mam.n
mam.s

length(mam.n$scientificName)
length(mam.s$scientificName)

length(intersect(mam.n$scientificName, concern.sp)) # all prio in mam north
mam.n.prio <- intersect(mam.n$scientificName, concern.sp)

length(intersect(mam.s$scientificName, concern.sp)) # all prio in south
mam.s.prio <- intersect(mam.s$scientificName, concern.sp)

mam.n.prio.uni <- setdiff(mam.n.prio, mam.s.prio) # unique north
length(mam.n.prio.uni) # 0 unique north

mam.s.prio.uni <- setdiff(mam.s.prio, mam.n.prio) # unique south
length(mam.s.prio.uni) # 2 unique south

shared.mam.prio <- intersect(mam.s.prio, mam.n.prio) # Shared
length(shared.mam.prio) # 3 shared

mam.n.prio.uni
mam.s.prio.uni
shared.mam.prio

# birds
aves.n
aves.s

length(intersect(aves.n$scientificName, concern.sp)) # all prio in mam north
ave.n.prio <- intersect(aves.n$scientificName, concern.sp)

length(intersect(aves.s$scientificName, concern.sp)) # all prio in south
ave.s.prio <- intersect(aves.s$scientificName, concern.sp)

ave.n.prio.uni <- setdiff(ave.n.prio, ave.s.prio) # unique north
length(ave.n.prio.uni) # 4 unique north

ave.s.prio.uni <- setdiff(ave.s.prio, ave.n.prio) # unique south
length(ave.s.prio.uni) # 1 unique south

shared.ave.prio <- intersect(ave.s.prio, ave.n.prio) # Shared
length(shared.ave.prio) # 6 shared

ave.n.prio.uni
ave.s.prio.uni
shared.ave.prio

# invert
art.n
art.s

length(intersect(art.n$scientificName, concern.sp)) # all prio in mam north
art.n.prio <- intersect(art.n$scientificName, concern.sp)

length(intersect(art.s$scientificName, concern.sp)) # all prio in south
art.s.prio <- intersect(art.s$scientificName, concern.sp)

art.n.prio.uni <- setdiff(art.n.prio, art.s.prio) # unique north
length(art.n.prio.uni) #  unique north

art.s.prio.uni <- setdiff(art.s.prio, art.n.prio) # unique south
length(art.n.prio.uni) #  unique south

shared.art.prio <- intersect(art.s.prio, art.n.prio) # Shared
length(shared.art.prio) #  shared

art.n.prio.uni
art.s.prio.uni
shared.art.prio

# plants
pla.n
pla.s

length(intersect(pla.n$scientificName, concern.sp)) # all prio in mam north
pla.n.prio <- intersect(pla.n$scientificName, concern.sp)

length(intersect(pla.s$scientificName, concern.sp)) # all prio in south
pla.s.prio <- intersect(pla.s$scientificName, concern.sp)

pla.n.prio.uni <- setdiff(pla.n.prio, pla.s.prio) # unique north
length(pla.n.prio.uni) #  unique north

pla.s.prio.uni <- setdiff(pla.s.prio, pla.n.prio) # unique south
length(pla.s.prio.uni) #  unique south

shared.pla.prio <- intersect(pla.s.prio, pla.n.prio) # Shared
length(shared.pla.prio) #  shared

pla.n.prio.uni
pla.s.prio.uni
shared.pla.prio

##### IV.- Baseline #####

# South Slope UNIQUE SP
south.data <- data[data$Slope == "South",]
south.sp <-  south.data[!duplicated(south.data$scientificName),]
dim(south.sp) # 82 sp

# North Slope UNIQUE SP
north.data <- data[data$Slope == "North",]
north.sp <-  north.data[!duplicated(north.data$scientificName),]
dim(north.sp) # 80 sp

all.uni <- as.data.frame(rbind(north.sp,south.sp))
all.uni <- all.uni[all.uni$Class != "FWI",]

ggplot(all.uni, aes(x = Order, fill = Slope)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 2) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  xlab("Taxonomic group") +
  ylab("Number of Species") +
  theme_bw() +
  #labs(title = "Plant diversity by Order") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5)) +
  facet_wrap(~Class, scale = "free") +
  theme(strip.text = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

###### a.- Verts ######

# animal data
animals <- data[data$Kingdom == "Animalia",]
dim(animals)

mammals <- data[data$Class == "Mammalia",]
mammals.s <- mammals[mammals$Slope == "South",]
mamms.s.uni <- mammals.s[!duplicated(mammals.s$scientificName),]
mammals.n <- mammals[mammals$Slope == "North",]
mamms.n.uni <- mammals.n[!duplicated(mammals.n$scientificName),]

aves <- data[data$Class == "Aves",]
aves.s <- aves[aves$Slope == "South",]
aves.s.uni <- aves.s[!duplicated(aves.s$scientificName),]
aves.n <- aves[aves$Slope == "North",]
aves.n.uni <- aves.n[!duplicated(aves.n$scientificName),]

aves.uni <- as.data.frame(rbind(aves.s.uni,aves.n.uni))
mamms.uni <- as.data.frame(rbind(mamms.s.uni,mamms.n.uni))

verts <- as.data.frame(rbind(aves.s.uni,aves.n.uni,mamms.s.uni,mamms.n.uni))
dim(verts)

# Plot verts
ggplot(verts, aes(x = Order, fill = Slope)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Order") +
  ylab("Number of Species") +
  theme_bw() +
  #labs(title = "Vertebrate diversity by Order") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~Class, scale = "free") +
  theme(strip.text = element_blank())

# Numbers and particular species
# birds

concern.sp

length(aves.s.uni$scientificName)
length(aves.n.uni$scientificName)

south.birds <- setdiff(unique(aves.s.uni$scientificName), unique(aves.n.uni$scientificName))
south.birds
length(south.birds)

north.birds <- setdiff(unique(aves.n.uni$scientificName),unique(aves.s.uni$scientificName))
north.birds
length(north.birds)

length(intersect(aves.s.uni$scientificName, aves.n.uni$scientificName))

length(intersect(aves.s.uni$scientificName, concern.sp))
length(intersect(aves.n.uni$scientificName, concern.sp))

# mams
length(mamms.s.uni$scientificName)
length(mamms.n.uni$scientificName)

south.mams <- setdiff(unique(mamms.s.uni$scientificName),unique(mamms.n.uni$scientificName))
south.mams
length(south.mams)

north.mams <- setdiff(unique(mamms.n.uni$scientificName),unique(mamms.s.uni$scientificName))
north.mams
length(north.mams)

shared.mam <- intersect(mamms.n.uni$scientificName, mamms.s.uni$scientificName)
intersect(shared.mam, concern.sp)

length(intersect(mamms.n.uni$scientificName, concern.sp))
length(intersect(mamms.s.uni$scientificName, concern.sp))

###### b.- Inverts ###### 
insect <- data[data$Class == "Insecta",]
insect.s <- insect[insect$Slope == "South",]
insect.s.uni <- insect.s[!duplicated(insect.s$scientificName),]
insect.n <- insect[insect$Slope == "North",]
insect.n.uni <- insect.n[!duplicated(insect.n$scientificName),]

insect.uni <- as.data.frame(rbind(insect.s.uni,insect.n.uni))

# Plot insects
ggplot(insect.uni, aes(x = Order, fill = Slope)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Order") +
  ylab("Number of Species") +
  theme_bw() +
  #labs(title = "Invertebrate diversity by Order") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  #facet_wrap(~Class, scale = "free") +
  theme(strip.text = element_blank())

# Numbers ans species of concern
concern.sp

length(insect.s.uni$scientificName)
length(unique(insect.s.uni$Order))

length(insect.n.uni$scientificName)
length(unique(insect.n.uni$Order))

south.unique <- setdiff(unique(insect.s.uni$scientificName),unique(insect.n.uni$scientificName))
south.unique
length(south.unique)
north.unique <- setdiff(unique(insect.n.uni$scientificName),unique(insect.s.uni$scientificName))
north.unique
length(north.unique)

intersect(insect.s.uni$scientificName, concern.sp)
intersect(insect.n.uni$scientificName, concern.sp)

###### c.- Plants ######
plant <- data[data$Kingdom == "Plantae",]

plant.s <- plant[plant$Slope == "South",]
plant.s.uni <- plant.s[!duplicated(plant.s$scientificName),]
plant.n <- plant[plant$Slope == "North",]
plant.n.uni <- plant.n[!duplicated(plant.n$scientificName),]

plant.uni <- as.data.frame(rbind(plant.s.uni,plant.n.uni))

ggplot(plant.uni, aes(x = Order, fill = Slope)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Order") +
  ylab("Number of Species") +
  theme_bw() +
  #labs(title = "Plant diversity by Order") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7)) +
  #facet_wrap(~Class, scale = "free") +
  theme(strip.text = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Numbers ans species of concern
concern.sp

length(plant.s.uni$scientificName)
length(unique(plant.s.uni$Order))

length(plant.n.uni$scientificName)
length(unique(plant.n.uni$Order))

length(unique(plant.n.uni$scientificName)) - length(intersect(unique(plant.s.uni$scientificName),
                                                     unique(plant.n.uni$scientificName)))
length(unique(plant.n.uni$Order)) - length(intersect(unique(plant.s.uni$Order),
                                                     unique(plant.n.uni$Order)))

inter.plants <- intersect(intersect(plant.n.uni$scientificName, concern.sp),
          intersect(plant.s.uni$scientificName, concern.sp))

union.plants <- union(intersect(plant.n.uni$scientificName, concern.sp),
      intersect(plant.s.uni$scientificName, concern.sp))

length(union.plants) - length(inter.plants)

###### d.- FWI ######
# FWI df
FWI.df <- data[data$Class == "FWI",]
view(FWI.df)
dim(FWI.df)

# FWI richness
FWI.s <- FWI.df[FWI.df$Slope == "South",]
FWI.n <- FWI.df[FWI.df$Slope == "North",]
FWI.rich <- as.data.frame(rbind(FWI.s,FWI.n))
view(FWI.rich)

# Plot FWI richness
FWI.plot <- ggplot(FWI.rich, aes(x = eventID, fill = Slope)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  xlab("Relative position to stream") +
  ylab("Number of Species") +
  scale_x_discrete(labels = c(
    'ND4FI_A' = 'NE Down', 'ND4FI_B' = 'NW Down',
    'NU4FI_A' = 'NE Up', 'NU4FI_B'= 'NW Up',
    'SD4FI_A' = 'SE Down', 'SD4FI_B' = 'SW Down',
    'SU4FI_A' = 'SE Up', 'SU4FI_B' = 'SW Up')) +
  theme_bw() +
  #labs(title = "Fresh Water Invertebrates Richness") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0)
  )

# FWI indexes
FWI.na <- FWI.df[!is.na(FWI.df$BMWP),]
dim(FWI.na)
view(FWI.na)

FWI.s.na <- FWI.na[FWI.na$Slope == "South",]
FWI.n.na <- FWI.na[FWI.na$Slope == "North",]
FWI.s.uni.na <- FWI.s.na[!duplicated(FWI.s.na$scientificName),]
FWI.n.uni.na <- FWI.n.na[!duplicated(FWI.n.na$scientificName),]
FWI.uni.na <- rbind(FWI.s.uni.na, FWI.n.uni.na)

# BMWP
BMWP.s <- sum(FWI.s.uni.na$BMWP)
BMWP.n <- sum(FWI.n.uni.na$BMWP)

Slope <- c("South","North")
BMWP <- c(BMWP.s,BMWP.n)
BMWP.df <- as.data.frame(cbind(Slope, BMWP))

# ASPT = BMWP / n
ASPT.s <- round(BMWP.s/length(unique(FWI.s.uni.na$scientificName)), 1)
ASPT.n <- round(BMWP.n/length(unique(FWI.n.uni.na$scientificName)), 1)

ASPT <- c(ASPT.s,ASPT.n)
ASPT.df <- as.data.frame(cbind(ASPT, Slope))

# Plots BMWP and ASPT
BMWP.plot <- ggplot(BMWP.df, aes(x = Slope, y = BMWP, fill = Slope)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = BMWP), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  xlab("Slope") +
  ylab("Total BMWP values") +
  theme_bw() +
  #labs(title = "BMWP values for each slope") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ASPT.plot <- ggplot(ASPT.df, aes(x = Slope, y = ASPT, fill = Slope)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ASPT), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("South" = "cadetblue", "North" = "tan")) +
  xlab("Slope") +
  ylab("ASPT values") +
  theme_bw() +
  #labs(title = "ASPT values for each slope") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),panel.grid.minor = element_blank())

grid.arrange(BMWP.plot, ASPT.plot, FWI.plot,
             layout_matrix = rbind(c(1, 2), c(3, 3)))

# Numbers ans species of concern
concern.sp

FWI.s.uni <- FWI.s[!duplicated(FWI.s$scientificName),]
FWI.n.uni <- FWI.n[!duplicated(FWI.n$scientificName),]

FWI.s.df <- as.data.frame(cbind(FWI.s.uni$Slope, FWI.s.uni$Order, FWI.s.uni$scientificName,
                                FWI.s.uni$BMWP))
FWI.n.df <- as.data.frame(cbind(FWI.n.uni$Slope, FWI.n.uni$Order, FWI.n.uni$scientificName,
                                FWI.n.uni$BMWP))

FWI.table <- as.data.frame(rbind(FWI.s.df, FWI.n.df))

length(FWI.s.uni$scientificName)
length(unique(FWI.s.uni$Order))

length(FWI.n.uni$scientificName)
length(unique(FWI.n.uni$Order))

length(FWI.s.uni.na$scientificName)
length(unique(FWI.s.uni.na$Order))

length(FWI.n.uni.na$scientificName)
length(unique(FWI.n.uni.na$Order))
