# Figure 1: Coral cover and fish biomass, 2010 - 2020

library(tidyverse); theme_set(theme_classic())
MyTheme <- theme(axis.title=element_text(size=14), axis.text=element_text(size=12), 
                 legend.title=element_text(size=10), legend.text=element_text(size=9), 
                 panel.background=element_rect(fill="transparent"))

library(ggpubr)


# Read in coral cover data
coral <- read.csv("Data/Coral-cover-data_Annual-means_CRIOBE.csv")
head(coral)



# Plot coral cover (2010 - 2020, Moorea & Raiatea)
coral.cover <- ggplot(coral) + aes(x=as.factor(year), y=cover, colour=island) + 
  geom_line(linewidth=1.25, aes(group=island)) + 
  geom_point(size=3, position=position_dodge(0.1)) + 
  scale_colour_manual(values=c("springgreen3","darkorange1")) +
  MyTheme + theme(legend.position="none") +
  ylab("Hard coral cover (%)") + xlab(NULL) +
  scale_x_discrete(labels=c("2010","","2012","","2014","","2016","","2018","","2020")) +
  geom_text(label="Moorea", x=6.9, y=37, colour="springgreen3") +
  geom_text(label="Raiatea", x=8.5, y=19, colour="darkorange1") +
  geom_errorbar(aes(ymin = cover - cover.SE, ymax = cover + cover.SE), 
                width=0.3, size=0.3, position=position_dodge(0.1)) 
coral.cover




# Read in fish data
fish.nut <- read.csv("Data/Fish-trophic-groups_biomass&nutrients_Annual-means.csv")
head(fish.nut)


unique(fish.nut$trophic_group)
# "Herbivores Microvores Detritivores" "Mobile invertivore"                
# "Piscivore"                                              


# Plot biomass per trophic group (15cm+ fish only)

# Moorea
moo.bio <- ggplot(fish.nut[which(fish.nut$Island=="Moorea"),]) + 
  aes(x=as.factor(Year), y=biomass15, colour=trophic_group) + 
  geom_line(size=1.25, aes(group=trophic_group)) + 
  geom_point(size=3, position=position_dodge(0.1)) + 
  scale_colour_manual(values=c("#4AB095","#EEC110","#DF65B0")) +
  MyTheme + theme(legend.position="none") +
  ylab(expression(Biomass~(kg~ha^-1))) + xlab(NULL) +
  scale_x_discrete(labels=c("2010","","2012","","2014","","2016","","2018","","2020")) +
  ggtitle("Moorea") +
  geom_text(label="Herbivore", x=3.9, y=210, colour="#4AB095") +
  geom_text(label="Mobile invertivore", x=2.4, y=15, colour="#EEC110") +
  geom_text(label="Piscivore", x=7.8, y=100, colour="#DF65B0") +
  geom_errorbar(aes(ymin = biomass15-biomass.SE15, ymax = biomass15+biomass.SE15), 
                width=0.3, size=0.3, position=position_dodge(0.1)) 
moo.bio


# Raiatea
rai.bio <- ggplot(fish.nut[which(fish.nut$Island=="Raiatea"),]) + 
  aes(x=as.factor(Year), y=biomass15, colour=trophic_group) + 
  geom_line(size=1.25, aes(group=trophic_group)) + 
  geom_point(size=3, position=position_dodge(0.1)) + 
  scale_colour_manual(values=c("#4AB095","#EEC110","#DF65B0")) +
  MyTheme + theme(legend.position="none") +
  ylab(expression(Biomass~(kg~ha^-1))) + xlab(NULL) +
  ggtitle("Raiatea") +
  geom_errorbar(aes(ymin = biomass15-biomass.SE15, ymax = biomass15+biomass.SE15), 
                width=0.3, size=0.3, position=position_dodge(0.1)) 
rai.bio



# Arrange figure panels
( coral.bio <- ggarrange(coral.cover, moo.bio, rai.bio, 
                        nrow=3, ncol=1, align="v", labels="auto") )

#pdf("Plots/Fig1_Coral cover & fish biomass.pdf", width=6, height=10)
#coral.bio
#dev.off()



##### END OF SCRIPT #####