# Artisanal fishers in Small Island Developing States -----------------------------
# and their perception of environmental change: the case study of Mauritius
#
# Authors: Chandani APPADOO, Riad SULTAN, Monique SIMIER, 
#          Verena TANDRAYEN, Manuela CAPELLO
# 
# Email addresses: chandani@uom.ac.mu; r.sultan@uom.ac.mu, Monique.Simier@ird.fr,
#               v.tandrayen@uom.ac.mu, manuela.capello@ird.fr
#
# Script for data analysis and preparation of figures and tables


# Prepare analyses ----

## Load R libraries
library(ggplot2)              
library(tidyverse)
library(ggrepel)
library(ade4)                 
library(FactoMineR)
library(RColorBrewer)
library(pheatmap)
library(chisq.posthoc.test)


# Clear working directory
rm(list=ls())

# Import csv dataset
df<-read.csv2("Data_Mauritius_220912.csv",header = T, stringsAsFactors = F)


# Results -----------------------------

# 3.1. Characterization of the fishermen ----

### Age ----
range(df$Age,na.rm=T)
mean(df$Age,na.rm=T)
sd(df$Age,na.rm=T)

hist(df$Age, breaks=20)

### Figure 2. Fishers' characteristics ----

### A. Region
reg<-as.data.frame(table(df$Region))
colnames(reg)=c("Region","Count")
reg$Frequency<-round(reg$Count/sum(reg$Count)*100,1)
reg
sum(reg$Frequency)

# pie chart 
reg$Region <- factor(reg$Region, levels=c("North","West", "South","East"))
reg <- reg[order(reg$Region),]

pdf("Fig2A.pdf",4,4)
ggplot(reg,aes(x="",Frequency, fill=Region)) + 
  geom_bar(width=1,stat="identity",color="black",alpha=.5) + 
  coord_polar("y", start=1)+
  geom_text(aes(label = paste0(Frequency,"%")), position=position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = rainbow(7)) +
  theme_void() 
dev.off()


### B. Fishing Area 
dfwhere<-data.frame(Area=c("Along the Coast","Deep sea","Reef area","Other"))

dfwhere$Frequency<-c(
  sum(df$Along.the.coast)/247*100,
  sum(df$Deep.sea)/247*100,
  sum(df$In.the.reef.area)/247*100,
  sum(df$Other)/247*100
)
dfwhere$Frequency<-round(dfwhere$Frequency,1)
dfwhere

dfwhere$Area <- factor(dfwhere$Area, levels=c("Deep sea","Along the Coast","Reef area","Other"))

pdf("Fig2B.pdf",3,3)
ggplot(dfwhere, aes(x = Area, y = Frequency, fill=Area)) + 
  geom_bar(stat = "identity") +
  labs (y = "Frequency (%)") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position=c(0.85,0.8)) 
dev.off()


### C. Gear 
dfgear<-data.frame(Gear=c("Basket","Handline","Vertical line","Large net","Gillnet","Other"))

dfgear$Frequency<-c(
  sum(df$q20gearBasket)/247*100,
  sum(df$q20gearhandline)/247*100,
  sum(df$q20gearVerticalline)/247*100,
  sum(df$q20gearLargeNet)/247*100,
  sum(df$q20gearGillnet)/247*100,
  sum(df$q20gearOthers)/247*100
)

dfgear$Gear <- factor(dfgear$Gear, 
                      levels= c("Handline","Basket","Vertical line","Large net","Gillnet","Other"))
dfgear <- dfgear[order(dfgear$Gear),]
dfgear

pdf("Fig2C.pdf",3,3)
ggplot(dfgear, aes(x = Gear, y = Frequency, fill=Gear)) + 
  geom_bar(stat = "identity") +
  labs (y = "Frequency (%)") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position=c(0.85,0.8)) 
dev.off()


###. D. Type of engine
df$q21engine1[df$q21engine1==""]<- NA
levels(as.factor(df$q21engine1))

# Rename labels
df$q21engine1[df$q21engine1=="Outboard motor power <8"] <- "Outboard motor power <8 CV"
df$q21engine1[df$q21engine1=="Outboard motor  power >26"] <- "Outboard motor power >26 CV"
df$q21engine1[df$q21engine1=="Outboard motor  power 9-25"] <- "Outboard motor power 9-25 CV"
levels(as.factor(df$q21engine1))

engine<-data.frame(table(df$q21engine1,useNA="always"))
engine$Freq<-round(engine$Freq/247*100,1)
colnames(engine)=c("Engine","Frequency")
engine

engine$Engine <- factor(engine$Engine, 
                        levels= c("Outboard motor power <8 CV",
                                  "Outboard motor power 9-25 CV",
                                  "Outboard motor power >26 CV", 
                                  "Inboard motor (large)", 
                                  "Oars and sails no engine",
                                  "On foot",
                                  ""))
engine <- engine[order(engine$Engine),]
engine

# pie chart 
engine$pos = (cumsum(c(0, engine$Frequency)) + c(engine$Frequency / 2, .01))[1:nrow(engine)]
engine

pdf("Fig2D.pdf",5.5,5.5)
ggplot(engine, aes(1, Frequency, fill = Engine)) +
  geom_bar(width=1,stat="identity",color = 'black', alpha=.5,
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  geom_text_repel(aes(x = 1.4, y = pos, label = paste0(Frequency,"%")), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = FALSE) +
  coord_polar('y') +
  scale_fill_manual(values = rainbow(7)) +
  theme_void()
dev.off()


### E. Type of boat
summary(as.factor(df$q19boatsize))
df$q19boatsize[df$q19boatsize==""]<- NA
levels(as.factor(df$q19boatsize))

boat<-data.frame(table(df$q19boatsize,useNA="always"))
boat$Freq<-boat$Freq/247*100
boat$Freq<-round(boat$Freq,1)
colnames(boat)=c("Boat","Frequency")

boat$Boat <- factor(boat$Boat , levels=c("Wooden Pirogue < 8 m","Wooden Pirogue > 8 m", "Fibre glass", "Other", ""))
boat <- boat[order(boat$Boat),]
boat

# pie chart 
# for position of labels on the pie chart
boat$pos = (cumsum(c(0, boat$Frequency)) + c(boat$Frequency / 2, .01))[1:nrow(boat)]
boat

pdf("Fig2E.pdf",5,5)
ggplot(boat, aes(1, Frequency, fill = Boat)) +
  geom_bar(width=1,stat="identity",color = 'black', alpha=.5,
           position = position_stack(reverse = TRUE), 
           show.legend = TRUE) +
  geom_text_repel(aes(x = 1.4, y = pos, label = paste0(Frequency,"%")), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = FALSE) +
  coord_polar('y') +
  scale_fill_manual(values = rainbow(7)) +
  theme_void()
dev.off()


### F. FADs use
df$q32Fisnearfad[df$q32Fisnearfad==0] <- "No"
df$q32Fisnearfad[df$q32Fisnearfad==1] <- "Yes"
levels(as.factor(df$q32Fisnearfad))

fad<-as.data.frame(table(df$q32Fisnearfad))
fad$Freq<-round(fad$Freq/247*100,1)
colnames(fad)=c("FAD","Frequency")
fad

fad <- fad %>% 
  arrange(desc(FAD)) %>% 
  mutate(y_pos = cumsum(Frequency)-0.5*Frequency)
fad

# pie chart 
pdf("Fig2F.pdf",5.5,5.5)
ggplot(fad,aes(x="",Frequency, fill=FAD)) + 
  geom_bar(width=1,stat="identity",color="black",alpha=.5) + 
  coord_polar("y", start=0)+
  geom_text(aes(y = y_pos, label = paste0(Frequency,"%")), color = "black")+
  scale_fill_manual(values = rainbow(7)) +
  theme_void() 
dev.off()

rm(list=c("boat","dfgear","dfwhere","engine","fad","reg"))


## Fuzzy Correspondence Analysis ----

dfs <- df
row.names(dfs) <- dfs$Response.No.

# Cleaning the new dataset : removing rows with missing or rare information for certain questions

# Remove q19boatsize = Unknown (7) & Other (3)
levels(as.factor(dfs$q19boatsize))
dfs <- dfs[df$q19boatsize %in% c("Fibre glass", "Wooden Pirogue < 8 m" ,"Wooden Pirogue > 8 m"),]
dfs <- droplevels(dfs)
nrow(dfs)

# Remove q21engine1 = Unknown(5), Inboard Motor (2), Oars and sailno engine (3), On foot (2)
levels(as.factor(dfs$q21engine1))
dfs <- dfs[dfs$q21engine1 %in% c("Outboard motor power >26 CV", "Outboard motor power 9-25 CV", "Outboard motor power <8 CV"),]
dfs <- droplevels(dfs)
nrow(dfs)

# Remove NA for q32Fisnearfad
dfs <- dfs[!is.na(dfs$q32Fisnearfad),]
levels(as.factor(dfs$q32Fisnearfad)) # No / Yes
dfs <- droplevels(dfs)
nrow(dfs)

# Remove rows without any answer for fishing area (2) and gear (2)
colsel <- c("Along.the.coast","Deep.sea","In.the.reef.area")
dfs <- dfs[rowSums(dfs[,colsel])>0,]
dfs <- droplevels(dfs)
nrow(dfs)

# Gear 
colsel <- c("q20gearBasket","q20gearhandline","q20gearVerticalline",              
            "q20gearLargeNet" ,"q20gearGillnet", "q20gearOthers")
dfs <- dfs[rowSums(dfs[,colsel])>0,]
dfs <- droplevels(dfs)
nrow(dfs)


# Select variables and create table for the fuzzy CA

# First the variables to be coded in 0/1
colsel=c("Region", "q19boatsize","q21engine1", "q32Fisnearfad")
df1 <- dfs[, colsel]

# Check the modalities
levels(as.factor(df1$Region))
levels(as.factor(df1$q19boatsize))
levels(as.factor(df1$q21engine1))
levels(as.factor(df1$q32Fisnearfad))

# Rename levels to get shorter labels
df1$q19boatsize[df1$q19boatsize=="Wooden Pirogue < 8 m"] <-"Pirogue<8m"
df1$q19boatsize[df1$q19boatsize=="Wooden Pirogue > 8 m"] <-"Pirogue>8m"

df1$q21engine1[df1$q21engine1=="Outboard motor power <8"] <-"Power<8CV"
df1$q21engine1[df1$q21engine1=="Outboard motor  power >26"] <-"Power>26CV"
df1$q21engine1[df1$q21engine1=="Outboard motor  power 9-25"] <-"Power 9-25CV"

df1 <- data.frame(lapply(df1, as.factor), row.names=row.names(df1))
str(df1)
summary(df1)

# Transform df1 in 0/1 (disjunctive table)
df1disj <- acm.disjonctif(df1)
str(df1disj)


# Then select fuzzy variables : Zone and Gear
colsel=c("Along.the.coast", "Deep.sea", "In.the.reef.area", 
         "q20gearBasket", "q20gearhandline", "q20gearVerticalline", "q20gearLargeNet", 
         "q20gearGillnet", "q20gearOthers")
df2 <- dfs[,colsel]
names(df2)
str(df2) # already coded in 0/1


# combine variables (all variables are now coded 0/1) to final dataset df3
df3 <- cbind(df1disj[1:4], df2[1:3],df2[4:9], df1disj[8:10], df1disj[5:7], df1disj[11:12] )
str(df3)

# Rename columns of df3
names(df3)
names(df3) <- c("East","North","South","West",
                "Along the coast","Deep sea","Reef area",
                "Basket","Handline","Vertical Line","Large net","Gill net","Other",  
                "Outboard <8CV","Outboard >26CV","Outboard 9-25CV" ,
                "Fibre glass","Pirogue<8m","Pirogue>8m",          
                "No","Yes")
names(df3)


### Figure 3. Fuzzy Correspondence Analysis ----

fz2 <- prep.fuzzy.var(df3, c(4,3,6,3,3,2)) # number of levels for each variable
#names(attr(fz2, "col.blocks")) <- c("A. Region", "B. Area",  "C. Gear", "D. Engine", "E. Boat", "F. Use FADs ?")
names(attr(fz2, "col.blocks")) <- c("A", "B",  "C", "D", "E", "F")

fza2 <- dudi.fca(fz2, scannf = FALSE, nf = 2)
inertia.dudi(fza2, col.inertia=T)

pdf("Fig3.pdf",6,8)
scatter(fza2, clab.moda=2, csub=3)
dev.off()


## Characterize groups of fishermen using their coordinates ($li) in the FCA ----

# Compute distances between fishermen
d1<- dist(fza2$li)

# Use this distance matrix to compute a hierarchical classification
h1 <- hclust(d1, method="ward.D2")

# Select a level to cut the tree and define groups of fishermen
iner <- sort(h1$height,decreasing = T)
plot(iner[1:10], type="s")

# Cut the tree to get 6 groups of fishers
# NB: In this case we should choose to make 3 groups but we prefer smaller groups
k <- cutree(h1,6)


### Figure S1. Hierarchical clustering of fishers ----

pdf("FigS1.pdf", 6,8)
plot(h1, hang = -1, labels=F, xlab="", ylab="", sub="", main="")
abline(h = 3, col=2)
dev.off()

### Figure 4. Plot the groups on the FCA axes ----

pdf("Fig4.pdf", 6, 8)
s.chull(fza2$li, as.factor(k), optchull=1, cpoint=1, 
        label=c("G1","G2","G3","G4","G5","G6"),
        grid=T)
dev.off()

# Explore the distribution of fishers among groups
table(k)
table(dfs$Region, k)
table(dfs$Along.the.coast, k)
table(dfs$Deep.sea, k)
table(dfs$In.the.reef.area, k)
table(dfs$q19boatsize, k)
table(dfs$q21engine1, k)
table(dfs$q32Fisnearfad,k)
table(dfs$q20gearBasket,k)
table(dfs$q20gearhandline,k)
table(dfs$q20gearVerticalline,k)
table(dfs$q20gearLargeNet,k)
table(dfs$q20gearGillnet,k)
table(dfs$q20gearOthers,k)


# Add the number of cluster (k) to the FCA dataset (df3)
ks2 <- merge(k,df3, by=0)
str(ks2)
row.names(ks2) <- ks2[,1]
ksrn <- ks2[, 1:2] # To merge later with fish dataset

ks2 <- ks2[,-1] # Remove row names
for (i in 1:22) ks2[,i] <- factor(ks2[,i]) # transform  as  factor
str(ks2)
names(ks2)
names(ks2) <- c("k" , "1a.Region.East", "1b.Region.North",  "1c.Region.South",  "1d.Region.West" ,  
                "4a.Area.Along the coast", "4b.Area.Deep sea" ,       "4c.Area.Reef area" , 
                "5a.Gear.Basket","5b.Gear.Handline","5c.Gear.Vertical Line",
                "5d.Gear.Large net","5e.Gear.Gill net" ,"5f.Gear.Other Gears",
                "3a.Engine.Outboard <8cv","3c.Engine.Outboard >26cv", "3b.Engine.Outboard 9-25cv",     
                "2c.Boat.Fibre glass" ,  "2a.Boat.Pirogue<8m" ,     "2b.Boat.Pirogue>8m" ,     
                "6a.FAD.No",  "6b.FAD.Yes")       

catdes.ks2 <- catdes(ks2, num.var=1, proba=1)
k1 <- catdes.ks2$category$`1` ; k1

k1.vtest <- k1[order(row.names(k1)),5]

k2 <- catdes.ks2$category$`2`; k2
k2.vtest <- k2[order(row.names(k2)),5]

k3 <- catdes.ks2$category$`3`
k3.vtest <- k3[order(row.names(k3)),5]

k4 <- catdes.ks2$category$`4`
k4.vtest <- k4[order(row.names(k4)),5]

k5 <- catdes.ks2$category$`5`
k5.vtest <- k4[order(row.names(k5)),5]

k6 <- catdes.ks2$category$`6`
k6.vtest <- k4[order(row.names(k6)),5]

# Create tabcatdes gathering the values of vtest for the modalities of the 6 variables
tabcatdes <- rbind(k1.vtest,k2.vtest,k3.vtest,k4.vtest,k5.vtest,k6.vtest)
# for each modality keep only response "1"
tabcatdes1 <- tabcatdes[,seq(2,42,2)]
dim(tabcatdes1)
# Remove fad.No
tabcatdes1 <- tabcatdes1[,-20]
print(t(tabcatdes1), digits=2)

# color palette for the heatmap
col = rev(brewer.pal(n = 5, name = "RdBu"))

labrow <- c("Region.East", "Region.North",  "Region.South",  "Region.West" ,  
            "Boat.Pirogue<8m" ,     "Boat.Pirogue>8m" , "Boat.Fibre glass" ,     
            "Engine.Outboard <8cv","Engine.Outboard 9-25cv", "Engine.Outboard >26cv",     
            "Area.Along the coast", "Area.Deep sea" ,       "Area.Reef area" , 
            "Gear.Basket","Gear.Handline","Gear.Vertical Line",
            "Gear.Large net","Gear.Gill net" ,"Gear.Other Gears",
            "FAD.Yes")
labcol <- c("G1","G2","G3","G4","G5","G6")

### Figure S2 - Heatmap ----

pheatmap(t(tabcatdes1),cluster_rows = F,cluster_cols = F,scale="column",
         color = col, legend=T,
         labels_row = labrow, labels_col = labcol)

#Save heatmap as PDF file using the filename parameter in pheatmap()
pheatmap(t(tabcatdes1),cluster_rows = F,cluster_cols = F,scale="column",
         color = col, legend=T,
         filename="FigS2_pheatmap.pdf", width=10, height=8,
         labels_row = labrow, labels_col = labcol)
dev.off()

rm(list=c("catdes.ks2", "df1", "df1disj", "df2","df3","fz2","fza2","h1","k1","k2",
          "k3", "k4", "k5", "k6", "tabcatdes", "tabcatdes1"))
rm(list=c("colsel","d1","i","iner","k1.vtest","k2.vtest","k3.vtest",
          "k4.vtest","k5.vtest","k6.vtest","labcol","labrow"))


# 3.2. Global Change --------

# 1. Select variables relative to GC in a new table dfgc
#60.	Have you observed any change in rainfall pattern
#63.	Do you find a change in the frequency of torrential rain
#64.	If the temperature is higher, are you observing more or less fish in summer
#65.	If the temperature is higher, are you observing more or less fish in winter
#66.	Are there any change in the number of species of fish

colsel=c("q60rainy1n0", "q63m3s2l1", "q64summ1l0", "q65winm1l0", "q66m3s2l1","q32Fisnearfad")
dfgc <- df[, colsel]
str(dfgc)
dfgc <- na.omit(dfgc)
summary(dfgc)

# Clean modalities of GC variables
# q60rainy1n0 : 60.	Have you observed any change in rainfall pattern/distribution in different seasons?
summary(as.factor(dfgc$q60rainy1n0)) 
dfgc <- dfgc[dfgc$q60rainy1n0 %in% c(0,1),] # keep only 0 or 1
table(as.factor(dfgc$q60rainy1n0))

# q63m3s2l1 : 63.	Do you find a change in the frequency of torrential rain over the last 10-15 years?
summary(as.factor(dfgc$q63m3s2l1)) 
dfgc <- dfgc[dfgc$q63m3s2l1 %in% c(1,2,3),] # keep only 1,2,3
table(as.factor(dfgc$q63m3s2l1))

# q64summ1l0 : 64.	If the temperature is higher, are you observing more or less fish in summer?
summary(as.factor(dfgc$q64summ1l0))
dfgc$q64summ1l0[dfgc$q64summ1l0=="oui"] <- "1"
dfgc <- dfgc[dfgc$q64summ1l0 %in% c(0,1),] # keep only 0 or 1
table(as.factor(dfgc$q64summ1l0))

# q65winm1l0: 65.	If the temperature is higher, are you observing more or less fish in winter?
summary(as.factor(dfgc$q65winm1l0))
dfgc$q65winm1l0[dfgc$q65winm1l0=="non"] <- "0"
dfgc <- dfgc[dfgc$q65winm1l0 %in% c("0","1"),]  # keep only 0 or 1
table(as.factor(dfgc$q65winm1l0))

#q66m3s2l1: 66.	Are there any change in the number of species of fish -compare 10-15 years from now?
summary(as.factor(dfgc$q66m3s2l1))
dfgc <- dfgc[dfgc$q66m3s2l1 %in% c(1,2,3),]  # keep only 1,2,3
table(as.factor(dfgc$q66m3s2l1))

## Table 1. Percentage table ----

#60.	Have you observed any change in rainfall pattern
100*table(as.factor(dfgc$q60rainy1n0))/dim(dfgc)[1]
#63.	Do you find a change in the frequency of torrential rain
100*table(as.factor(dfgc$q63m3s2l1))/dim(dfgc)[1]
#64.	If the temperature is higher, are you observing more or less fish in summer
100*table(as.factor(dfgc$q64summ1l0))/dim(dfgc)[1]
#65.	If the temperature is higher, are you observing more or less fish in winter
100*table(as.factor(dfgc$q65winm1l0))/dim(dfgc)[1]
#66.	Are there any change in the number of species of fish
100*table(as.factor(dfgc$q66m3s2l1))/dim(dfgc)[1]


# Variables as factors
dfgc <- droplevels(dfgc)
dfgc1 <- data.frame(lapply(dfgc, as.factor), row.names=rownames(dfgc))
summary(dfgc1)
names(dfgc1)
names(dfgc1) <- c("q60_change_rain_y1n0","q63_torrential_rain_m3s2l1","q64_fish_sum_m1l0","q65_fish_win_m1l0",
                 "q66_nb_species_m3s2l1","q32_FishNearFAD")
names(dfgc1)


## Chi-2 tests on the 5 GC variables according to Fuzzy CA groups

# Add the cluster number (k) in this table of GC variables
dfk <-as.data.frame(k)
dim(dfk)
rownames(dfk)

dim(dfgc1)
rownames(dfgc1)

# merge by rowname (by=0) => ks
ks <- merge(k, dfgc1, by=0)
names(ks)
rownames(ks)<-ks[,1]
row.names(ks)
ks <- ks[,-1]
ks$x <- as.factor(ks$x)
str(ks)

## Table S2. Numbers and Chisq tests according to cluster number ----
table(ks$q60_change_rain_y1n0,ks$x)
chisq.test(ks$q60_change_rain_y1n0,ks$x)

table(ks$q63_torrential_rain_m3s2l1,ks$x)
chisq.test(ks$q63_torrential_rain_m3s2l1,ks$x)

table(ks$q64_fish_sum_m1l0, ks$x)
chisq.test(ks$q64_fish_sum_m1l0,ks$x)

table(ks$q65_fish_win_m1l0, ks$x)
chisq.test(ks$q65_fish_win_m1l0, ks$x)

table(ks$q66_nb_species_m3s2l1, ks$x)
chisq.test(ks$q66_nb_species_m3s2l1, ks$x)


# Proceed with post hoc test when chisq.test significant (pvalue < 0.05)
print(chisq.test(ks$q64_fish_sum_m1l0,ks$x)$expected, digits=2)
print(chisq.test(ks$q64_fish_sum_m1l0,ks$x)$observed, digits=2)

chisq.posthoc.test(table(ks$q64_fish_sum_m1l0,ks$x), method="bonferroni")



# 3.3. Main species per cluster ------

# Read catch file by fisherman and species
fish_catch <- read.csv2("Fish_catch_Mauritius_220912.csv",header = T, stringsAsFactors = F)

# Merge this file with cluster number for each fisherman saved above
fishk <- merge(fish_catch, ksrn, by.x="Response.No.", by.y="Row.names")
head(fishk)

# Cross table of numbers by cluster x species
t6n <- table(fishk$Fish.code,fishk$x)
t6n

# Make it a data frame
d6n <- data.frame(cbind(t6n[,1],t6n[,2],t6n[,3],t6n[,4],t6n[,5],t6n[,6]))
names(d6n)<-c("G1","G2","G3","G4","G5","G6")
rownames(d6n)
colSums(d6n)


### Table S3 ----

# Read fish list with other names of fish
fish_list_Mauritius <- read.csv2("Fish_list_Mauritius_220912.csv")
head(fish_list_Mauritius)
str(fish_list_Mauritius)

# Merge with the crosstable
d6n$Fish.code <- row.names(d6n)
table3 <- merge (fish_list_Mauritius, d6n, all.y=T)

# Sum and percent per species
table3$total <- rowSums(table3[,5:10])
tot <- sum(table3$total)
table3$percent <- 100* table3$total/sum(table3$total)

# Order the cross table by decreasing total numbers per species
table3 <-table3[order(rowSums(table3[,5:10]),decreasing = T),]
head(table3, digits=1)

write.csv2(table3, "Table3.csv", row.names = F)


# chi-squared test between the 6 groups of fishers and the 14 main species caught
# Cross table of numbers by cluster x species with common name
t6n <- table(fishk$Common.name,fishk$x)
d6n <- data.frame(cbind(t6n[,1],t6n[,2],t6n[,3],t6n[,4],t6n[,5],t6n[,6]))
names(d6n)<-c("G1","G2","G3","G4","G5","G6")
d6n <-d6n[order(rowSums(d6n),decreasing = T),]
head(d6n)

chi6n <- chisq.test(d6n[1:14,])
chi6n <- chisq.test(d6n[1:14,], simulate.p.value = T, B=5000)
chi6n

# Standardized residuals, (observed - expected) / sqrt(V), 
# where V is the residual cell variance (Agresti, 2007, section 2.4.5 for the case 
# where x is a matrix, n * p * (1 - p) otherwise).
difchi2 <-data.frame(chi6n$stdres) 
print(difchi2, digits=2)

### Figure 5. Heatmap showing the standardized residuals of the chi-sq test ----
pheatmap(chi6n$stdres, scale="column", margins = c(2,10), clustering_method = "ward.D2",
         clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean",
         legend=T,color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
         filename="Fig5.pdf", width=10, height=10)
