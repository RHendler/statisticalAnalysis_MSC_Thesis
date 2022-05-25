library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(corrr)
library(ggpubr)
library(geomorph)
library(stats)
library(grid)
library(scales)
library(cocor)
library(lmodel2)
library(lmtest)
library(car)
library(rstatix)
library(tidyverse)
library(ggpmisc)
library(emmeans)
library(FSA)
library(multcomp)
library(MASS)
library(AICcmodavg)


library(mcr)
library(CPAT)
###################################################################################################################################
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

###############################################################################
Data_all <- read_excel("Data.xlsx",sheet = "data.all") %>% select(ID,species, block, canopy_1 ,canopy_2 ,height.m,canopy_1.ortho	,canopy_2.ortho,
                                                                  canopy.area,crown.2d.manual, canopy.2d, stem.area, stem.total.d, count_branches,SW_sum)

Data_stained <- read_excel("Data.xlsx",sheet = "stained") %>% select(species,block,id,sap,prop_sap,total_area,diameter,rad,bark_depth)

Data_stained$sap.cm2 <- log(Data_stained$sap)
Data_stained$diameter.cm <- log(Data_stained$diameter)
Data_stained$rad.cm <- log(Data_stained$rad)
Data_stained$species <- factor(Data_stained$species,
                               levels = c("C. alexandri","C. mopane","D. cinerea", "S. mellifera", "V. nebrownii", "V. reficiens"))

Data_all$species <- factor(Data_all$species,
                           levels = c("C. alexandri","C. mopane","D. cinerea", "S. mellifera", "V. nebrownii", "V. reficiens"))

Data_heights <- read_excel("Data.xlsx",sheet = "heights") %>% select(species,	low,	high,	low_branches,	high_branches,	canopy)
Data_ups <- read_excel("Data.xlsx",sheet = "luca_data") %>% select(Set,crown.area,total.stem.area,mean_diameter,total.stem.d, SW_sum,count_stems, height_m) %>% filter(!is.na(total.stem.area))
Data_gps <- read_excel("Data.xlsx",sheet = "GPS") 
Data_gps$species <- factor(Data_gps$species,
                           levels = c("C. alexandri","C. mopane","D. cinerea", "S. mellifera", "V. nebrownii", "V. reficiens"))


###################################################################################################################################

for (specie in levels(Data_all$species)){
  
  if(specie == "C. alexandri"){
    temp_col = "#fde725"
  b <-5.72}
  else if (specie == "S. mellifera"){
    temp_col ="#2a788e"
  b <-6.65} 
  else if (specie == "C. mopane"){
    temp_col = "#7ad151"
  b <-12.97} 
  else if (specie == "V. nebrownii"){
    temp_col ="#414487"
  b <-10.14}
  else if (specie == "V. reficiens"){
    temp_col = "#440154"
  b <-1.71}
  else if (specie == "D. cinerea"){
    temp_col = "#22a884"
  b <-3.93}
  

crowns <- Data_all$canopy.area[Data_all$species == specie]

sap.p <- (b*(crowns^1.36))/10000
# 
# plot(crowns,sap.p)
# plot(log(crowns),log(sap.p))

data.comb <- data.frame(crowns,sap.p)
p <- ggplot(data.comb, aes(x = crowns, y = sap.p))+
  geom_point(color = temp_col, size = 1.5)+
  ylab(expression(Sapwood~area~(m^2)))+
  xlab(expression(Crown~area~(m^2)))+
  ggtitle(specie)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=16))


assign(paste0(specie,"_CSAPplot"),p)


}

CSAPfigure <- ggarrange(`C. alexandri_CSAPplot`,`C. mopane_CSAPplot`,`D. cinerea_CSAPplot`,
                       `S. mellifera_CSAPplot`,`V. nebrownii_CSAPplot`,`V. reficiens_CSAPplot`,
                       
                       labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                       ncol = 2, nrow = 3)
CSAPfigure
###################################################################################################################################
for (specie in levels(Data_all$species)){
  
  
}

ggplot(Data_all,aes(log(canopy.area),log(SW_sum)))+
  geom_point(aes(col = species))+
  scale_color_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  
  
  geom_smooth(method = 'lm',formula=y~x, col = "black")+
  facet_wrap(~species, scales = "free")+
  theme_classic2()+
  
  xlab(expression(Crown~Area~(log~m^2)))+
  ylab(expression(Sapwood~Area~(log~cm^2)))+
  theme(legend.position = 'none')

temp <- data.frame(Data_all$species,Data_all$stem.total.d,Data_all$count_branches)
stem_count <- lm(data = temp, Data_all.count_branches ~ 0 + .)
summary(stem_count)

mop <- Data_all %>% filter(species == "C. mopane")
stem_count_mop <- lm(data = temp_mop, mop.count_branches ~ 0 + mop.stem.total.d)
summary(stem_count_mop)

###################################################################################################################################
for (specie in levels(factor(Data_all$species))){
  temp <- Data_all %>% filter(species == specie)
  hist(temp$count_branches,main= as.character(specie))
  
}
###################################################################################################################################

ggplot(Data_stained,aes(x = diameter,y = sap/total_area))+
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                     values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  scale_color_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  
  facet_wrap(~species)+
  geom_point(col = "darkred", size = 2)+
  geom_smooth(method = "lm")+
  xlab(expression(Stem~Diameter~(cm)))+
  ylab(expression(Relative~sapwood~area))+
    theme_classic2()+
  theme(strip.text = element_text(face = "italic"),
        text = element_text(size=17))


Data_stained <- Data_stained %>%  mutate(proportion = sap/total_area)

### plots 

for (specie in levels(factor(Data_all$species))){
  
  temp <- Data_stained %>% filter(species == specie)  %>% mutate(proportion = sap/total_area)
  prop.model <- lm(proportion ~ diameter.cm, data = temp)
  
  if(specie == "C. alexandri"){temp_col = "#fde725"}
  else if (specie == "S. mellifera"){temp_col ="#2a788e"} 
  else if (specie == "C. mopane"){temp_col = "#7ad151"} 
  else if (specie == "V. nebrownii"){temp_col ="#414487"}
  else if (specie == "V. reficiens"){temp_col = "#440154"}
  else if (specie == "D. cinerea"){temp_col = "#22a884"}
  
  p <- ggplot(temp,aes(diameter/10,sap/total_area))+
    
    geom_point(size = 2,col = temp_col)+
    geom_smooth(method = "lm", col = "black", se = F)+
    annotate("text",y = 0.99, x = 1,size = 5, label =  deparse( substitute( R^2 == rr, list(
      rr= format(round(summary(prop.model)$r.squared,2)))
    )), parse=TRUE) +
    annotate("text",y = 0.985, x = 4,size = 5, label = paste0(" , p-value = ", round(lmp(prop.model),2)))+
    theme_classic2()+
    xlab(expression(Stem~Diameter~(cm)))+
    ylab(expression(Relative~Sapwood~Area))+
    ggtitle(specie)+
    ylim(0,1)+
    xlim(0,10)+
    # ylim()+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=16))
  
  assign(paste0(specie,"_plot"),p)
  
  
  print(specie)
  print(summary(prop.model))
  
  res <- resid(prop.model)
  
  tempo <- data.frame(resid(prop.model),fitted(prop.model))
  
  pp <- ggplot(tempo,aes(x=fitted.prop.model.,y=resid.prop.model.))+
    geom_point(col = temp_col,size = 2)+
    geom_abline(intercept = 0, slope = 0)+
    xlab("Fitted Values (log stem diameter)")+
    ylab("Residuals")+

    ggtitle(specie)+
    # ylim()+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=16))
  
  assign(paste0(specie,"_RESplot"),pp)


}

figure_relative_Sap <- ggarrange(`C. alexandri_plot`,`C. mopane_plot`,`D. cinerea_plot`,
                    `S. mellifera_plot`,`V. nebrownii_plot`,`V. reficiens_plot`,
                    
                    labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                    ncol = 2, nrow = 3)
figure_relative_Sap

RESfigure <- ggarrange(`C. alexandri_RESplot`,`C. mopane_RESplot`,`D. cinerea_RESplot`,
                       `S. mellifera_RESplot`,`V. nebrownii_RESplot`,`V. reficiens_RESplot`,
                       
                       labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                       ncol = 2, nrow = 3)
RESfigure

#######################################################################################

for (specie in levels(factor(Data_all$species))){
  
  temp <- Data_all %>% filter(species == specie)
  data_temp <- data.frame(temp$canopy.area,temp$SW_sum)
  
  
  lm_i <- lm(log(temp.SW_sum) ~ log(temp.canopy.area) , data = data_temp)
  print(specie)
  print(summary(lm_i))
  # print(c("CF: ",logbtcf(lm_i,base = exp(1))))
  

  
  #intervals <- confint(lm_i)
  
  if(specie == "C. alexandri"){temp_col = "#fde725"}
  else if (specie == "S. mellifera"){temp_col ="#2a788e"} 
  else if (specie == "C. mopane"){temp_col = "#7ad151"} 
  else if (specie == "V. nebrownii"){temp_col ="#414487"}
  else if (specie == "V. reficiens"){temp_col = "#440154"}
  else if (specie == "D. cinerea"){temp_col = "#22a884"}
  
  
  x.model <- lm(log(temp.SW_sum) ~ log(temp.canopy.area),data = data_temp)
  
  p <- ggplot(data_temp,aes(log(temp.canopy.area),log(temp.SW_sum)))+
    
    geom_point(col = temp_col)+
    annotate("text",y = 7.5, x = -1.5,size = 5, label =  deparse( substitute( R^2 == rr, list(
      rr= format(round(summary(x.model)$r.squared,2)))
    )), parse=TRUE) +
    annotate("text",y = 7.45, x = 0.5,size = 5, label = " , p-value < 0.001 ")+
    geom_smooth(method = "lm", col = "black")+
    theme_classic2()+
    xlab(expression(Crown~Area~(log~m^2)))+
    ylab(expression(Sapwood~Area~(log~cm^2)))+
    ggtitle(specie)+
    ylim(-2,8)+
    xlim(-2,5)+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=16))
  
  assign(paste0(specie,"_plot"),p)
  assign(paste0("lm.",specie),lm_i)
  
  res <- resid(lm_i)
  
  tempo <- data.frame(resid(lm_i),fitted(lm_i))
  
  # plot(fitted(lm_i), res,xlab = "fitted values (log total sapwood area)", main= as.character(specie),
  #      ylab = "residuals")
  # abline(0,0, lty = "dashed",col = "red")
  
  pp <- ggplot(tempo,aes(x=fitted.lm_i.,y=resid.lm_i.))+
    geom_point(col = temp_col)+
    geom_abline(intercept = 0, slope = 0)+
    xlab("Fitted Values (log sapwood area)")+
    labs(title = specie)+
    ylab("Residuals")+
    theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=18))+
    theme_classic2()
  
  assign(paste0(specie,"_RESplot"),pp)
  
}


figure <- ggarrange(`C. alexandri_plot`,`C. mopane_plot`,`D. cinerea_plot`,
                    `S. mellifera_plot`,`V. nebrownii_plot`,`V. reficiens_plot`,
                    
                    labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                    ncol = 2, nrow = 3)
figure


RESfigure <- ggarrange(`C. alexandri_RESplot`,`C. mopane_RESplot`,`D. cinerea_RESplot`,
                    `S. mellifera_RESplot`,`V. nebrownii_RESplot`,`V. reficiens_RESplot`,
                    
                    labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                    ncol = 3, nrow = 2)
RESfigure
######################################################################################
# ALLOMETRY -  STEM DIAMETER VS. SAPWOOD AREA

ggplot(Data_stained,aes(x = exp(diameter.cm),y=exp(sap.cm2)))+
  geom_point(shape = 21,alpha = 0.9, color = "black",
             aes( fill = species), size = 3)+  theme_classic2()+
  xlab(expression(Stem~Diameter~(cm)))+
  ylab(expression(Sapwood~Area~(cm^2)))+
  scale_color_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                     values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  
  theme(legend.text = element_text(size = 20,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.13,0.78),legend.title=element_blank(),legend.key.size = unit(1., "cm"),
        text = element_text(size=18))


lm_all_a <- lm((sap.cm2)  ~ (diameter.cm) *  species, data = Data_stained)
lm_all_no_int <- lm((sap.cm2)  ~ diameter.cm  + species, data = Data_stained)
lm_all_no_sp <- lm((sap.cm2)  ~ (diameter.cm), data = Data_stained)

summary(lm_all_no_sp)
summary(lm_all_a)
summary(lm_all_no_int)

lm_all_a.mop <- lm((sap.cm2)  ~ (diameter.cm) , data = Data_stained %>% filter(species == "C. mopane"))
summary(lm_all_a.mop)
confint(lm_all_a.mop)
ggplot(Data_stained %>% filter(species == "C. mopane"),aes(x = , y = ))

anova(lm_all_no_sp,lm_all_a,lm_all_no_int)
anova(lm_all_a,lm_all_no_int)

AIC(lm_all_no_sp)
AIC(lm_all_a)
AIC(lm_all_no_int)

anova(lm_all_a)

models <- list(lm_all_no_sp,lm_all_a,lm_all_no_int)
model.names <- c("no.sp","interaction","no interaction")
aictab(models,model.names)

# residual plot

res <- resid(lm_all_a)
plot(fitted(lm_all_a), res,xlab = "fitted values (log sapwood area)",ylab = "residuals")
abline(0,0, lty = "dashed",col = "red")


# model plot

all_sap <- ggplot(data = Data_stained, aes(x = (diameter.cm), y = (sap.cm2)))+
  geom_point(shape = 21,alpha = 0.9, color = "black",
             aes( fill = species), size = 3)+
  xlab(expression(Stem~Diameter~(log~cm)))+
  ylab(expression(Sapwood~Area~(log~cm^2)))+
  annotate("text",y = 9, x = 1.5,size = 6, label =  deparse( substitute( R^2 == rr, list(
    rr= format(summary(lm_all_a)$r.squared, digits = 2))
  )), parse=TRUE) +
  annotate("text",y = 8.95, x = 2.1,size = 6, label = ", p-value < 2.2e-16")+
  geom_abline(intercept = coef(lm_all_a)[1], slope  = coef(lm_all_a)[2],col = '#fde725', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)["speciesC. mopane"], slope  = coef(lm_all_a)[2]+ coef(lm_all_a)["diameter.cm:speciesC. mopane"],col = '#7ad151', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)["speciesD. cinerea"], slope  = coef(lm_all_a)[2]+coef(lm_all_a)["diameter.cm:speciesD. cinerea"],col = '#22a884', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)["speciesS. mellifera"], slope  = coef(lm_all_a)[2]+coef(lm_all_a)["diameter.cm:speciesS. mellifera"],col = '#2a788e', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)["speciesV. nebrownii"], slope  = coef(lm_all_a)[2]+coef(lm_all_a)["diameter.cm:speciesV. nebrownii"],col = '#414487', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)["speciesV. reficiens"], slope  = coef(lm_all_a)[2]+coef(lm_all_a)["diameter.cm:speciesV. reficiens"],col = '#440154', lty = 1, lwd = 1.0) +
  
  scale_color_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                     values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  theme_classic()+
  theme(legend.text = element_text(size = 18,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.89,0.26),legend.title=element_blank(),legend.key.size = unit(1, "cm"),
        text = element_text(size=17))
all_sap

confint(lm_all_a)
# pairs 

test.aov <- aov(lm_all_a)
summary(test.aov)
summaryAOV <- summary(glht(test.aov, linfct = mcp(species = "Tukey")))
par(mar = c(2.5,12,4,2))
plot(summaryAOV)

confint(lm_all_a)


lm_i <- lm((sap.cm2)  ~ (diameter.cm) *  species, data = Data_stained)

for (specie in levels(factor(Data_stained$species))){
  
  temp <- Data_stained %>% filter(species == specie)
  data_temp <- data.frame(temp$diameter.cm,temp$sap.cm2)
  
  
  print(specie)
  print(summary(lm_i))
  # print(c("CF: ",logbtcf(lm_i,base = exp(1))))
  print(confint(lm_i))
  
  #intervals <- confint(lm_i)
  
  if(specie == "C. alexandri"){
    temp_col = "#fde725"
    inter<-1
    slop<-2
  }else if (specie == "S. mellifera"){
    temp_col ="#2a788e"
    inter<-paste0("species",specie)
    slop<-paste0("diameter.cm:species",specie)
  }else if (specie == "C. mopane"){
    temp_col = "#7ad151"
    inter=paste0("species",specie)
    slop=paste0("diameter.cm:species",specie)
  }else if (specie == "V. nebrownii"){
    temp_col ="#414487"
    inter=paste0("species",specie)
    slop=paste0("diameter.cm:species",specie)
  }else if (specie == "V. reficiens"){
    temp_col = "#440154"
    inter=paste0("species",specie)
    slop=paste0("diameter.cm:species",specie)
  }else if (specie == "D. cinerea"){
    temp_col = "#22a884"
    inter=paste0("species",specie)
    slop=paste0("diameter.cm:species",specie)
  }
  
  
  p <- ggplot(data = data_temp, aes(x = (temp.diameter.cm), y = (temp.sap.cm2)))+
    
    geom_point(shape = 21,alpha = 0.9, size = 3, fill = temp_col)+     
    
    # geom_abline(intercept = coef(lm_all_a)[1]+coef(lm_all_a)[inter], 
    #           slope  = coef(lm_all_a)[2]+ coef(lm_all_a)[slop],
    #           col = temp_col, lty = 1, lwd = 1.0) +
    
    geom_smooth(method = "lm", col = "black")+
    theme_classic2()+
    xlab(expression(Stem~Diameter~(log~cm)))+
    ylab(expression(Sapwood~Area~(log~cm^2)))+
    ggtitle(specie)+
    ylim(min(Data_stained$sap.cm2),max(Data_stained$sap.cm2))+
    xlim(min(Data_stained$diameter.cm),max(Data_stained$diameter.cm))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5,face = 'italic'),text=element_text(size=16))
  
  assign(paste0(specie,"_plot"),p)
  assign(paste0("lm.",specie),lm_i)
  
  
  
}


figure <- ggarrange(`C. alexandri_plot`,`C. mopane_plot`,`D. cinerea_plot`,
                    `S. mellifera_plot`,`V. nebrownii_plot`,`V. reficiens_plot`,
                    
                    labels = c("A", "B", "C", "D", "E", "F"),label.y = 0,
                    ncol = 2, nrow = 3)
figure



###################################################################################################################################
# ALLOMETRY - CROWN AREA VS. SAPWOOD AREA


lm_all2_canopy <- lm(log(SW_sum) ~ log(canopy.area) + factor(species)  , data = Data_all)
lm_all2_canopy.int <- lm(log(SW_sum) ~ log(canopy.area) * factor(species)  , data = Data_all)
lm_all2_canopy.no.sp <- lm(log(SW_sum) ~ log(canopy.area)  , data = Data_all)

summary(lm_all2_canopy)
summary(lm_all2_canopy.int)
summary(lm_all2_canopy.no.sp)

anova(lm_all2_canopy.no.sp,lm_all2_canopy,lm_all2_canopy.int)

AIC(lm_all2_canopy)
AIC(lm_all2_canopy.int)
AIC(lm_all2_canopy.no.sp)

anova(lm_all2_canopy.int)
anova(lm_all2_canopy)

models <- list(lm_all2_canopy.no.sp,lm_all2_canopy.int,lm_all2_canopy)
model.names <- c("no.sp","interaction","no interaction")
aictab(models,model.names)
# residual plot

res <- resid(lm_all2_canopy)
plot(fitted(lm_all2_canopy), res,xlab = "fitted values (ln total stem diameter)",ylab = "residuals")
abline(0,0, lty = "dashed",col = "red")

# model plot

plot_canopy <- ggplot(data = Data_all, aes(x = log(canopy.area), y = log(SW_sum)))+
  geom_point(shape = 21,
             color = "black", size = 2,aes(fill = species))+
  annotate("text",y = 7.1, x = -2.4,size = 6, label =  deparse( substitute( R[adj]^2 == rr, list(
    rr= format(summary(lm_all2_canopy)$r.squared, digits = 2))
  )), parse=TRUE) +
  annotate("text",y =  7.1, x = -0.9,size = 6, label = ", p-value < 2.2e-16")+
  geom_abline(intercept = coef(lm_all2_canopy)[1], slope  = coef(lm_all2_canopy)[2],col = '#fde725', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy)[1]+coef(lm_all2_canopy)["speciesC. mopane"], slope  = coef(lm_all2_canopy)[2],col = '#7ad151', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy)[1]+coef(lm_all2_canopy)["speciesD. cinerea"], slope  = coef(lm_all2_canopy)[2],col = '#22a884', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy)[1]+coef(lm_all2_canopy)["speciesS. mellifera"], slope  = coef(lm_all2_canopy)[2],col = '#2a788e', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy)[1]+coef(lm_all2_canopy)["speciesV. nebrownii"], slope  = coef(lm_all2_canopy)[2],col = '#414487', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy)[1]+coef(lm_all2_canopy)["speciesV. reficiens"], slope  = coef(lm_all2_canopy)[2],col = '#440154', lty = 1, lwd = 1.0) +
  
  xlab(expression(Crown~area~(ln~m^2)))+
  ylab(expression(Sapwood~Area~(ln~cm^2)))+
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  scale_color_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                     values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  theme_classic()+
  theme(legend.text = element_text(size = 20,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.86,0.17),legend.title=element_blank(),legend.key.size = unit(1.1, "cm"),
        text = element_text(size=17))

plot_canopy

# pairs 

class(Data_all$species)

test.aov <- aov(lm_all2_canopy)
summary(test.aov)
summaryAOV <- summary(glht(test.aov, linfct = mcp(species = "Tukey")))
par(mar = c(2.5,12,4,2))
plot(summaryAOV)
  
confint(lm_all2_canopy)
###################################################################################################################################
# METHODS COMPARISON



###################################################################################################################################
# POLYGONS PABA

cor1.data <- Data_all %>% filter(!is.na(canopy.2d))

plot((cor.data$canopy.2d),(cor.data$canopy.area))
abline(a = 0,b=1,col = "Red")


ks.test(cor.data$canopy.2d,cor.data$canopy.area)

PB.reg1 <- mcreg(cor.data$canopy.2d,cor.data$canopy.area, method.reg = "PaBa")
PB.reg1@para

par(mar=c(5,5,3,2))
MCResult.plot(PB.reg1, equal.axis = TRUE,
              cex.lab = 1.4, y.lab = expression(Crown~area~(m^2)~-~field),ylim = c(0,130),
              x.lab = expression(Crown~area~(m^2)~-~polygons), points.col = "darkgreen",
              points.pch = 19, ci.area = TRUE, main = "",sub = "", 
              add.grid = T, points.cex = 0.7)


for (specie in levels(factor(Data_all$species))){
cor1.data <- Data_all %>% filter(!is.na(canopy.2d),species == specie)

PB.reg1 <- mcreg(cor1.data$canopy.2d,cor1.data$canopy.area, method.reg = "PaBa")
print(specie)
print(PB.reg1@para)

par(mar=c(5,5,3,2))
MCResult.plot(PB.reg1, equal.axis = TRUE,
              cex.lab = 1.4, y.lab = expression(Crown~area~(m^2)~-~field),
              x.lab = expression(Crown~area~(m^2)~-~polygons), points.col = "darkgreen",
              points.pch = 19, ci.area = TRUE, main = specie,sub = "", 
              add.grid = T, points.cex = 0.7)
}
###################################################################################################################################
# ACS PABA

cor2.data <- Data_all %>% filter(!is.na(crown.2d.manual))

plot(cor2.data$crown.2d.manual,(cor2.data$canopy.area))
abline(a = 0,b=1,col = "Red")


ks.test(cor2.data$crown.2d.manual,cor2.data$canopy.area)

PB.reg2 <- mcreg(cor2.data$crown.2d.manual,cor2.data$canopy.area, method.reg = "PaBa")
PB.reg2@para

par(mar=c(5,5,3,2))
MCResult.plot(PB.reg2, equal.axis = TRUE,
              cex.lab = 1.4, y.lab = expression(Crown~area~(m^2)~-~field),ylim = c(0,130),
              x.lab = expression(Crown~area~(m^2)~-~digital~ACS), points.col = "darkgreen",
              points.pch = 19, ci.area = TRUE, main = "",sub = "", 
              add.grid = T, points.cex = 0.7)



# per species
for (specie in levels(factor(Data_all$species))){
  
  
cor2.data <- Data_all %>% filter(!is.na(crown.2d.manual),species == specie)

PB.reg2 <- mcreg(cor2.data$crown.2d.manual,cor2.data$canopy.area, method.reg = "PaBa")
print(specie)
print(PB.reg2@para)

par(mar=c(5,5,3,2))
MCResult.plot(PB.reg2, equal.axis = TRUE,
              cex.lab = 1.4, y.lab = expression(Crown~area~(m^2)~-~field),
              x.lab = expression(Crown~area~(m^2)~-~digital~ACS), points.col = "darkgreen",
              points.pch = 19, ci.area = TRUE, main = specie,sub = "", 
              add.grid = T, points.cex = 0.7)
}
###################################################################################################################################
# height PABA

mar=c(5,12,3,2)
ks.test(cor.data$height.m,cor.data$canopy.area, na.rm = F)

new <- cor.data %>% filter(!is.na(height.m))
PB.reg3 <- mcreg(new$canopy.area,new$height.m, method.reg = "PaBa")
PB.reg3@para

par(mar=c(5,5,3,2))
MCResult.plot(PB.reg3, equal.axis = TRUE,
              cex.lab = 1.3, y.lab = "Crown area - field", 
              x.lab = "height (m)", points.col = "darkgreen",
              points.pch = 19, ci.area = TRUE, main = "",sub = "", 
              add.grid = FALSE, points.cex = 0.5)


###################################################################################################################################
# ALLOMETRY - REMOTE SENSING

# POLYGONS

polygons_lm <- lm(log(stem.total.d) ~ log(canopy.2d) + species,cor.data)
polygons_lm.int <- lm(log(stem.total.d) ~ log(canopy.2d) * species,cor.data)
polygons_lm.no.sp <- lm(log(stem.total.d) ~ log(canopy.2d) ,cor.data)


summary(polygons_lm)
summary(polygons_lm.int)
summary(polygons_lm.no.sp)

AIC(polygons_lm)
AIC(polygons_lm.int)
AIC(polygons_lm.no.sp)

anova(polygons_lm,polygons_lm.no.sp,polygons_lm.int) # - - interaction not significant


res <- resid(polygons_lm)
plot(fitted(polygons_lm), res,xlab = "fitted values (ln total stem diameter)",ylab = "residuals")
abline(0,0, lty = "dashed",col = "red")


plot_canopy.d <- ggplot(data = Data_all , aes(x = log(canopy.2d), y = log(stem.total.d)))+
  geom_point(shape = 21,
             color = "black", size = 3,aes(fill = species))+
  xlab(expression(Crown~area~-~polygons~(log~m^2)))+
  ylim(1,5.2)+
  ylab(expression(Total~stem~diameter~(log~cm)))+
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  annotate("text",y = 5.2, x = -0.75,size = 6, label =  deparse(substitute( R[adj]^2 == rr, list(
    rr= format(summary(polygons_lm)$r.squared, digits = 2))
  )), parse=TRUE) +
  annotate("text",y =  5.2, x = 0.425,size = 6, label = ", p-value < 2.2e-16")+
  
  geom_abline(intercept = coef(polygons_lm)[1], slope  = coef(polygons_lm)[2],col = '#fde725', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(polygons_lm)[1]+coef(polygons_lm)["speciesC. mopane"], slope  = coef(polygons_lm)[2],col = '#7ad151', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(polygons_lm)[1]+coef(polygons_lm)["speciesD. cinerea"], slope  = coef(polygons_lm)[2],col = '#22a884', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(polygons_lm)[1]+coef(polygons_lm)["speciesS. mellifera"], slope  = coef(polygons_lm)[2],col = '#2a788e', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(polygons_lm)[1]+coef(polygons_lm)["speciesV. nebrownii"], slope  = coef(polygons_lm)[2],col = '#414487', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(polygons_lm)[1]+coef(polygons_lm)["speciesV. reficiens"], slope  = coef(polygons_lm)[2],col = '#440154', lty = 1, lwd = 1.0) +
  
  theme_classic()+
  theme(legend.text = element_text(size = 20,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.86,0.17),legend.title=element_blank(),legend.key.size = unit(1.1, "cm"),
        text = element_text(size=17))
plot_canopy.d


################################################################################################################################################################

# ORTHOPHOTOS - ACS

lm_all2_canopy.2dia <- lm(log(stem.total.d) ~ log(crown.2d.manual) + species  , data = Data_all )
lm_all2_canopy.2dia.int <- lm(log(stem.total.d) ~ log(crown.2d.manual) * species  , data = Data_all )
lm_all2_canopy.2dia.no.sp <- lm(log(stem.total.d) ~ log(crown.2d.manual)  , data = Data_all )

AIC( lm_all2_canopy.2dia)
AIC( lm_all2_canopy.2dia.int)
AIC( lm_all2_canopy.2dia.no.sp)

summary(lm_all2_canopy.2dia)
summary(lm_all2_canopy.2dia.int)
summary(lm_all2_canopy.2dia.no.sp)

anova(lm_all2_canopy.2dia.int,lm_all2_canopy.2dia,lm_all2_canopy.2dia.no.sp)

res <- resid(lm_all2_canopy.2dia)
plot(fitted(lm_all2_canopy.2dia), res,xlab = "fitted values (ln total stem diameter)",ylab = "residuals")
abline(0,0, lty = "dashed",col = "red")

plot_canopy.d2 <- ggplot(data = Data_all , aes(x = log(crown.2d.manual), y = log(stem.total.d), fill = species))+
  geom_point(shape = 21,
             color = "black", size = 3)+
  xlab(expression(Crown~area~-~ACS~(log~m^2)))+
  ylim(1,5.2)+
  ylab(expression(Total~stem~diameter~(log~cm)))+
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  annotate("text",y = 5.2, x = -1.2,size = 6, label =  deparse( substitute( R[adj]^2 == rr, list(
    rr= format(summary(lm_all2_canopy.2dia)$r.squared, digits = 2))
  )), parse=TRUE) +
  annotate("text",y =  5.2, x = 0.,size = 6, label = ", p-value < 2.2e-16")+
  
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1], slope  = coef(lm_all2_canopy.2dia)[2],col = '#fde725', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1]+coef(lm_all2_canopy.2dia)["speciesC. mopane"], slope  = coef(lm_all2_canopy.2dia)[2],col = '#7ad151', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1]+coef(lm_all2_canopy.2dia)["speciesD. cinerea"], slope  = coef(lm_all2_canopy.2dia)[2],col = '#22a884', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1]+coef(lm_all2_canopy.2dia)["speciesS. mellifera"], slope  = coef(lm_all2_canopy.2dia)[2],col = '#2a788e', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1]+coef(lm_all2_canopy.2dia)["speciesV. nebrownii"], slope  = coef(lm_all2_canopy.2dia)[2],col = '#414487', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(lm_all2_canopy.2dia)[1]+coef(lm_all2_canopy.2dia)["speciesV. reficiens"], slope  = coef(lm_all2_canopy.2dia)[2],col = '#440154', lty = 1, lwd = 1.0) +
  
  theme_classic()+
  theme(legend.text = element_text(size = 20,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.86,0.17),legend.title=element_blank(),legend.key.size = unit(1.1, "cm"),
        text = element_text(size=17))
plot_canopy.d2

################################################################################################################################################################

# -- compare all 3 with field  allometric formula and compare outcome

Data_compare <- Data_all %>% filter(!is.na(crown.2d.manual),!is.na(canopy.2d),species == "C. mopane") %>% select(canopy.area,canopy.2d,crown.2d.manual,stem.total.d)

x <- c(Data_compare$canopy.area,Data_compare$canopy.2d,Data_compare$crown.2d.manual)
method1 <- c(rep("Field",nrow(Data_compare)),rep("Polygons",nrow(Data_compare)),rep("Digital ACS",nrow(Data_compare)))
# y = 10.52*x^0.63

y <- (0.35 * ((5.88*(x^0.63))^1.11)^2.06)^1.04


compare <-  data.frame(x,y,method1)

ggplot(data = compare ,aes(x = method1, y = y, fill = method1))+
  geom_bar(stat = "identity")+
  theme_classic2()+
  ylab(expression(Predicted~sapwood~area~(cm^2)))+
  xlab("Method")+
 # ylim(0,18500)+
  theme(legend.position = "none",legend.title=element_blank(),legend.key.size = unit(.7, "cm"),legend.text = element_text(size = 13),
        text = element_text(size=14))

sum(compare$y[compare$method1 == "Field"])
sum(compare$y[compare$method1 == "Polygons"])
sum(compare$y[compare$method1 == "Digital ACS"])


compare$y[compare$method1 == "Polygons"] <- (compare$y[compare$method1 == "Polygons"]-1.05)/0.93
compare$y[compare$method1 == "Digital ACS"] <- (compare$y[compare$method1 == "Digital ACS"]+0.12)/1.06

y = 1.05 + 0.93 * x
x = (y - 1.05)/0.93

summary(aov(y~method1,data = compare))



for (specie in levels(factor(Data_all$species))){
  
  if(specie == "C. alexandri"){
    
    a <- 0.63
    b <- 5.88
  }else if (specie == "C. mopane"){
    
    a <- 0.63
    b <- 10.52
  }else if (specie == "D. cinerea"){
    
    a <- 0.63
    b <- 6.36
  }else if (specie == "S. mellifera"){
    
    a <- 0.63
    b <- 8.33
  }else if (specie == "V. nebrownii"){
    
    a <- 0.63
    b <- 6.82
  }else if (specie == "V. reficiens"){
    
    a <- 0.63
    b <- 6.23
  }
         
  
  data_temp <- Data_all %>% filter(!is.na(crown.2d.manual),!is.na(canopy.2d),species == specie) %>% select(canopy.area,crown.2d.manual,canopy.2d,stem.total.d)
  
  x <- c(Data_compare$canopy.area,Data_compare$canopy.2d,Data_compare$crown.2d.manual)
  method1 <- c(rep("Field",nrow(Data_compare)),rep("Polygons",nrow(Data_compare)),rep("ACS",nrow(Data_compare)))
  y = b*x^a
  
  compare <-  data.frame(x,y,method1)
  
  ggplot(data = compare ,aes(x = method1, y = y, fill = method1))+
    geom_bar(stat = "identity")+
    theme_classic2()+
    ylab(expression(Predicted~sapwood~area~(cm^2)))+
    xlab("Method")+
    # ylim(0,18500)+
    theme(legend.position = "none",legend.title=element_blank(),legend.key.size = unit(.7, "cm"),legend.text = element_text(size = 13),
          text = element_text(size=14))
  
  assign(paste0(specie,"_plot"),p)
  assign(paste0("lm.",specie),lm_i)
  
}


figure.compare <- ggarrange(`V. reficiens_plot`,`V. nebrownii_plot`,`S. mellifera_plot`,
                           `C. mopane_plot`,`D. cinerea_plot`,`C. alexandri_plot`,
                           labels = c("A", "B", "C", "D", "E", "F"),
                           ncol = 2, nrow = 3)
figure.compare


################################################################################################################################################################
# CHM - HEIGHT


stem.height <- lm( log( stem.total.d) ~ log( height.m)  +  species, data = Data_all)
stem.height.int <- lm( log( stem.total.d) ~ log( height.m)  *  species, data = Data_all)
stem.height.no.sp <- lm( log( stem.total.d) ~ log( height.m)  , data = Data_all)


summary(stem.height)
summary(stem.height.int)
summary(stem.height.no.sp)

AIC(stem.height)
AIC(stem.height.int)
AIC(stem.height.no.sp)

anova(stem.height,stem.height.int,stem.height.no.sp)

nrow(Data_all %>% filter(!is.na(height.m)))

res <- resid(stem.height.int)
plot(fitted(stem.height.int), res,xlab = "fitted values (log total stem diameter)",ylab = "residuals")
abline(0,0, lty = "dashed",col = "red")


all_heights <- ggplot(data = Data_all , aes(y = log(stem.total.d), x = log(height.m)))+
  geom_point(shape = 21,
             color = "black", size = 3,aes(fill = species))+
  xlab(expression(Crown~height~(log~m)))+
  ylab(expression(Total~stem~diameter~(log~cm)))+
  ylim(1,5.2)+
  scale_fill_manual(breaks = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"),
                    values = c("#fde725"  , "#2a788e", "#7ad151","#414487","#440154","#22a884")) +
  annotate("text",y = 5.2, x = -0.4,size = 6, label =  deparse(substitute( R[adj]^2 == rr, list(
    rr= format(summary(stem.height)$r.squared, digits = 2))
  )), parse=TRUE) +
  annotate("text",y =  5.2, x = 0.05,size = 6, label = ", p-value < 2.2e-16")+
  
  geom_abline(intercept = coef(stem.height.int)[1], slope  = coef(stem.height.int)[2],col = '#fde725', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(stem.height.int)[1]+coef(stem.height.int)["speciesC. mopane"], slope  = coef(stem.height.int)[2]+ coef(stem.height.int)["log(height.m):speciesC. mopane"],col = '#7ad151', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(stem.height.int)[1]+coef(stem.height.int)["speciesD. cinerea"], slope  = coef(stem.height.int)[2]+coef(stem.height.int)["log(height.m):speciesD. cinerea"],col = '#22a884', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(stem.height.int)[1]+coef(stem.height.int)["speciesS. mellifera"], slope  = coef(stem.height.int)[2]+coef(stem.height.int)["log(height.m):speciesS. mellifera"],col = '#2a788e', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(stem.height.int)[1]+coef(stem.height.int)["speciesV. nebrownii"], slope  = coef(stem.height.int)[2]+coef(stem.height.int)["log(height.m):speciesV. nebrownii"],col = '#414487', lty = 1, lwd = 1.0) +
  geom_abline(intercept = coef(stem.height.int)[1]+coef(stem.height.int)["speciesV. reficiens"], slope  = coef(stem.height.int)[2]+coef(stem.height.int)["log(height.m):speciesV. reficiens"],col = '#440154', lty = 1, lwd = 1.0) +
  theme_classic()+
  theme(legend.text = element_text(size = 20,face = 'italic') ,
        legend.direction = "vertical",legend.position = c(0.86,0.17),legend.title=element_blank(),legend.key.size = unit(1.1, "cm"),
        text = element_text(size=17))

all_heights

###################################################################################################################################
# Upscaling

mean(Data_ups$height_m[Data_ups$Set == 1])
mean(Data_ups$height_m[Data_ups$Set == 2])
mean(Data_ups$height_m[Data_ups$Set == 3])
mean(Data_ups$height_m[Data_ups$Set == 4])
mean(Data_ups$height_m[Data_ups$Set == 5])



mean(Data_ups$total.stem.d[Data_ups$Set == 1])
mean(Data_ups$mean_diameter[Data_ups$Set == 1])

length(Data_ups$total.stem.d[Data_ups$Set == 1 & !is.na(Data_ups$total.stem.d)])

sd(Data_ups$total.stem.d[Data_ups$Set == 1])
mean(Data_ups$total.stem.d[Data_ups$Set == 2])
mean(Data_ups$mean_diameter[Data_ups$Set == 2])

length(Data_ups$total.stem.d[Data_ups$Set == 2 & !is.na(Data_ups$total.stem.d)])
sd(Data_ups$total.stem.d[Data_ups$Set == 2])
mean(Data_ups$total.stem.d[Data_ups$Set == 3])
mean(Data_ups$mean_diameter[Data_ups$Set == 3])

length(Data_ups$total.stem.d[Data_ups$Set == 3& !is.na(Data_ups$total.stem.d)])
sd(Data_ups$total.stem.d[Data_ups$Set == 3])
mean(Data_ups$total.stem.d[Data_ups$Set == 4])
mean(Data_ups$mean_diameter[Data_ups$Set == 4])

length(Data_ups$total.stem.d[Data_ups$Set == 4 & !is.na(Data_ups$total.stem.d)])
sd(Data_ups$total.stem.d[Data_ups$Set == 4])
mean(Data_ups$mean_diameter[Data_ups$Set == 5])

mean(Data_ups$total.stem.d[Data_ups$Set == 5])

length(Data_ups$total.stem.d[Data_ups$Set == 5 & !is.na(Data_ups$total.stem.d)])
sd(Data_ups$total.stem.d[Data_ups$Set == 5])

median((Data_ups$total.stem.d[Data_ups$Set == 1]))
median((Data_ups$total.stem.d[Data_ups$Set == 2]))
median((Data_ups$total.stem.d[Data_ups$Set == 3]))
median((Data_ups$total.stem.d[Data_ups$Set == 4]))
median((Data_ups$total.stem.d[Data_ups$Set == 5]))

Data_ups$mean_diameter


barplot(c(361.8,240.3,253.2,211.2,644.2),names.arg = c(1,2,3,4,5),width = 1,col = "lightblue", xlab = "Plot",cex.axis = 1.5,cex.names = 1.5)



library(lme4)
library(MuMIn)

model_1 <- lmer(log(SW_sum) ~ log(crown.area) + log(total.stem.area) + log(mean_diameter) + count_stems + (1|Set), data = Data_ups, REML=FALSE)
model_2 <- lmer(log(SW_sum) ~  log(total.stem.area) + log(mean_diameter) + count_stems + (1|Set), data = Data_ups, REML=FALSE)
model_3 <- lmer(log(SW_sum) ~  log(mean_diameter) + count_stems + (1|Set), data = Data_ups, REML=FALSE)
model_4 <- lmer(log(SW_sum) ~  log(mean_diameter) + (1|Set), data = Data_ups, REML=FALSE)


par(mfrow=c(2,2))
plot(model_1)
plot(model_2)
plot(model_3)
plot(model_4)

coef(model)

r.squaredGLMM(model_1)
r.squaredGLMM(model_2)
r.squaredGLMM(model_3)
r.squaredGLMM(model_4)


step(model)


modali <- lm(log(SW_sum) ~  log(mean_diameter) + as.factor(Set),  data = Data_ups)
summary(modali)

models <- list(model_1,model_2,model_3,model_4)
model.names <- c("model_1","model_2","model_3","model_4")
aictab(models,model.names)



AIC(model)

anova(birds.null,birds.model)

model <- lm(log(SW_sum) ~  log(total.stem.area) + log(mean_diameter) + count_stems , data = Data_ups)
summary(model)
anova(model)

hist(log(Data_ups$SW_sum))

ggplot(Data_ups,aes(x= total.stem.d, y = ((0.35*(total.stem.d)^2.06)^1.04)*25.51/1000))+
  facet_wrap(~Set,ncol = 5)+
  geom_point(col = "darkgreen")+
  ylab(expression(Estimated~transpiration~rate~(kg~d^-1)))+
  xlab(expression(Total~stem~diameter~(cm)))+
  # theme_pubr()+
  theme(panel.spacing = unit(2, "lines"))

#y = (

stems <- ggplot(Data_ups,aes(x = total.stem.d))+
  facet_wrap(~Set,ncol = 5)+
  geom_histogram(fill = "darkgreen",bins = 15)+
  ylab("Number of shrubs")+
  xlab(expression(Total~stem~diameter~(cm)))+
  # theme_pubr()+
  theme(panel.spacing = unit(1, "lines"))



transpirations <- ggplot(Data_ups,aes(x = ((0.35*(total.stem.d)^2.06)^1.04)*25.51/1000))+
  facet_wrap(~Set,ncol = 5)+
  geom_histogram(fill = "blue4", bins = 15)+
  xlab(expression(Estimated~sap~flow~rate~(kg~d^-1)))+
  ylab("Number of shrubs")+
  theme_pubr()+
  theme(panel.spacing = unit(1, "lines"),
        strip.background = element_blank(), strip.text = element_blank())

hists <- ggarrange(stems,transpirations,nrow = 2)
hists

Data_ups$Set <- factor(Data_ups$Set)
levels(Data_ups$Set)
# 
# ggplot(Data_ups,aes(x = Set,fill = Set,y = 16*((0.35*(total.stem.d)^2.06)^1.04)*25.51/1000))+
#   geom_bar(stat = "identity")+
#   ylab(expression(Estimated~stand~transpiration~rate~(Kg~ha^-1~d^-1)))+
#   ylim(0,500)+
#   xlab(expression("Plot"))+
#   theme_classic2()+
#   theme(legend.position = "none")


ggplot(Data_ups,aes(x = Set,fill = Set,y = 16*(SW_sum)*25.51/1000))+
  geom_bar(stat = "identity", fill = "black")+
  ylab(expression(Estimated~stand~transpiration~rate~(Kg~ha^-1~d^-1)))+
  ylim(0,500)+
  xlab(expression("Plot"))+
  theme_classic2()+
  theme(legend.position = "none")

vec = 0
for (set in levels(factor(Data_ups$Set))){
 
  temp <- Data_ups %>% filter(Set == set)
  sum <- sum(16*(temp$SW_sum)*25.51/1000) # kg/d
  means <- mean(temp$crown.area)
  # print(paste0("Plot ",set,": ",round(sum,2)," kg/ha/d"))
  
  vec <- c(vec,sum)
  # sumsw <- sum(temp$SW_sum)
  # print(temp)
  print(c("Plot ",set,means))
}

sd(vec[2:6])

sum(Data_ups$SW_sum)
  

###################################################################################################################################
# SIMULATION

simulation <- function(total,Mean,SD){
  
  trees <- vector()
  while(sum(total) > 0){
    newtree <- abs(rnorm(3, mean=Mean, sd=SD))
    trees <- c(trees,newtree)
    total <- total - newtree
  }
  if (sum(total) < 0){
    
    newtree <- -1*total
    trees <- c(trees,newtree)
  }
  

  sap <- ((((5.93*trees^1.35)^1.25)  * 25.51)/1000)/10000
  
  
  return(c(sum(sap,na.rm=TRUE),length(trees)))
  
  
}
# (0.35 * ((5.88*(x^0.63))^1.11)^2.06)^1.04

coverage <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7)
avg <- c(10,30,50,70,90,110)


sapFlow <- vector()
cov.Vec <- vector()
avg.Vec <- vector()
sim.Vec <- vector()
n.Vec   <- vector()


for (sim in 1:100){
  
  
  for (i in coverage){
    
    total <- i*10000
    
    for (j in avg){
      
      Mean <- j
      
      sapFlow <- c(sapFlow,simulation(total,Mean,20)[1])
      cov.Vec <- c(cov.Vec,i)
      avg.Vec <- c(avg.Vec,Mean)
      sim.Vec <- c(sim.Vec,sim)
      n.Vec <- c(n.Vec,simulation(total,Mean,20)[2])
      
    }
    
  }
  
}



Data <- data.frame(sapFlow,cov.Vec,avg.Vec,n.Vec)

Data.new <- Data %>% group_by(cov.Vec, avg.Vec) %>%  summarise(MeanSF = mean(sapFlow,na.rm = T),MeanN = mean(n.Vec,na.rm = T))

ggplot(Data.new,aes(x = cov.Vec, y = avg.Vec, fill = MeanSF))+
  geom_tile(alpha = 0.9)+
  # labs(fill = expression(kg~m^-2~d^-1))+
  labs(fill = expression(mm~d^-1))+
  scale_fill_continuous(type = "viridis",alpha = 0.9)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7),labels=c(10,20,30,40,50,60,70))+
  scale_y_continuous(breaks=c(10,30,50,70,90,110),labels=c(0,30,50,70,90,110))+
  geom_text(aes(label = paste(round(MeanSF,3),"\n","n =",round(MeanN,1))),size = 4.5)+
  ylab(expression(Mean~crown~area~(m^2)))+
  xlab("Canopy coverage (%)")+
  theme_classic()+
  theme(legend.position = "right", 
        legend.text = element_text(size = 16),
        legend.key.size = unit(1, "cm"),
        text = element_text(size=16),
        axis.title = element_text(size = 18))



write.csv(Data.new$MeanSF,file="MeanSF.csv",row.names=F)


###################################################################################################################################
# MAP

library(geosphere)


pal <- colorFactor(palette = c("#fde725","#2a788e", "#7ad151","#414487","#440154","#22a884"), 
              levels = c("C. alexandri","S. mellifera", "C. mopane", "V. nebrownii", "V. reficiens","D. cinerea"))


map <- leaflet(Data_gps) %>%
  addTiles() %>%
  addPolygons(lat= c(-19.31026,-19.28763,-19.24474,-19.22558,-19.21179,-19.22117,-19.2255,-19.2275,-19.2230,-19.22606,-19.23736,-19.27507,-19.2973,-19.30586),
              lng = c(15.27128,15.28584,15.28757,15.27171,15.24722,15.21375,15.20226,15.13415,15.09021,15.08616,15.08538,15.15896,15.1195,15.1135),
              color  = "darkred",
              weight = 2,
              opacity = 0.9,
              fill = TRUE,
              fillOpacity = 0.1) %>% 
  addCircleMarkers(lng=~lon, lat=~lat, color = ~pal(species),weight = 2,radius = 1,opacity = 0.7,
                   label = ~ID,labelOptions = labelOptions(noHide = F)) %>% 
  addLegend(pal = pal, position = "bottomleft",values= ~species) 

map

map_out <- leaflet(Data_gps) %>% addProviderTiles('Esri.WorldImagery') 

DIC_59 <- c(-19.31026,15.27128)
MOP_35 <- c(-19.28763, 15.28584) 
DIC_60 <- c(-19.24474,15.28757)
MEL_22 <- c(-19.22558,15.27171)
NEB_40 <- c(-19.21179,15.24722)
NEB_37 <- c(-19.22117, 15.21375)
DIC_07 <- c(-19.2255,15.20226)
DIC_55 <-c(-19.2275,15.13415)
MOP_34 <-c(-19.22305,15.09021)
REF_31 <-c(-19.22606,15.08616)
MOP_32 <-c(-19.23736,15.08538)
REF_13 <-c(-19.27507,15.15896)
DIC_18 <-c(-19.2973,15.1195)
NEB_18 <-c(-19.30586,15.1135)

p <- rbind(DIC_59,MOP_35,DIC_60,MEL_22,NEB_40,NEB_37,DIC_07,DIC_55,MOP_34,REF_31,MOP_32,REF_13,DIC_18,NEB_18)

areaPolygon(p)/1000000


##########################################################################################################
#Data tables

library(vtable)

Data_table <- Data_all
Data_table$SW_sum

table <- sumtable(Data_all,vars = c("SW_sum") ,group = "species",
                  group.long	= F,
                  summ = c('min(x)','max(x)','mean(x)','sd(x)'))

table <- sumtable(Data_stained,vars = c("diameter","sap") ,group = "species",
                  group.long	= F,
                  summ = c('min(x)','max(x)','mean(x)','sd(x)'))



##########################################################################################################
# error propagation


x = Data_all$canopy.area

y <- (0.35 * ((5.88*(x^0.63))^1.11)^2.06)^1.04
sum(y)

a1 <- (1.3 * ((4.35*(x^0.58))^1.11)^1.66)^1.04
a2 <- (1.3 * ((4.35*(x^0.68))^1.11)^1.66)^1.04
a3 <- (1.3 * ((8*(x^0.58))^1.11)^1.66)^1.04
a4 <- (1.3 * ((8*(x^0.68))^1.11)^1.66)^1.04
a5 <- (1.3 * ((4.35*(x^0.58))^1.11)^2.47)^1.04
a6 <- (1.3 * ((4.35*(x^0.68))^1.11)^2.47)^1.04
a7 <- (1.3 * ((8*(x^0.58))^1.11)^2.47)^1.04
a8 <- (1.3 * ((8*(x^0.68))^1.11)^2.47)^1.04 # highest

a9 <- (0.01 * ((4.35*(x^0.58))^1.11)^1.66)^1.04 # lowest
a10 <- (0.01 *((4.35*(x^0.68))^1.11)^1.66)^1.04
a11 <- (0.01 * ((8*(x^0.58))^1.11)^1.66)^1.04
a12 <- (0.01 * ((8*(x^0.68))^1.11)^1.66)^1.04
a13 <- (0.01 * ((4.35*(x^0.58))^1.11)^2.47)^1.04
a14 <- (0.01 * ((4.35*(x^0.68))^1.11)^2.47)^1.04
a15 <- (0.01 * ((8*(x^0.58))^1.11)^2.47)^1.04
a16 <- (0.01 * ((8*(x^0.68))^1.11)^2.47)^1.04

max(sum(a1),
sum(a2),
sum(a3),
sum(a4),
sum(a5),
sum(a6),
sum(a7),
sum(a8),
sum(a9),
sum(a10),
sum(a11),
sum(a12),
sum(a13),
sum(a14),
sum(a15),
sum(a16))



logbtcf(lm_all2_canopy)
logbtcf(lm_all_a)

#######################################################################################################



