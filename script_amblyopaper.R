require(ggplot2)
require(tidyverse)
require(scales)



# Treat data for Figure 3, then plot individual results

data = read.table("datacytoksaliva.csv", header = TRUE, sep = ",")

#normalization
normalize <- function(m){
  (m/m[1])*100 - 100
}

data$individual = factor(data$individu, levels = c("1973", "9567", "9906"))
data$pilo = factor(data$pilo, levels = c("N","Y"))
data$LPS = factor(data$LPS, levels = c("Without LPS", "With LPS"))
data$cytokine = factor(data$cytokine, levels = c("IL10","IL12", "TNF-A"))
#data$saliva = factor(data$saliva, levels = c("0","31.25","62.5","125","250"))

data1 = data %>%
  group_by(individual, cytokine, LPS) %>%
  mutate(normalized = normalize(measure)) %>%
  ungroup()





 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "IL10", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[IL10] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [IL10] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="loess" , aes(x= saliva,y=measure) )

ggsave("IL10_loess.pdf")


 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "IL10", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[IL10] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [IL10] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="lm" , aes(x= saliva,y=measure) )

ggsave("IL10_LM.pdf")


 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "TNF-A", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[TNF-A] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [TNF-A] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="loess" , aes(x= saliva,y=measure) )

ggsave("TNF-A_loess.pdf")


 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "TNF-A", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[TNF-A] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [TNF-A] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="lm" , aes(x= saliva,y=measure) )

ggsave("TNF-A_LM.pdf")


 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "IL12", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[IL12] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [IL12] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="loess" , aes(x= saliva,y=measure) )

ggsave("IL12_loess.pdf")


 ggplot(data =  data1 %>% filter (pilo == "N",cytokine== "IL12", LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = measure, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[IL12] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  ggtitle("Influence of Amblyomma saliva on [IL12] without LPS")  + 
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="lm" , aes(x= saliva,y=measure) )

ggsave("IL12_LM.pdf")

summary(lm(measure ~ saliva,  data1 %>% filter (pilo == "N",cytokine== "TNF-A", LPS == "With LPS") ))   
summary(lm(measure ~ saliva,  data1 %>% filter (pilo == "N",cytokine== "IL12", LPS == "With LPS") ))   
summary(lm(measure ~ saliva,  data1 %>% filter (pilo == "N",cytokine== "IL10", LPS == "With LPS") ))   



# Plot NO data for figure 3 


datano = read.table("datanopilo.csv",sep=",",header = T)


datano$individu = factor(datano$individu, levels = c("1973", "9567", "9906"))
datano$pilo = factor(datano$pilo, levels = c("N","Y"))

 ggplot(data =  datano %>% filter (pilo == "N")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = mesure, group = individu,color = individu)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("[NO] ng/mL")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) + 
  geom_smooth(method="lm" , aes(x= saliva,y=mesure) )

ggsave("NO_LM.pdf",dpi=300)

summary(lm(saliva~mesure,datano %>% filter(pilo=="N")))



# Plot markers for Figure 2 


datac = read.table("MPsalive_marqueurs_relativeactivation.csv",header=T,sep=',')
datac$individual = factor(datac$individu, levels = c("1973", "9567", "9906"))
datac$relative.activation = as.numeric(datac$relative.activation)

 ggplot(data =  datac %>% filter (pilo == "NO",LPS == "With LPS")  %>% mutate(saliva=ifelse(saliva==0,15,saliva))) + 
  geom_point(shape=15,size=2,  aes(x = saliva,y = relative.activation, group = individual,color = individual)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  #geom_line()+
  xlab("saliva concentration") + ylab("expression change, % relative to control (no LPS)")+
  geom_segment(aes(x=15,xend=250,y=0,yend=0),alpha=1, colour = "black")+
  scale_x_continuous(trans =log2_trans(),breaks = c(15,31.25,62.5,125,250),label = c(0,31.25,62.5,125,250)) +
  scale_y_continuous() +
  geom_smooth(method="lm" , aes(x= saliva,y=relative.activation) ) + 
  facet_wrap(~ marqueur,ncol=2,scale="free_y")

ggsave("Figure2.pdf",dpi=300)


summary(lm(relative.activation ~ saliva,  datac %>% filter (pilo == "NO",marqueur== "MHCII", LPS == "With LPS") )) 
summary(lm(relative.activation ~ saliva,  datac %>% filter (pilo == "NO",marqueur== "CD80", LPS == "With LPS") ))   
summary(lm(relative.activation ~ saliva,  datac %>% filter (pilo == "NO",marqueur== "CD86", LPS == "With LPS") ))   
summary(lm(relative.activation ~ saliva,  datac %>% filter (pilo == "NO",marqueur== "CD40", LPS == "With LPS") ))   


# Create Figure 1 and compute linear model

datap = read.table("dataprolif.csv",sep=",",header=T)
ggplot(datap) + geom_point(aes(x=saliva,y=moy,color=individual),size=2) + 
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  xlab("saliva concentration") + ylab("relative PBMC activation (% relative to control)")+
  geom_smooth(method="lm" , aes(x= saliva,y=moy) )  
ggsave("Figure1.pdf",dpi=300)
summary(lm(saliva~moy,datap))



