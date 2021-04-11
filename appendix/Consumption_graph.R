################################################################################
# File Name:    Consumption_graph.R

# Description:  This R script creates the top half of Figure AS3: consumption results by round

################################################################################

#-------- clear workspace
rm(list = ls())

#-------- load libaries
library(data.table)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(readxl)
library(dplyr)
library(grid)
library(gridExtra)

#-------- set main directory
setwd(".")

#------- load dataset of estimates from Excel
data<- read_excel("temp/Consumption_results_for_R.xlsx")

#------- remove KLPS-2 from plotting data
consumption.df <- subset(data, !grepl("klps2",model))

#------- variables for graph style "mapping"
consumption.df$mapping <- ifelse(grepl("female",consumption.df$model),"female",ifelse(grepl("all",consumption.df$model),"all","male"))
consumption.df$mapping <- ifelse(grepl("pooled_all",consumption.df$model),"pooled_all",consumption.df$mapping)
consumption.df$mapping <- ifelse(grepl("pooled_female",consumption.df$model),"pooled_female",consumption.df$mapping)
consumption.df$mapping <- ifelse(grepl("pooled_male",consumption.df$model),"pooled_male",consumption.df$mapping)

consumption.df$pooled <- ifelse(grepl("pooled",consumption.df$model),1,0)

#------- create variables for graphing
# Change KLPS-3 males to missing in order to not plot error bars
consumption.df$ll[which(consumption.df$model=="klps3_male")]<-NA
consumption.df$ul[which(consumption.df$model=="klps3_male")]<-NA

mean_pooled_all <- round(consumption.df$control_mean[consumption.df$model=="pooled_all"],digits=0)
mean_pooled_female <- round(consumption.df$control_mean[consumption.df$model=="pooled_female"],digits=0)
mean_pooled_male <- round(consumption.df$control_mean[consumption.df$model=="pooled_male"],digits=0)

mean_klps3_all <- round(consumption.df$control_mean[consumption.df$model=="klps3_all"],digits=0)
mean_klps3_female <- round(consumption.df$control_mean[consumption.df$model=="klps3_female"],digits=0)
mean_klps3_male <- round(consumption.df$control_mean[consumption.df$model=="klps3_male"],digits=0)

mean_klps4_all <- round(consumption.df$control_mean[consumption.df$model=="klps4_all"],digits=0)
mean_klps4_female <- round(consumption.df$control_mean[consumption.df$model=="klps4_female"],digits=0)
mean_klps4_male <- round(consumption.df$control_mean[consumption.df$model=="klps4_male"],digits=0)


treat_pooled_all <- round(consumption.df$treat_effect[consumption.df$model=="pooled_all"],digits=0)
treat_pooled_female <- round(consumption.df$treat_effect[consumption.df$model=="pooled_female"],digits=0)
treat_pooled_male <- round(consumption.df$treat_effect[consumption.df$model=="pooled_male"],digits=0)

treat_klps3_all <- round(consumption.df$treat_effect[consumption.df$model=="klps3_all"],digits=0)
treat_klps3_female <- round(consumption.df$treat_effect[consumption.df$model=="klps3_female"],digits=0)
treat_klps3_male <- round(consumption.df$treat_effect[consumption.df$model=="klps3_male"],digits=0)

treat_klps4_all <- round(consumption.df$treat_effect[consumption.df$model=="klps4_all"],digits=0)
treat_klps4_female <- round(consumption.df$treat_effect[consumption.df$model=="klps4_female"],digits=0)
treat_klps4_male <- round(consumption.df$treat_effect[consumption.df$model=="klps4_male"],digits=0)

control_mean_text = textGrob("Control \nMean",gp=gpar(fontsize=8))

#------- plot
p1<- ggplot(consumption.df,aes(x=model_num, y=b)) +
  geom_point(aes(shape=mapping,size=mapping,colour=mapping)) + #point estimates
  ylim(-500,2500) +
  scale_shape_manual(values=c(0,1,2, 15,19,17)) +
  scale_size_manual(values=c(2,2,2,3,3,3)) +
  scale_x_continuous(breaks=c(2,6,10,14),labels=c(paste("Pooled \n(2007-2019) \n \n[A:",mean_pooled_all,"] \n[F:",mean_pooled_female ,"] \n[M:",mean_pooled_male,"]",sep=""),"KLPS-2 \n(2007-2009) \n \n*Not collected",paste("KLPS-3 \n(2011-2014) \n \n[A:",mean_klps3_all,"] \n[F:", mean_klps3_female,"] \n[M:",mean_klps3_male,"]",sep=""),paste("KLPS-4 \n(2017-2019) \n \n[A:",mean_klps4_all,"] \n[F:",mean_klps4_female,"] \n[M:",mean_klps4_male,"]",sep=""))) +
  geom_errorbar(aes(ymin = ll, ymax = ul, colour=mapping), width=.4) + #error bars at upper and lower confidence intervals
  geom_vline(xintercept=11, colour="azure4") +
  scale_colour_manual(values=c("azure4","azure4","azure4","black","black","black"), aesthetics = "colour") +
  geom_hline(yintercept = 0) + #line at Y=0
  ylab("Treatment Effect (2017 USD PPP)") + 
  xlab("") +
  annotate("text", x=1, y=700,label='atop(bold("All"))',parse=TRUE,size=3) +
  annotate("text",x=2.1,y=450,label='atop(bold("Female"))',parse=TRUE, size=3) +
  annotate("text",x=3,y=1100,label='atop(bold("Male"))',parse=TRUE, size=3) +
  annotate("text",x=12.8,y=2400,label="Upper C.I. = 3922",size=3) +
  annotate("text",x=12.8,y=-400,label="Lower C.I. = -761", size=3) +
  annotate("text",x=1.45, y=180,label=paste("[",treat_pooled_all,"%]",sep=""),size=2.5) +
  annotate("text",x=2.52,y=150,label=paste("[",treat_pooled_female,"%]",sep=""),size=2.5) +
  annotate("text",x=3.6,y=380,label=paste("[",treat_pooled_male,"%]",sep=""),size=2.5) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="pooled_all")]+.5,y=consumption.df$b[which(consumption.df$model=="pooled_all")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="pooled_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="pooled_female")]+.5,y=consumption.df$b[which(consumption.df$model=="pooled_female")]+200,label=paste(round(consumption.df$b[which(consumption.df$model=="pooled_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="pooled_male")]+.58,y=consumption.df$b[which(consumption.df$model=="pooled_male")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="pooled_male")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps3_all")]+.58,y=consumption.df$b[which(consumption.df$model=="klps3_all")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps3_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps3_female")]+.5,y=consumption.df$b[which(consumption.df$model=="klps3_female")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps3_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps3_male")]+.58,y=consumption.df$b[which(consumption.df$model=="klps3_male")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps3_male")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps4_all")]+.5,y=consumption.df$b[which(consumption.df$model=="klps4_all")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps4_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps4_female")]+.5,y=consumption.df$b[which(consumption.df$model=="klps4_female")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps4_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=consumption.df$model_num[which(consumption.df$model=="klps4_male")]+.58,y=consumption.df$b[which(consumption.df$model=="klps4_male")]+60,label=paste(round(consumption.df$b[which(consumption.df$model=="klps4_male")],0),sep=""),parse=TRUE,size=2.75) +
      theme_bw() + #black and white theme 
  theme(axis.text.x = element_text(face = c('bold','plain','plain','plain')), 
        axis.title=element_text(size=10),
        legend.position="none") 

p2<- p1 + annotation_custom(grob=control_mean_text,xmin=-.9,xmax=.2,ymin=-1900,ymax=-1500) +
  coord_cartesian(clip="off")

p2

ggsave("output/KLPS4_consumption_graph.eps")