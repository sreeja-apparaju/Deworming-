################################################################################
# File Name:    Earnings_graph.R

# Description:  This R script creates the bottom half of Figure AS3: earnings results by round

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
library(ggrepel)
library(grid)

#-------- set main directory
setwd(".")

#------- load dataset of estimates from Excel
earnings.df<- read_excel("temp/Earnings_results_for_R.xlsx")

#------- variables for graph style "mapping"
earnings.df$mapping <- ifelse(grepl("female",earnings.df$model),"female",ifelse(grepl("all",earnings.df$model),"all","male"))
earnings.df$mapping <- ifelse(grepl("pooled_all",earnings.df$model),"pooled_all",earnings.df$mapping)
earnings.df$mapping <- ifelse(grepl("pooled_female",earnings.df$model),"pooled_female",earnings.df$mapping)
earnings.df$mapping <- ifelse(grepl("pooled_male",earnings.df$model),"pooled_male",earnings.df$mapping)

earnings.df$pooled <- ifelse(grepl("pooled",earnings.df$model),1,0)

#------- create variables for graphing
mean_pooled_all <- round(earnings.df$control_mean[earnings.df$model=="pooled_all"],digits=0)
mean_pooled_female <- round(earnings.df$control_mean[earnings.df$model=="pooled_female"],digits=0)
mean_pooled_male <- round(earnings.df$control_mean[earnings.df$model=="pooled_male"],digits=0)

mean_klps2_all <- round(earnings.df$control_mean[earnings.df$model=="klps2_all"],digits=0)
mean_klps2_female <- round(earnings.df$control_mean[earnings.df$model=="klps2_female"],digits=0)
mean_klps2_male <- round(earnings.df$control_mean[earnings.df$model=="klps2_male"],digits=0)

mean_klps3_all <- round(earnings.df$control_mean[earnings.df$model=="klps3_all"],digits=0)
mean_klps3_female <- round(earnings.df$control_mean[earnings.df$model=="klps3_female"],digits=0)
mean_klps3_male <- round(earnings.df$control_mean[earnings.df$model=="klps3_male"],digits=0)

mean_klps4_all <- round(earnings.df$control_mean[earnings.df$model=="klps4_all"],digits=0)
mean_klps4_female <- round(earnings.df$control_mean[earnings.df$model=="klps4_female"],digits=0)
mean_klps4_male <- round(earnings.df$control_mean[earnings.df$model=="klps4_male"],digits=0)


treat_pooled_all <- round(earnings.df$treat_effect[earnings.df$model=="pooled_all"],digits=0)
treat_pooled_female <- round(earnings.df$treat_effect[earnings.df$model=="pooled_female"],digits=0)
treat_pooled_male <- round(earnings.df$treat_effect[earnings.df$model=="pooled_male"],digits=0)

treat_klps2_all <- round(earnings.df$treat_effect[earnings.df$model=="klps2_all"],digits=0)
treat_klps2_female <- round(earnings.df$treat_effect[earnings.df$model=="klps2_female"],digits=0)
treat_klps2_male <- round(earnings.df$treat_effect[earnings.df$model=="klps2_male"],digits=0)

treat_klps3_all <- round(earnings.df$treat_effect[earnings.df$model=="klps3_all"],digits=0)
treat_klps3_female <- round(earnings.df$treat_effect[earnings.df$model=="klps3_female"],digits=0)
treat_klps3_male <- round(earnings.df$treat_effect[earnings.df$model=="klps3_male"],digits=0)

treat_klps4_all <- round(earnings.df$treat_effect[earnings.df$model=="klps4_all"],digits=0)
treat_klps4_female <- round(earnings.df$treat_effect[earnings.df$model=="klps4_female"],digits=0)
treat_klps4_male <- round(earnings.df$treat_effect[earnings.df$model=="klps4_male"],digits=0)

control_mean_text = textGrob("Control \nMean",gp=gpar(fontsize=8))

#------- plot
p1<- ggplot(earnings.df,aes(x=model_num, y=b)) +
  geom_point(aes(shape=mapping,size=mapping,colour=mapping)) + #point estimates
  scale_shape_manual(values=c(0,1,2, 15,19,17)) +
  scale_size_manual(values=c(2,2,2,3,3,3)) +
  scale_x_continuous(breaks=c(2,6,10,14),labels=c(paste("Pooled \n(2007-2019) \n \n[A:",mean_pooled_all,"] \n[F:",mean_pooled_female ,"] \n[M:",mean_pooled_male,"]",sep=""),paste("KLPS-2 \n(2007-2009) \n \n[A:",mean_klps2_all,"] \n[F:",mean_klps2_female,"] \n[M:",mean_klps2_male,"]",sep=""),paste("KLPS-3 \n(2011-2014) \n \n[A:",mean_klps3_all,"] \n[F:", mean_klps3_female,"] \n[M:",mean_klps3_male,"]",sep=""),paste("KLPS-4 \n(2017-2019) \n \n[A:",mean_klps4_all,"] \n[F:",mean_klps4_female,"] \n[M:",mean_klps4_male,"]",sep=""))) +
  geom_errorbar(aes(ymin = ll, ymax = ul, colour=mapping), width=.4) + #error bars at upper and lower confidence intervals
  scale_colour_manual(values=c("azure4","azure4","azure4","black","black","black"), aesthetics = "colour") +
  geom_hline(yintercept = 0) + #line at Y=0
  ylab("Treatment Effect (2017 USD PPP)") + 
  xlab("") +
  #geom_text(position = position_nudge(x = .5, y=-15),size=3) +
  annotate("text", x=1, y=250,label='atop(bold("All"))',parse=TRUE,size=2.75) +
  annotate("text",x=2,y=175,label='atop(bold("Female"))',parse=TRUE, size=2.75) +
  annotate("text",x=3,y=400,label='atop(bold("Male"))',parse=TRUE, size=2.75) +
  annotate("text",x=1.55, y=70,label=paste("[",treat_pooled_all,"%]",sep=""),size=2.5) +
  annotate("text",x=2.55,y=60,label=paste("[",treat_pooled_female,"%]",sep=""),size=2.5) +
  annotate("text",x=3.55,y=100,label=paste("[",treat_pooled_male,"%]",sep=""),size=2.5) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="pooled_all")]+.5,y=earnings.df$b[which(earnings.df$model=="pooled_all")]+60,label=paste(round(earnings.df$b[which(earnings.df$model=="pooled_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="pooled_female")]+.5,y=earnings.df$b[which(earnings.df$model=="pooled_female")]+90,label=paste(round(earnings.df$b[which(earnings.df$model=="pooled_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="pooled_male")]+.5,y=earnings.df$b[which(earnings.df$model=="pooled_male")]+60,label=paste(round(earnings.df$b[which(earnings.df$model=="pooled_male")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps2_all")]+.5,y=earnings.df$b[which(earnings.df$model=="klps2_all")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps2_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps2_female")]+.5,y=earnings.df$b[which(earnings.df$model=="klps2_female")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps2_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps2_male")]+.5,y=earnings.df$b[which(earnings.df$model=="klps2_male")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps2_male")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps3_all")]+.5,y=earnings.df$b[which(earnings.df$model=="klps3_all")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps3_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps3_female")]+.5,y=earnings.df$b[which(earnings.df$model=="klps3_female")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps3_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps3_male")]+.5,y=earnings.df$b[which(earnings.df$model=="klps3_male")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps3_male")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps4_all")]+.5,y=earnings.df$b[which(earnings.df$model=="klps4_all")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps4_all")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps4_female")]+.5,y=earnings.df$b[which(earnings.df$model=="klps4_female")]+40,label=paste(round(earnings.df$b[which(earnings.df$model=="klps4_female")],0),sep=""),parse=TRUE,size=2.75) +
  annotate("text",x=earnings.df$model_num[which(earnings.df$model=="klps4_male")]+.5,y=earnings.df$b[which(earnings.df$model=="klps4_male")]+10,label=paste(round(earnings.df$b[which(earnings.df$model=="klps4_male")],0),sep=""),parse=TRUE,size=2.75) +
      theme_bw() + #black and white theme 
  theme(legend.position="none", 
        axis.text.x = element_text(face = c('bold','plain','plain','plain')),
        axis.title=element_text(size=10))

p2<- p1 + annotation_custom(grob=control_mean_text,xmin=-.9,xmax=.2,ymin=-1200,ymax=-700) +
  coord_cartesian(clip="off")

p2

ggsave("output/KLPS4_earnings_graph.eps")