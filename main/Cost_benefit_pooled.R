################################################################################
# File Name:    Cost_benefit_Graph.R
#
# Description:  This R script creates Figure 1: the cost-benefit figure 

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

#-------- set output directory
setwd("KLPS/20-yr-replication_materials/output")

##25-year - Third graph
ggplot(data=NULL) +
  ylab("Costs and Benefits (2017 USD PPP)") + 
  xlab("Years Since Start of PSDP (1998)") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60),labels=c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  scale_y_continuous(breaks=c(-50,0,50,100,150,200,250,300,350),labels=c(-50,0,50,100,150,200,250,300,350)) +
  annotate("rect",xmin=0,xmax=2.4,ymin=(-0.834646), ymax=0, fill="gray40",color=NA) +
  annotate("rect",xmin=1,xmax=2,ymin=.47241, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=2,xmax=3,ymin=-1.86461, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=3,xmax=4,ymin=-6.92666, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=4,xmax=5,ymin=-7.31358, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=5,xmax=6,ymin=-6.27182, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=6,xmax=7,ymin=-7.4814, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=7,xmax=8,ymin=-1.73332, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=8,xmax=9,ymin=-2.2388, ymax=0, fill="gray60",color=NA) +
  annotate("rect",xmin=10,xmax=25,ymin=0, ymax=79.515, fill="gray90",color=NA) +
  annotate("rect",xmin=15,xmax=25,ymin=79.515,ymax=305.108,fill="gray95",color=NA) +
  annotate("text",x=17.5,y=37,label="Earnings Gains",size=3.2) +
  annotate("text",x=20,y=200,label="Consumption \nGains",size=3.2) +
  annotate("text",x=.5,y=-34,label="Deworming \nCost",size=3.2,hjust=0) +
  annotate("text",x=9,y=-33,label="Teacher \nCosts",size=3.2, hjust=0) + 
  annotate("text",x=30,y=-30,label="Assume Gain = $0 after Year 25",size=3.2,hjust=0,fontface='italic') +
  annotate("text",x=26,y=15,label="$7.99 (Needed for Social IRR = 10%)",size=3.2, hjust=0,fontface='italic') +
  annotate("text",x=26,y=85,label="Implied Social IRR = 40.7%",size=3.2,hjust=0) +
  annotate("text",x=26,y=305.108,label="Implied Social IRR = 36.7%",size=3.2,hjust=0) +
  annotate('text',x=17.5,y=67,label="$80",size=3.2) +
  annotate("text",x=20,y=290,label="$305",size=3.2) +
  annotate("text",x=1,y=-50,label="")+
  geom_segment(aes(x=10,y=7.99,xend=25,yend=7.99),linetype="dotted") +
  geom_segment(aes(x=10,y=79.515,xend=25,yend=79.515)) + #KLPS-2,3,4 pooled earnings
  geom_segment(aes(x=15,y=305.108,xend=25,yend=305.108)) + #KLPS-3 consumption
  geom_segment(aes(x=25,y=0,xend=50,yend=0),linetype="dashed") +
  geom_segment(aes(x=1.2,y=-1,xend=1.4,yend=-12)) +
  geom_segment(aes(x=0,y=0,xend=25,yend=0)) +
  geom_segment(aes(x=8,y=-6,xend=8.75,yend=-17)) +
  geom_segment(aes(x=40,y=-8,xend=40,yend=-20)) +
  geom_vline(xintercept = 0) + #vertical line at x=0
  theme_bw() +
  theme(axis.line.x=element_line(colour="grey50"), 
        axis.line.y=element_line(colour="grey50"))

ggsave("KLPS4_cost_benefit_pooled.eps")