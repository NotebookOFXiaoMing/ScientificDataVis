library(ggplot2)
library(patchwork)
library(readxl)
library(tidyverse)
library(ggpmisc)
library(scatterpie)

fig1b.dat<-read_excel("D:/R_4_1_0_working_directory/env001/2024.data/20240717/41564_2024_1751_MOESM6_ESM.xlsx")

fig1b.dat %>% 
  mutate(`Genome quality`=factor(`Genome quality`,
                                 levels=c("near-complete","high-quality","medium-quality"))) -> fig1b.dat

fig1b.dat %>% colnames()

ggplot(data = fig1b.dat,aes(x=`% Completeness`))+
  geom_histogram(aes(fill=`Genome quality`),
                 binwidth = 0.5,
                 color="grey",
                 linewidth=0.1)+
  scale_fill_manual(values = c("#80b1d3","#fdb461","#8dd3c7"))+
  scale_y_continuous(limits = c(0,3000),
                     expand = expansion(mult = c(0,0)),
                     labels = scales::comma)+
  theme_classic()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y="Number of\nMAGs")+
  theme(legend.position = "none") -> p1

p1

ggplot(data = fig1b.dat,aes(y=`% Contamination`))+
  geom_histogram(aes(fill=`Genome quality`),
                 binwidth = 0.06,
                 color="grey",
                 linewidth=0.1)+
  scale_fill_manual(values = c("#80b1d3","#fdb461","#8dd3c7"))+
  scale_x_continuous(limits = c(0,4000),
                     expand = expansion(mult = c(0,0)),
                     labels = scales::comma)+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  labs(x="Number of MAGs")+
  theme(legend.position = "none") -> p4

p4

ggplot(data = fig1b.dat,aes(x=`% Completeness`,y=`% Contamination`))+
  geom_point(aes(color=`Genome quality`))+
  scale_color_manual(values = c("#80b1d3","#fdb461","#8dd3c7"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none") -> p3

p3

p1+plot_spacer()+
  p3+p4+
  plot_layout(heights = c(1,4),widths = c(3,1)) -> p134


wrap_plots(p1,plot_spacer(),p3,p4)+
  plot_layout(heights = c(1,4),widths = c(3,1))

library(gridExtra)
library(gtable)
?ggpubr::annotate_figure()
grid.draw
p134
grid::grid.draw(p.pie)
