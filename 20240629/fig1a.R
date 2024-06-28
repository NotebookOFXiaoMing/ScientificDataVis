library(readxl)
library(tidyverse)

read_excel("2024.data/20240629/fig1a.xlsx") %>% 
  pivot_longer(!`Days after tumor inoculation`) %>% 
  mutate(name=str_extract(name,pattern = "[A-z]+")) %>% 
  group_by(`Days after tumor inoculation`,name) %>% 
  summarise(mean_value=mean(value),
            sd_value=sd(value)) %>% 
  ungroup() -> fig1a.dat

pdf(file = "2024.data/20240629/fig1a.pdf",width = 6,height = 6)
ggplot(data=fig1a.dat,aes(x=`Days after tumor inoculation`,y=mean_value))+
  geom_point(aes(color=name),size=5)+
  geom_errorbar(aes(ymin=mean_value,
                    ymax=mean_value+sd_value,
                    color=name),
                width=0.6)+
  geom_line(aes(color=name))+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = c(0.2,0.9),
        legend.title = element_blank())+
  scale_y_continuous(limits = c(0,1200),
                     expand = expansion(mult=c(0,0)))+
  scale_x_continuous(limits = c(0,40),
                     expand = expansion(mult=c(0,0)))+
  coord_cartesian(clip = "off")+
  scale_color_manual(values = c('Ctrl'="black",
                                "Vac"="red"),
                     label=c("Ctrl"="Ctrl",
                             "Vac"="Vaccine ***"))+
  labs(x="Days after tumor inoculation",
       y=expression(Tumor~volume~(mm^3)))+
  annotate(geom = "segment",x=10,y=420,xend=10,yend=300,
           arrow=arrow(angle = 20,type="closed",length = unit(3,'mm')))+
  annotate(geom = "segment",x=18,y=460,xend=18,yend=350,
           arrow=arrow(angle = 20,type="closed",length = unit(3,'mm')))
dev.off()
