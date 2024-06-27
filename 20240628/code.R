library(tidyverse)
library(gridExtra)
library(patchwork)

read.csv("data/20221211/Sourcedata/Fig2c&d/Fst_adaption.csv",
         row.names = 1) %>% 
  as.matrix() %>% 
  Matrix::tril() %>% 
  as.dist() -> fst.adaption.dist

read.csv("data/20221211/Sourcedata/Fig2c&d/Fst_neutral.csv",
         row.names = 1) %>% 
  as.matrix() %>% 
  Matrix::tril() %>% 
  as.dist() -> fst.neutral.dist

read.csv("data/20221211/Sourcedata/Fig2c&d/geo_dist.csv",
         row.names = 1) %>% 
  as.matrix() %>% 
  Matrix::tril() %>% 
  as.dist() -> geo.dist


vegan::mantel(fst.adaption.dist,geo.dist,permutations = 999)
vegan::mantel(fst.neutral.dist,geo.dist,permutations = 999)

vegan::mantel.partial(fst.adaption.dist,env.dist,geo.dist,permutations = 999)
vegan::mantel.partial(fst.neutral.dist,env.dist,geo.dist,permutations = 999)

dat<-read_csv("data/20221211/Sourcedata/Fig2c&d/plot_data.csv")

tt1<-ttheme_minimal(core=list(fg_params=list(hjust=0,
                                        parse=TRUE,
                                        x=0,
                                        fontsize=15,
                                        col=c("#db6786","#db6786",
                                              "#609ac6","#609ac6")),
                         bg_params=list(fill="#e1eff5")))

table2c<-tibble(x=c("italic(Mantel)*minute*italic(s)~italic(r)=='0.450'",
                    "italic(Mantel)*minute*italic(s)~italic(p)=='0.001'",
                    "italic(Mantel)*minute*italic(s)~italic(r)=='0.451'",
                    "italic(Mantel)*minute*italic(s)~italic(p)=='0.001'"))

p2c<-dat %>% 
  select(geo_dit,fst1779,fst53w) %>% 
  pivot_longer(!geo_dit) %>% 
  ggplot(aes(x=geo_dit/100000,y=value))+
  geom_point(aes(color=name,
                 fill=name),
             shape=21,
             size=5)+
  scale_color_manual(values = c("#609ac6","#db6786"))+
  scale_fill_manual(values = c("#d6dde2","#fbf3d5"))+
  geom_smooth(aes(color=name),
              method = "lm",
              formula = 'y~x')+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank(),
        legend.position = "none")+
  labs(x="Geographic distance (100 km)",
       y=expression(italic(F)[ST]/(1-italic(F)[ST])))+
  annotation_custom(tableGrob(table2c,
                              rows = NULL,
                              cols = NULL,
                              theme = tt1),
                    xmin=0.5, xmax=5, ymin=0.75, ymax=1) 


p2c

table2d<-tibble(x=c("italic(partial~Mantel)*minute*italic(s)~italic(r)=='0.676'",
                    "italic(partial~Mantel)*minute*italic(s)~italic(p)=='0.002'",
                    "italic(partial~Mantel)*minute*italic(s)~italic(r)=='0.180'",
                    "italic(partial~Mantel)*minute*italic(s)~italic(p)=='0.117'"))

p2d<-dat %>% 
  select(env_dist,fst1779,fst53w) %>% 
  pivot_longer(!env_dist) %>% 
  ggplot(aes(x=env_dist,y=value))+
  geom_point(aes(color=name,
                 fill=name),
             shape=21,
             size=5)+
  scale_color_manual(values = c("#609ac6","#db6786"))+
  scale_fill_manual(values = c("#d6dde2","#fbf3d5"))+
  geom_smooth(aes(color=name),
              method = "lm",
              formula = 'y~x')+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_blank(),
        legend.position = "none")+
  labs(x="Environmental distance",
       y=expression(italic(F)[ST]/(1-italic(F)[ST])))+
  annotation_custom(tableGrob(table2d,
                              rows = NULL,
                              cols = NULL,
                              theme = tt1),
                    xmin=0.5, xmax=5, ymin=0.75, ymax=1) 

p2d



p2c+p2d
