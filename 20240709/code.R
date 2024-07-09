library(readxl)
library(tidyverse)
library(circlize)
library(RColorBrewer)

read_excel("data/20221211/Sourcedata/Fig1/chrLen.xlsx") %>% 
  set_colnames(c("chrom","end")) %>% 
  mutate(start=0) -> chr.len

pdf(file = "fig1a.pdf",width = 14,height = 14)
circos.clear()
circos.par(start.degree =86,clock.wise = T,track.margin=c(0.001,0),
           gap.degree=c(rep(1,18),10))


circos.initialize(factors = chr.len$chrom, 
                  xlim = matrix(c(chr.len$start,chr.len$end),ncol=2))

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.05,
                       bg.border = NA, 
                       #ylim=CELL_META$ylim,
                       panel.fun = function(x, y) {
                         circos.text(mean(CELL_META$xlim),mean(CELL_META$ylim),
                                     get.cell.meta.data("sector.index"))
                       },
                       bg.col="#CCCCCC")

brk<-c(0,10000000,20000000,30000000,40000000,50000000,60000000)
brk.label<-c()
for (i in brk){
  ifelse(i%%10^7==0,brk.label<-append(brk.label,
                                      paste0(i/10^7,"0M")),
         brk.label<-append(brk.label,""))
}
brk.label[1]<-"0M"
brk.label

for (chromosome in chr.len$chrom){
  circos.axis(sector.index = chromosome,
              h = 14,
              major.at = brk,
              minor.ticks = 0,
              labels = brk.label,
              labels.facing="clockwise",
              labels.cex = 0.6)
}

## 第一圈

colorRamp2(breaks = 0:7,
           col = brewer.pal(n = 8, name = "BuPu")) -> col_fun01

read_tsv("data/20221211/Sourcedata/Fig1/genedensity.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% 
  mutate(X5=col_fun01(X4)) -> dat.01

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.rect(sector.index = chromosome,
              xleft=dat.01[dat.01$X1==chromosome,]$X2,
              xright=dat.01[dat.01$X1==chromosome,]$X3,
              ybottom=0,
              ytop=10,
              col=dat.01[dat.01$X1==chromosome,]$X5,
              border=NA)
}

## 第二圈


colorRamp2(breaks = seq(0,1.6,0.2),
           col = brewer.pal(n = 9, name = "YlOrRd")) -> col_fun02

read_tsv("data/20221211/Sourcedata/Fig1/TEdensity.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% 
  mutate(X5=col_fun02(X4)) -> dat.02

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.rect(sector.index = chromosome,
              xleft=dat.02[dat.02$X1==chromosome,]$X2,
              xright=dat.02[dat.02$X1==chromosome,]$X3,
              ybottom=0,
              ytop=10,
              col=dat.02[dat.02$X1==chromosome,]$X5,
              border=NA)
}


## 第三圈

colorRamp2(breaks = seq(0,80,10),
           col = brewer.pal(n = 9, name = "BuGn")) -> col_fun03

read_tsv("data/20221211/Sourcedata/Fig1/snp.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% 
  mutate(X5=col_fun03(X4)) -> dat.03

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.rect(sector.index = chromosome,
              xleft=dat.03[dat.03$X1==chromosome,]$X2,
              xright=dat.03[dat.03$X1==chromosome,]$X3,
              ybottom=0,
              ytop=10,
              col=dat.03[dat.03$X1==chromosome,]$X5,
              border=NA)
}


## 第四圈

colorRamp2(breaks = seq(0,1320,200),
           col = brewer.pal(n = 7, name = "BuPu")) -> col_fun04

read_tsv("data/20221211/Sourcedata/Fig1/indel.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% 
  mutate(X5=col_fun04(X4)) -> dat.04

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.rect(sector.index = chromosome,
              xleft=dat.04[dat.04$X1==chromosome,]$X2,
              xright=dat.04[dat.04$X1==chromosome,]$X3,
              ybottom=0,
              ytop=10,
              col=dat.04[dat.04$X1==chromosome,]$X5,
              border=NA)
}


## 第五圈


colorRamp2(breaks = seq(0,302,50),
           col = brewer.pal(n = 7, name = "Oranges")) -> col_fun05

read_tsv("data/20221211/Sourcedata/Fig1/sv.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% 
  mutate(X5=col_fun05(X4)) -> dat.05

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.rect(sector.index = chromosome,
              xleft=dat.05[dat.05$X1==chromosome,]$X2,
              xright=dat.05[dat.05$X1==chromosome,]$X3,
              ybottom=0,
              ytop=10,
              col=dat.05[dat.05$X1==chromosome,]$X5,
              border=NA)
}


## 第六圈

read_tsv("data/20221211/Sourcedata/Fig1/snp_env.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) -> dat.06

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.segments(sector.index = chromosome,
              x0=dat.06[dat.06$X1==chromosome,]$X2,
              x1=dat.06[dat.06$X1==chromosome,]$X2,
              y0=0,
              y1=10,
              col="#669dc1")
}


## 第7圈

read_tsv("data/20221211/Sourcedata/Fig1/indel_env.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) %>% filter(X4>=1) -> dat.07

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.segments(sector.index = chromosome,
              x0=dat.07[dat.07$X1==chromosome,]$X2,
              x1=dat.07[dat.07$X1==chromosome,]$X2,
              y0=0,
              y1=10,
              col="#e58441")
}


## 第8圈

read_tsv("data/20221211/Sourcedata/Fig1/snp_env.txt",col_names = FALSE) %>% 
  mutate(X1=case_when(
    str_length(X1) == 4 ~ str_replace(X1,"chr","chr0"),
    TRUE ~ X1
  )) -> dat.08

circos.trackPlotRegion(chr.len$chrom, 
                       ylim = c(0, 10),
                       track.height = 0.1,
                       bg.col = NA, 
                       bg.border = NA)


for (chromosome in chr.len$chrom){
  circos.segments(sector.index = chromosome,
              x0=dat.08[dat.08$X1==chromosome,]$X2,
              x1=dat.08[dat.08$X1==chromosome,]$X2,
              y0=0,
              y1=10,
              col="#106134")
}

circos.clear()

dev.off()
