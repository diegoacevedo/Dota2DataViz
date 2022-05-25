library(httr)
library(jsonlite)
library(tidyverse)
library(gridExtra)
library(RcppSimdJson)
library(patchwork)
library(grid)
library(sqldf)

# set your working directory
setwd()


# load files
df2021=read_csv('ti2021.csv')
dfstats=read_csv('hero_stats.csv')


# transform win_rate variable
df2021=df2021 %>% 
  mutate(
    win_rate_num=str_remove(win_rate,'%'),
    win_rate_num=as.numeric(win_rate_num)/100)


# join dataframes
df2021=left_join(x=df2021,y=dfstats,by='name')

# transform atribute to factor and replace 'na' in win_rate_num
df2021=df2021 %>% 
  mutate(attribute_factor=as.factor(attribute)) %>% 
  mutate(win_rate_num=ifelse(is.na(win_rate_num),0,win_rate_num))

df2021=is.na(df2021$win_rate_num)=0

# preset colors for data points
paleta=c('springgreen','cyan','tomato')

# upload heroe's images
eldertitan=png::readPNG('elder_titan2.png',T)
bane=png::readPNG('base_bane.png',T)
huskar=png::readPNG('huskar.png',T)

# preset font
windowsFonts(A=windowsFont('Segoe UI'))

# make the visual
ggplot(df2021,
       aes(x=total_picks,
           y=win_rate_num,
           color=attribute_factor))+
  geom_point(shape=19,
             alpha=0.5,
             aes(size=df2021$total_ban))+
  scale_size_continuous(range = c(1,20))+
  scale_color_manual(values=paleta)+
  theme(
    plot.title = element_text(hjust=0.5,colour ='lightgoldenrod3',face = 'bold',size=20),
    plot.title.position = 'plot',
    plot.subtitle = element_text(hjust=0.5,colour ='lemonchiffon2',size=15),
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 1,size=7,color ='gray80'),
    panel.background = element_blank(),
    plot.background = element_rect(fill='grey20'),
    text = element_text(family = 'A'),
    panel.grid = element_blank(),
    axis.line.x = element_line(linetype = 'solid',color='gray80'),
    axis.line.y = element_line(linetype = 'solid',color='gray80'),
    axis.text.x = element_text(colour = 'gray80'),
    axis.text.y = element_text(colour = 'gray80'),
    axis.title = element_text(colour = 'gray80',face = 'bold'),
    axis.title.y = element_text(hjust = 1),
    axis.title.x = element_text(hjust = 1),
    legend.position = 'left',
    legend.justification = c(0.5,0.5),
    legend.background = element_blank(),
    legend.key=element_blank(),
    legend.title = element_text(color='gray80'),
    legend.text = element_text(color='gray80'),
  )+
  labs(
    y='Win Rate',
    x='Total Picks',
    title = 'The International 2021 Dota 2 Championship Heroes',
    subtitle = 'Total Picks, Win Rate and Total Bans',
    caption = 'Data obtained from kaggle.com (Dota 2 Last Three The International Hero Stats), visual created by Diego Jose Acevedo',
    color='Main Attribute',
    size='Total Bans'
  )+
  annotation_raster(eldertitan,xmin=64,xmax=74,ymin=0.62,ymax=0.83,interpolate=F)+
  annotation_raster(bane,xmin=27,xmax=37,ymin=0.77,ymax=1,interpolate=F)+
  annotation_raster(huskar,xmin=-4,xmax=7,ymin=0.05,ymax=0.26,interpolate=F)+
  
  annotate(geom='text',label='Elder titan:',color='gray80',x=62,y=0.9,size=2.9,hjust=0,fontface='bold')+
  annotate(geom='text',label='Picks: 73',color='gray80',x=62,y=0.87,size=2.8,hjust=0)+
  annotate(geom='text',label='Win Rate: 0.6027',color='gray80',x=62,y=0.84,size=2.8,hjust=0)+
  annotate(geom='text',label='Bans: 48',color='gray80',x=62,y=0.81,size=2.8,hjust=0)+
  
  annotate(geom='text',label='Bane: ',color='gray80',x=37,y=0.95,size=2.9,hjust=0,fontface='bold')+
  annotate(geom='text',label='Picks: 32',color='gray80',x=37,y=0.92,size=2.8,hjust=0)+
  annotate(geom='text',label='Win Rate: 0.7813',color='gray80',x=37,y=0.89,size=2.8,hjust=0)+
  annotate(geom='text',label='Bans: 16',color='gray80',x=37,y=0.86,size=2.8,hjust=0)+
  
  annotate(geom='text',label='Huskar: ',color='gray80',x=-2,y=0.39,size=2.9,hjust=0,fontface='bold')+
  annotate(geom='text',label='Picks: 0',color='gray80',x=-2,y=0.36,size=2.8,hjust=0)+
  annotate(geom='text',label='Win Rate: 0.00',color='gray80',x=-2,y=0.33,size=2.8,hjust=0)+
  annotate(geom='text',label='Bans: 1',color='gray80',x=-2,y=0.30,size=2.8,hjust=0)