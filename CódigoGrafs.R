#1
library(magrittr); library (ggplot2); library(ggpubr); library(gridExtra); library(cowplot); library(dplyr)

#funsión para gráfica 1
set.seed(42)
n<-1000
x<-runif(n)* 3
y<-x*sin(1/x)+rnorm(n)/25
df<- data.frame(x=x,y=y)

#Gráfica x vs y
xy<- ggplot(df, aes(x,y))+geom_point(alpha=0.3, col="navyblue")+
  geom_smooth(se=FALSE)+scale_x_continuous(limits=c(0,0.5))+
  scale_y_continuous(limits=c(-0.3,0.4))+ theme_bw()

#Gráfica dose vs len
dl<- ggdotplot(ToothGrowth, x="dose", y= "len",
               color= "dose", palette= "jco", binwidth = 1)+ theme_classic()

mtcars$name=rownames(mtcars)
mtcars$cyl=as.factor(mtcars$cyl)

#Gráfico de barras por registro
barras<-ggbarplot(mtcars, x="name", y="mpg", fill="cyl",
              color="white", palette="jco",
              sort.val="asc", sort.by.groups=TRUE,
              x.text.angle=90)+font("x.text",size=8)

#Gráfico de dispersión con regresión y ecuaciones
regs<-ggscatter(mtcars, x="wt", y="mpg", add="reg.line", conf.int=TRUE,
              color="cyl", palette="jco", shape="cyl")+
  stat_cor(aes(color=cyl), label.x.npc = "centre", label.y.npc = "top")

#Gráficos de densidad 
rho<-ggplot(diamonds, aes(depth, fill=cut, colour=cut))+
  geom_density(alpha=0.2, na.rm = TRUE)+
  xlim(58,68)+theme_classic()+theme(legend.position="bottom")

rhot<-ggplot(faithfuld, aes(eruptions, waiting))+
  geom_raster(aes(fill=density))+theme_classic()+
  scale_fill_gradientn(colours=heat.colors(10, rev=TRUE), na.value = "white")


#grafica multiple
Grafica1<- ggarrange(barras,
                   ggarrange(rho,
                             ggarrange(xy,dl,regs,rhot, nrow = 2, ncol = 2,
                                      labels= c("B", "c", "D", "E")),
                            ncol = 2, labels= "F"),
                            nrow = 2, labels= "A")
ggsave(filename="Grafica1.png", height= 14, width = 14,
       plot = Grafica1, dpi = 400)

Grafica1

#2
ruta<-"https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/gapminder.csv"
df_gapminder <- read.table(file = ruta, sep=';', header = TRUE, stringsAsFactors = FALSE)
head(df_gapminder, 2)

ev1<-df_gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent, label=ifelse(pop > 30000000, country, ""))) + 
  geom_point(alpha=0.7)+
  ggrepel::geom_text_repel(aes(size =pop), nudge_x = 1000, nudge_y = -2, box.padding = 1) +
  guides(size=FALSE)+
  labs(title="Esperanza de vida debido al ingreso promedio - 2007", caption = "Fuente: Gapminder",
       x="Ingreso Promedio (USD)",
       y="Expectativa de vida al nacer (años)",
       color="Continente:")+
  theme_bw()+
  theme(plot.subtitle = element_text(hjust=1, vjust = -1.2),
        plot.caption = element_text(hjust = 1, vjust = 0, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        legend.position = "bottom") 

ev2 <- df_gapminder %>% 
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent, label = country)) + 
  geom_point(alpha = 0.7) +
  scale_x_continuous(limits = c(6000, 8000), expand = c(0, 2)) +
  scale_y_continuous(limits = c(65, 80), expand = c(0, 2)) +
  ggrepel::geom_text_repel(size = 3, nudge_x = 1, nudge_y = 0.5) +
  guides(size = FALSE, color = FALSE) +
  labs(x = "", y = "") +
  theme_bw()

Grafica2<-ev1+ annotation_custom(ggplotGrob(ev2), xmin=30000, xmax=50000, ymin=40, ymax = 60)

ggsave(filename="Grafica2.png", height= 14, width = 14,
       plot = Grafica2, dpi = 400)

Grafica2


