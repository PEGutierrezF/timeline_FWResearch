


# ---------------------------------------------
# Timeline of Freshwater Research Global North - Latin America
# 06 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  

install.packages("flextable")
library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)


#Create table to plot
data <- tribble(~start_date, ~event, ~displ, ~model,~bolded,
                 ymd(19680101), "Secondary production (Hynes & Coleman 1968)", 1, 'SP',"No bold",
                 ymd(19980101), "Secondary production (Ramírez & Pringle 1998)", 1, 'SP',"Bold",

                 ymd(19800101), "River continuum concept (Vannote 1980)", 0.7, 'RCC',"No bold",
                 ymd(20000101), "River continuum concept (Cressa 2000)", 0.7, 'RCC',"Bold",
                
                 ymd(20010101), "Urban stream (Paul & Meyer 2001)", 0.5,'US',"No bold",
                 ymd(20060101), "Urban stream (Cunico et al. 2006)", 0.4,'US',"Bold",
                
                 ymd(19560101), "Stream metabolism (Odum 1956)", -1,'SM',"No bold",
                 ymd(20090101), "Stream metabolism (Ortiz-Zayas 2005)", -1,'SM',"Bold",

                 ymd(19740101), "Organic matter proc. (Petersen & Cummins 1974)", -0.7,'OM',"No bold",
                 ymd(19940101), "Organic matter proc. (Irons et al. 1994)", -0.7,'OM',"Bold",
              
                 ymd(19890101), "Flood pulse concept (Junk et al. 1989)", -0.5,'LA',"Bold",
                 ymd(19890101), "Flood pulse concept (Bayley et al. 1991)", -0.4,'LA',"No bold")

  
#Function to shift x-axis to 0 adapted from link shown above
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, size = 0.5,
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}


#Conditionally set whether text will be above or below the point
vjust = ifelse(data$displ > 0, -1.5, 1.5)

#plot
p1 <- data %>% 
  ggplot(aes(start_date, displ)) +
  geom_lollipop(point.size =1 ) +
  # geom_text(aes(x = start_date, y = displ, label = event), data = data,
   # hjust = 0, vjust = vjust, size = 4) +
  
  geom_label(aes(x = start_date, y = displ, label = event, color=model,
            fontface = ifelse(bolded == "Bold", "bold", "plain")),
             data = data, hjust = 0, vjust = 0, size = 4.3) +
#color=factor(model)
    geom_point(aes(colour = factor(model)), size = 2) +
  labs(x = "", y = "", 
    title = "",
    subtitle = "") +
  
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12,face="bold")) +
  
  theme(axis.line =  element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank()) +

  expand_limits(x = c(ymd(19500101), ymd(20210101)), y =1) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9)) +
  
  scale_colour_manual(values=c("brown3", "darkorchid4","deeppink2", "turquoise3","royalblue3", "springgreen3"))+
                                # FP        #OM          # RCC         #SM         #SP         # US
                       
  theme(legend.position = "none")

#and run the function from above
timeline <- shift_axis(p1, ymd(19500101), ymd(20210101))
timeline

#
ggsave("Figure 1.jpg",timeline, dpi=300, width = 40, height = 20, units = "cm")




