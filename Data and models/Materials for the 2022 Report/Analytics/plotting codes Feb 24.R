
synthesis <- 
  readXL("H:/Brazzaville/HRH 24/Materials/HWF Expenditures and Salaries/1 Seydou Coulibaly et al 2023 HWF remunration within health expenditures.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Synthesis", stringsAsFactors=TRUE)


##############


library(ggplot2)
library(dplyr)
library(reshape2)
library(directlabels)
theme_set(theme_minimal())


head(synthesis)
attach(synthesis)



#####################

##plotting

#####################


###  Determinants
########################

plot(che_pc_usd, Core.health.workforce.density)

p <- ggplot(synthesis, aes(che_pc_usd, Core.health.workforce.density, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Per capita health expenditure in USD and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="SDG 3 c tracer occupations density",trans='log10')+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 200, y = 200, label = "Spearman's rho = 0.74, p-value = 0.00",col="red")



#####

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(ggrepel)
library(plotly)

my_plot = ggplot(synthesis, aes(x=che_pc_usd, y=Core.health.workforce.density, size = Core.health.workforce.density, color = Income)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
geom_text(aes(label=Country.code), size=5,color=red)+
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none")+
scale_y_continuous(name="SDG 3 c tracer occupations density",trans='log10')+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$",trans='log10')
 my_plot


 
ggplotly(my_plot)


######

p <- ggplot(synthesis, aes(dom_che, Core.health.workforce.density, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Domestic Health Expenditure as % of Current Health Expenditure and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="SDG 3 c tracer occupations density",trans='log10')+
scale_x_continuous(name="Domestic Health Expenditure as % of Current Health Expenditure",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 50, y = 200, label = "Spearman's rho = 0.41, p-value = 0.004",col="red")



#####


######

p <- ggplot(synthesis, aes(gghed_che, Core.health.workforce.density, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Domestic General Government Health Expenditure as % Current Health Expenditure and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="SDG 3 c tracer occupations density",trans='log10')+
scale_x_continuous(name="Domestic General Government Health Expenditure as % Current Health Expenditure",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 15, y = 200, label = "Spearman's rho = 0.63, p-value = 0.00",col="red")



#####



######

p <- ggplot(synthesis, aes(ext_che, Core.health.workforce.density, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "External Health Expenditure as % of Current Health Expenditure and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="SDG 3c tracer occupations density",trans='log10')+
scale_x_continuous(name="External Health Expenditure  as % of Current Health Expenditure",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 15, y = 200, label = "Spearman's rho = - 0.407, p-value = 0.004",col="red")



######

p <- ggplot(synthesis, aes(oops_che, Core.health.workforce.density, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Out-of-pocket as % of Current Health Expenditure and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="SDG 3c tracer occupations density",trans='log10')+
scale_x_continuous(name="Out-of-pocket as % of Current Health Expenditure",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 15, y = 200, label = "Spearman's rho = - 0.398, p-value = 0.006",col="red")



######

p <- ggplot(synthesis, aes(Core.health.workforce.density, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access",trans='log10')+
scale_x_continuous(name="SDG 3c tracer occupations density",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 50, y = 100, label = "Spearman's rho = 0.721, p-value = 0.000",col="red")




######

p <- ggplot(synthesis, aes(Core.health.workforce.density, NCDs, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and NCDs index",caption = "Source: WHO 2024")+
scale_y_continuous(name="NCDs index",trans='log10')+
scale_x_continuous(name="SDG 3c tracer occupations density",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 50, y = 75, label = "Spearman's rho = 0.407, p-value = 0.004",col="red")




###
p <- ggplot(synthesis, aes(Core.health.workforce.density, RMNCAH, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and RMNCAH index",caption = "Source: WHO 2024")+
scale_y_continuous(name="RMNCAH index",trans='log10')+
scale_x_continuous(name="SDG 3c tracer occupations density",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 150, y = 40, label = "Spearman's rho = 0.664, p-value = 0.000",col="red")




###
p <- ggplot(synthesis, aes(Core.health.workforce.density, Infectious.diseases, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and Infectious diseases index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Infectious diseases index",trans='log10')+
scale_x_continuous(name="SDG 3c tracer occupations density",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 150, y = 40, label = "Spearman's rho = 0.298, p-value = 0.042",col="red")




###
p <- ggplot(synthesis, aes(Core.health.workforce.density, HALE, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and HALE",caption = "Source: WHO 2024")+
scale_y_continuous(name="HALE",trans='log10')+
scale_x_continuous(name="SDG 3c tracer occupations density",trans='log10')+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 150, y = 50, label = "Spearman's rho = 0.244, p-value = 0.09",col="red")



##################



###
p <- ggplot(synthesis, aes(Core.health.workforce.density, HALE, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Core health workforce density and HALE",caption = "Source: WHO 2024")+
scale_y_continuous(name="HALE")+
scale_x_continuous(name="Core health workforce density")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 150, y = 50, label = "Spearman's rho = 0.244, p-value = 0.09",col="red")







#####################

p <- ggplot(synthesis, aes(Doctors, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Doctors and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access")+
scale_x_continuous(name="Medical doctors density per 10,000 population")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 100, label = "Spearman's rho = 0.608, p-value = 0.000",col="red")




######

#####################

p <- ggplot(synthesis, aes(Nurses, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Nursing personnel and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access")+
scale_x_continuous(name="Nursing personnel density per 10,000 population")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 25, y = 100, label = "Spearman's rho = 0.678, p-value = 0.000",col="red")




######


#####################

p <- ggplot(synthesis, aes(Midwives, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Midwives and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access")+
scale_x_continuous(name="Midwives density per 10,000 population")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 100, label = "Spearman's rho = 0.104, p-value = 0.487",col="red")




#####################

p <- ggplot(synthesis, aes(Dentists, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Dentists and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access")+
scale_x_continuous(name="Dentists density per 10,000 population")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 1, y = 100, label = "Spearman's rho = 0.664, p-value = 0.000",col="red")




#####################

p <- ggplot(synthesis, aes(Pharmacists, Service.capacity.and.access, label = as.character(Country.code)))
p + geom_point(aes(colour= Income),size = 4) + scale_size_continuous(range=c(0,1000))+
geom_text(aes(label=Country.code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Pharmacists and Service capacity and access index",caption = "Source: WHO 2024")+
scale_y_continuous(name="Service capacity and access")+
scale_x_continuous(name="Pharmacists density per 10,000 population")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 2, y = 100, label = "Spearman's rho = 0.607, p-value = 0.000",col="red")






