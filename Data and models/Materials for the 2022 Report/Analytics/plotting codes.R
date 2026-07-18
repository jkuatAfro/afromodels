TrailHR <- 
  readXL("H:/Brazzaville/HRH 24/Materials/HWF Expenditures and Salaries/Seydou Coulibaly et al 2023 HWF remunration within health expenditures.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE)


Trial2 <- 
  readXL("H:/Brazzaville/HRH 24/Materials/HWF Expenditures and Salaries/1 Seydou Coulibaly et al 2023 HWF remunration within health expenditures.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE)

library(ggplot2)
library(dplyr)
library(reshape2)
library(directlabels)
theme_set(theme_minimal())


#####################

##plotting

#####################


###  Determinants
########################



p <- ggplot(Trial2, aes(che_usd2019_pc, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Per capita health expenditure in USD (2019) and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Per capita health expenditure in USD (2019)")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 100, y = 8, label = "Spearman's rho = 0.68, p-value = 0.00",col="red")



#####


######


p <- ggplot(Trial2, aes(hr_che, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Health workforce (HWF) remuneration expenditure and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Health workforce (HWF) remuneration expenditure")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 25, y = 8, label = "Spearman's rho = 0.368, p-value = 0.03",col="red")


#####################



p <- ggplot(Trial2, aes(pop, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Population (thousand) and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Population (thousand)")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 100000, y = 8, label = "Spearman's rho = - 0.19, p-value = 0.29",col="red")


############


p <- ggplot(Trial2, aes(hr_usd2019_pc, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Per capita HWF expenditure in USD (2019) and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Per capita HWF expenditure in USD (2019)")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 100, y = 8, label = "Spearman's rho = 0.63, p-value = 0.00",col="red")


############################


p <- ggplot(Trial2, aes(gghed_gge, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "General health expenditure as a share of general government expenditure and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="General health expenditure as a share of general government expenditure")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 8, label = "Spearman's rho = 0.38, p-value = 0.03",col="red")


######################################


p <- ggplot(Trial2, aes(sr_hr_pvtd_hr, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Remunerations on health workforce (HWF) from private domestic sources as a share of health workforce expenditure and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Remunerations on health workforce (HWF) from private domestic sources as a share of health workforce expenditure")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 50, y = 8, label = "Spearman's rho = - 0.08, p-value = 0.69",col="red")


##########################


p <- ggplot(Trial2, aes(sr_hr_ext_hr, m_doctors_density, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Remunerations on health workforce (HWF) from external sources as a share of health workforce expenditure and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Medical doctors density")+
scale_x_continuous(name="Remunerations on health workforce (HWF) from external sources as a share of health workforce expenditure")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 25, y = 8, label = "Spearman's rho = - 0.46, p-value = 0.009",col="red")




#########################################

####Impacts

p <- ggplot(Trial2, aes(m_doctors_density,NCD , label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "UHC NCD index and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="NCD index")+
scale_x_continuous(name="Medical doctors density")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 45, label = "Spearman's rho = 0.44, p-value = 0.01",col="red")



#####


p <- ggplot(Trial2, aes(m_doctors_density,NCD , label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "UHC NCD index and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="NCD index")+
scale_x_continuous(name="Medical doctors density")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 45, label = "Spearman's rho = 0.44, p-value = 0.01",col="red")



#####


p <- ggplot(Trial2, aes(m_doctors_density,HALE, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "HALE and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="HALE")+
scale_x_continuous(name="Medical doctors density")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 45, label = "Spearman's rho = 0.35, p-value = 0.05",col="red")



#####



p <- ggplot(Trial2, aes(m_doctors_density, Infectious.dieases, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Infectious dieases index and Medical doctors density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Infectious dieases index")+
scale_x_continuous(name="Medical doctors density")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 5, y = 30, label = "Spearman's rho = 0.31, p-value = 0.08",col="red")



#####







p <- ggplot(Trial2, aes(che_usd2019_pc, hr_usd2019_pc, label = as.character(code)))
p + geom_point(aes(colour= income),size = 4) + scale_size_continuous(range=c(0,100))+
geom_text(aes(label=code), size=5)+
theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Per capita health expenditure in USD (2019)",caption = "Source: WHO 2024")+
scale_y_continuous(name="Per capita HWF expenditure in USD (2019)")+
scale_x_continuous(name="Per capita health expenditure in USD (2019)")+
theme(legend.position="right")+
theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14, angle=0),
          axis.text.y = element_text(face="bold", color="#993333", 
                           size=14, angle=0))+
theme( axis.line = element_line(colour = "darkblue", 
                      size = 1, linetype = "solid"))+
geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
annotate("text", x = 100, y = 200, label = "Spearman's rho = 0.9, p-value = 0.00",col="red")







