eduden 

library(ggplot2)

theme_bw()

theme_get()

theme_set(theme_bw())
doctgrad <- readXL("H:/Brazzaville/HRH 24/Analytics/Mapping/Stock grad.xlsx", rownames=FALSE, header=TRUE, na="", 
  sheet="Stock grad Doctors", stringsAsFactors=TRUE)



p <- ggplot(doctgrad, aes(Doctors.stock.2022, Doctors.graduates, label = as.character(Country)))
p + geom_point() +
geom_text(aes(label=Country), size=4, color="red")+
scale_x_continuous(trans='log10')+
scale_y_continuous(trans='log10')+
geom_smooth(method='lm')



NursesGrad <- readXL("H:/Brazzaville/HRH 24/Analytics/Mapping/Stock grad.xlsx", rownames=FALSE, header=TRUE, na="",
   sheet="Nurses", stringsAsFactors=TRUE)

p <- ggplot(NursesGrad, aes(Nurses.Stock.2022, Nurses.Graduates, label = as.character(Country)))
p + geom_point() +
geom_text(aes(label=Country), size=4, color="red")+
scale_x_continuous(trans='log10')+
scale_y_continuous(trans='log10')+
geom_smooth(method='lm')


scale_y_continuous(name="Doctors and Nurses graduates (log scale)")+
scale_x_continuous(name="Doctors and Nurses stock in 2022 (log scale)")





DocNurses

#theme_light()

#theme_classic()

theme_bw()

p <- ggplot(StockgradocNur, aes(Doctors.and.Nurses.Stock.2022, Doctors.and.Nurses.graduates, label = as.character(Country.code)))
p + geom_point() +
geom_text(aes(label=Country.code), size=4, color="red")+
scale_x_continuous(trans='log10')+
scale_y_continuous(trans='log10')+
geom_smooth(method='lm')+
labs(x="Doctors and Nurses stock in 2022 (log scale)",y="Doctors and Nurses graduates (log scale)",
               title="Relationship between the graduates and stock")+
annotate("text", x = 1000, y = 10000, label = "Spearman's rho = 0.79, p-value < 0.0001",col="blue")



p <- ggplot(StockgradocNur, aes(Doctors.and.Nurses.Stock.2022, Doctors.and.Nurses.graduates, label = as.character(Country.code)))
p + geom_point() +
geom_text(aes(label=Country.code), size=4, color="red")+
scale_x_continuous(trans='log10')+
scale_y_continuous(trans='log10')+
geom_vline(xintercept=43944)+
geom_hline(yintercept = 3399) +
labs(x="Doctors and Nurses stock in 2022 (log scale)",y="Doctors and Nurses graduates (log scale)",
               title="Relationship between the graduates and stock")+
annotate("text", x = 1000, y = 10000, label = "Spearman's rho = 0.79, p-value < 0.0001",col="blue")


p <- ggplot(StockgradocNur, aes(Density, Ratio, label = as.character(Country.code)))
p + geom_point() +
geom_text(aes(label=Country.code), size=4, color="red")+
geom_vline(xintercept=17.3)+
geom_hline(yintercept = 23.9) +
labs(x="Density of Doctors and Nurses stock in 2022 (log scale)",y="Doctors and Nurses Stock to graduates ratio (log scale)",
               title="Relationship between the graduates ratios and Density")+
annotate("text", x = 100, y = 100, label = "Spearman's rho = 0.79, p-value < 0.0001",col="blue")



p <- ggplot(StockgradocNur, aes(Percentage.of.health.budget.spent.on.HWF, GGHE.D.as.percentage.GGE, label = as.character(Country.code)))
p + geom_point() +
geom_text(aes(label=Country.code), size=4, color="red")+
geom_vline(xintercept=42)+
geom_hline(yintercept = 7.3) +
labs(x="Percentage of health budget spent on HWF",y="GGHE-D as % General Government Expenditure (GGE)")


+
annotate("text", x = 1000, y = 10000, label = "Spearman's rho = 0.79, p-value < 0.0001",col="blue")


























scale_y_continuous(name="Doctors and Nurses graduates (log scale)")+
scale_x_continuous(name="Doctors and Nurses stock in 2022 (log scale)")


geom_abline(intercept = 0, slope = 0.8, size = 0.5) 




+stat_qq_line()




theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=10))+
labs(title = "Per capita health expenditure in USD and Core health workforce density",caption = "Source: WHO 2024")+
scale_y_continuous(name="Core health workforce density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")+
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






p <- ggplot(eduden, aes(Doctors.and.Nurses.Density.per.10.000..2022., Graduates.Doctors.and.Nurses.per.10.000
, label = as.character(Country...2)))
p + geom_point() +
geom_text(aes(label=Country...2), size=4, color="red")+
scale_x_continuous(trans='log10')+
scale_y_continuous(trans='log10')+
geom_smooth(method='lm')+
geom_vline(xintercept= 22)+
geom_hline(yintercept = 1.6) +
labs(x="Doctors and Nurses per 10,000 pop. in 2022 (log scale)",y="Doctors and Nurses graduates per 10,000 pop. (log scale)",
               title="Relationship between the graduates and stock densities")+
annotate("text", x = 5, y = 5, label = "Spearman's rho = 0.51, p-value = 0.0003",col="blue")


