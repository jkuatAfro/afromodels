library(ggplot2)

p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Core.health.workforce.density)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Core.health.workforce.density),
                method = "glm", method.args = list(family = "Gamma"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Core health workforce density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2         




p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Dentists)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Dentists),
                method = "glm", method.args = list(family = "gaussian"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Dentists density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2   







p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Doctors)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Doctors),
                method = "glm", method.args = list(family = "Gamma"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Doctors density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2   






p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Nurses)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Nurses),
                method = "glm", method.args = list(family = "Gamma"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Nurses density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2  




p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Pharmacists)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Pharmacists),
                method = "glm", method.args = list(family = "Gamma"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Pharmacists density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2 



p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Pharmacists)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Pharmacists),
                method = "glm", method.args = list(family = "Gamma"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Pharmacists density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2  



p <- ggplot(data = SynthesisModels, aes(che_pc_usd, Midwives)) +
    geom_jitter()
p2 <- p + geom_smooth( aes(che_pc_usd, Midwives),
                method = "glm", method.args = list(family = "gaussian"),
                se = FALSE, 
                colour = "black", size = 0.8) +
                theme_bw()+
scale_y_continuous(name="Midwives density")+
scale_x_continuous(name="Current Health Expenditure (CHE) per Capita in US$")
p2   

    
