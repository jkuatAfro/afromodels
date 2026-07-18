library(ggplot2)
dframe1 <- structure(list(a = 1:6, b = c(5, 7, 9, 10.5, 11.7, 17), category = structure(c(1L, 
1L, 1L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor")), .Names = c("a", 
"b", "category"), class = "data.frame", row.names = c(NA, -6L
))
qplot(a, b, data = dframe1, colour = category) + geom_smooth(method = lm)

#####

ggplot(dframe1, aes(x = a, y = b)) +
    stat_smooth(method = lowess) +
    geom_point(aes(color = category))

######


set.seed(20)
df <- as.data.frame(matrix(rnorm(120), 30, 16))
df[,1] <- 1980:2009
colnames(df) <- c("Year","Model 1", "Model 2", "Model 3","Model 4","Model 5","Model 6","Model 7","Model 8","Model 9","Model 10","Model 11","Model 12","Model 13","Model 14","Model 15")
df

reg=data.frame(start=2000,end=2009,group=1)



library(tidyverse)
df %>%
  as_tibble() %>%
  pivot_longer(-1)

df %>%
  as_tibble() %>%
  pivot_longer(-1) %>%
  ggplot(aes(Year, value, color = name)) +
  geom_point() +  # Add points
  geom_line()     # Connect points with lines


#####or

library(ggalt)

df %>%
  as_tibble() %>%
  pivot_longer(-1) %>%
  filter(grepl("12|13|14", name)) %>%
  ggplot(aes(Year, value, color = name)) +
theme_minimal() +
  geom_point() +
  geom_xspline()+
geom_rect(data=reg, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(df$value),
                ymax=max(df$value), group=group), color="transparent", fill="orange", alpha=0.3)+
annotate("text", x = 2003, y = 1.9, label = "Predicted values")+
theme(legend.position = "bottom",text = element_text(size=20))+
xlab("My x label") +
ylab("My y label") 

#####################################

###UHC


uhc=read.table("H:/Brazzaville/HRH 24/Analytics/March/uhc.txt",header=T,sep='\t')

reg=data.frame(start=2023,end=2031,group=1)


uhc %>%
  as_tibble() %>%
  pivot_longer(-1) %>%
    ggplot(aes(Year, value, color = name,shape=name)) +
theme_minimal() +
  geom_point(size=3) +
  geom_xspline(size=2.5)+
geom_rect(data=reg, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(uhc$value),
                ymax=max(uhc$value), group=group), color="transparent", fill="orange", alpha=0.3)+
annotate("text", x = 2027, y = 75, label = "Predicted values", col='blue')+
theme(legend.position = "right",text = element_text(size=15),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
xlab("Year") +
ylab("UHC Service Coverage Index") +
labs(color='INDEX') +
scale_x_continuous(breaks = seq(min(uhc$Year), max(uhc$Year), by = 1))+
scale_y_continuous(breaks = seq(10, 80, by = 10))+
guides(color = guide_legend(override.aes = list(shape = c(19,17,15,3,7))))


#######

## Example data
set.seed(0)
dat <- data.frame(dates=seq.Date(Sys.Date(), Sys.Date()+99, 1),
                  value=cumsum(rnorm(100)))

## Determine highlighted regions
v <- rep(0, 100)
v[c(5:20, 30:35, 90:100)] <- 1

## Get the start and end points for highlighted regions
inds <- diff(c(0, v))
start <- dat$dates[inds == 1]
end <- dat$dates[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(dat$dates, 1))

## highlight region data
rects <- data.frame(start=start, end=end, group=seq_along(start))

library(ggplot2)
ggplot(data=dat, aes(dates, value)) +
  theme_minimal() +
  geom_line(lty=2, color="steelblue", lwd=1.1) +
  geom_point() +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(dat$value),
                ymax=max(dat$value), group=group), color="transparent", fill="orange", alpha=0.3)

