data <- read.csv(file = "online_popularity_data.csv")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
##data_life
##data_top <- data[order(-data$shares),]
##data_top <- data_top[1:5946,]
##data_bottom <- data[order(data$shares),]
##data_bottom <-data_bottom[1:5946,]
##write.csv(data_top, file = "top15.csv", row.names = FALSE)
##write.csv(data_bottom, file = "bottom15.csv", row.names = FALSE)
##summary(data_bottom$shares)

##lifestyle
datalife <- data[-c(19:23)]
data_life <- subset(datalife,datalife$data_channel_is_lifestyle == 1)
data_life <- data_life[order(-data_life$shares),]
##write.csv(data_life, file = "lifestyle.csv", row.names = FALSE)
##selcting top 15%
data_life <- data_life[1:315,]
library(corrplot)
data_lifecorr <- data_life[c(5,12,14,15,17)]
cor <- cor(data_lifecorr)
cor
corrplot(cor, method = "number")

#shares and imgs

al <- ggplot(data_life, aes(x = log(data_life$num_imgs), y = log(data_life$shares)))+
  geom_point() +
  xlab('images') + ylab('shares') +
  ggtitle('images and shares(lifestyle)') +
  geom_smooth(method = "lm") +
  theme_bw()


##shares vs videos
bl <- ggplot(data_life, aes(x = log(data_life$num_videos), y = log(data_life$shares))) +
  geom_point() +
  xlab('videos') + ylab('shares') +
  ggtitle('videos and shares(lifestyle)') +
  geom_smooth(method = "lm") +
  theme_bw()



##mean share for articles published on weekend
data_life_weekend<- (subset(data_life,data_life$is_weekend == 1))
summary(data_life_weekend$shares)
##Not on weekend
data_life_nweekend<- (subset(data_life,data_life$is_weekend == 0))
summary(data_life_nweekend$shares)
##  links and shares
cl <- ggplot(data_life, aes(x = log(data_life$num_hrefs), y = log(data_life$shares))) +
  geom_point() +
  xlab('links') + ylab('shares') +
  ggtitle('links and shares(lifestyle)') +
  geom_smooth(method = "lm") +
  theme_bw()


##keywords

dl <- ggplot(data_life, aes(x = log(data_life$num_keywords), y = log(data_life$shares))) +
  geom_point() +
  xlab('keywords') + ylab('shares') +
  ggtitle('keywords and shares(lifestyle)') +
  geom_smooth(method = "lm") +
  theme_bw()

ggarrange(al, bl, cl,dl,
          ncol = 2, nrow = 2)
summary(data_life$shares)

summary(data_life$num_videos)
##entertainment
dataent <- data[-c(18,20:23)]
data_ent <- subset(dataent,dataent$data_channel_is_entertainment ==1)
data_ent <- data_ent[order(-data_ent$shares),]
#write.csv(data_ent, file = "entertainment.csv", row.names = FALSE)
data_ent <- data_ent[1:1059,]
data_entcorr <- data_ent[c(5,12,14,15,17)]
cor <- cor(data_entcorr)
cor
##mean share for articles published on weekend
data_ent_weekend<- (subset(data_ent,data_ent$is_weekend == 1))
summary(data_ent_weekend$shares)
##Not on weekend
data_ent_nweekend<- (subset(data_ent,data_ent$is_weekend == 0))
summary(data_ent_nweekend$shares)


ae <- ggplot(data_ent, aes(x = log(data_ent$num_imgs), y = log(data_ent$shares))) +
  geom_point() +
  xlab('images') + ylab('shares') +
  ggtitle('images and shares(ENT)') +
  geom_smooth() +
  theme_bw()


##shares vs videos
be <- ggplot(data_ent, aes(x = log(data_ent$num_videos), y = log(data_ent$shares))) +
  geom_point() +
  xlab('videos') + ylab('shares') +
  ggtitle('videos and shares(ENT)') +
  geom_smooth() +
  theme_bw()


##  links and shares
ce <- ggplot(data_ent, aes(x = log(data_ent$num_hrefs), y = log(data_ent$shares))) +
  geom_point() +
  xlab('videos') + ylab('shares') +
  ggtitle('links and shares(ENT)') +
  geom_smooth() +
  theme_bw()

##keywords

de <- ggplot(data_ent, aes(x = log(data_ent$num_keywords), y = log(data_ent$shares))) +
  geom_point() +
  xlab('images') + ylab('shares') +
  ggtitle('keywords and shares(ENT)') +
  geom_smooth(method = "lm") +
  theme_bw()

ggarrange(ae, be, ce,de,ncol = 2, nrow = 2)


##soc
datasoc <- data[-c(18:20,22:23)]
data_soc <- subset(datasoc, datasoc$data_channel_is_socmed == 1)
data_soc <- data_soc[order(-data_soc$shares),]
write.csv(data_soc, file = "social.csv", row.names = FALSE)
data_soc <- data_soc[1:348,]
data_soccorr <- data_soc[c(5,12,14,15,17)]
cor <- cor(data_soccorr)
cor

##mean share for articles published on weekend
data_soc_weekend<- (subset(data_soc,data_soc$is_weekend == 1))
summary(data_soc_weekend$shares)
##Not on weekend
data_soc_nweekend<- (subset(data_soc,data_soc$is_weekend == 0))
summary(data_soc_nweekend$shares)

as <- ggplot(data_soc, aes(x = log(data_soc$num_imgs), y = log(data_soc$shares))) +
  geom_point() +
  xlab('images') + ylab('shares') +
  ggtitle('images and shares(SOCIAL MEDIA)') +
  geom_smooth(method = "lm") +
  theme_bw()


##shares vs videos
bs <- ggplot(data_soc, aes(x = log(data_soc$num_videos), y = log(data_soc$shares))) +
  geom_point() +
  xlab('videos') + ylab('shares') +
  ggtitle('videos and shares(SOCIAL MEDIA)') +
  geom_smooth(method = "lm") +
  theme_bw()
##  links and shares
cs <- ggplot(data_soc, aes(x = log(data_soc$num_hrefs), y = log(data_soc$shares))) +
  geom_point() +
  xlab('links') + ylab('shares') +
  ggtitle('links and shares(SOCIAL MEDIA)') +
  geom_smooth(method = 'lm') +
  theme_bw()


##keywords
ds <- ggplot(data_soc, aes(x = log(data_soc$num_keywords), y = log(data_soc$shares))) +
  geom_point() +
  xlab('keywords') + ylab('shares') +
  ggtitle('keywords and shares(SOCIAL MEDIA)') +
  geom_smooth(method = "lm") +
  theme_bw()

ggarrange(as, bs, cs,ds,ncol = 2, nrow = 2)

##tecg
datatech <- data[-c(18:21,23)]
data_tech <- subset(datatech, datatech$data_channel_is_tech == 1)
data_tech <- data_tech[order(-data_tech$shares),]
write.csv(data_tech, file = "tech.csv", row.names = FALSE)
data_tech <- data_tech[1:1102,]
summary(data$is_weekend)
sum(data_life$shares)
summary(data_life$shares)

data_techcorr <- data_tech[c(5,12,14,15,17)]
cor <- cor(data_techcorr)
cor

##mean share for articles published on weekend
data_tech_weekend<- (subset(data_tech,data_tech$is_weekend == 1))
summary(data_tech_weekend$shares)
##Not on weekend
data_tech_nweekend<- (subset(data_tech,data_tech$is_weekend == 0))
summary(data_tech_nweekend$shares)


at <- ggplot(data_tech, aes(x = log(data_tech$num_imgs), y = log(data_tech$shares))) +
  geom_point() +
  xlab('images') + ylab('shares') +
  ggtitle('images and shares(tech)') +
  geom_smooth() +
  theme_bw()


##shares vs videos
bt <- ggplot(data_tech, aes(x = log(data_tech$num_videos), y = log(data_tech$shares))) +
  geom_point() +
  xlab('videos') + ylab('shares') +
  ggtitle('videos and shares(Tech)') +
  geom_smooth() +
  theme_bw()

##  links and shares
ct <- ggplot(data_tech, aes(x = log(data_tech$num_hrefs), y = log(data_tech$shares))) +
  geom_point() +
  xlab('links') + ylab('shares') +
  ggtitle('links and shares(TECH)') +
  geom_smooth() +
  theme_bw()

dt <- ggplot(data_tech, aes(x = log(data_tech$num_keywords), y = log(data_tech$shares))) +
  geom_point() +
  xlab('keywords') + ylab('shares') +
  ggtitle('keywords and shares(TECH') +
  geom_smooth(method = "lm") +
  theme_bw()

ggarrange(at, bt, ct,dt,ncol = 2, nrow = 2)


##BUSINESS
databus <- data[-c(18,19,21:23)]
data_bus <- subset(databus, databus$data_channel_is_bus == 1)
data_bus <- data_bus[order(-data_bus$shares),]
write.csv(data_bus, file = "business.csv", row.names = FALSE)

dataworld <- data[-c(18:22)]
data_world <- subset(dataworld, dataworld$data_channel_is_world == 1)
data_world <- data_world[order(-data_world$shares),]
write.csv(data_world, file = "world.csv", row.names = FALSE)
