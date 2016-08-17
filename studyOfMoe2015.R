## This program is to analize Bilibili Moe 2015

library(ggplot2)
load(file = 'Moe_PreSeason.Rda')

dataAll$Pass = as.numeric(as.character(dataAll$Pass))
dataAll$Anime = as.character(dataAll$Anime)
dataAll$Vote = as.numeric(as.character(dataAll$Vote))
dataAll$Date = as.Date(as.character(dataAll$Date), '%Y/%m/%d')
dataAll$Image = as.character(dataAll$Image)
dataAll$Name = as.character(dataAll$Name)

# Pass rate by Anime

dataByAnime = aggregate(Pass ~ Anime, dataAll, sum)
dataByAnime$Participation = as.vector(table(dataAll$Anime))
dataByAnime$Rate = round(dataByAnime$Pass / dataByAnime$Participation, 2)
totalvote = aggregate(Vote ~ Anime, dataAll, sum)
dataByAnime$Average = round(totalvote$Vote / dataByAnime$Participation, 2)
rm(totalvote)
View(dataByAnime)
test = sort(dataByAnime$Rate, decreasing = T)
test = dataByAnime[order(dataByAnime$Rate, decreasing = T),]
# rownames(test) <- seq(1:nrow(test))
test[1:10,]
ggplot(test[1:10,]) +
      geom_bar(aes(reorder(Anime, -Rate), Rate), stat = 'identity') +
      theme(axis.text.x=element_text(angle = 90, size = 7, colour = "black"))

# by date

dataByDate = aggregate(Vote ~ Date, dataAll, sum)
dataByDate$Average = dataByDate$Vote/6
View(dataByDate)
ggplot(dataByDate) + geom_line(aes(Date, Average)) +
      theme_bw()


specificAnime <- dataAll[(dataAll$Anime) == '我的青春恋爱物语果然有问题 第二季', ]
View(specificAnime)
specificAnime <- dataAll[grepl("青春恋爱", dataAll$Anime, ignore.case = TRUE),]
View(specificAnime)



specificGroup <- dataAll[(dataAll$Group) == '162', ]
View(specificGroup)

allPass <- dataAll[(dataAll$Pass) == 1, ]
View(allPass)

