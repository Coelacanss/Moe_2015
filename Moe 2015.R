### This program is to grab Bilibili Moe 2015 data

library(XML)
library(rvest)
library(Unicode)
library(stringr)
library(stringi)

load(file = 'MOE_Revival.Rda')

for (i in 12:12) {
            
      if (i < 10) {
            day = paste('0', i, sep = '')
      } else {
            day = as.character(i)
      }
      urlFile <- paste('http://moe.bilibili.com/api/s/getResult?date=2015-12-', 
                       day, sep = '')
      web <- readLines(urlFile)
      str(web)
      dataDay <- strsplit(web, '}')
      dataDay = unlist(dataDay)
      dataDay = dataDay[1:(length(dataDay)-2)]
      
      # 1. extract ID
      id = str_extract(dataDay[2:length(dataDay)], "\\bid.{3}\\d{1,5}")
      id = str_extract(id, "\\d.*\\d")
      
      # 2. extract number of votes
      vote = str_extract(dataDay[2:length(dataDay)], "\\bvotes_count.{3}\\d{1,8}")
      vote = str_extract(vote, "\\d.*\\d")
      
      # 3. pass or not
      pass = str_extract(dataDay[2:length(dataDay)], "\\bpass.{3}\\d{1}")
      pass = str_extract(pass, "\\d")
      
      # 4. extract name
      name = str_extract(dataDay[2:length(dataDay)], "\\bname.*sex")
      name = gsub('name\":\"', '', name)
      name = gsub('\",\"sex', '', name)
      name = stri_unescape_unicode(name)
      
      # 4. extract anime
      anime = str_extract(dataDay[2:length(dataDay)], "\\bbangumi.*\"")
      anime = gsub('bangumi\":\"','',anime)
      anime = gsub('\"','', anime)
      anime = stri_unescape_unicode(anime)
      
      # 5. extract gender
      gender = str_extract(dataDay[2:length(dataDay)], "\\bsex.{3}\\d{1}")
      gender = str_extract(gender, "\\d")
      
      # 6. extrat image URL
      image = str_extract(dataDay[2:length(dataDay)], "\\bsmall.*bangumi")
      image = gsub('\\\\', '', image)
      image = gsub('\",\"bangumi', '', image)
      image = gsub('small_image_url\":\"', '', image)
      
      # 7. create group number and date
      group = str_extract(dataDay[2], "\\bdata.{4}\\d{1,3}")
      group = str_extract(group, "\\d.*\\d")
      group1 = rep(group, length(which(gender == '1')))
      group2 = rep(as.character(as.numeric(group)+1), length(which(gender == '0')))
      group = c(group1, group2)
      date = paste("2015/12/", day, sep = "")
      date = rep(date, length(id))
      
      # create daily database
      dataToday <- data.frame(ID = id, Date = date, Group = group, Name = name, Anime = anime,
                              Vote = vote, Pass = pass, Gender = gender, Image = image)
#      MOE_PlayOff = data.frame(rbind(MOE_PlayOff, dataToday))
#      rm(dataToday)
      
}

save(MOE_Revival, file = 'MOE_Revival.Rda')
