#https://github.com/ropensci/RSelenium
#install.packages("RSelenium")
library("RSelenium")
library(dplyr)
library(stringr)
library(rebus)


########################
#   basic operations   #
########################
webdriver <- rsDriver(port = 4444L, browser="firefox") #open server
webdriver #info
browser<-webdriver$client    
# browser$open()
# browser$close() 
# webdriver$server$stop()   
########################



########################
##### list of links ####
########################
influ <- c(
  'https://www.instagram.com/jemerced/',
  'https://www.instagram.com/juliawieniawa/',
  'https://www.instagram.com/maffashion_official/',
  'https://www.instagram.com/karolina_pisarek/',
  'https://www.instagram.com/malgorzatakozuchowska_/',
  'https://www.instagram.com/macademiangirl/'
)
########################





########################
###      START       ###
########################



postD <- NULL
for (f in 1:length(influ)){
  
  #wejscie na odpowiedni¹ stronê  
  browser$navigate(influ[f])
  
  #####################################################
  ####  extraction of links - START                  ##
  #####################################################
  
  links <- NULL
  for (k in 1:10) {
    pageI <- browser$findElement("css", "body")
    pageI$sendKeysToElement(list(key = "end"))
    Sys.sleep(1)
    
    for (l in 1:24) {
      x = ((l - 1)%%3) + 1
      y = (l - x)/3 + 1
      
      webElem <- browser$findElement(using = "css", paste0("main > div > div:last-child > article > div > div 
                                                           > div:nth-child(",y,") > div:nth-child(",x,") > a "))
      link <- webElem$getElementAttribute('href')[[1]]
      
      if (is.null(links)) {
        links <- cbind(k, y, x, link)
      } else {
        wiersz <- cbind(k, y, x, link)
        links <- rbind(links, wiersz)
      }
    }
    #print(Sys.time())
  }
  
  
  linksF <- data.frame(links, stringsAsFactors = FALSE)
  linksD <- linksF %>% 
    group_by(link) %>% 
    distinct(link)
  #####################################################
  ####  extraction of links - END                    ##
  #####################################################
  
  
  
  #####################################################
  ####  browsing links - START                       ##
  #####################################################
  
  postI <-  NULL
  for (z in 1:nrow(linksD)) {
    
    #name of the influencer
    person <- substr(influ[f],27,nchar(influ[f])-1)
    print(paste0('photo ', z, ' ', person))
    
    # coordinates of photo
    # x = ((z - 1)%%3) + 1
    # y = (z - x)/3 + 1
    
    photoI <- as.character(linksD[z,1])
    browser$navigate(photoI)
    Sys.sleep(1)
    
    
    # influence's posts - start
    postS <- browser$findElements(using = "css", paste0("article > div:nth-child(3) > div 
                                                        > ul > div > li:nth-child(1) > div > div > div:nth-child(2) >  span"))
    if (length(postS) > 0) {
      message <- unlist(postS[[length(postS)]]$getElementText())
      print(message)
      date <- browser$findElement(using = "css", paste0(" article > div:nth-child(3) > div:nth-child(4) > a > time"))
      when <- date$getElementAttribute('datetime') [[1]]
      
      if (is.null(postI)) {
        postI <- data.frame(cbind(z, when, message, photoI))
        
      } else {
        lol <- data.frame(cbind(z, when, message, photoI))
        postI <- rbind(postI, lol)
      }
    }
    # influencer's posts - end 
  }
  
  postI$z <- as.character(postI$z)
  postI$influ <- person
  

  if (is.null(postD)){
    postD <- postI
  } else {
    postD <- rbind(postD, postI)
  }
  #####################################################
  ####  browsing links - END                         ##
  #####################################################   
  


  

  
  
  
  #################
  # extracting  @ #
  #################
  
  pat <-  "@" %R% one_or_more(WRD)
  
  dataSign <- str_extract_all(postI$message, pattern = pat)
  
  # head(dataSign)
  # class(dataSign)
  
  #remove obs with zero values
  dataSign2 <- dataSign[lapply(dataSign,length)>0]
  
  #unlist data
  for (i in 1:length(dataSign2)) {
    for (k in 1:length(dataSign2[[i]])) {
      ohlala <- cbind(i, k, dataSign2[[i]][k])
      if (i == 1 & k == 1) {
        dataSign3 <- ohlala
      } else {
        dataSign3 <- rbind(dataSign3, ohlala)
      }
    }
  }
  
  #create dataframe
  dataSign4 <- data.frame(dataSign3, stringsAsFactors = FALSE)
  
  
  if (f == 1 ) {
    dataA <- dataSign4 %>% 
      mutate(inf = person)
  } else {
    dataSign4$inf <- person
    dataA <- rbind(dataA, dataSign4)
  }
  
  
  #################
  # extracting  # #
  #################
  
  pat <-  "#" %R% one_or_more(WRD)
  
  dataSign <- str_extract_all(postI$message, pattern = pat)
  
  #remove obs with zero values
  dataSign2 <- dataSign[lapply(dataSign,length)>0]
  
  #unlist data
  for (i in 1:length(dataSign2)) {
    for (k in 1:length(dataSign2[[i]])) {
      ohlala <- cbind(i, k, dataSign2[[i]][k])
      if (i == 1 & k == 1) {
        dataSign3 <- ohlala
      } else {
        dataSign3 <- rbind(dataSign3, ohlala)
      }
    }
  }
  
  #create dataframe
  dataSign4 <- data.frame(dataSign3, stringsAsFactors = FALSE)
  
  
  if (f == 1 ) {
    dataB <- dataSign4 %>% 
      mutate(inf = person)
  } else {
    dataSign4$inf <- person
    dataB <- rbind(dataB, dataSign4)
  }
  
  

}


########################
###      END         ###
########################


# postD - list of posts
# dataA - list of references
# dataB - list of hashtags
  