library(rvest)
library(tidyverse)
library(lubridate)

url <- "https://www.the-numbers.com/box-office-chart/daily/2022/03/20"
html <- read_html(url)
tables <- html %>% html_table(fill=TRUE)

length(tables)

A = tables[[2]]
names(A)[1 : 2] = c("Rank", "Rank.Last.Week")
str(A)

A = A %>% mutate(TotalGross = parse_number(TotalGross),
                 PerTheater = parse_number(PerTheater),
                 Theaters = parse_number(Theaters),
                 Gross = parse_number(Gross))

url0 = "https://www.the-numbers.com/box-office-chart/daily/"

url.date = "2022/03/20"

paste0(url0, url.date)


date0 = "2022/01/01"
data0.time = ymd(date0)
Result = c()
for (i in 1 : 10){
  data.temp.time = data0.time + i
  data.temp = as.character(data.temp.time)
  data.temp.final = gsub("-", "/", data.temp)
  link = paste0(url0, data.temp.final)
  #print(link)
  
  html = read_html(link)
  tables = html %>% html_table(fill=TRUE)
  A = tables[[2]]
  names(A)[1 : 2] = c("Rank", "Rank.Last.Week")
  A = A %>% mutate(TotalGross = parse_number(TotalGross),
                   PerTheater = parse_number(PerTheater),
                   Theaters = parse_number(Theaters),
                   Gross = parse_number(Gross),
                   date = data.temp.time)
  Result = rbind(Result, A)
}


Result1 = Result %>% filter(Theaters > 300)
sum(is.na(Result1$`Movie Title`))

Result2 = Result1 %>% group_by(`Movie Title`) %>% 
  summarise(Max_Gross = max(TotalGross))

Result1 %>% ggplot(aes(x = Date, 
                       y = log(TotalGross), 
                       group = `Movie Title`)) + 
  geom_line() + 
  geom_text(data = Result2, 
            aes(x = ymd("2022/01/20"), 
                y = log(Max_Gross), 
                label = `Movie Title`))

###### Web scrapping by CSS ###### 

url0 = "http://www.baseball-reference.com/players/a/"
html0 = read_html(url0)
players = html0 %>% html_nodes("#div_players_ a") 
players.name = players %>% html_text()
players.link = players %>% html_attr(name = "href")

h0 = "http://www.baseball-reference.com"
url.player = paste0(h0, players.link)

Result = c()
for (i in 1 : 10){
  url = url.player[i]
  html1 = read_html(url)
  
  variables = html1 %>% html_nodes(".poptip strong") %>% html_text()
  values = html1 %>% html_nodes(".stats_pullout p") %>% html_text()
  values = parse_number(values[-1])
  
  data = data.frame(variable = variables, value = values, player = players.name[i])
  Result = rbind(Result, data)
}

###### functions in R ######

mymean = function(x, na.rm = F) {
  if (na.rm == T) {x = na.omit(x)}
  res = sum(x) / length(x)
  return(res)
}

mymean(c(1 : 15, NA), na.rm = T)

boxoffice_scraper <- function(url) {
  library(rvest)
  library(tidyverse)
  html <- read_html(url)
  tables <- html %>% html_table(fill=TRUE)
  box <- tables[[2]]
  names(box)[1:2] <- c("Rank", "Rank.Last.Week")
  box <- box %>% mutate(
    TotalGross = parse_number(TotalGross),
    PerTheater = parse_number(PerTheater),
    Theaters = parse_number(Theaters),
    Gross = parse_number(Gross)
  )
  return(box)  
}

res = boxoffice_scraper("https://www.the-numbers.com/weekend-box-office-chart")

html2 = read_html("https://www.the-numbers.com/weekend-box-office-chart")
newurl = html2 %>% html_nodes(".previous a") %>% html_attr("href")
paste0("https://www.the-numbers.com/", newurl)

boxoffice_scraper(paste0("https://www.the-numbers.com/", newurl))

##### write the previous codes in a loop to extract data for each weekend #####

url = "https://www.the-numbers.com/weekend-box-office-chart"

res = c()
for (i in 1 : 10){
  html2 = read_html(url)
  data = boxoffice_scraper(url)
  res = rbind(res, data)
  newurl = html2 %>% html_nodes(".previous a") %>% html_attr("href")
  url = paste0("https://www.the-numbers.com/", newurl)
}

##### Baseball example #####

url = "http://www.baseball-reference.com/players/a/aardsda01.shtml"

bb_scraper <- function(url) {
  library(rvest)
  html <- read_html(url)
  
  names <- html %>% html_nodes("span strong") %>% html_text()
  values <- html %>% html_nodes(".stats_pullout p") %>% html_text() 
  player <- html %>% html_nodes("h1") %>% html_text()
  position <- html %>% html_nodes("h1+ p") %>% html_text()
  names <- trimws(names)
  player <- trimws(player)
  position <- trimws(position)
  data.frame(player=player, position=position, 
             statistics=names[-1],  values=parse_number(values[-1]))
}

bb_scraper("http://www.baseball-reference.com/players/a/aardsda01.shtml")

bb_scraper("https://www.baseball-reference.com/players/m/matheed01.shtml")

