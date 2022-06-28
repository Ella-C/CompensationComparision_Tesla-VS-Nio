# 1. Libraries
#############################
library(tidyverse)
library(showtext)
showtext_auto()
library(jsonlite)
library(rvest)
#############################


# 2. Read HTML INDEX: Tesla
#############################
s <- '特斯拉'
d <- '~/Github/MySubmission/'
df <- data.frame()
for(ii in sequence(12)){
    url <- paste0(d,s,ii,'.html')
page <- read_html(url,encoding = 'gbk')  
job <- page %>% html_elements('.zw-name') %>% html_text()
jobhref <- page %>% html_elements('p.t1 a.zw-name') %>% html_attr('href')
salary  <- page %>% html_elements('.t7') %>% html_text()
location <- page %>% html_elements('.t3') %>% html_text()
x <- page %>% html_elements('.t2') %>% html_text()
experience <- c()
education <- c()
for(i in sequence(length(x))){
    x1 <- x[i] %>% str_split(' ') %>% unlist()
    if(length(x1)>=3){
        experience <- c(experience,x1[1])
        education <- c(education,x1[3])
    }else{
        experience <- c(experience,x1[1])
        education <- c(education,'no value')
    }
}
refreshTime <- page %>% html_elements('.t5') %>% html_text()
y <- cbind(job,salary,location,experience,education,refreshTime,jobhref)
df <- rbind(df,y)
}

write_csv(df,file='tesla.csv')
#############################

# 3. Read HTML JD
#############################
DF <- data.frame()
df <- read_csv('tesla.csv')
d <- '~/Pycharm/Selenium/HTML/'
s <- 'tesla_'
for(i in sequence(dim(df)[1])){
    s1 <- df$jobhref[i] %>% str_extract('[0-9]+.html')
page <- read_html(paste0(d,s,s1),encoding = 'utf-8')
job <- page %>% html_element('h1') %>% html_text() %>% str_trim()
salary <- page %>% html_element('.cn strong') %>% html_text()
x <- page %>% html_element('.ltype') %>% html_text() %>% str_squish()
location <- x %>% str_extract('\\w+-\\w+')
experience <- x %>% str_extract('[0-9]+-[0-9]+年经验')
education <- x %>% str_extract('中专/中技|高中|大专|本科|博士|硕士')
benefits <- page %>% html_elements('.sp4') %>% html_text() %>% str_squish() %>% toString()
jd1 <- page %>% html_elements('.job_msg') %>% html_text() %>% str_squish() %>% toString()
jd2 <- page %>% html_elements('.job_msg p') %>% html_text() %>% str_squish() %>% toString()
jd3 <- page %>% html_elements('.job_msg div') %>% html_text() %>% str_squish() %>% toString()
jd <- paste(jd1,jd2,jd3,sep=' &&& ')

address <- page %>% html_element('.tBorderTop_box+ .tBorderTop_box') %>% html_text() %>% str_squish() 
com <- page %>% html_element('.com_name p') %>% html_text()
com_intro <- page %>% html_element('.tmsg') %>% html_text() %>% str_squish()

x1 <- page %>% html_elements('.com_tag .at') %>% html_text() %>% str_squish()
com_type <- x1[1]
com_size <- x1[2]
industry <- x1[3]

cb <- cbind(job,salary,location,experience,education,benefits,jd,address,com,com_type,com_size,industry,com_intro)
DF <- rbind(DF,cb)
}

write_csv(DF,file='teslaJD.csv')


#############################



