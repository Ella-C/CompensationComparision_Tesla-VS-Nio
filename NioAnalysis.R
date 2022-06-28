# 1. Libraries
#############################
library(tidyverse)
library(showtext)
showtext_auto()
library(jsonlite)
library(rvest)
library(Metrics)
library(randomForest)
library(xgboost)
#############################

# 2. Read HTML Index: NIO
#############################
s <- 'NIO'
d <- '~/Github/MySubmission/'
df <- data.frame()
for(ii in sequence(19)){
    url <- paste0(d,s,ii,'.html')
    page <- read_html(url,encoding = 'gbk|utf-8')  
    job <- page %>% html_elements('span.caseName') %>% html_text()
    a <- page %>% html_elements('li.clearfix a') %>% html_attr('href')
    jobhref <- a[seq(1,length(a),2)]
    y <- cbind(job,jobhref)
    df <- rbind(df,y)
}

unique(df) %>% dim
unique(df$job) %>% length
unique(df$jobhref)

write_csv(df,file=paste0(s,'.csv'))

#############################


# 3. Read HTML JD:NIO
#############################
DF <- data.frame()
df <- read_csv('NIO.csv')
d <- '~/Pycharm/Selenium/HTML/'
s <- 'NIO_'
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

DF <- distinct(DF)

table(DF$location)
table(DF$experience)

v <- DF[grep('年经验|发布',DF$location),] %>% select('address')
v1 <- v[[1]] %>% str_split(' ') %>% sapply('[[',2) %>% str_split('[:punct:]') %>%  sapply('[[',2)
DF$location[grep('年经验|发布',DF$location)] <- v1

write_csv(DF,file='NIOJD.csv')

#############################

# 4. Analyse NIO
#############################
DF <- read_csv('NIOJD.csv')
DF[is.na(DF)] <- 'no value'
DF$salary[DF$salary=="no value"] <- '0-0'
DF$benefits[DF$benefits==''|DF$benefits==' '] <- 'no value'

## benefits_NIO
DF$benefits %>% str_split(',') %>% unlist %>% str_trim() %>% table() %>% data.frame() %>% filter(`.`!='no value') %>% arrange(-Freq) %>% head(27)%>% ggplot(aes(fct_reorder(`.`,Freq),Freq))+geom_col()+coord_flip()+labs(title='Benefits of NIO in China蔚来中国区福利',x='Benefits福利')

table(DF$salary) %>% view()
DF$salary <- DF$salary %>% str_split('·') %>% sapply('[[',1)
grep('元/天',DF$salary,value=TRUE) %>% unique()
grep('元/小时',DF$salary,value=TRUE) %>% unique()
grep('以上',DF$salary,value=TRUE) %>% unique()
grep('以下',DF$salary,value=TRUE) %>% unique()
grep('千/天',DF$salary,value=TRUE) %>% unique()
DF$salary[DF$salary==''|DF$salary==" "] <- '0-0'
DF$salary[DF$salary=='100元/天'] <- '2.2-2.2千/月'
DF$salary[DF$salary=='150元/天'] <- '3.3-3.3千/月'
DF$salary[DF$salary=='200元/天'] <- '4.4-4.4千/月'
DF$salary[DF$salary=='3千及以下'] <- '3-3千/月'

DF$salaryMin <- DF$salary %>% str_split('-') %>% sapply('[[',1) %>% parse_number()
DF$salaryMax <- DF$salary %>% str_split('-') %>% sapply('[[',2) %>% parse_number()

i1 <- grep('[0-9]+-[0-9]+千$|[0-9]+.[0-9]+-[0-9]+.[0-9]+千$|[0-9]+-[0-9]+.[0-9]+千$|[0-9]+.[0-9]+-[0-9]+千$',DF$salary)
DF$salaryMin[i1] <- DF$salaryMin[i1]*1000
DF$salaryMax[i1] <- DF$salaryMax[i1]*1000

i2 <- grep('[0-9]+-[0-9]+万$|[0-9]+.[0-9]+-[0-9]+.[0-9]+万$|[0-9]+-[0-9]+.[0-9]+万$|[0-9]+.[0-9]+-[0-9]+万$',DF$salary)
DF$salaryMin[i2] <- DF$salaryMin[i2]*10000
DF$salaryMax[i2] <- DF$salaryMax[i2]*10000

i3 <- grep('.*千-.*万',DF$salary)
DF$salaryMin[i3] <- DF$salaryMin[i3]*1000
DF$salaryMax[i3] <- DF$salaryMax[i3]*10000

i4 <- grep('千/月',DF$salary)
DF$salaryMin[i4]<- round(DF$salaryMin[i4]*1000,0)
DF$salaryMax[i4] <- round(DF$salaryMax[i4]*1000,0)

i5 <- grep('万/年',DF$salary)
DF$salaryMin[i5] <- round(DF$salaryMin[i5]*10000/12,0)
DF$salaryMax[i5] <- round(DF$salaryMax[i5]*10000/12,0)

DF <- DF %>% mutate(salaryAvg=round((salaryMin+salaryMax)/2,0))

DF <- DF %>% distinct() 

DF$location <- DF$location %>% str_split('-') %>% sapply('[[',1)

DF %>% subset(experience !='no value'&education!='no value') %>% group_by(experience,education) %>%summarize(n=n(),salary=round(mean(salaryAvg),0)) %>% ggplot(aes(education))+geom_col(aes(y=n*100))+geom_line(aes(y=salary),group=1,color='red')+facet_grid(~experience)+labs(y='Average Salary/平均月薪元',title='NIO Salaries Overview in China蔚来中国区市场薪酬概览')+geom_text(aes(y=salary+200,label=salary),color='red',cex=3)

DF %>% subset(experience=='8-9年经验'&education=='大专',select=c('job','salary','salaryMin','salaryMax','salaryAvg'))

DF %>% select(c('job','education')) %>% view

#############################









