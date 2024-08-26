Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_GB.UTF-8")
getwd()
setwd("./../Desktop/Political Text/Final")

library(purrr)
library(tidyverse)
library(readtext)
library(quanteda)
library(stm)
library(jiebaR)
library(stopwords)
library(stringr)
library(readxl)
library(tidytext)
library(rvest)
library(dplyr)
library(magrittr)
library(knitr)
library(tmcn)
library(caret)
library(naivebayes)
library(e1071)
library(randomForest)
library(quanteda.textstats)


# Read data
data <- read_excel("./../Data_1/data.xlsx")
data1 <- read_excel("./../Data_1/data1.xlsx")
data2 <- read_excel("./../Data_1/data2.xlsx")
data3 <- read_excel("./../Data_1/data3.xlsx")
data4 <- read_excel("./../Data_2/data4.xlsx")
data5 <- read_excel("./../Data_2/data5.xlsx")
data6 <- read_excel("./../Data_2/data6.xlsx")
data7 <- read_excel("./../Data_2/data7.xlsx")


# Merge the data
file_names <- c("data.xlsx", "data1.xlsx", "data2.xlsx", "data3.xlsx", 
                "data4.xlsx", "data5.xlsx", "data6.xlsx", "data7.xlsx")

all_data <- map_dfr(file_names, read_excel)
text_column_index <- which(colnames(data) == "Content")
text <- as.character(data[, text_column_index])
all_data$Time <- as.numeric(all_data$Time)
all_data$Time <- as.Date(all_data$Time, origin = "1899-12-30")

# Check NA
sum(is.na(all_data$Time))
sum(is.na(all_data$CommentNum))
sum(is.na(all_data$Type))

all_data <- all_data %>%
  filter(!is.na(Time))

# head(all_data)
# tail(all_data)

##########################################################
#                     Question 1                         #
##########################################################

#----------------------------------------------------#
#-----------------Frequency Chart--------------------#
#----------------------------------------------------#

# Filter Data
pop_articles <- all_data %>%
  filter(Type == "Y")

np_articles <- all_data %>%
  filter(Type == "N")

pop_daily_counts <- pop_articles %>%
  mutate(Date = format(Time, "%Y-%m-%d")) %>%
  count(Date)

np_daily_counts <- np_articles %>%
  mutate(Date = format(Time, "%Y-%m-%d")) %>%
  count(Date)

combined_data <- bind_rows(
  pop_daily_counts %>% mutate(Type = "Y"),
  np_daily_counts %>% mutate(Type = "N")
)

# Plotting

ggplot(combined_data, aes(x = as.Date(Date), y = n, color = Type)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, aes(color = Type) ) + 
  labs(title = "Daily Article Counts by Type",
       x = "Date",
       y = "Number of Articles") +
  scale_y_continuous(limits = c(0, 32)) +
  theme_minimal()

#----------------------------------------------------#
#-------------------Ratio Chart----------------------#
#----------------------------------------------------#


ratio_counts <- combined_data %>%
  group_by(Date, Type) %>%
  summarise(n = sum(n)) %>%
  spread(Type, n, fill = 0)

ratio_counts <- ratio_counts %>%
  mutate(ratio = N / (N + Y))

# Plotting 

ggplot(ratio_counts, aes(x = as.Date(Date), y = ratio)) +
  geom_smooth(method = "loess", se = FALSE, color="coral1") +  # 添加趨勢線
  labs(title = "Daily Ratio of Type N Articles",
       x = "Date",
       y = "Ratio of Type N") +
  scale_y_continuous(limits = c(0, 1.2)) +
  theme_minimal()

#----------------------------------------------------#
#-------------------Growth Rate----------------------#
#----------------------------------------------------#

daily_counts <- combined_data %>%
  group_by(Date, Type) %>%
  summarise(daily_count = sum(n)) %>%
  spread(Type, daily_count, fill = 0)


daily_growth <- daily_counts %>%
  mutate(growth_rate_N = (N - lag(N)) / lag(N),
         growth_rate_Y = (Y - lag(Y)) / lag(Y))

daily_growth_long <- daily_growth %>%
  select(Date, growth_rate_N, growth_rate_Y) %>%
  gather(key = "Type", value = "GrowthRate", -Date) %>%
  filter(!is.na(GrowthRate))

ggplot(daily_growth_long, aes(x = as.Date(Date), y = GrowthRate, color = Type)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # 添加趨勢線
  labs(title = "Daily Growth Rate of Type N and Type Y Articles",
       x = "Date",
       y = "Growth Rate",
       color = "Type") +
  theme_minimal()


##########################################################
#                      Question 2                        #
##########################################################

stopwords <- readLines("stopwords.txt", encoding = "UTF-8")
userdict <- "userdict.txt"


cutter <- worker(bylines = TRUE, user = userdict)
all_data$Content <- sapply(all_data$Content, function(x) {
     segment(x, cutter)
   })

all_data$Content <- lapply(all_data$Content, function(x) {
       x <- x[!(x %in% stopwords)]
       x <- x[nchar(x) >= 2]
       paste(x, collapse = " ")
     })
all_data$Content <- unlist(all_data$Content)
all_data$Content <- as.character(all_data$Content)

head(all_data$Content)

# Creating DFM
corpus <- corpus(all_data, text_field = "Content")
head(corpus)

tokens <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE, 
                 remove_numbers = TRUE, split_hyphens = TRUE, 
                 remove_separators = TRUE)
dtm <- dfm(tokens)
head(dtm)
dfm_trim <- dfm_trim(dtm, min_docfreq = 50, max_docfreq = 1000)

dfm_trim[ ntoken(dfm_trim) == 0, ]
dfm_trim <- dfm_trim[ ntoken(dfm_trim) > 0, ]

head(dfm_trim)
head(docvars(dfm_trim))

#----------------------------------------------------#
#--------------------Model Test----------------------#
#----------------------------------------------------#

stm_dfm <- convert(dfm_trim, 
                        to = "stm", 
                        docvars( dfm_trim, c("Title", "Type", "Time", "CommentNum") )
)

head(stm_dfm)

# 6

set.seed(408)
article_stm_1 <- stm(stm_dfm$documents, 
                   stm_dfm$vocab, 
                    K = 6, 
                    prevalence =~ CommentNum + Type + s(Time),
                    data = stm_dfm$meta,
                    init.type = "Spectral")

labelTopics( article_stm_1, n = 10)

# 6

article_stm_2 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 6, 
                     prevalence =~ CommentNum + Type ,
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_2, n = 10)

# 8

article_stm_3 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 8, 
                     prevalence =~ CommentNum + Type ,
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_3, n = 10)


# 10

article_stm_5 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 10, 
                     prevalence =~ CommentNum + Type ,
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_5, n = 10)

# 9

article_stm_6 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 9, 
                     prevalence =~ CommentNum + Type ,
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_6, n = 10)


# 9 s(Time)


article_stm_7 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 9, 
                     prevalence =~ CommentNum + Type + s(Time),
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_7, n = 10)

# 13

article_stm_8 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 13, 
                     prevalence =~ CommentNum + Type + s(Time),
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_8, n = 10)

# 11

article_stm_9 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 11, 
                     prevalence =~ CommentNum + Type + s(Time),
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_9, n = 10)

# 12 ****(the best)****

article_stm_10 <- stm(stm_dfm$documents, 
                     stm_dfm$vocab, 
                     K = 12, 
                     prevalence =~ CommentNum + Type + s(Time),
                     data = stm_dfm$meta,
                     init.type = "Spectral")

labelTopics( article_stm_10, n = 10)

#----------------------------------------------------#
#-------------------Correlation Map------------------#
#----------------------------------------------------#

stm_corr <- topicCorr(article_stm_10)
plot.topicCorr(stm_corr, vlabels = c("1",
                                    "2",
                                    "3",
                                    "4",
                                    "5",
                                    "6",
                                    "7",
                                    "8",
                                    "9",
                                    "10",
                                    "11",
                                    "12"))


#----------------------------------------------------#
#--------------Time Effect Estimation----------------#
#----------------------------------------------------#

head(stm_dfm$meta)
tail(stm_dfm$meta)

stm_dfm$meta$Time_numeric <- as.numeric(stm_dfm$meta$Time)
monthEffects <- estimateEffect(1:12 ~ s(Time_numeric), 
                               article_stm_10,
                               meta = stm_dfm$meta,
                               uncertainty = "Global")


print(monthEffects)
plot(monthEffects, "Time_numeric", 
     method = "continuous", topics = 1:12,
     model = article_stm_10,
     printlegend = FALSE,
     xaxt = "n")  

title(xlab = "Time")

par(mar = c(5, 4, 4, 8) + 0.1)  
#axis(side=1, labels=FALSE, tick=FALSE)
topics <- c("1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9",
            "10",
            "11",
            "12")

colors <- rainbow(length(topics)) 
legend(x="topright", inset=c(-0.2, 0), legend=1:12, col=colors, lty=1, cex=0.8, xpd=TRUE)


##########################################################
#                     Question 3                         #
##########################################################

#----------------------------------------------------#
# Preparing for Model Training: Train-Test Splitting #
#----------------------------------------------------#
head(dfm_trim)

dfm_trim$Type <- as.factor(dfm_trim@docvars$Type)
dfm_trim$numeric_id <- 1:nrow(dfm_trim)

set.seed(4567)

# Since the data is too large, we subset only a part of original data
sample <- createDataPartition(dfm_trim$Type, 
                                p = 0.05, 
                                list = TRUE, 
                                times = 1)

dfm_trim_sampled <- dfm_subset(dfm_trim, dfm_trim$numeric_id %in% sample$Resample1 )

# Check ratio
dim(dfm_trim_sampled)
table(docvars(dfm_trim)$Type)
table(docvars(dfm_trim_sampled)$Type)

dfm_trim_sampled

# Rename numeric_id
dfm_trim_sampled$numeric_id <- seq_len(nrow(dfm_trim_sampled))
head(dfm_trim_sampled$numeric_id)


trainIndex <- createDataPartition(dfm_trim_sampled$Type, 
                                  p = 0.8, 
                                  list = TRUE, 
                                  times = 1)
trainIndex

travel_dfm.train <- dfm_subset(dfm_trim_sampled, dfm_trim_sampled$numeric_id %in% trainIndex$Resample1 )
travel_dfm.test <- dfm_subset(dfm_trim_sampled, !dfm_trim_sampled$numeric_id %in% trainIndex$Resample1 )

table(docvars(travel_dfm.train)$Type)
table(docvars(travel_dfm.test)$Type)

travel_dfm.train
travel_dfm.test

#----------------------------------------------------#
# Preparing for Model Training: Matrix Conversion    #
#----------------------------------------------------#

travel.train <- as.matrix(travel_dfm.train)
travel.test <- as.matrix(travel_dfm.test)

######################################################
#------------------classification--------------------#
######################################################

#----------------------------------------------------#
#-------------------Naive Bayes----------------------#
#----------------------------------------------------#


set.seed(123)
travel.nb <- multinomial_naive_bayes(x = travel.train,
                                    y = travel_dfm.train$Type)

summary(travel.nb)
travel.nb.predict <- predict(travel.nb, newdata = travel.test)

confusionMatrix(travel.nb.predict,
                travel_dfm.test$Type)


#----------------------------------------------------#
#------------------Random Forest---------------------#
#----------------------------------------------------#

travel.rf <- randomForest(x = travel.train,
                         y = travel_dfm.train$Type)

travel.rf.predict <- predict(travel.rf, travel.test)


confusionMatrix(travel.rf.predict,
                travel_dfm.test$Type)


#----------------------------------------------------#
#----------------lexical diversity-------------------#
#----------------------------------------------------#


lexdiv <- textstat_lexdiv(dfm_trim)
high_ttr <- textstat_lexdiv(dfm_trim)[textstat_lexdiv(dfm_trim)$TTR > 0.5, ]
num_high_ttr <- nrow(high_ttr)

num_high_ttr/nrow(lexdiv)

