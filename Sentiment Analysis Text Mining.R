getwd()

# Set the working Directory
setwd("D:/Nilesh")

# Read the files from stored location
readLines("textanalytics.txt")

# Example execution
a <- c("hello","world","My")
b <- paste(a,collapse = " ")

# Convert whole text file into continous chunk
chunk_2 <- readLines("textanalytics.txt")
chunk_pasted_2 <- paste(chunk_2 ,collapse = " ")
head(chunk_pasted_2)

# Convert read lines into lower Case
clean_data1 <- tolower(chunk_pasted_2)
head(clean_data1)

# Remove Punctuation Mark : "W"- for Remove Punctuation : "w" for remove text 
clean_data2 <- gsub(pattern = "\\W" , replace = " " , clean_data1)
head(clean_data2)

# Remove the Number from Text
clean_data3 <- gsub(pattern = "\\d" , replace = " " , clean_data2)
head(clean_data3)

# Visualize what we had Removed the Number from Text
clean_data_3 <- gsub(pattern = "\\D" , replace = " " , clean_data2)
head(clean_data_3)

# My new clean data from 3rd step
head(clean_data3)

#Remove the stopwords from the data
#install.packages("tm")
library(tm)
stopwords()
d <- stopwords()
clean_data4 <- removeWords(clean_data3,d)
head(clean_data4)

# Remove any single letter is in Text
clean_data5 <-gsub(pattern = "\\b[A-Z]\\b{1}", replace=" ", clean_data4)
head(clean_data5)


# Remove the White spaces
#stripwhitespace() function for Text file cleaning
clean_data6 <- stripWhitespace(clean_data5) 
head(clean_data6)  

############## Frequency of the words #############
# Lookking for counting th wors
# We Split individual words and add a space between them as a splitter

clean_data7 <- strsplit(clean_data6, " ")
head(clean_data7)

# Word Frequency
#install.packages("tables")
library(tables)
word_freq1 <- table(clean_data7)
head(word_freq1)


# with cbind function convert words into table format
word_freq2 <- cbind(names(word_freq1),as.integer(word_freq1))
head(word_freq2)  


# Save the table format words again into .csv file inside working Directory
write.csv(word_freq2, "Word Frequincies in file.csv")  

class(clean_data7)

unclass(clean_data7)

head(word_freq2)
View(word_freq2)
#install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library(wordcloud)
library(RColorBrewer)

wordcloud(word_freq2)
wordcloud(word_freq2, min.freq=0.5 , random.order = FALSE)

pal2 <- brewer.pal(5, "Dark2")
wordcloud(word_freq2, min.freq = 0.5, random.order = FALSE, color= pal2)


# Reference file for +ve & -Ve score comparison of text file
positive <- scan("positive.txt",
                 what = "charatcer",
                 comment.char = ";")



negative <- scan("negative.txt",
                 what = "charatcer",
                 comment.char = ";")

head(positive)
head(negative)

# compare your Data set Values with Downloaded Positive and Negative Score
senti_analysis <- unlist(clean_data7)
match(senti_analysis,positive)

senti_analysis <- unlist(clean_data7)
match(senti_analysis,negative)

# Sum of all +Ve Score values in text file
p_score <- sum(!is.na(match(senti_analysis,positive)))
p_score

# Sum of all -Ve Score values in text file
p_score <- sum(!is.na(match(senti_analysis,negative)))
p_score













