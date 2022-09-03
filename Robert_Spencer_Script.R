#setting working directory
setwd("C:/Users/tiger/Dropbox/Command & Control/Trinity/Independent Research/Islamophobia/Code")

#loading necessary packages
library("seededlda")
library("stringr")
library("ggplot2")
library("quanteda")
library("quanteda.textmodels")
library("quanteda.textstats")
library("readtext")
library("spacyr")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("wordcloud")
library("stopwords")
library("quanteda.textplots")
library("quanteda.textplots")
library("dplyr")
library("rvest")
library("stringr")
library("quanteda.textplot")
library("quanteda.corpora")
library("tm")

#########################FaceBook######################
JihadWatch<-read.csv("Dataset_1_FaceBook.csv")

#specify number of posts being analyzed below. 
post_number<-113

#blank object that will have all the URLs, reaction counts, and dates
#to look through. 
#the facebook page is just these URLs
URL_List<-NA
Reaction_List<-NA
Date_List<-NA

#for loop that concatenates a string with the column name
#then, we use column name to get the URL and store it in the list
#113 is the number of posts we're looking at
for (j in 0:post_number){
  string1<-"posts."
  string2<-toString(j)
  string3<-".postLinks.0"
  
  #for URLs
  URL<-paste(string1,string2,string3,sep="")
  URL_List[j]<-JihadWatch[,c(URL)]
  
  #for reactions
  string4<-".postStats.reactions"
  Reaction<-paste(string1,string2,string4,sep="")
  Reaction_List[j]<-JihadWatch[,c(Reaction)]
  
  #for dates
  string5<-".postDate"
  Date<-paste(string1,string2,string5,sep="")
  Date_List[j]<-JihadWatch[,c(Date)]
}

#Date List looks like "2022-07-07T12:26:04.000Z"
#we need to clean it so it looks like: "2022-07-07"
Date_List<-gsub("T.*","",Date_List)

#now, we want to go through each of the URLs in URL_List
#and create a dataframe with: 1) URL, 2) title, 3) article text, 4) date, 5) number of reactions

#reads in URL, selects 9, the part of the text which has the body
#isolates title by deleting before the first colon
#and after the first \n

#to isolate text, delete before the first colon... twice!
#and delete after "\nFollow" 


#going to store titles in "Title_List" and bodies in "Body_List"
#snippets will store text snippets of the HTML
#this loop is separate. Running the URL's requires pausing the loop
#every 25 entries (15 to be safe) so website doesn't block me
#so, I want to separate out getting the HTML from extracting title/body
#from the HTML 
#tried moving system rest to 30 seconds, but then the website blocks me

Title_List<-NA
Body_List<-NA
snippets<-NA

#need a progress bar to check when code is close to done
pb <- txtProgressBar(min = 0,      
                     max = post_number, 
                     style = 3,    
                     width = 50,   
                     char = "=")   

for (y in 1:post_number){
  text<-read_html(URL_List[y])%>%html_nodes("body")%>%html_children()%>%html_text() 
  snippets[y]<-toString(text[9])
  
  #if statement to make sure website doesn't block the scrape. 
  #turns loop off every 15 cycles, waits 60 seconds, resumes. 
  if(y%%10==0){
    Sys.sleep(60)
  } 
  setTxtProgressBar(pb, y)
}
#I wrote a function below which converts the URl into a 
#close approximation of the title
#usually title is the words in between the dashes at the end of the URL
URL_to_Title <- function(URL) {
  Title <-sub(".*/","",URL)
  return(gsub("-"," ",Title))
}


for (z in 1:post_number){
  working_snippet<-gsub("^[^:]*:", "", snippets[z])
  print(working_snippet)
  
  #need this if statement to capture titles where the colon method doesn't work
  #i.e. if there's a time thing, it starts deleting from there
  if (substring(working_snippet,1,2)=="00"|substring(working_snippet,4,5)=="am"|substring(working_snippet,4,5)=="pm"){
    working_snippet<-gsub("^[^\n]*\n", "", working_snippet)
  }
  
  Title_List[z]<-gsub("\n.*","",working_snippet)
  
  working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
  working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet_2)
  Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
  #Filed Under is added since \nFollow missed a few articles
  Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  
  #sometimes, title is in a really weird place in the HTML
  #if this happens, code ends up picking up first paragraph of article
  #as title. If the title is above 20 words, 
  #this if statement appends the wrong title to the body of the article
  #to get the title, we have to guess (it's located unpredictably in 
  #HTML). 
  if(str_count(Title_List[z], "\\w+")>20){
    Body_List[z]<-paste(Title_List[z], Body_List[z],sep="")
    Title_List[z]<-URL_to_Title(URL_List[z])
  }
  
  
  #sometimes there's a PJ mdia advertisement which throws off the code
  #to get around it, title has to be via URL. 
  #body has one less cut with \n
  #has to be at the end of the loop, otherwise it throws off other elements
  if (substring(working_snippet,1,9)=="New in PJ"){
    Title_List[z]<-URL_to_Title(URL_List[z])
    
    working_snippet<-gsub("^[^\n]*\n", "", working_snippet)
    working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
    Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
    #Filed Under is added since \nFollow missed a few articles
    Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  }
  
  #sometimes, there's an extra \n which causes the code to
  #break completely and return nothing for the title. 
  #here, use URL to title function and one less \n slice, and it works
  if (Title_List[z]==""){
    Title_List[z]<-URL_to_Title(URL_List[z])
    working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
    Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
    #Filed Under is added since \nFollow missed a few articles
    Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  }
}

#we don't want to do much pre-processing of corpus, but we should remove \n and \t's
#pre-processing is bad because we want punctuation,  
#\n isn't punctuation
#we also need to clean where there isn't a space between words. 
#For example, lots of committee.Several - which gets read as one word
#the gsub does the spacing between periods 

Body_List<-str_replace_all(Body_List, "\n", replacement = "")
Title_List<-str_replace_all(Title_List, "\n", replacement = "")
Body_List<-gsub("\\.(?=[A-Za-z])", ". ", Body_List, perl = TRUE)
Title_List<-gsub("\\.(?=[A-Za-z])", ". ", Title_List, perl = TRUE)

###going to make a dataframe from twitter posts from Spencer
###need to have these four columns: text, date, likes, URL

#combining two data frames to get all 1600 tweets 
Robert_Spencer<-read.csv("dataset_twitter-scraper_2022-08-18_05-28-52-454.csv")
Robert_Spencer_2<-read.csv("Photo_Scraper.csv")
Robert_Spencer<-rbind(Robert_Spencer, Robert_Spencer_2)

#############making all the twitter columns
tweet_content<-Robert_Spencer$full_text
#getting rid of speical characters in tweet_content and axing the "\n"
tweet_content<-iconv(tweet_content, from = 'UTF-8', to = 'ASCII//TRANSLIT')
tweet_content<-str_replace_all(tweet_content,"\n"," ")
twitter_likes<-Robert_Spencer$favorite_count
twitter_retweets<-Robert_Spencer$retweet_count
twitter_comments<-Robert_Spencer$reply_count
twitter_date<-Robert_Spencer$created_at
#modify date to exclude everything after the T, which is the specific time
for (b in 1:length(twitter_date)){
  twitter_date[b]<-substring(twitter_date[b],1,10)
}
twitter_URL<-Robert_Spencer$url

#changing variable names for facebook
facebook_likes<-Reaction_List
facebook_date<-Date_List
facebook_url<-URL_List
facebook_content<-paste(Title_List,Body_List,sep=" ")

#merging facebook and twitter data into singular columns
likes<-c(twitter_likes, facebook_likes)
date<-c(twitter_date,facebook_date)
URL<-c(twitter_URL, facebook_url)
Content<-c(tweet_content,facebook_content)


#creating corpus, appending likes, date, URL, and tweet content and putting it into a CSV to view
Corpus_Spencer<-corpus(Content)
docvars(Corpus_Spencer,"Likes") <-likes
docvars(Corpus_Spencer, "Date") <- date
docvars(Corpus_Spencer, "URL") <- URL
docvars(Corpus_Spencer,"Content") <-Content
write.csv(summary(Corpus_Spencer),"Post Data from Spencer Twitter and Facebook.csv")

#tokenize everything and make into a DFM for analysis
#clean dataset: remove numbers and symbols in tokens stage (only possible then), remove punctuation, stop words, get to lower case
#did word stemming last - made it easier to check DFM was getting the results I want while testing the code
tokens_Corpus_Spencer<-tokens(Corpus_Spencer, remove_numbers = TRUE, remove_symbol = TRUE)
DFM_Corpus_Spencer <- dfm(tokens_Corpus_Spencer,remove_punct = TRUE)
DFM_Corpus_Spencer<-dfm_remove(DFM_Corpus_Spencer, pattern = stopwords("english"))
DFM_Corpus_Spencer_Stemmed<-dfm_wordstem(DFM_Corpus_Spencer, language = quanteda_options("language_stemmer"))

#need to separate the data first by Muslim vs non-Muslim posts. 
#use a Muslim dictionary
#Muslim_Dictionary<-dictionary(list(Muslim=c("verse", "muslim","islam","allah","israel","palestine","afghanistan","pakistan","isis")))

Muslim_Dictionary<-dictionary(list(Muslim=c("muslim","islam","allah","isis"),
                                   Filter=c("RT","typo","autocorrect","tweet","Catholic","fentanyl")))

#fit seededlda model 
library(seededlda)
slda <- textmodel_seededlda(DFM_Corpus_Spencer_Stemmed, Muslim_Dictionary, residual = TRUE)
topwords_Spencer<-as.data.frame(seededlda::terms(slda, 20))
head(topics(slda), 20)


#store topics thus identified into the tweets data frame
Corpus_Spencer$topic <- seededlda::topics(slda)
View(Corpus_Spencer)

#tabulate frequencies of each topic in your corpus:
topics_table<-ftable(Corpus_Spencer$topic)
View(topics_table)
topicsprop_table<-as.data.frame(prop.table(topics_table))
View(topicsprop_table)

#need to construct the dictionary for four tropes. first, just get Muslim results and tokenize
#also viewing results in a csv
Corpus_Spencer_Muslim<-corpus_subset(Corpus_Spencer, topic == "Muslim")
write.csv(summary(Corpus_Spencer_Muslim),"Corpus_Spencer_Muslim.csv")
tokens_Corpus_Spencer_Muslim<-tokens(Corpus_Spencer_Muslim, remove_numbers = TRUE, remove_symbol = TRUE)
DFM_Corpus_Spencer_Muslim <- dfm(tokens_Corpus_Spencer_Muslim,remove_punct = TRUE)
DFM_Corpus_Spencer_Muslim<-dfm_remove(DFM_Corpus_Spencer_Muslim, pattern = stopwords("english"))
DFM_Corpus_Spencer_Muslim_Stemmed<-dfm_wordstem(DFM_Corpus_Spencer_Muslim, language = quanteda_options("language_stemmer"))

#do a key word search for Muslim, 10 words around it, then, sort tweets into buckets and write associated words in
#respective dictionaries
#this can be made more precise with more tweets - there's probably even an algorithmic way to make it more precise
key_word_Muslim<-kwic(Corpus_Spencer_Muslim, pattern = "Muslim", window = 10, valuetype = "fixed")
write.csv(key_word_Muslim,"key_word_Muslim.csv")


#I only need to define a dictionary for the four categories; everything else is other - should go through other
#and make progressively more and more precise each time
Islamophobic_Dictionary <- dictionary(list(PreModern = c("strict", "reform","akbar","allahu","children"),
                        Antidemocratic = c("bigotry","bigot", "salman", "Salman", "Sharia","moderate","freedom","speech","rushdie","Rushdie","koran","Koran","ban","banned","islamophobia"),
                        Misogyny = c("rapist", "raped", "women","woman", "hijab","rape", "girl","marriage","husband","trafficking","misogynistic","misogyny"),
                        Expansionist = c("migrants", "migrant", "refugee","refugees", "border", "invasion", "terrorist","jihad","kill","stab","Israel","turkish","Turkish","israel","explosive","bomb","jihadis","jihadi","Afghanistan","ISIS","isis","islamic state","Islamic State","taliban","Taliban"),
                        filter=c("i","tell","ask","how","porn")))

#fit seededlda model 
library(seededlda)
slda <- textmodel_seededlda(DFM_Corpus_Spencer_Muslim_Stemmed, Islamophobic_Dictionary, residual = TRUE)
topwords_Spencer<-as.data.frame(seededlda::terms(slda, 20))
head(topics(slda), 20)

#store topics thus identified into the tweets data frame
Corpus_Spencer_Muslim$trope <- seededlda::topics(slda)


#Now, we need to get a graph of 1) percentage of each trope and 2) percentage likes per topic. 
#subdividing corpus into the four tropes
PreModern_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "PreModern")
Antidemocratic_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Antidemocratic")
Misogyny_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Misogyny")
Expansionist_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Expansionist")

#writing each into a CSV for analysis
write.csv(summary(PreModern_Corpus),"PreModern_Corpus.csv")
write.csv(summary(Antidemocratic_Corpus),"Antidemocratic_Corpus.csv")
write.csv(summary(Misogyny_Corpus),"Misogyny_Corpus.csv")
write.csv(summary(Expansionist_Corpus),"Expansionist_Corpus.csv")

#getting the post amount for each trope and summing them to find total posts analyzed 
PreModern_Post_Amount<-length(PreModern_Corpus)
Antidemocratic_Post_Amount<-length(Antidemocratic_Corpus)
Misogyny_Post_Amount<-length(Misogyny_Corpus)
Expansionist_Post_Amount<-length(Expansionist_Corpus)
Total_Post_Amount<-PreModern_Post_Amount+Antidemocratic_Post_Amount+Misogyny_Post_Amount+Expansionist_Post_Amount

#finding proportions for how often each post shows up
PreModern_Proportion<-PreModern_Post_Amount/Total_Post_Amount
Antidemocratic_Proportion<-Antidemocratic_Post_Amount/Total_Post_Amount
Misogyny_Proportion<-Misogyny_Post_Amount/Total_Post_Amount
Expansionist_Proportion<-Expansionist_Post_Amount/Total_Post_Amount

#finding how often each type of post is liked
PreModern_Likes<-sum(as.data.frame(PreModern_Corpus$Likes))
Antidemocratic_Likes<-sum(as.data.frame(Antidemocratic_Corpus$Likes))
Misogyny_Likes<-sum(as.data.frame(Misogyny_Corpus$Likes))
Expansionist_Likes<-sum(as.data.frame(Expansionist_Corpus$Likes))
Total_Likes<-PreModern_Likes+Antidemocratic_Likes+Misogyny_Likes+Expansionist_Likes

#finding proportions for likes of each post 
PreModern_Proportion_Likes<-PreModern_Likes/Total_Likes
Antidemocratic_Proportion_Likes<-Antidemocratic_Likes/Total_Likes
Misogyny_Proportion_Likes<-Misogyny_Likes/Total_Likes
Expansionist_Proportion_Likes<-Expansionist_Likes/Total_Likes

#plot topic wise proportions. First, get proportions into a vector and names into a vector.
#Second, roll names and topic proportions into data frame. Third, GGplot it
Percentage<-c(PreModern_Proportion,Antidemocratic_Proportion,Misogyny_Proportion,Expansionist_Proportion)
Trope<-c("Premodern","Antidemocratic","Misogynistic","Expansionist")
Trope_Table<-data.frame(Trope,Percentage)
write.csv(Trope_Table,"Trope_Table_Islamophobia.csv")

ggplot(data=Trope_Table, aes(x=Trope, y=Percentage,fill=Trope))+
  geom_bar(stat="identity")+ggtitle("Distribution of Posts by Islamophobic Trope")

#plot like proportions. First, get likes into data frame. Second, GGplot it
Like_Percentages<-c(PreModern_Proportion_Likes,Antidemocratic_Proportion_Likes,Misogyny_Proportion_Likes,Expansionist_Proportion_Likes)


Like_Table<-data.frame(Trope,Like_Percentages)
write.csv(Like_Table,"Like_Table_Islamophobia.csv")
ggplot(data=Like_Table, aes(x=Trope, y=Like_Percentages,fill=Trope))+
  geom_bar(stat="identity")+
  ggtitle("Distribution of Likes by Islamophobic Trope")+
  ylab("Like Percentage")



##################################################################################################

#Tahrir ut-Hizb portion
Hizb_al_Tahrir<-read.csv("Hizb_al_Tahrir.csv")


#############making all the twitter columns
tweet_content_Hizb<-Hizb_al_Tahrir$full_text
#getting rid of speical characters in tweet_content and axing the "\n"
tweet_content_Hizb<-iconv(tweet_content_Hizb, from = 'UTF-8', to = 'ASCII//TRANSLIT')
tweet_content_Hizb<-str_replace_all(tweet_content_Hizb,"\n"," ")
twitter_likes_Hizb<-Hizb_al_Tahrir$favorite_count
twitter_retweets_Hizb<-Hizb_al_Tahrir$retweet_count
twitter_comments_Hizb<-Hizb_al_Tahrir$reply_count
twitter_date_Hizb<-Hizb_al_Tahrir$created_at
#modify date to exclude everything after the T, which is the specific time
for (b in 1:length(twitter_date_Hizb)){
  twitter_date_Hizb[b]<-substring(twitter_date_Hizb[b],1,10)
}
twitter_URL_Hizb<-Hizb_al_Tahrir$url

#creating corpus, appending likes, date and URL, and putting it into a CSV to view
Corpus_Hizb_al_Tahrir_Europe<-corpus(tweet_content_Hizb)
docvars(Corpus_Hizb_al_Tahrir_Europe,"Likes") <-twitter_likes_Hizb
docvars(Corpus_Hizb_al_Tahrir_Europe, "Date") <- twitter_date_Hizb
docvars(Corpus_Hizb_al_Tahrir_Europe, "URL") <- twitter_URL_Hizb
docvars(Corpus_Hizb_al_Tahrir_Europe,"Content") <-tweet_content_Hizb
write.csv(summary(Corpus_Hizb_al_Tahrir_Europe),"Post Data from Hizb al-Tahrir Twitter.csv")

#tokenize everything and make into a DFM for analysis
#clean dataset: remove numbers and symbols in tokens stage (only possible then), remove punctuation, stop words, get to lower case
#did word stemming last - made it easier to check DFM was getting the results I want while testing the code
tokens_Corpus_Hizb_al_Tahrir_Europe<-tokens(Corpus_Hizb_al_Tahrir_Europe, remove_numbers = TRUE, remove_symbol = TRUE)
DFM_Corpus_Hizb_al_Tahrir_Europe <- dfm(tokens_Corpus_Hizb_al_Tahrir_Europe,remove_punct = TRUE)
DFM_Corpus_Hizb_al_Tahrir_Europe<-dfm_remove(DFM_Corpus_Hizb_al_Tahrir_Europe, pattern = stopwords("english"))
DFM_Corpus_Hizb_al_Tahrir_Europe_Stemmed<-dfm_wordstem(DFM_Corpus_Hizb_al_Tahrir_Europe, language = quanteda_options("language_stemmer"))

#do a key word search for Muslim, 10 words around it, then, sort tweets into buckets and write associated words in
#respective dictionaries
#this can be made more precise with more tweets - there's probably even an algorithmic way to make it more precise
key_word_Muslim_Hizb<-kwic(Corpus_Hizb_al_Tahrir, pattern = "Muslim", window = 10, valuetype = "fixed")
write.csv(key_word_Muslim_Hizb,"key_word_Muslim_Hizb.csv")

#I only need to define a dictionary for the four categories; everything else is other - should go through other
#and make progressively more and more precise each time
Europhobic_Dictionary <- dictionary(list(Materialistic = c("enlightened","enlighten", "intellectual","good","evil", "capitalist system", "inflation", "prices","price", "kufr", "capitalist","capitalism","values","secular","culture","sacred","rights","family"),
                                           Hypocritical = c("discussion", "salman","Salman", "Rushdie affair","rushdie affair", "insults","insult", "crimes","crime", "beat","beaten","lynch","lynched","burn","burned", "mosque", "debate", "westernise", "Satanic Verses","satanic verses", "prosecute","prosecuted", "rushdie","Rushdie", "extrajudicial", "refugees","refugee", "blasphemy", "deport", "OurProphetOurHonour", "da'wah", "rights","islamophobia","oppressed","oppression","hate","speech","protest"),
                                           Imperialist = c("butt", "Zionist", "Iraq","iraq","USA","usa", "UN", "afghanistan","Afghanistan","invade","invaded", "ukraine","Ukraine","security","armed","forces", "NUSSRAH","nussrah","NUSSRAH_PAK1", "biden","Biden", "zionist","regime","colonialist","colonial","kill","liberate","army","conquer","superpowers","overthrow","overthrown","occupy","occupied","masters"),
                                           Religion = c("conference", "fasting","fast","eid","Eid","Mumbarak","mumbarak")))

#fit seededlda model 
slda_Hizb <- textmodel_seededlda(DFM_Corpus_Hizb_al_Tahrir_Europe_Stemmed, Europhobic_Dictionary, residual = TRUE)
topwords_Hizb<-as.data.frame(seededlda::terms(slda_Hizb, 20))
head(topics(slda_Hizb), 20)

#store topics thus identified into the tweets data frame
Corpus_Hizb_al_Tahrir_Europe$trope <- seededlda::topics(slda_Hizb)


#Now, we need to get a graph of 1) percentage of each trope and 2) percentage likes per topic. 
#subdividing corpus into the four tropes
Materialistic_Corpus<-corpus_subset(Corpus_Hizb_al_Tahrir_Europe, trope == "Materialistic")
Hypocritical_Corpus<-corpus_subset(Corpus_Hizb_al_Tahrir_Europe, trope == "Hypocritical")
Misogyny_Corpus_Hizb<-corpus_subset(Corpus_Hizb_al_Tahrir_Europe, trope == "Misogyny")
Imperialist_Corpus<-corpus_subset(Corpus_Hizb_al_Tahrir_Europe, trope == "Imperialist")

#writing each into a CSV for analysis
write.csv(summary(Materialistic_Corpus),"Materialistic_Corpus.csv")
write.csv(summary(Hypocritical_Corpus),"Hypocritical_Corpus.csv")
write.csv(summary(Misogyny_Corpus_Hizb),"Misogyny_Corpus_Hizb.csv")
write.csv(summary(Imperialist_Corpus),"Imperialist_Corpus.csv")

#getting the post amount for each trope and summing them to find total posts analyzed 
Materialistic_Post_Amount<-length(Materialistic_Corpus)
Hypocritical_Post_Amount<-length(Hypocritical_Corpus)
Misogyny_Hizb_Post_Amount<-length(Misogyny_Corpus_Hizb)
Imperialist_Post_Amount<-length(Imperialist_Corpus)
Total_Post_Amount_Hizb<-Materialistic_Post_Amount+Hypocritical_Post_Amount+Misogyny_Hizb_Post_Amount+Imperialist_Post_Amount

#finding proportions for how often each post shows up
Materialistic_Proportion<-Materialistic_Post_Amount/Total_Post_Amount_Hizb
Hypocritical_Proportion<-Hypocritical_Post_Amount/Total_Post_Amount_Hizb
Misogyny_Proportion_Hizb<-Misogyny_Hizb_Post_Amount/Total_Post_Amount_Hizb
Imperialist_Proportion<-Imperialist_Post_Amount/Total_Post_Amount_Hizb

#finding how often each type of post is liked
Materialistic_Likes<-sum(as.data.frame(Materialistic_Corpus$Likes))
Hypocritical_Likes<-sum(as.data.frame(Hypocritical_Corpus$Likes))
Misogyny_Likes_Hizb<-sum(as.data.frame(Misogyny_Corpus_Hizb$Likes))
Imperialist_Likes<-sum(as.data.frame(Imperialist_Corpus$Likes))
Total_Likes<-Materialistic_Likes+Hypocritical_Likes+Misogyny_Likes_Hizb+Imperialist_Likes

#finding proportions for likes of each post 
Materialistic_Proportion_Likes<-Materialistic_Likes/Total_Likes
Hypocritical_Proportion_Likes<-Hypocritical_Likes/Total_Likes
Misogyny_Proportion_Likes_Hizb<-Misogyny_Likes_Hizb/Total_Likes
Imperialist_Proportion_Likes<-Imperialist_Likes/Total_Likes

#plot topic wise proportions. First, get proportions into a vector and names into a vector.
#Second, roll names and topic proportions into data frame. Third, GGplot it
Percentage_hizb<-c(Materialistic_Proportion,Hypocritical_Proportion,Misogyny_Proportion_Hizb,Imperialist_Proportion)
Trope_hizb<-c("Materialistic","Hypocritical","Misogynistic","Expansionist")
Trope_Table_hizb<-data.frame(Trope_hizb,Percentage_hizb)
write.csv(Trope_Table_hizb,"Trope_Table_hizb.csv")
ggplot(data=Trope_Table_hizb, aes(x=Trope_hizb, y=Percentage_hizb,fill=Trope_hizb))+
  geom_bar(stat="identity")+xlab("Trope")+ylab("Like Percentage")+
  scale_fill_discrete(name = "Trope")+ggtitle("Distribution of Posts by Europhobic Trope")

#plot like proportions. First, get likes into data frame. Second, GGplot it
Like_Percentages_hizb<-c(Materialistic_Proportion_Likes,Hypocritical_Proportion_Likes,Misogyny_Proportion_Likes_Hizb,Imperialist_Proportion_Likes)


Like_Table<-data.frame(Trope_hizb,Like_Percentages_hizb)
write.csv(Like_Table,"Like_Table_Europhobia.csv")
ggplot(data=Like_Table, aes(x=Trope_hizb, y=Like_Percentages_hizb,fill=Trope_hizb))+
  geom_bar(stat="identity")+
  ggtitle("Distribution of Likes by Europhobic Trope")+
  ylab("Like Percentage")+xlab("Trope")+
  scale_fill_discrete(name = "Trope")+ggtitle("Distribution of Likes by Europhobic Trope")
