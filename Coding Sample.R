#Setting working directory, loading dataset, and invoking required packages. 
#Stringr for language cleaning, rvest for webpage scraping, ggplot for data graphing
#proxyC, seededlda, and quanteda for the latent-dirichlet allocation
setwd("C:/Users/tiger/Dropbox/Command & Control/Trinity/Independent Research/Islamophobia/Code")
JihadWatch<-read.csv("Dataset_1_FaceBook.csv")
library("stringr")
library("rvest")
library("ggplot2")
library("proxyC")
library("seededlda")
library("quanteda")

#specify number of posts being analyzed below. 
post_number<-113

#setting up blank, container objects that  contain all URLs, reaction counts, and dates
URL_List<-NA
Reaction_List<-NA
Date_List<-NA

#this for loop converts our dataset into a usable dataframe
#the Apify dataset is such that each post has a separate column for the number of likes
#this for loop puts the URL, like amount, and date into usable columns
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
#gsub converts lengthy date into usable format "2022-07-07"
Date_List<-gsub("T.*","",Date_List)


#Now, need to identify the title and body of blogs associated with the URLs in URL_List
#To do so, read in each URL, select 9 (the part of the text with the blog post), and
#delete extraneous information with gsub's. 

#creating 3 container objects to store title, body, and relevant snippets of text respectively
#title and body will go in our final dataframe, snippets will be used in the for loop to
#temporarily hold helpful bits of text prior to their cleaning within this loop
Title_List<-NA
Body_List<-NA
snippets<-NA

#need a progress bar to check when code is close to done - this takes a second 
pb <- txtProgressBar(min = 0,      
                     max = post_number, 
                     style = 3,    
                     width = 50,   
                     char = "=")   

for (y in 1:post_number){
  text<-read_html(URL_List[y])%>%html_nodes("body")%>%html_children()%>%html_text() 
  snippets[y]<-toString(text[9])
  
  #if statement to make sure website doesn't block the scrape. 
  #website blocks IP addresses which access the site with frequencies above the one used here
  #turns loop off every 10 cycles, waits 60 seconds, and resumes. 
  if(y%%10==0){
    Sys.sleep(60)
  } 
  setTxtProgressBar(pb, y)
}
#when the title cannot be extracted from the HTML, we use this function to construct a 
#close proxy of the title: the words between the dashes at the end of the URL
URL_to_Title <- function(URL) {
  Title <-sub(".*/","",URL)
  return(gsub("-"," ",Title))
}

for (z in 1:post_number){
  working_snippet<-gsub("^[^:]*:", "", snippets[z])

  #this if statement formats titles where the HTML begins with a time (like 5am)
  if (substring(working_snippet,1,2)=="00"|substring(working_snippet,4,5)=="am"|substring(working_snippet,4,5)=="pm"){
    working_snippet<-gsub("^[^\n]*\n", "", working_snippet)
  }
  
  #these gsub's extract the body of blog posts from the sea of extraneous HTML 
  Title_List[z]<-gsub("\n.*","",working_snippet)
  working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
  working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet_2)
  Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
  Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  
  #sometimes, the title is located in a different place than usual
  #in this case, the code returns the first paragraph of the article as the title
  #for this case, use the URL-to-Title function as a proxy for the title
  #and make sure that the first paragraph of the article is relocated to the "Body_List" object
  if(str_count(Title_List[z], "\\w+")>20){
    Body_List[z]<-paste(Title_List[z], Body_List[z],sep="")
    Title_List[z]<-URL_to_Title(URL_List[z])
  }
  
  #Sometimes, an advertisement pushes the title into a different spot of HTML. 
  #In these cases, the URL-to-Title function is used as a proxy for the title. 
  #This if statement must be at the end, lest the other parts of the for loop be compromised.
  if (substring(working_snippet,1,9)=="New in PJ"){
    Title_List[z]<-URL_to_Title(URL_List[z])
    
    working_snippet<-gsub("^[^\n]*\n", "", working_snippet)
    working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
    Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
    Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  }
  
  #Sometimes, there's an extra \n which causes the code to return nothing for the title. 
  #In this case, the URL-to-title function return a proxy for the title
  if (Title_List[z]==""){
    Title_List[z]<-URL_to_Title(URL_List[z])
    working_snippet_2<-gsub("^[^\n]*\n", "", working_snippet)
    Body_List[z]<-gsub("\nFollow.*","",working_snippet_2)
    Body_List[z]<-gsub("Filed Under.*","",Body_List[z])
  }
}

#Pre-process the corpus to remove \n and \t's left over from the HTML. 
#Add spaces to smashed-together words and punctuation, like "committee.Several". 
#Pre processing is necessary to ensure conversion of words to base form is accurate. 

Body_List<-str_replace_all(Body_List, "\n", replacement = "")
Title_List<-str_replace_all(Title_List, "\n", replacement = "")
Body_List<-gsub("\\.(?=[A-Za-z])", ". ", Body_List, perl = TRUE)
Title_List<-gsub("\\.(?=[A-Za-z])", ". ", Title_List, perl = TRUE)

#Creating corpus, appending likes, date, URL, and tweet content
Content<-paste(Title_List,Body_List,sep=" ")
Corpus_Spencer<-corpus(Content)
docvars(Corpus_Spencer,"Likes") <-Reaction_List
docvars(Corpus_Spencer, "Date") <- Date_List
docvars(Corpus_Spencer, "URL") <- URL_List
docvars(Corpus_Spencer,"Content") <-paste(Title_List,Body_List,sep=" ")

#Placing corpus into CSV to view. 
write.csv(summary(Corpus_Spencer),"Test Data Set.csv")

#Tokenizing and converting into DFM
tokens_Corpus_Spencer<-tokens(Corpus_Spencer, remove_numbers = TRUE, remove_symbol = TRUE)
DFM_Corpus_Spencer <- dfm(tokens_Corpus_Spencer,remove_punct = TRUE)

#Removing numbers and symbols, punctuation, stop words, converting to lower case, and stemming
DFM_Corpus_Spencer<-dfm_remove(DFM_Corpus_Spencer, pattern = stopwords("english"))
DFM_Corpus_Spencer_Stemmed<-dfm_wordstem(DFM_Corpus_Spencer, language = quanteda_options("language_stemmer"))

#First necessary division is splitting data into Muslim vs non-Muslim posts. 
Muslim_Dictionary<-dictionary(list(Muslim=c("muslim","islam","allah","isis"),
                                   Filter=c("RT","typo","autocorrect","tweet","Catholic","fentanyl")))

#Fitting seededlda model 
library(seededlda)
slda <- textmodel_seededlda(DFM_Corpus_Spencer_Stemmed, Muslim_Dictionary, residual = TRUE)
topwords_Spencer<-as.data.frame(seededlda::terms(slda, 20))
head(topics(slda), 20)
Corpus_Spencer$topic <- seededlda::topics(slda)

#Now, repeat the above process to split remaining posts into relevant tropes.  
#First, select all the posts related to topic "Muslim" 
Corpus_Spencer_Muslim<-corpus_subset(Corpus_Spencer, topic == "Muslim")
#Second, place the corpus into a CSV to view. 
write.csv(summary(Corpus_Spencer_Muslim),"Corpus_Spencer_Muslim.csv")
#Third, remove numbers, stopwords, and stem corpus. 
tokens_Corpus_Spencer_Muslim<-tokens(Corpus_Spencer_Muslim, remove_numbers = TRUE, remove_symbol = TRUE)
DFM_Corpus_Spencer_Muslim <- dfm(tokens_Corpus_Spencer_Muslim,remove_punct = TRUE)
DFM_Corpus_Spencer_Muslim<-dfm_remove(DFM_Corpus_Spencer_Muslim, pattern = stopwords("english"))
DFM_Corpus_Spencer_Muslim_Stemmed<-dfm_wordstem(DFM_Corpus_Spencer_Muslim, language = quanteda_options("language_stemmer"))

#Constructing my dictionary for the four tropes
Islamophobic_Dictionary <- dictionary(list(PreModern = c("strict", "reform","akbar","allahu","children"),
                                           Antidemocratic = c("bigotry","bigot", "salman", "Salman", "Sharia","moderate","freedom","speech","rushdie","Rushdie","koran","Koran","ban","banned","islamophobia"),
                                           Misogyny = c("rapist", "raped", "women","woman", "hijab","rape", "girl","marriage","husband","trafficking","misogynistic","misogyny"),
                                           Expansionist = c("migrants", "migrant", "refugee","refugees", "border", "invasion", "terrorist","jihad","kill","stab","Israel","turkish","Turkish","israel","explosive","bomb","jihadis","jihadi","Afghanistan","ISIS","isis","islamic state","Islamic State","taliban","Taliban"),
                                           filter=c("i","tell","ask","how","porn")))
#Fitting seededlda model 
library(seededlda)
slda <- textmodel_seededlda(DFM_Corpus_Spencer_Muslim_Stemmed, Islamophobic_Dictionary, residual = TRUE)
topwords_Spencer<-as.data.frame(seededlda::terms(slda, 20))
head(topics(slda), 20)

#Storing topics identified into data frame
Corpus_Spencer_Muslim$trope <- seededlda::topics(slda)

#Subdividing corpus into four tropes to begin data visualization 
PreModern_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "PreModern")
Antidemocratic_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Antidemocratic")
Misogyny_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Misogyny")
Expansionist_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "Expansionist")
Other_Corpus<-corpus_subset(Corpus_Spencer_Muslim, trope == "filter")

#Storing the post amount for each trope and summing them to find total posts analyzed 
PreModern_Post_Amount<-length(PreModern_Corpus)
Antidemocratic_Post_Amount<-length(Antidemocratic_Corpus)
Misogyny_Post_Amount<-length(Misogyny_Corpus)
Expansionist_Post_Amount<-length(Expansionist_Corpus)
Other_Post_Amount<-length(Other_Corpus)
Total_Post_Amount<-PreModern_Post_Amount+Antidemocratic_Post_Amount+Misogyny_Post_Amount+Expansionist_Post_Amount+Other_Post_Amount

#Finding proportions for how often each post shows up
PreModern_Proportion<-PreModern_Post_Amount/Total_Post_Amount
Antidemocratic_Proportion<-Antidemocratic_Post_Amount/Total_Post_Amount
Misogyny_Proportion<-Misogyny_Post_Amount/Total_Post_Amount
Expansionist_Proportion<-Expansionist_Post_Amount/Total_Post_Amount
Other_Proportion<-Other_Post_Amount/Total_Post_Amount

#Finding how often each type of post is liked
PreModern_Likes<-sum(as.data.frame(PreModern_Corpus$Likes))
Antidemocratic_Likes<-sum(as.data.frame(Antidemocratic_Corpus$Likes))
Misogyny_Likes<-sum(as.data.frame(Misogyny_Corpus$Likes))
Expansionist_Likes<-sum(as.data.frame(Expansionist_Corpus$Likes))
Other_Likes<-sum(as.data.frame(Other_Corpus$Likes))
Total_Likes<-PreModern_Likes+Antidemocratic_Likes+Misogyny_Likes+Expansionist_Likes+Other_Likes

#Finding proportions for likes of each post 
PreModern_Proportion_Likes<-PreModern_Likes/Total_Likes
Antidemocratic_Proportion_Likes<-Antidemocratic_Likes/Total_Likes
Misogyny_Proportion_Likes<-Misogyny_Likes/Total_Likes
Expansionist_Proportion_Likes<-Expansionist_Likes/Total_Likes
Other_Proportion_Likes<-Other_Likes/Total_Likes

#Plotting topic wise proportions and storing output into a csv. 
Percentage<-c(PreModern_Proportion,Antidemocratic_Proportion,Misogyny_Proportion,Expansionist_Proportion,Other_Proportion)
Trope<-c("Premodern","Antidemocratic","Misogynistic","Expansionist","Other")
Trope_Table<-data.frame(Trope,Percentage)
write.csv(Trope_Table,"Trope_Table_Islamophobia.csv")
ggplot(data=Trope_Table, aes(x=Trope, y=Percentage,fill=Trope))+
  geom_bar(stat="identity")+ggtitle("Distribution of Posts by Islamophobic Trope")

#Plotting like proportions
Like_Percentages<-c(PreModern_Proportion_Likes,Antidemocratic_Proportion_Likes,Misogyny_Proportion_Likes,Expansionist_Proportion_Likes,Other_Proportion_Likes)
Like_Table<-data.frame(Trope,Like_Percentages)
write.csv(Like_Table,"Like_Table_Islamophobia.csv")
ggplot(data=Like_Table, aes(x=Trope, y=Like_Percentages,fill=Trope))+
  geom_bar(stat="identity")+
  ggtitle("Distribution of Likes by Islamophobic Trope")+
  ylab("Like Percentage")