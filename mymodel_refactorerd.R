source('init.R')

library(plyr)
library(arm)
library(reshape2)
library(odbc)
library(RMySQL)
library(utf8)
library(matrixStats)
library(dplyr)
library(quanteda)
library(ggplot2)
library(cluster)
library(dendextend)
library(purrr)
library(shiny)
library(epiDisplay)
library(tm)
library(eatGADS)
library(magrittr)
library(tidyr)
library(ggdendro)

# Указания: достоевский 1, достоевский - 2, полонский - 9, щебальский - 27, победоносцев - 26, страхов - 14/23, григорьев - 4,мещерский -15

#Initialization



USE_INTERFACE =  FALSE;
CUTTED_NUMBER <- 500
FedorDostoyevsky_texts = c(35,195,196,136,44,38,126,268,40,25,32,43,86,99,145,153,154,155,156,157,164,163,166,201,202,203,165,167,200,96,121,36,130,298,299,188,97,76,77,42,75,78,45,205,206,204,207,209,46);
MichailDostoyevsky_texts = c(23,109,111,113,114,115,141,314)
Polonsky_texts = c(13,39)
Shchebalsky_texts = c(245,246,247,248)
Pobedonostsev_texts = c(237,238,239)
Strahov_texts = c(85,116,117,125,199,292,293,294,295,296)
Grigoriev_texts = c(11,89,90,177,198,178,6)
Meshchersky_texts = c(146,151,152,180,287,290,286,289,308,309,310,311,312)
checklist = c(30,109,113,211,6,178,261,262,263,264,265,279,280,281,282,284,291)
table_of_textes <- c();

list_of_texts = list(list('name' = "Fedor Dostoevsky"   , 'val' = FedorDostoyevsky_texts   , 'color' = "red"),
                     list('name' = "Michail Dostoevsky" , 'val' = MichailDostoyevsky_texts , 'color' = "blue"),
                     list('name' = "Polonsky"           , 'val' = Polonsky_texts           , 'color' = "black"),
                     list('name' = "Shchebalsky"        , 'val' = Shchebalsky_texts        , 'color' = "green"),   
                     list('name' = "Pobedonostsev"      , 'val' = Pobedonostsev_texts      , 'color' = "purple"), 
                     list('name' = "Strahov"            , 'val' = Strahov_texts            , 'color' = "orange"),
                     list('name' = "Grigoriev"          , 'val' = Grigoriev_texts          , 'color' = "darkgreen"),
                     list('name' = "Meshchersky"        , 'val' = Meshchersky_texts        , 'color' = "chocolate4"));

save_plot <- function(name,p){
  ggsave(name, plot = p,width=25, height = 11) 
}

used_authors <- (rep(TRUE,length(list_of_texts)))
if (USE_INTERFACE){
  source('interface.R')
  a <- print(shinyApp(ui, server))
  used_authors <- list_of_texts %>% map('name') %>% unlist() %in% a
}

list_of_etalon_texts <- list_of_texts[used_authors]  %>% map('val') %>% unlist()
list_of_colors <- list_of_texts[used_authors] %>% map(function(x){rep(x$color,length(x$val))}) %>%  unlist()
  
#connecting smalt data base

mydb <- 'mydatabase'
con <- dbConnect(MySQL(), user='root', password='', dbname='');
dbSendQuery (con, "SET NAMES utf8");
#Getting data of authors
string_query <- paste0("select (select lower(initial_form) from entries where id=dictword_id) as WORD,TEXT_ID,(select title from text where id=text_id) as TITLE,
  (select author_id from text where id=text_id) as AUTHOR , DICTWORD_ID from word where 
  text_id in (",paste(list_of_etalon_texts,collapse = ","),") and (select param_01 from entries where id=dictword_id)!=16 order by text_id,id_word;");
rs <- dbSendQuery(con, string_query);
table_of_textes <-  fetch(rs, n = -1);
on.exit(dbDisconnect(con))

#Counting Words in all textes

number_of_textes <- length(split(table_of_textes,table_of_textes$TEXT_ID))
table_of_textes_list <- lapply(split(table_of_textes,table_of_textes$TEXT_ID) %>% map("WORD"),function(x){paste(x,collapse = " ")}) %>% unlist()
text_corpus <- Corpus(VectorSource(table_of_textes_list))

#Counting Frequency of words in each text

doc_term_mat <- TermDocumentMatrix(text_corpus ,control = list(stopwords = FALSE,wordLengths = c(1, Inf)))#,weighting = 'weightTfIdf'))
freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
all_count_statistic <- (doc_term_mat[freq_terms,] %>% as.matrix()) 
all_count_statistic <- all_count_statistic[,paste(list_of_etalon_texts)]
all_count_statistic <- all_count_statistic[(rev(order(rowSums(all_count_statistic)))), ]

#Most frequent words barplot 
 p <-  all_count_statistic[1:20,]  %>% rowSums() %>%  data.frame(Term= names(.),Frequency = .)   %>%  mutate(Term = reorder(Term, Frequency)) %>%
  ggplot(aes(Term,Frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Term\n", y = "\n Frequency ", title = "Frequent Words\n") +
  geom_text(aes(label =Frequency), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))
p
save_plot('img/barplot.jpg',p)
#Calculating z-statistics of each word in each text

for (i in 1:number_of_textes){
  all_count_statistic[,i] = all_count_statistic[,i]/ sum(all_count_statistic[,i])
}
z_table <- (all_count_statistic - rowMeans(all_count_statistic,na.rm = TRUE))/sqrt(rowVars(all_count_statistic,na.rm = TRUE))
z_table <- z_table[1:CUTTED_NUMBER,]
#for(i in 1:nrow(z_table)){
#  z_table[i,] = z_table[i,] * (CUTTED_NUMBER - i+1)/(CUTTED_NUMBER)
#}
#building difference Matrix

difference_matrix <- matrix(0,nrow = number_of_textes,ncol = number_of_textes);
for( i in 1:number_of_textes)
  for( j in 1:number_of_textes){
    difference_matrix[i,j] = sum(abs(z_table[,i] - z_table[,j]))/CUTTED_NUMBER
}

#Bulding plot for distance Matrix

nn <-as.data.frame(difference_matrix)
names(nn) <- list_of_etalon_texts
nn <- cbind(nn,list_of_etalon_texts)
nn.m <- melt(nn,id.vars = "list_of_etalon_texts")
nn.m <- ddply(nn.m, .(variable), transform)
base_size <- 9
nn.m$list_of_etalon_texts <- factor(nn.m$list_of_etalon_texts,levels=list_of_etalon_texts)
p <- ggplot(nn.m, aes(variable, list_of_etalon_texts))  + geom_tile(aes(fill = value),     colour = "white") + scale_fill_gradient(low = "yellow",   high = "red") + xlab("Text1") + ylab("Text2") + theme(axis.text.x = element_text(colour =list_of_colors)) + theme(axis.text.y = element_text(colour =list_of_colors))
p
save_plot('img/distant_matrix.jpg',p)

# doing hierarchical clustering and plotting dendragram

mm <- nn[-length(nn)]
rownames(mm) <- list_of_etalon_texts
colnames(mm) <- list_of_etalon_texts
distances = as.dist(mm)
eward = hclust(distances, method="ward.D2")
dend <- as.dendrogram(eward)
colors_to_use <- list_of_colors
colors_to_use <- colors_to_use[order.dendrogram(dend)]
q <- ggdendrogram(eward) + theme(axis.text.x = element_text(colour = colors_to_use,size = 12))
save_plot('img/dendrogramm.jpg',q)




