library(rJava)
library(NLP)
library(openNLP)
library(stringr)
library(Rcpp)
library(RcppArmadillo)
library(BH)
# the most full function package
library(quanteda)
my_string = "Example STRING, with numbers (12, 15 and also 10.2)?!"
second_string = 'WOw, two sentences'
my_string = paste(my_string, second_string, sep = '')
lower_string = tolower(my_string)
lower_string
my_string_vector = str_split(my_string, '!')[[1]]
grepl("\\?", my_string_vector)
question = my_string_vector[grepl("\\?", my_string_vector) == TRUE]
wotE = str_replace_all(my_string, 'e', "-")
num_in_string = str_extract_all(my_string, '[0-9]+')
num_in_string

Clean_String = function(string) {
  #first lower
  temp = str_to_lower(string)
  temp = stringr::str_replace_all(temp, '[^a-zA-Z\\s]', ' ')
  #second remove extra space
  temp = stringr::str_replace_all(temp, "[\\s+]", " ")
  #split it
  temp = stringr::str_split(temp, ' ')[[1]]
  indexes = which(temp == '')
  if (length(indexes) > 0) {
    temp = temp[-indexes]
  }
  return (temp)
}
Clean_Text_Block = function(text) {
  if (length(text) <= 1) {
    # Check to see if there is any text at all with another conditional
    if (length(text) == 0) {
      cat("There was no text in this document! \n")
      to_return = list(num_tokens = 0,
                       unique_tokens = 0,
                       text = "")
    } else{
      # If there is , and only only one line of text then tokenize it
      clean_text = Clean_String(text)
      num_tok = length(clean_text)
      num_uniq = length(unique(clean_text))
      to_return = list(num_tokens = num_tok,
                       unique_tokens = num_uniq,
                       text = clean_text)
    }
  } else{
    # Get rid of blank lines
    indexes = which(text == "")
    if (length(indexes) > 0) {
      text = text[-indexes]
    }
    # Loop through the lines in the text and use the append() function to
    clean_text = Clean_String(text[1])
    for (i in 2:length(text)) {
      # add them to a vector
      clean_text = append(clean_text, Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a
    # named list object.
    num_tok = length(clean_text)
    num_uniq = length(unique(clean_text))
    to_return = list(num_tokens = num_tok,
                     unique_tokens = num_uniq,
                     text = clean_text)
  }
  return(to_return)
}
sentence = "The term 'data science' (originally used interchangeably with 'datalogy') has existed for over thirty years and was used initially as a substitute for computer science by Peter Naur in 1960."
Clean_string(sentence)

speech = readLines(choose.files())
speech = as.String(speech)
speech
Clean_string(speech)
Clean_text_block(speech)
summary(Clean_text_block(speech))
Rcpp::sourceCpp('Generate_Document_Word_Matrix.cpp')

#with quanteda
 speech1=readLines(file.choose())
speech2=readLines(file.choose())
clean_speech1=Clean_Text_Block(speech1)
clean_speech2=Clean_Text_Block(speech2)
text1=as.String(speech1)
text2=as.String(speech2)
#' Create a list containing a vector of tokens in each document for each
#' document. These can be extracted from the cleaned text objects as follows.
docs=c(paste0(speech1,collapse = " "),paste0(speech2,collapse = " ")) 
#' Create a vector of document lengths (in tokens)
doc_length=c(clean_speech1$num_tokens,clean_speech2$num_tokens)
#' Generate a vector containing the unique tokens across all documents.
unique_words=unique(c(clean_speech1$text,clean_speech2$text))  
#' The number of unique tokens across all documents
n_unique_words=length(unique_words)  
#unique_words
#' The number of documents we are dealing with. 
ndoc=2  
#' Now feed all of this information to the function as follows:
Doc_Term_Matrix=Generate_Document_Word_Matrix(
  number_of_docs = ndoc,
  number_of_unique_words = n_unique_words,
  unique_words = unique_words,
  Document_Words = doc_list,
  Document_Lengths = doc_lengths
)  

#' Make sure to add column names to you Doc-Term matrix, then take a look! 
colnames(Doc_Term_Matrix)=unique_words