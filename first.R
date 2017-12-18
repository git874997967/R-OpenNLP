# overView of opennlp
library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(qdap)

#some text
s = paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),collapse='')
s
s = as.String(s) # trans the character verctor into string
#   generate different kinds of annotators. Several choices are
# Pos entity sent word chunk
s
sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
#  download the openNLPmodels.en from http://datacube.wu.ac.at/src/contrib/ to solve
# the problem
chunk_token_annotator = Maxent_Chunk_Annotator()
entity_token_annotator = Maxent_Entity_Annotator()
pos_tag_annotator = Maxent_POS_Tag_Annotator()
# in the example only two (sentense token and word token are used)
a2 = annotate(s, list(sent_token_annotator, word_token_annotator))
a3=annotate(s,pos_tag_annotator,a2)
a3
a3w=subset(a3,type=='word')
tags=sapply(a3w$features,'[[','POS')
table(tags)
#extract token/Pos pairs(all of them ): easy??
#sprintf means string pint format C-style
          
sprintf('%s/%s',s[a3w],tags)
# small example on sapply and lapply
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x
x_mean=lapply(x,mean)
x_mean
x_quan=sapply(x,quantile)
x_quan
x_quan1=lapply(x,quantile,probs=0:4/4)
x_quan1







# Demo of openNlp named entity extraction visualise output
text=readLines(file.choose())
print(text)
text=paste(text,collapse =" ")
text=as.String(text)
# use three annotators in first.R

 
# define other annotators
person_token_annotator=Maxent_Entity_Annotator(kind='person')
loaction_token_annotator=Maxent_Entity_Annotator(kind='location')
date_token_annotator=Maxent_Entity_Annotator(kind='date')
org_token_annotator=Maxent_Entity_Annotator(kind='org')
pipeLine=list(sent_token_annotator,word_token_annotator,person_token_annotator,location_token_annotator,date_token_annotator,org_token_annotator)
# this is just as the previous exercise  get a new collection of annotators
text_annotations=annotate(text,pipeLine)
text_doc=AnnotatedPlainTextDocument(text,text_annotations)

entities = function(doc,kind)
{
  s = doc$content
  a = annotations(doc)[[1]]
  if (hasArg(kind))
  {
    k = sapply(a$features,'[[',"kind")
    s[a[k == kind]]
  }
  else
  {
    s[a[a$type == "entity"]]
  }
}
 plot(table(entities(text_doc,kind='person')))
 entities(text_doc,kind='location')
 entities(text_doc,kind='date')
 entities(text_doc,kind='organization')
 library(googleVis)
 # library(rattle)
 # rattle()
dfp=data.frame(table(entities(text_doc,kind='person')))
Barp=gvisColumnChart(dfp) 
plot(Barp)
dfl=data.frame(table(entities(text_doc,kind='location')))
Barl=gvisComboChart(dfl) 
plot(Barl)
 