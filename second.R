library(rJava)
library(NLP)
library(openNLP)


# first prepare the text
textSample = paste(
  c(
    "Pierre Vinken, 61 years old, will join the board as a ",
    "nonexecutive director Nov. 29.\n",
    "Mr. Vinken is chairman of Elsevier N.V., ",
    "the Dutch publishing group."
  ),
  collapse = ''
)
textSample = as.String(textSample)

#prepare the annotator

sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
chunk_token_annotator = Maxent_Chunk_Annotator()
entity_token_annotator = Maxent_Entity_Annotator()
pos_token_annotator = Maxent_POS_Tag_Annotator()
 
ann1=annotate(textSample,list(sent_token_annotator,word_token_annotator))
ann2=annotate(textSample,pos_token_annotator,ann1)
ann2w=subset(ann2,type=='word')
ann2_tags=sapply(ann2w$features,'[[','POS')
table(ann2_tags)
sprintf('%s/%s',s[ann2w],ann2_tags)

