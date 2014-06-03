### Demo of the NDL package.

#Install NDL
install.packages("ndl")

#load NDL library
require(ndl)
require(stringr)

### 1. Preprocess corpus corpus 

unix%> ndl.preproc.py 3 3 < mini.utf8 > hungarian.events.tsv

### This program is found inside the ndl package in R! You can also get it here:
#
##    https://bitbucket.org/cyrusshaoul/ndl/src/823e50e13dae6e17bb5bc74911dfc71ba7eae911/inst/scripts/ndl.preproc.py
#
### 2. Load corpus

#events <- read.table(file="~/Hungarian/hungarian.events.tsv",header=T,as.is=T,comment.char="",quote="",sep="\t")

eh <- read.table(file="morph.output.txt.gz",header=F,as.is=T,comment.char="",quote="",sep="\t")

colnames(eh)=c("form","lemma","morph")

eh$c1 = sub("^.*/","",eh$morph)
eh$c2 = sub("<cas<","_",eh$c1)
eh$c3 = gsub(">>","",eh$c2)
eh$c4 = gsub("><","_",eh$c3)
eh$c5 = gsub("-","",eh$c4)
eh$c6 = gsub(">","",eh$c5)
eh$c7 = gsub("<","_",eh$c6)
eh$c8 = gsub("\\+","_",eh$c7)
eh$c9 = gsub("\\[.*\\]","",eh$c8)
eh$c10 = gsub(".*\\?","",eh$c9)

#Clean up numbers and punctuation
eh$f2 = gsub("[0-9]","",eh$form)
eh$f3 = gsub("[[:punct:]]","",eh$f2)
eh$l2 = gsub("[0-9]","",eh$lemma)
eh$l3 = gsub("[[:punct:]]","",eh$l2)

#remove any rows with empty data.
eh=eh[eh$f3 != "",]
eh=eh[eh$l3 != "",]
eh=eh[eh$c10 != "",]

#Put the cleaned up data into a new dataframe.
eh2 = eh[,c("f3","l3","c10")]
#Rename columns
colnames(eh2) = c("form","lemma","morpho")
#Remove unneeded rows
eh3 = eh2[eh2$morph !="punct",]
#save data just in case
saveRDS(eh3,"final.input.rds")

#load data, if you want to recover it.
eh3=readRDS("final.input.rds")


### 3. Create WeightMatrix

# When in a rush, use first million only
eh7=eh3[1:1000000,]


#Create bigram Cues  
eh7$Cues = orthoCoding(eh7$form)
# Combine lemma and morph-tags into the outcome list
eh7$Outcomes = paste(eh7$lemma, eh7$morpho,sep="_")
# Set the frequencies of each event
eh7$Frequency=1


# estimate R-W weights
w = estimateWeights(eh7,saveCounts=F,verbose=TRUE)

### 4. Save it for later!

saveRDS(w,"HungarianWM.rds")


################## EXAMPLES ########################

### For these examples to work, please load all the functions like this.
source("ndl.demo.functions.R")

### Some Examples for Childes
### Outcome comparisons

OutcomeActivityComparison(c("lenne","kézilabda","vízilabbda","abda","labda","lambda"),"labda")
#OutcomeActivityComparison(c("ball","is","it","was","green","yellow","red","blue","balloon"),"ball")

### Cue Comparisons
CueActivityComparison("labda",c("piros","fekete","barna","lila","labda","lambda"))
#CueActivityComparison("blue",c("blue","brown","boo","red","yellow","ball"),wm=w)

#### Most active outcomes

topNactiveOutcomes("labda",wm=w)
topNactiveOutcomes("labdarúgó",wm=w)
topNactiveOutcomes("piros",wm=w)

topNactiveCues("labda",wm=w)
topNactiveCues("piros",wm=w)
topNactiveCues("poss",wm=w)

#### Self-activations
stims = c("piros","fekete","barna","lila","labda","lambda","bűnökröl","bűnöktöl")
selfActivation(stims)

