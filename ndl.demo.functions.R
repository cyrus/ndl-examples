################### Demo Functions ###############################

CueActivityComparison <- function (Word,Outcomes,wm=w) {
  Cues = orthoCoding(Word)
  cueslist = strsplit(Cues, "_")[[1]]
  cueslist = cueslist[1:length(cueslist)-1]
  a = numeric()
  for (i in 1:length(Outcomes)) {
    a = c(a, round(sum(wm[cueslist, Outcomes[i]]),6))
  }
  res = data.frame(cues=Cues,outcomes=Outcomes,a,row.names=NULL)
  return(res[order(-res$a),])
}

OutcomeActivityComparison <- function (Words,Outcome,wm=w) {
  a = numeric()
  d = character()
  for (i in 1:length(Words)) {
    Cues = orthoCoding(Words[i])
    cueslist = strsplit(Cues, "_")[[1]]
    a = c(a, round(sum(wm[cueslist, Outcome]),6))
    d = c(d, Cues)
  }
  res = data.frame(cues=d,outcomes=Outcome,a,row.names=NULL)
  return(res[order(-res$a),])
}

topNactiveOutcomes <- function (Word,N=20,wm=w) {
  Cues = orthoCoding(Word)
  cueslist = strsplit(Cues, "_")[[1]]
  allwords = colnames(wm)
  allcues = rownames(wm)
  new.cueslist = cueslist[cueslist %in% allcues]
#  print(new.cueslist)
  a = apply(wm[new.cueslist, allwords], 2, sum)
  res=data.frame(cues=Cues,outcomes=allwords,a,row.names=NULL)
  sorted = res[order(-res$a),]
  return(sorted[1:N,])
}

topNactiveCues <- function (Word,N=20,wm=w) {
  cuew = w[, Word]
  sorted = sort(cuew,de=T)
  return(sorted[1:N])
}

CueActivations <- function (Word,N=20,wm=w) {
  Cues = orthoCoding(Word,maxn=3)
  cueslist = strsplit(Cues, "_")[[1]]
  cuew = w[cueslist, Word]
  sorted = sort(cuew,de=T)
  return(sorted)
}


selfActivation <- function(Words,wm=w) {
  Cues = orthoCoding(Words)
  DF = data.frame(Cues=Cues,Outcomes=Words)
  a = estimateActivations(DF, wm)
  activations = a$activationMatrix
  for (word in 1:length(Words)) {
    print(activations[word,Words[word]])
  }
}

