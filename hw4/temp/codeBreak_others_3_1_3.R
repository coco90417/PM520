#########################################################
########################## hw4 ##########################
#########################################################

setwd("/home/cocodong/class/hw4/")

###################### Prerequisite #####################
# if you do not have string distance package installed,
# plesae install it using following command
# install.packages("stringdist")
library(stringdist)


###################### Subroutines ######################
parseText <- function(file){
    data = tolower(readLines(file))
    return(data)
}

loadFrequencyTable <- function(){
    data = read.table("LetterPairFreqFrom7Novels.txt")
    data = data/sum(data)
    return(data)
}

initializeCodeBook <- function(select, frequency, data){
    fakeFrequency = getFrequency(data)
    if(select=="row"){
        trueFrequency = rowSums(frequency)
    }else if(select =="column"){
        trueFrequency = colSums(frequency)
    }else if(select == "all"){
        trueFrequency = colSums(frequency) + rowSums(frequency)
    }else{
        trueFrequency = sample(length(letters), replace = F)
    }
    return(getRank(fakeFrequency, trueFrequency))
}

getFrequency <- function(data){
    dataArray = unlist(strsplit(data, split="", fixed = T))
    singleLetterFrequency = rep(0, length(letters))
    for(i in seq_along(letters)){
        singleLetterFrequency[i] = sum(dataArray==letters[i])
    }
    return(singleLetterFrequency)
}

getRank <- function(fakeFrequency, trueFrequency){
    orderedFakeFrequency = rank(fakeFrequency, ties.method= "random")
    orderedTrueFrequency = rank(trueFrequency, ties.method= "random")
    codebook = rep(0, length(orderedFakeFrequency))
    for(i in seq_along(orderedFakeFrequency)){
        codebook[i] = letters[orderedTrueFrequency == orderedFakeFrequency[i]]
    }
    return(codebook)
}


getPosteriorLogLikelihood <- function(data, frequency, codebook){
    logLikelihood = 0
    data = chartr(old=paste(letters, collapse=""), new = paste(codebook,collapse=""), data)
    dataArray = unlist(strsplit(data, split="", fixed = T))
    for(i in seq_along(dataArray)){
        # first letter is in alphabet
        if(sum(letters == dataArray[i]) == 1){
            # second letter is in alphabet
            if(sum(letters == dataArray[i+1]) == 1){
                logLikelihood = logLikelihood + log(frequency[letters == dataArray[i],letters == dataArray[i+1]])
            }
        }
    }
    return(logLikelihood)
}

proposedKernel <- function(codebook){
    myIndex = sample(length(codebook), 2, replace=F)
    mySubstitute = codebook[myIndex]
    mySubstitute = rev(mySubstitute)
    codebook[myIndex] = mySubstitute
    return(codebook)
}

mcmc <- function(file, iteration, select){
    data = parseText(file)
    start = Sys.time()
    frequency = loadFrequencyTable()
    codebook = initializeCodeBook(select, frequency, data)
    for(i in 1:iteration){
        p = runif(1,0,1)
        newCodebook = proposedKernel(codebook)
        newLogLikelihood = getPosteriorLogLikelihood(data, frequency, newCodebook)
        logLikelihood = getPosteriorLogLikelihood(data, frequency, codebook)
        h = min(1, exp(newLogLikelihood-logLikelihood))
        if(p < h){
            codebook = newCodebook
        }
        if(i %% 100 == 0){
            cat(paste("at", i, "iteration", "\n"))
            cat(chartr(old=paste(letters, collapse=""), new = paste(codebook,collapse=""), data))
            cat("\n")
            cat(logLikelihood)
            cat("\n")
        }
    }
    end = Sys.time()
    return(list(data = chartr(old=paste(letters, collapse=""), new = paste(codebook,collapse=""), data), time = end-start))
}

###################### Realization ######################

codeBreak <- function(fileHeader, decodedFileHeader, selector, iteration){
    fileOut = paste(fileHeader,"_", selector, "_", iteration, ".txt", sep="")
    trueContent = getTrueContent(decodedFileHeader)
    decodedMsg = mcmc(fileHeader, iteration = iteration, select = selector)
    decodedContent = decodedMsg$data
    myAccuracy = 1-stringdist(trueContent,decodedContent,method='jw')
    myTime = decodedMsg$time
    write.table(c(time = myTime, accuracy = myAccuracy), fileOut)
}

getTrueContent <- function(file){
    data = tolower(readLines(file))
    return(data)
}


fileHeaders = c("CodedMessage_short.txt", "CodedMessage_medium.txt", "CodedMessage_long.txt")
decodedFileHeaders = c("DecodedMessage_short.txt", "DecodedMessage_medium.txt", "DecodedMessage_long.txt")
selectors = c("row", "column", "all", "neither")
iterations = c(100, 500, 1000, 5000, 10000)



codeBreak(fileHeaders[3], decodedFileHeaders[3], selectors[1], iterations[3])




