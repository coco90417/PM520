#########################################################
########################## hw4 ##########################
#########################################################

setwd("/home/cocodong/class/hw4/")

###################### Prerequisite #####################
# if you do not have string distance package installed,
# plesae install it using following command
# install.packages("stringdist")
library(stringdist)
library(gplots)
palette(rev(rich.colors(32)))

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

codeBreak <- function(fileHeader, decodedFileHeader, selector, iteration, repeatTimes){
    fileOut = paste(fileHeader,"_", selector, "_", iteration, ".txt", sep="")
    myAccuracy = rep(0, repeatTimes)
    myTime = rep(0, repeatTimes)
    trueContent = getTrueContent(decodedFileHeader)
    for(i in 1:repeatTimes){
        decodedMsg = mcmc(fileHeader, iteration = iteration, select = selector)
        decodedContent = decodedMsg$data
        myAccuracy[i] = 1-stringdist(trueContent,decodedContent,method='jw')
        myTime[i] = decodedMsg$time
    }
    write.table(c(time = mean(myTime), sdtime = sd(myTime), accuracy = mean(myAccuracy), sdaccuracy = sd(myAccuracy)), fileOut)
}

getTrueContent <- function(file){
    data = tolower(readLines(file))
    return(data)
}



fileHeaders = c("CodedMessage_veryshort.txt", "CodedMessage_short.txt", "CodedMessage_medium.txt", "CodedMessage_long.txt")
decodedFileHeaders = c("DecodedMessage_veryshort.txt", "DecodedMessage_short.txt", "DecodedMessage_medium.txt", "DecodedMessage_long.txt")
selectors = c("row", "column", "all", "neither")
iterations = c(100, 500, 1000, 5000, 10000)
myrepeatTimes = 20



for(myfile in seq_along(fileHeaders)){
    for(myselector in seq_along(selectors)){
        for(myiteration in seq_along(iterations)){
            codeBreak(fileHeaders[myfile], decodedFileHeaders[myfile], selectors[myselector], iterations[myiteration], myrepeatTimes)
        }
    }
}

######################## Plot #######################
# acc
pdf("hw4_barplot_accuracy_100.pdf")
combined.mean = c( 0.620788486341019, 0.626654957402831,  0.619713973610356,  0.562766061611945)
combined.sd = c(0.015512291318081, 0.0126401190499964, 0.0229481302068396, 0.0108289967561198)
plot(1,1,xlim=c(0,4),ylim=c(0,1.2),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Accuracy", main = "Boxplot of accuracy for 100 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_accuracy_500.pdf")
combined.mean = c( 0.650495657020514, 0.622517427423205,  0.630220950646915,  0.607367344846125)
combined.sd = c( 0.0845903443316758, 0.0153570059164698, 0.0171175969480632, 0.0227256559028652)
plot(1,1,xlim=c(0,4),ylim=c(0,1.2),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Accuracy", main = "Boxplot of accuracy for 500 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()


pdf("hw4_barplot_accuracy_1000.pdf")
combined.mean = c( 0.657669229826875, 0.670358649284159,  0.644015831925981,  0.634926683235548)
combined.sd = c(0.0835544572454977, 0.114058596417812, 0.0286343441059139, 0.0923956581895719)
plot(1,1,xlim=c(0,4),ylim=c(0,1.2),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Accuracy", main = "Boxplot of accuracy for 1000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_accuracy_5000.pdf")
combined.mean = c( 0.902524755123826, 0.921427015105324,  0.885438363315245,  0.805151738781532)
combined.sd = c(0.17183224743388, 0.159922608277652, 0.178057244200303, 0.199608909218936)
plot(1,1,xlim=c(0,4),ylim=c(0,1.2),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Accuracy", main = "Boxplot of accuracy for 5000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_accuracy_10000.pdf")
combined.mean = c( 0.88219884667547, 0.903718869348602,  0.903905677538387,  0.80783467)
combined.sd = c(0.1816209,	0.1569009,	0.17969886	,0.19695566)
plot(1,1,xlim=c(0,4),ylim=c(0,1.2),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Accuracy", main = "Boxplot of accuracy for 10000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()


# time

pdf("hw4_barplot_time_100.pdf")
combined.mean = c( 5.9499187707901, 6.18013949394226,  5.88100811243057,  5.88040099143982)
combined.sd = c(0.101125272600745, 0.339908217502877, 0.0069628861738575, 0.00808085840783968)
plot(1,1,xlim=c(0,4),ylim=c(0,max(combined.mean) + 1),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Time (s)", main = "Boxplot of time for 100 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_time_500.pdf")
combined.mean = c( 29.3675058603287, 31.0881053090096,  29.3596543908119,  29.2630707383156)
combined.sd = c( 0.0916543391740818, 1.72386214123976, 0.242736958494068, 0.0329267525418031)
plot(1,1,xlim=c(0,4),ylim=c(0,max(combined.mean) + 3),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Time (s)", main = "Boxplot of time for 500 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()


pdf("hw4_barplot_time_1000.pdf")
combined.mean = c( 58.5428589105606, 58.542179608345,  58.5429945588112,  58.6434027075768)
combined.sd = c(0.0788060612708737, 0.207418844921501, 0.209997902436707, 0.262614260343169)
plot(1,1,xlim=c(0,4),ylim=c(0,max(combined.mean) + 1),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Time (s)", main = "Boxplot of time for 1000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_time_5000.pdf")
combined.mean = c( 4.87957460661729, 4.89219984591007,  4.87926462054253,  4.91701014618079) * 60
combined.sd = c(0.010314429687945, 0.0803748679618566, 0.00870445052323772, 0.0831503573949455) * 60
plot(1,1,xlim=c(0,4),ylim=c(0,max(combined.mean) + 10),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Time (s)", main = "Boxplot of time for 5000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()

pdf("hw4_barplot_time_10000.pdf")
combined.mean = c( 11.781443,	11.877834,	11.8940069,	11.79795439) * 60
combined.sd = c(0.61301229	,0.5252886,	0.51107,	0.4275558) * 60
plot(1,1,xlim=c(0,4),ylim=c(0,max(combined.mean) + 10),type="n", xlab="",ylab="",axes=FALSE)
axis(side=1,at=0.5:3.5,labels=c("row", "column", "all", "none"),cex.axis=1, las=2)
axis(side=2)
barx <- barplot(combined.mean, beside=TRUE,col=c(1,5,11,15), xlim=c(0,4),  ylab="Time (s)", main = "Boxplot of time for 10000 iterations", width = 0.83, add = TRUE)
arrows(barx,combined.mean + combined.sd, barx, combined.mean, angle=90, code=1, length=0.4)
dev.off()


