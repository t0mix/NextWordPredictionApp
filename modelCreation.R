################################################################################
### Settings
################################################################################

# Setting the working directory
setwd("C:/Users/tsang.t.1/Documents/___Coursera/Data Scientist Track/10 - Capstone Project/Project")

# Loading required library
library(gdata)

################################################################################
### Downloading text files
################################################################################

# zip file name
zf <- "Coursera-SwiftKey.zip"
# download into the placeholder file
download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", zf, mode="wb")
# unzip zip file
unzip(zf)
# get folders and files listing
fnf <- unzip(zf,list=T)
# get txt files names
tfn <- fnf[grep(".txt$",fnf[,1]),"Name"]

################################################################################
### Sampling data, creating 5 non-overlapping sample subsets of a 1000th 
### of the data for training, and a validation dataset following the same
### mechanism
################################################################################

# get english file paths
etfn <- tfn[grep(".*en_US.*",tfn)]

# exclude twitter file
etfn <- etfn[-grep(".*twitter.*",etfn)]

# create sample directory
sdir <- "sample"
dir.create(sdir, showWarnings = FALSE)

# sampling of every a lines
a <- 1000

# number of sampled subset
p <- 5

# define unique sample file for training
fp <- paste0(sdir,"/","sample_",1:p,".txt")

# define testing file
fp <- c(fp,paste0(sdir,"/","testing.txt"))

for(s in 1:(p+1)) {
        
        # define unique sample file
        cfp <- fp[s]
        
        # write a sample in sample directory 
        sink(cfp)
        
        # for each of the files, generate sample files with 1 out of k lines only
        for(t in 1:length(etfn)) {
                
                # establish connection to file
                con <- file(etfn[t], "r")
                
                # init index line
                ix <- 1
                
                # read line by line, sample lines and do preprocess them 
                while (length(oneLine <- readLines(con, n = 1, warn = FALSE, encoding = "UTF-8")) > 0) {
                        if(ix %% (a+s-1) == 0) {                        
                                # append line and line break
                                cat(oneLine)
                                cat("\n")
                        }
                        ix <- ix + 1
                }
                
                # close file connection
                close(con)
                
        }
        
        # write into file
        sink()          
        
}


################################################################################
### Getting list of badwords
################################################################################

# download list of badwords from Google as badwords.txt
download.file("http://badwordslist.googlecode.com/files/badwords.txt", "badwords.txt", mode="wb")

# reading badwords.txt
bwdf <- read.table(file = "badwords.txt",header = F,as.is = T)

# turn list of bad words into vector and remove special characters skrewing up
bwv <- gsub("[*()]","",bwdf[,1])
bwv <- bwv[-grep("[^[:alnum:][:space:]']", bwv)] # remove numbers and special characters
bwv <- bwv[-grep("[0-9]", bwv)] # remove numbers
bwv <- tolower(bwv) # remove to lower
bwv <- trim(bwv) # remove to lower
bwv <- unique(bwv) # keep unique
bwv <- bwv[-grep("^.{1,2}$", bwv)] # remove all badword of 1 or 2 characters
bwv <- bwv[-grep("blow|job|the", bwv)] # remove exception from badwords
# build regex
bwrgx <- paste0(paste0(" ",bwv," "),collapse ="|")


################################################################################
### preprocessing function
################################################################################

# sourcing pre-processing function and prediction function from same file used
# within Shiny App
source("model.R")


################################################################################
### tokenize
################################################################################

# Set up to how many ngrams
n <- 4

# to store models
models <- list()
for( i in 1:p ) {
        models[[i]] <- list()            
}

# load the training samples files and calculate the n-grams (uni, bi, tri and 
# four)
for(q in 1:p) {
        
        # get sample training file path
        fpath <- fp[q]
        
        # to store sentences
        sentences <- character()
        
        # to store ngrams
        ngrams <- list()
        for( i in 1:n ) {
                ngrams[[i]] <- character()                
        }
        
        # establish connection to file
        con <- file(fpath, "r")
        
        # read line by line as ASCII to get non UTF-8 characters properly 
        while (length(oneLine <- readLines(con, n = 1, warn = FALSE, encoding = "ASCII")) > 0) {
                
                # preprocessing 
                uniqueSentences <- preprocessLine(oneLine) 
                
                # append to sentences vector
                sentences <- c(sentences,uniqueSentences)        
        }
        
        # close file connection
        close(con)
        
        # remove all lines with bad words
        exclude <- grep(bwrgx, sentences)
        if(length(exclude)>0) sentences <- sentences[-exclude] 

        
        # Generate the ngrams
        for(s in 1:length(sentences)) {
                
                # Split the sentences in words
                words <- strsplit(sentences[s]," ")[[1]]
                
                # concatenate one-grams
                ngrams[[1]] <- c(ngrams[[1]],words[grep("[^<s>|</s>|0-9]",words)])
                
                # concatenate n-grams (from 2-grams to n-grams)
                for( i in 2:n ) {
                        # check if words vector has enough words for n-grams to be created 
                        k <- length(words)-i+1
                        if(k>0) { 
                                # for the length of the sentence including tags minus n
                                for(j in 1:k) {
                                        
                                        # include the ngram only if non of the words are only numbers 
                                        if( length(grep("^[0-9]+$",words)) == 0 ) {
                                                # concatenate the ngrams
                                                ngrams[[i]] <- c(ngrams[[i]],paste(words[j:(j+i-1)],collapse = " ")) 

                                        }
                                }
                        }
                }
                
        }        
        
        # store the list of ngrams got from each training samples files 
        models[[q]] <- ngrams
        
}



################################################################################
### build frequency list, e.g. prediction data
################################################################################

# init empty freqs list
freqs <- list()
for( i in 1:n ) {
        freqs[[i]] <- list()            
}

# assembling frequencies from ngrams coming from each tranining samples files
for(q in 1:p) {
        
        # for each ngrams
        for( i in 1:n ) {
                
                # get i ngram from model q
                ngram <- models[[q]][[i]]
                
                freq <- data.frame(table(ngram)) # contengency table
                
                if( q == 1 ) { # frequencies from first sample file for training 
                        freqs[[i]] <- freq
                } else { # join frequencies from subsequent sample files for training 
                        freqs[[i]] <- merge(x = freqs[[i]], y = freq, by = "ngram", all = TRUE)
                        
                        # NA count to 0
                        freqs[[i]][is.na(freqs[[i]]$Freq.x),"Freq.x"] <- 0
                        freqs[[i]][is.na(freqs[[i]]$Freq.y),"Freq.y"] <- 0
                        
                        # merge counts
                        freqs[[i]]$Freq <- freqs[[i]]$Freq.x+freqs[[i]]$Freq.y
                        freqs[[i]]$Freq.x <- NULL
                        freqs[[i]]$Freq.y <- NULL
                        
                }
                
        }
        
        
}

# building nice sorted frequency data.frame with summary information
for( i in 1:n ) {     
        
        freq <- freqs[[i]]
        
        freq <- freq[order(-freq$Freq),] # order by frequency
        
        freq$RelativeFreq <- freq$Freq / sum(freq$Freq) # add relative frequency
        
        freq$nMinusOneGram <- ""
        freq$lastToken <- freq$ngram
        # if larger than unigram, split into last word and what's before
        if( i > 1 ) {
                rgx <- paste0("(",paste(rep(".+",i-1),collapse = " "),") .+")
                freq$nMinusOneGram <- gsub(rgx,"\\1",freq$ngram)
                freq$lastToken <- gsub("^.+ ","",freq$ngram)
        }
        
        # print information carried by ngrams OR CALCULATE PERPLEXITY HERE 
        print( paste0("Unique ",i,"-grams per ",i,"-grams count : ",nrow(freq)/sum(freq$Freq)) )
        
        freqs[[i]] <- freq

}

# keep onl what's needed, e.g. top frequencies per n-1-grams, and 2 columns
# with the n-1-grams and the associated last token
for(i in 2:n) {

        freq <- freqs[[i]]
        
        # remove ngram with </s> at the end
        freq <- freq[freq$lastToken!="</s>",]
        
        # get max for each nMinusOneGram
        maxs <- aggregate(Freq ~ nMinusOneGram, data = freq, FUN = max)
        freq <- merge(maxs, freq)
        freq <- freq[order(-freq$Freq),] # order by frequency
        
        freqs[[i]] <- freq[,c("nMinusOneGram","lastToken")]

}

# keep only top 10 backoff unigram
freqs[[1]] <- freqs[[1]][1:10,c("nMinusOneGram","lastToken")]


################################################################################
### Evaluate model accuracy on the validation dataset
################################################################################

# get validation file path
tfpath <- fp[length(fp)]

# to store sentences
sentences <- character()

# establish connection to file
con <- file(fpath, "r")

# read line by line as ASCII to get non UTF-8 characters properly 
while (length(oneLine <- readLines(con, n = 1, warn = FALSE, encoding = "ASCII")) > 0) {
        
        # preprocessing 
        uniqueSentences <- preprocessLine(oneLine) 
        
        # append to sentences vector
        sentences <- c(sentences,uniqueSentences)        
}

# close file connection
close(con)

# remove all lines with bad words
exclude <- grep(bwrgx, sentences)
if(length(exclude)>0) sentences <- sentences[-exclude] 

# remove all sentences with only numbers
sentences <- sentences[-grep("^<s> [0-9 ]+ </s>$",sentences)]

endword <- gsub( ".+( [a-zA-Z0-9']+ </s>)", "\\1", sentences)
sentenceBeginning <- gsub( "(.+) [a-zA-Z0-9']+ </s>", "\\1", sentences)
endword <- gsub( " (.+) </s>", "\\1", endword)

# run the algo on testing set
prediction <- character()
for(g in 1:length(sentences)) {
        prediction[g] <- backOffPredict(sentenceBeginning[g],freqs)
}

accuracy <- sum(endword==prediction) / length(endword)

print(paste0("Model tested on ",length(endword)," sentences coming from held out
             validation dataset, giving an accuracy of ",accuracy)


################################################################################
### Saving prediction data
################################################################################

# this is the file containing the top frequent n-1-grams and associated
# last token for prediction purpose, to be used with backOffPredict function
# from model.R
save(freqs, file = "freqs.RData")



################################################################################
### End
################################################################################

