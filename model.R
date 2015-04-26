# required library for trim()
library("gdata")

################################################################################
### preprocessing function
################################################################################

preprocessLine <- function(oneLine) {
        
        
        # removing tags
        oneLine <- gsub("<.*>","",oneLine) 
        oneLine <- gsub("<.*/>","",oneLine)
        
        # try catch when iconv is failing due to null characters ('\0') 
        oneLine <- tryCatch(
        { 
                # convert from ASCII to UTF-8
                oneLine <- iconv(oneLine, "ASCII", "UTF-8",sub='') 
                
                # change italic apostrophe to proper UTF-8 apostrophe
                oneLine <- gsub(iconv("\022", "UTF-8","ASCII",sub=''), "'", oneLine) 
                
                # put a separator where italic guillemets are found
                oneLine <- gsub(iconv("\023", "UTF-8","ASCII",sub=''), " </sep> ", oneLine) 
                oneLine <- gsub(iconv("\024", "UTF-8","ASCII",sub=''), " </sep> ", oneLine) 
                # do that for proper guillemets as well...???
                
                oneLine
                #return(oneLine)
                
        }, error=function(cond) {
                
                oneLine
                #return(oneLine)
                
        }
        )

        # remove special characters but point, coma, dollar, dash, apostrophe, and tag characters
        oneLine <- gsub("[^[:alnum:][:space:].,$-'</>]", "", oneLine) 
        
        # to lower case
        oneLine <- tolower(oneLine) 
        
        # remove extra spaces
        oneLine <- trim(gsub("[[:space:]]{2,}", " ", oneLine)) # remove extra spaces
        
        # threat am and pm
        oneLine <- gsub("a\\.m\\.", "am", oneLine) # threat a.m.
        oneLine <- gsub("p\\.m\\.", "pm", oneLine) # threat p.m.
        
        # add end of sentence tag where dot, question mark or exclamation is found
        oneLine <- gsub("\\.{1,}[[:space:]]{1,}", " </point> ", oneLine) 
        oneLine <- gsub("\\?{1,}[[:space:]]{1,}", " </point> ", oneLine) 
        oneLine <- gsub("\\!{1,}[[:space:]]{1,}", " </point> ", oneLine) 
        
        # add coma tags
        oneLine <- gsub("[,]", " </coma> ", oneLine) # add coma tags
        
        # split line into logical subsentences on coma, points andd other separatoer 
        uniqueSentences <- strsplit(oneLine," </point> | </coma> | </sep> ")[[1]]
        
        # remove unessecary custom tags
        uniqueSentences <- gsub("</.*>","",uniqueSentences) 
        # remove points
        uniqueSentences <- gsub("[\\.+]", " ", uniqueSentences) 
        # remove comas
        uniqueSentences <- gsub("[,+]", " ", uniqueSentences) 
        # remove amperands
        uniqueSentences <- gsub("[&]", " ", uniqueSentences) 
        # remove extra spaces
        uniqueSentences <- trim(gsub("[[:space:]]{2,}", " ", uniqueSentences)) 
        # trim all remaining tokens
        uniqueSentences <- trim(uniqueSentences) 
        
        # remove starting/alone/ending with apo
        uniqueSentences <- gsub(" ['] ", " ", uniqueSentences) 
        uniqueSentences <- gsub("^[']", "", uniqueSentences) 
        uniqueSentences <- gsub(" [']", " ", uniqueSentences) 
        uniqueSentences <- gsub("[']$", "", uniqueSentences) 
        uniqueSentences <- gsub("['] ", " ", uniqueSentences) 
        
        # remove empty tokens
        uniqueSentences <- uniqueSentences[uniqueSentences!=""] 
        
        # add start and end of sentence tags
        uniqueSentences <- paste("<s>", uniqueSentences, "</s>") # add start of and end of sentence tags
        
        return(uniqueSentences)

}        


################################################################################
### prediction function, first argument is the given phrase for which to predict
### the next word. Second argument is a list of data.frame containing the 
### prediction data which consits of most frequent unigrams, then n-gram (from
### bigrams to higher order ngrams) in the form of data.frame with n-1-grams 
### as the first column and last token as the second column.
################################################################################

backOffPredict <- function(phrase,freqs) {
        
        # preprocessing 
        uniqueSentences <- preprocessLine(phrase)      
        
        # get tthe last sentence from the input
        phrase <- uniqueSentences[length(uniqueSentences)]
        
        # remove end of phrase
        phrase <- gsub(" </s>","",phrase) 
        
        # get the number of words
        length <- length(strsplit(phrase,split = " ")[[1]])
        
        # if number of words bigger than 0
        if(length > 0) {
                
                # look into ngram to get potential next word
                # starting from highest order ngram
                for(n in min(length(freqs)-1,length):1) { 
                        
                        # build a regex that will keep the n-1 words of the given phrase
                        rgx <- paste0(".* (",paste(rep(".+",n),collapse = " "),")") 
                        
                        # get the n-1 words of the given phrase
                        ngram <- gsub(rgx,"\\1",phrase)
                        
                        # clean any remaining space at the end
                        ngram <- gsub("[[:space:]*]$","",ngram)
                        
                        # get potential next words
                        nws <- freqs[[n+1]][ freqs[[n+1]][,"nMinusOneGram"]==ngram ,"lastToken"]
                        
                        # if at least one potential next word
                        if( length(nws) > 0 ) {
                                
                                # randomly pick next word with equal prob
                                nw <- nws[ sample(1:length(nws),1) ] 
                                
                                # substitute as . if end of sentence (not really needed
                                # as would not happen given the above logic) 
                                # or put a capital I if i
                                if(nw=="</s>") nw <- "."
                                if(nw=="i") nw <- "I"
                                
                                return (nw)
                                
                        }
                        
                        
                }
                
                # unigram here
                nw <- as.character( freqs[[1]][ sample(1:nrow(freqs[[1]]),1), "lastToken" ] ) # randomly pick one from the top unigrams
                
                if(!is.na(nw)) {
                        # substitute as . if end of sentence (not really needed
                        # as would not happen given the above logic) 
                        # or put a capital I if i
                        if(nw=="</s>") nw <- "."
                        if(nw=="i") nw <- "I"
                        
                        return (nw)
                }
                
                
        } else { 
                # not really needed as there will always be <s> ...  
        }
        
        
}