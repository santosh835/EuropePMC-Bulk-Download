# This R script will bulk downlaod EuropePMC data registry by Investigator/Author names
# EuropePMC contains close to 33 million publication records
# This code will separate author data,publication data,journal data, funding data into different dataframes


library(europepmc)
library(sqldf)
library(data.table)


author_df <- 'load your dataframe with investigator full names'

author_df$FULL_NAME <- iconv(author_df$FULL_NAME , "utf-8", "ascii//translit")  

mylist <- list()

for(i in 1:nrow(author_df)){
  cat(i)
  tryCatch({
    
    my_list1 <- epmc_search(query = author_df$FULL_NAME[i], limit=epmc_hits(author_df$FULL_NAME[i]), output = 'raw')
    
    mylist <- c(mylist,my_list1)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}




# ******************************************************************************
# Main Function to convert the list to dataframe

tl <- function(e) { if (is.null(e)) return(NULL); ret <- typeof(e); if (ret == 'list' && !is.null(names(e))) ret <- list(type='namedlist') else ret <- list(type=ret,len=length(e)); ret; };
mkcsv <- function(v) paste0(collapse=',',v);
keyListToStr <- function(keyList) paste0(collapse='','/',sapply(keyList,function(key) if (is.null(key)) '*' else paste0(collapse=',',key)));

extractLevelColumns <- function(
  nodes, ## current level node selection
  ..., ## additional arguments to data.frame()
  keyList=list(), ## current key path under main list
  sep=NULL, ## optional string separator on which to join multi-element vectors; if NULL, will leave as separate columns
  mkname=function(keyList,maxLen) paste0(collapse='.',if (is.null(sep) && maxLen == 1L) keyList[-length(keyList)] else keyList) ## name builder from current keyList and character vector max length across node level; default to dot-separated keys, and remove last index component for scalars
) {
  tryCatch({
    
    cat(sprintf('extractLevelColumns(): %s\n',keyListToStr(keyList)));
    if (length(nodes) == 0L) return(list()); ## handle corner case of empty main list
    tlList <- lapply(nodes,tl);
    typeList <- do.call(c,lapply(tlList,`[[`,'type'));
    if (length(unique(typeList)) != 1L) stop(sprintf('error: inconsistent types (%s) at %s.',mkcsv(typeList),keyListToStr(keyList)));
    type <- typeList[1L];
    if (type == 'namedlist') { ## hash; recurse
      allKeys <- unique(do.call(c,lapply(nodes,names)));
      ret <- do.call(c,lapply(allKeys,function(key) extractLevelColumns(lapply(nodes,`[[`,key),...,keyList=c(keyList,key),sep=sep,mkname=mkname)));
    } else if (type == 'list') { ## array; recurse
      lenList <- do.call(c,lapply(tlList,`[[`,'len'));
      maxLen <- max(lenList,na.rm=T);
      allIndexes <- seq_len(maxLen);
      ret <- do.call(c,lapply(allIndexes,function(index) extractLevelColumns(lapply(nodes,function(node) if (length(node) < index) NULL else node[[index]]),...,keyList=c(keyList,index),sep=sep,mkname=mkname))); ## must be careful to translate out-of-bounds to NULL; happens automatically with string keys, but not with integer indexes
    } else if (type%in%c('raw','logical','integer','double','complex','character')) { ## atomic leaf node; build column
      lenList <- do.call(c,lapply(tlList,`[[`,'len'));
      maxLen <- max(lenList,na.rm=T);
      # maxLen <- 25
      
      if (is.null(sep)) {
        ret <- lapply(seq_len(maxLen),function(i) setNames(data.frame(sapply(nodes,function(node) if (length(node) < i) NA else node[[i]]),...),mkname(c(keyList,i),maxLen)));
      } else {
        ## keep original type if maxLen is 1, IOW don't stringify
        ret <- list(setNames(data.frame(sapply(nodes,function(node) if (length(node) == 0L) NA else if (maxLen == 1L) node else paste(collapse=sep,node)),...),mkname(keyList,maxLen)));
      }; ## end if
    } else stop(sprintf('error: unsupported type %s at %s.',type,keyListToStr(keyList)));
    if (is.null(ret)) ret <- list(); ## handle corner case of exclusively empty sublists
    ret;
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}; ## end extractLevelColumns()
## simple interface function
flattenList <- function(mainList,...) do.call(cbind,extractLevelColumns(mainList,...));

#########################################################################################################################################

## Execution

#Author Data
#**************************************************************

Author_data_list <- lapply(mylist, function(x) {x[c("id","pmid","authorString","authorList","authorIdList")]})

Author_data <- flattenList(Author_data_list)

write.csv(Author_data, file="Author_data.csv")

rm(Author_data_list,Author_data)

##########################################################################################################3
#Journal Data
############################################################################################################

Journal_data_list <- lapply(mylist, function(x) {x[c("id","pmid","journalInfo")]})

Journal_data <- flattenList(Journal_data_list)

write.csv(Journal_data, file="Journal_data.csv")

rm(Journal_data_list,Journal_data)
#################################################################################################

#Publication Data
############################################################################################################

Publication_data_list <- lapply(mylist, function(x) {x[c("id","pmid","source","title","pubYear","pubTypeList","abstractText","affiliation","language","pubModel","isOpenAccess","inEPMC","inPMC","pmcid","hasPDF","hasBook","doi","citedByCount","dateOfCreation","dateOfRevision","electronicPublicationDate","firstPublicationDate","fullTextUrlList","bookOrReportDetails","pageInfo","dateOfCompletion","hasReferences","hasTextMinedTerms","hasDbCrossReferences","hasLabsLinks","authMan","epmcAuthMan","nihAuthMan","hasTMAccessionNumbers","manuscriptId","embargoDate","hasSuppl","license","chemicalList","tmAccessionTypeList","dbCrossReferenceList","subsetList")]})

Publication_data <- flattenList(Publication_data_list)

write.csv(Publication_data, file="Publication_data.csv")

rm(Publication_data_list,Publication_data)
#################################################################################################


#Investigator Data
############################################################################################################

Investigator_data_list <- lapply(mylist, function(x) {x[c("id","pmid","investigatorList")]})

Investigator_data <- flattenList(Investigator_data_list)

write.csv(Investigator_data, file="Investigator_data.csv")

rm(Investigator_data_list,Investigator_data)
#################################################################################################


#Mesh_Keyword Data 
############################################################################################################

Mesh_Keyword_data_list <- lapply(mylist, function(x) {x[c("id","pmid","keywordList","meshHeadingList")]})

Mesh_Keyword_data <- flattenList(Mesh_Keyword_data_list)

write.csv(Mesh_Keyword_data, file="Mesh_Keyword_data.csv")

rm(Mesh_Keyword_data_list,Mesh_Keyword_data)
#################################################################################################


#Funding_data
############################################################################################################

Funding_data_list <- lapply(mylist, function(x) {x[c("id","pmid","grantsList")]})

Funding_data <- flattenList(Funding_data_list)
write.csv(Funding_data, file="Funding_data.csv")

rm(Funding_data_list,Funding_data)
#################################################################################################

#End of creation of data frames
######################################################################################################################





###############################################################################################
###############################################################################################
# Save data for the authors 26 and beyond

extractLevelColumns_26 <- function(
  nodes, ## current level node selection
  ..., ## additional arguments to data.frame()
  keyList=list(), ## current key path under main list
  sep=NULL, ## optional string separator on which to join multi-element vectors; if NULL, will leave as separate columns
  mkname=function(keyList,maxLen) paste0(collapse='.',if (is.null(sep) && maxLen == 1L) keyList[-length(keyList)] else keyList) ## name builder from current keyList and character vector max length across node level; default to dot-separated keys, and remove last index component for scalars
) {
  tryCatch({
    
    cat(sprintf('extractLevelColumns(): %s\n',keyListToStr(keyList)));
    if (length(nodes) == 0L) return(list()); ## handle corner case of empty main list
    tlList <- lapply(nodes,tl);
    typeList <- do.call(c,lapply(tlList,`[[`,'type'));
    if (length(unique(typeList)) != 1L) stop(sprintf('error: inconsistent types (%s) at %s.',mkcsv(typeList),keyListToStr(keyList)));
    type <- typeList[1L];
    if (type == 'namedlist') { ## hash; recurse
      allKeys <- unique(do.call(c,lapply(nodes,names)));
      ret <- do.call(c,lapply(allKeys,function(key) extractLevelColumns_26(lapply(nodes,`[[`,key),...,keyList=c(keyList,key),sep=sep,mkname=mkname)));
    } else if (type == 'list') { ## array; recurse
      lenList <- do.call(c,lapply(tlList,`[[`,'len'));
      maxLen <- max(lenList,na.rm=T);
      if(maxLen < 26) return(list());
      allIndexes <- seq(from=26,to=maxLen);
      ret <- do.call(c,lapply(allIndexes,function(index) extractLevelColumns_26(lapply(nodes,function(node) if (length(node) < index) NULL else node[[index]]),...,keyList=c(keyList,index),sep=sep,mkname=mkname))); ## must be careful to translate out-of-bounds to NULL; happens automatically with string keys, but not with integer indexes
    } else if (type%in%c('raw','logical','integer','double','complex','character')) { ## atomic leaf node; build column
      lenList <- do.call(c,lapply(tlList,`[[`,'len'));
      maxLen <- max(lenList,na.rm=T);
      # maxLen <- 25
      
      if (is.null(sep)) {
        ret <- lapply(seq_len(maxLen),function(i) setNames(data.frame(sapply(nodes,function(node) if (length(node) < i) NA else node[[i]]),...),mkname(c(keyList,i),maxLen)));
      } else {
        ## keep original type if maxLen is 1, IOW don't stringify
        ret <- list(setNames(data.frame(sapply(nodes,function(node) if (length(node) == 0L) NA else if (maxLen == 1L) node else paste(collapse=sep,node)),...),mkname(keyList,maxLen)));
      }; ## end if
    } else stop(sprintf('error: unsupported type %s at %s.',type,keyListToStr(keyList)));
    if (is.null(ret)) ret <- list(); ## handle corner case of exclusively empty sublists
    ret;
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} ## end extractLevelColumns()

## simple interface function
flattenList <- function(mainList,...) do.call(cbind,extractLevelColumns_26(mainList,...));


############################################################
####END OF FUNCTION
#############################################################


#Investigator Data FOR 26TH iNVESTIGATOR AND BEYOND
############################################################################################################

Investigator_data_list <- lapply(mylist, function(x) {x[c("id","pmid","investigatorList")]})

Investigator_data <- flattenList(Investigator_data_list)

write.csv(Investigator_data, file="Investigator_data.csv")

rm(Investigator_data_list,Investigator_data)
#################################################################################################

#Author Data
#**************************************************************

Author_data_list <- lapply(mylist, function(x) {x[c("id","pmid","authorString","authorList","authorIdList")]})

Author_data <- flattenList(Author_data_list)

write.csv(Author_data, "Author_data.csv")

rm(Author_data_list,Author_data)

##########################################################################################################3






