# RNCBO - Mapping terms using NCBO Recommender & Search tools
# http://bioportal.bioontology.org/recommender
# eNanoMapper Developers | enanomapper.net

# Contact: muntisa [at] gmail [dot] com

# Input file    = CSV one column list of terms to bne mapped
# Output result = TAB-separated files with "Term","Score","Acronym","Ontology","ID","Notation","Description","Synonyms" for mapped terms
#                 and CSV for non-mapped terms

# Notes:
# - only the REST Recommender and Search have been used to extract information (some information could not exist into results)
# - some fields such as Description and Synonyms could contain multiple items separated by "|"

# ----------------------------------------------------------------------------------------------------------------------------------

library(RCurl)
library(rjson)
library(stringr)

# --------- FUNCTIONS --------------------------------------------------------------------------------------------------------------
NCBIOgetFieldsFromJSON <- function(CampaignJSON) {
  # Get NCBO REST ontology information for 1 term from 1 ontology using JSON from NCBO REST Search
  
  JSONList <- fromJSON(CampaignJSON)    # convert JSON object into a list
  results  <- JSONList$collection       # get collection class that includes descriptions for one term
  
  sId  <- c(results[1][[1]]$`@id`)      # get ID
  if (is.null(sId)) {                   # if there is no synonym class
    sId <- c("ERROR!")                  # pointing errors in Recommender with no Search values
  } else { sId <- c(paste(sId, collapse = '|')) } # colapse the possible list of IDs into one string separated by "|"  
  
  sNotation <- c(results[1][[1]]$notation)  # get Synonyms
  if (is.null(sNotation)) {             # if there is no synonym class
    sNotation <- c("N/A")
  } else { sNotation <- c(paste(sNotation, collapse = '|')) } # colapse the possible list of synonyms into one string separated by "|"  
  
  sDef <- c(results[1][[1]]$definition) # get Definitions
  if (is.null(sDef)) { # if there is no definition class
    sDef <- c("N/A")
  } else { sDef <- c(str_replace(str_replace(paste(sDef, collapse = '|'),"\n",""),"\n","")) }
  # colapse the possible list of descriptions into one string separated by "|"; "\n" and "\n\n" are eliminated from definitions!
  
  sSynonyms <- c(results[1][[1]]$synonym)  # get Synonyms
  if (is.null(sSynonyms)) {             # if there is no synonym class
    sSynonyms <- c("N/A")
  } else { sSynonyms <- c(paste(sSynonyms, collapse = '|')) } # colapse the possible list of synonyms into one string separated by "|"
   
  return(c(sId,sNotation,sDef,sSynonyms))
  # Return a vector of field information for [ID, sNotation, definition, synonyms]
}

# --------------------------------------------------------------------------------------------------------------------------
NCBIOgetURIsFromJSON <- function(CampaignJSON) {
  # Get 1 URI for 1 term from 1 ontology using JSON from NCBO REST Search
  # This function is not used in this version but it could be included in the future R package
  
  JSONList <- fromJSON(CampaignJSON) # convert JSON object into a list
  results <- JSONList$collection     # get collection class that includes descriptions for one term
  return(results[1][[1]]$`@id`)
  # return URI
}

# ---------------------------------------------------------------------------------------------------------------------------------------
NCBIRecommenderFromJSON <- function(sTerm,CampaignJSON) {
  # Get NCBO Recommender ontologies for 1 term (including scores) as data frame using NSBO Recommender JSON object
  
  scores <- NULL   # vector for recommender scores for all ontologies and 1 term
  acronyms <- NULL # vector of ontology acronyms
  oURIs <- NULL    # vector of ontology URIs
  terms <- NULL    # vector of with all values as 1 term to be mapped

  JSONList <- fromJSON(CampaignJSON) # convert JSON object to list
  results  <- JSONList               # get results
  RecomenderDF <- NULL               # initialize the Recommender data frame = the output of the function!
  
  # CHECKING ERRORS
  CollapseJSONList=as.character(paste(JSONList, collapse = '')[[1]])   # transform JSON list into a string
  if ( !is.null(JSONList) & CollapseJSONList != "") {    # if the JSON list is NULL
    if ( grep("score",CollapseJSONList[[1]]) == 1 ){     # if there is any result and we can find "score" inside it
      for (i in 1:length(results)) {                              # for each term in the REST result
        terms <- c(terms,sTerm)                                    # use the same term for all results 
        scores <- c(scores, results[i][[1]]$score)                 # get score for each ontology
        acronyms <- c(acronyms, results[i][[1]]$ontology$acronym)  # get acronym for each ontology
        oURIs <- c(oURIs, results[i][[1]]$ontology$links$ui)       # get ontology URI for each ontology
        RecomenderDF = data.frame(Term=terms,Score=scores,Ontology=acronyms,Ontology_URI=oURIs,stringsAsFactors=FALSE) # create a data frame with results
      } 
    }
  }
  return(RecomenderDF)
  # return a data frame with the results: Term, score, ontology acronym, URI (if errors, it is NULL!!!)
}

# ----------------------------------------------------------------------------------------------------
NCBOmapper <- function(sTermFile,sResultFile,apikey) {
  # Mapping terms using NCBIO REST Recomender and Seach tools
  
  # Read TERMS
  # -------------------------------------------------------------
  dfTerms=read.csv(sTermFile,header=F)       # read term file
  dfTerms <- unique(dfTerms)                 # create unique list of the terms
  dfResults <- NULL                          # initiate the data frame from the final results
  write(paste("Term","Score","Acronym","Ontology","ID","Notation","Description","Synonyms", sep="\t"), file=sResultFile, append=F) # create the result file with the header
  sErrorFile = paste(sResultFile,"NonMapped.csv",sep="")    # create a name for the error file
  write("NON-MAPPED", file=sErrorFile, append=F)            # create the error file with non-mapped terms
  
  # Mapping of each term from the input file
  # ----------------------------------------------------------------------------------------------------
  for (t in 1:dim(dfTerms)[1]) {
    dfFiltered <- NULL                  # initialize data frame with filtered results without errors)
    dfResults  <-NULL                   # initialize data frame with results including errors
    CurrTerm=as.character(dfTerms[t,1]) # get current term to process
    cat("#",t,"from",dim(dfTerms)[1],":",CurrTerm,"\n") # print the term index and the term to be mapped
    
    # ----------------------------------------------------------------------------------------------------
    # NCBO REST Recommender Tool
    # ----------------------------------------------------------------------------------------------------
    # http://data.bioontology.org/recommender?text=nanoparticle
    
    # NCBO REST Recommender command for 1 term
    sURL=sprintf("http://data.bioontology.org/recommender?text=%s&apikey=%s",URLencode(CurrTerm),apikey)
    CampaignJSON = getURL(sURL)  # get the NCBO REST Recommender result as JSON object
    cat("REST Recommender: ",sURL,"\n")               # print NCBO REST Recommender command
    
    res = NCBIRecommenderFromJSON(CurrTerm,CampaignJSON) # get NCBO recommended ontologies as JSON object
    if ( !is.null(res) ) {                               # if there is a result from NCBO Recommender (not NULL)
      # Find the URI of the current term in each recommended ontology
      tURIs <- NULL               # list of URIs for 1 term
      tNots <- NULL               # list of Notations for 1 term
      tDefs <- NULL               # list of Definitions for 1 term
      tSynons <- NULL             # list of Synonyms for 1 term
      for (r in 1:dim(res)[1]) {  # analyse each recommender ontology by NCBO REST Recommender 
        iOntology=res[r,3]
        
        # ----------------------------------------------------------------------------------------------------
        # GET URI for the term in this ontology
        # ----------------------------------------------------------------------------------------------------
        # Search one term into a specific ontology:
        # ex: http://data.bioontology.org/search?q=nanoparticle&ontologies=NPO&exact_match=true
        
        # NCBO REST Search command for 1 term from 1 ontology (exact mach)
        sURL2=sprintf("http://data.bioontology.org/search?q=%s&ontologies=%s&exact_match=true&include=synonym,definition,notation&include_context=false&include_links=false&apikey=%s",URLencode(CurrTerm),iOntology,apikey)
        # the link was optimised to be faster
        
        # cat("REST Search: ",sURL2,"\n")               # print NCBO REST Recommender command
        CampaignJSON2 = getURL(sURL2)                 # get Search result as JSON object
        
        iInfo=NCBIOgetFieldsFromJSON(CampaignJSON2)   # NEW INFO !!!!!!!
        
        tURIs   <- c(tURIs,   iInfo[[1]]) # add IDs for 1 term and all recommended ontologies
        tNots   <- c(tNots,   iInfo[[2]]) # add Notations for 1 term and all recommended ontologies
        tDefs   <- c(tDefs,   iInfo[[3]]) # add Definitions for 1 term and all recommended ontologies
        tSynons <- c(tSynons, iInfo[[4]]) # add Synonyms for 1 term and all recommended ontologies    
      }
     
      dfResults <- rbind(dfResults,data.frame(res,ID=tURIs,Notations=tNots,Definition=tDefs,Synonyms=tSynons)) # append the URIs to Recommended ontologies -> data frame including errors
      dfFiltered <- subset(dfResults, ID !='ERROR!')         # filter the results
      if ( dim(dfFiltered)[1] != 0 ){                        # if there is any result without ERRORS
        write.table(dfFiltered, file=sResultFile, append=T, row.names=F, col.names=F,  sep="\t") # append to output file results without errors for 1 term
        cat(dim(dfFiltered)[1]," mappings\n") # printing the number of mapped ontologies for 1 term
      }
      else {
        cat("--> No mapping!\n")
        write(CurrTerm, file=sErrorFile, append=T) # create the error file with non-mapped terms
      }
    }
    else {
      cat("--> NCBO recommender error!\n")
    }
  }
  
}

#########################################################################################################
# MAIN
#########################################################################################################

# ----------------------------------------------------------------------------------------------------
# PARAMETERS
# ----------------------------------------------------------------------------------------------------

apikey="" # !!! ADD YOUR API KEY HERE !!!

sTermFile   = "RNCBO_Terms.csv"              # input files with the TERMS to map
sResultFile = "RNCBO_Results.tab"      # output file with the results, TAB-separated values

cat("\n============================================================\n")
cat("RNCBO - ontology Mapping using Bioportal Recommender\n")
cat("============================================================\n")
cat("eNanoMapper Developers | enanomapper.net | enanomapper.net | muntisa [at] gmail [dot] com\n\n")
cat("Running ... please wait ...\n")
 
print(system.time(NCBOmapper(sTermFile,sResultFile,apikey))) # mapping to ontology terms 
