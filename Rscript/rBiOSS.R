# rBiOSS - retrieve the URIs for terms using ontology recommenter REST API  from
# http://bioportal.bioontology.org/recommender
# by Cristian R Munteanu | muntisa [at] gmail [dot] com
# ----------------------------------------------------------------------------------------------------

library(RCurl)
library(rjson)
library(stringr)

# --------- FUNCTIONS --------------------------------------------------------------------------------
NCBIOgetURIsFromJSON <- function(sTerm,sOntology,CampaignJSON) { # Get df with the URIs for {term, ontology}
  JSONList <- fromJSON(CampaignJSON)
  results <- JSONList$collection
  return(results[1][[1]]$`@id`) # return URI
}

# ----------------------------------------------------------------------------------------------------
NCBIRecommenderFromJSON <- function(sTerm,CampaignJSON) { # Get df with all ontologies for one term
  scores <- NULL   # list of recommender scores for ontologies and a specific term
  acronyms <- NULL # list of ontology acronyms
  oURIs <- NULL    # URIs for ontologies
  terms <- NULL    # list of terms

  JSONList <- fromJSON(CampaignJSON)
  results <- JSONList
  RecomenderDF <- NULL
  
  # CHECKING ERRORS !!!!
  CollapseJSONList=as.character(paste(JSONList, collapse = '')[[1]])
  if ( !is.null(JSONList) & CollapseJSONList != "") {    # if the JSON list is NULL
    if ( grep("score",CollapseJSONList[[1]]) == 1 ){     # if there is any result and we can find "score" inside it
      for (i in 1:length(results)) {                              # for each term in the REST result
        terms <- c(terms,sTerm)                                    # use the same term for all results 
        scores <- c(scores, results[i][[1]]$score)                 # get score for each ontology
        acronyms <- c(acronyms, results[i][[1]]$ontology$acronym)  # get acronym for each ontology
        oURIs <- c(oURIs, results[i][[1]]$ontology$links$ui)       # get ontology URI for each ontology
        RecomenderDF = data.frame(Term=terms,Score=scores,Ontology=acronyms,Ontology_URI=oURIs,stringsAsFactors=FALSE) # create a df from results
      } 
    }
  }
  return(RecomenderDF) # return a data frame with the results: Term, score, ontology acronym, URI (if errors, it is NULL!!!)
}

# ----------------------------------------------------------------------------------------------------
BiOSSmapper <- function(sTermFile,sResultFile,apikey) { # Mapping terms using BiOSS recomender
  # Read TERMS
  # -------------------------------------------------------------
  dfTerms=read.csv(sTermFile,header=F)       # read term file
  dfResults <- NULL                          # final results as data frame
  write(paste("Term","Score","Acronym","Ontology","Term_URI", sep=","), file=sResultFile, append=F) # create the result file with the header
  
  # Process each term
  # ----------------------------------------------------------------------------------------------------
  for (t in 1:dim(dfTerms)[1]) {
    CurrTerm=as.character(dfTerms[t,1]) # current term to process
    cat("#",t,"from",dim(dfTerms)[1],":",CurrTerm,"\n")
    # ----------------------------------------------------------------------------------------------------
    # Recommend the ontologies
    # ----------------------------------------------------------------------------------------------------
    # http://data.bioontology.org/recommender?text=nanoparticle
    sURL=sprintf("http://data.bioontology.org/recommender?text=%s&apikey=%s",URLencode(CurrTerm),apikey)
    CampaignJSON = getURL(sURL)
    cat(sURL,"\n")
    
    res = NCBIRecommenderFromJSON(CurrTerm,CampaignJSON) # get recommended ontologies from BiOSS
    if ( !is.null(res) ) {                               # if there is a result from recommender (not NULL)
      # complete with the term URI in each recommended ontology
      tURIs <- NULL    # list of term URIs
      for (r in 1:dim(res)[1]) {
        iOntology=res[r,3]
        # ----------------------------------------------------------------------------------------------------
        # GET URI for the term in this ontology
        # ----------------------------------------------------------------------------------------------------
        # Search one term into a specific ontology:
        # ex: http://data.bioontology.org/search?q=nanoparticle&ontologies=NPO&exact_match=true
        sURL2=sprintf("http://data.bioontology.org/search?q=%s&ontologies=%s&exact_match=true&apikey=%s",URLencode(CurrTerm),iOntology,apikey)
        CampaignJSON2 = getURL(sURL2)
        iURI=NCBIOgetURIsFromJSON(CurrTerm,iOntology,CampaignJSON2)
        if (length(iURI) == 0){
          iURI="ERROR!" # if there is no URI for a term into an ontology! Correcting the Recommender!
        }
        tURIs <- c(tURIs, iURI) # get term URI for all recommended ontologies
      }
      
      dfResults <- rbind(dfResults,data.frame(res,Term_URI=tURIs)) # append the results to the final data frame including errors
      dfFiltered <- subset(dfResults, Term_URI !='ERROR!')         # filter the results
      if ( dim(dfFiltered)[1] != 0 ){                              # if there is any result without ERRORS
        write.table(dfFiltered, file=sResultFile, append=T, row.names=F, col.names=F,  sep=",") # append results for each term without errors!
        cat(dim(dfFiltered)[1]," mappings\n")
      }
      else {
        cat("--> No mapping!\n")
      }
    }
    else {
      cat("--> BiOOS recommender error!\n")
    }
  }
}

#########################################################################################################
# MAIN
#########################################################################################################

# ----------------------------------------------------------------------------------------------------
# PARAMETERS
# ----------------------------------------------------------------------------------------------------

apikey="d1f81232-5447-46af-ad13-6c9c9e5c1da8" # !!! ADD YOUR API KEY HERE !!!

sTermFile   = "rBiOSS_Terms.csv"              # input files with the TERMS to map
sResultFile = "rBiOSS_Results.csv"            # output file with the results

cat("\n============================================================\n")
cat("rBiOOS - ontology Mapping using Bioportal Recommender\n")
cat("============================================================\n")
cat("by Cristian R Munteanu | muntisa [at] gmail [dot] com\n\n")
cat("Running ... please wait ...\n")
BiOSSmapper(sTermFile,sResultFile,apikey)     # mapping to ontology terms
cat("\nDone!\n")
