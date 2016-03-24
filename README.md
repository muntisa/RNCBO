RNCBO
======

Mapping ontology terms using NCBO recommender

eNanoMapper Developers |  [eNanoMapper Project] (http://www.enanomapper.net/)

The script is using RCurl and rjson in order to map a list of words to ontology terms using the scores from [NCBO recommender] (http://bioportal.bioontology.org/recommender).

You should add your API KEY into the script in order obtain results!

The script flow:
- Reading the terms to be mapped from input CSV file (one column) and a list of possible ontologies
- Use of NCBO REST Recommender to obtain the list of ontologies (ordered by score, filtered for errors)
- Use of NCBO REST Search to obtain detailed information about the mappings
- CSV output with results for mapped terms: Term, Score, Acronym, Ontology, ID, Notation, Description, Synonyms
- CSV list with non mapped terms

The R script is presented with examples of files for terms and possible ontologies. Please check the main R script.

Other details about the NCBO APIs: [http://data.bioontology.org/documentation] (http://data.bioontology.org/documentation)

Other example using different languages (Python, Java, bash, etc.): [https://github.com/ncbo/ncbo_rest_sample_code] (https://github.com/ncbo/ncbo_rest_sample_code). The recommender algorithm is presented into publication with [DOI: 10.1186/2041-1480-1-S1-S1] (http://www.ncbi.nlm.nih.gov/pubmed/20626921) (PMID: 20626921, PMCID: PMC2903720).


Thank you for the help of Marcos Martinez-Romero from University of Coruña, RNASA-IMEDIR group.

This version corresponds to the new NCBO APIs and it was updated by Manfred Kohler.
