RNCBO
======

Mapping ontology terms using NCBO recommender

eNanoMapper Developers |  [eNanoMapper Project] (http://www.enanomapper.net/)

The script is using RCurl and rjson in order to map a list of words to ontology terms using the scores from [NCBO recommender] (http://bioportal.bioontology.org/recommender).

You should add your API KEY into the script in order obtain results!

The script flow:
- Reading the terms to be mapped from input CSV file (one column)
- Use of NCBO REST Recommender to obtain the list of ontologies (ordered by score, filtered for errors)
- Use of NCBO REST Search to obtain detailed information about the mappings
- TAB-delimited results for mapped terms: Term, Score, Acronym, Ontology, ID, Notation, Description, Synonyms
- CSV list with non mapped terms


Other details about the NCBO APIs: [http://data.bioontology.org/documentation] (http://data.bioontology.org/documentation)

Other example using different languages (Python, Java, bash, etc.): [https://github.com/ncbo/ncbo_rest_sample_code] (https://github.com/ncbo/ncbo_rest_sample_code)


Thank you for the help of Marcos Martinez-Romero from University of Coruña, RNASA-IMEDIR group.
