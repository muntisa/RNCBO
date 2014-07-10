RNCBO
======

Mapping ontology terms using NCBO recommender

[BiGCaT] (http://www.bigcat.unimaas.nl/) - Maastricht University |  [eNanoMapper Project] (http://www.enanomapper.net/)

Thank you for the help of Marcos Martinez-Romero from University of Coruña, RNASA-IMEDIR group.

The script is using RCurl and rjson in order to map a list of words to ontology terms using the scores from [NCBO recommender] (http://bioportal.bioontology.org/recommender).

You should add your API KEY into the script in order obtain results!

The script flow:
- TERMS <- Input CSV file
- Recommended ONTOLOGIES <- NCBIRecommenderFromJSON (ordered by score, filtered for errors)
- URIs for TERMS<- NCBIOgetURIsFromJSON
- CSV Results for mapped terms: Term | Score | Acronym | Ontology | Term_URI
- CSV list with non mapped terms:: Terms
