RNCBO
======

Mapping ontology terms using NCBO recommender

by Cristian R Munteanu | muntisa [at] gmail [dot] com

[BiGCaT] (http://www.bigcat.unimaas.nl/) - Maastricht University

[eNanoMapper Project] (http://www.enanomapper.net/)

Thank you for the help of Marcos Martinez-Romero from University of Coruña, RNASA-IMEDIR group.

The script is using RCurl and rjson in order to map a list of words to ontology terms using the scores from [NCBO recommender] (http://bioportal.bioontology.org/recommender). All the mappings are lister and some errors are removed from the results.

Addition error corrections should be included. The input and output files are CSV format.

You should add your API KEY into the script in order obtain results!

The script flow:
- TERMS <- Input CSV file
- Recommended ONTOLOGIES <- NCBIRecommenderFromJSON (ordered by score, filtered for errors)
- URIs for TERMS<- NCBIOgetURIsFromJSON
- Results as CSV: Term | Score | Acronym | Ontology | Term_URI
