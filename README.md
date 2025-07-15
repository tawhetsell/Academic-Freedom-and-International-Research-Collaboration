# Academic-Freedom-and-International-Research-Collaboration
Code and data for "Academic Freedom and International Research Collaboration"

QSS_WOS_Parse_Countries_Matrices.py parses through the WOS XML raw data and generates 30 one mode symetrical matrices for every author affiliation country co-occurence by each year from 1993 to 2022. Cell values of each matrix are populated by instances when two or more countries co-occur on a paper. The script also attempts to standardize names of countries as well as possible prior to disambiguation. Three sets of matrices are generated based on the WOS XML subject headings, ST, SS, and AH. 

QSS_WOS_Country_Disambigation.py disambiguates the country names in the matrices produced by the previous script. A full list of various country spelling was identified from all of the matrixes and then paired to standardized three letter iso codes in a disambigation dictionary. ChatGPT 4 and then later o3-mini-high was used to generate the dictionary pairings. 

Matrices folder contains the disambiguated matrices for ST, SS, and AH.

Covariates folder contains other covariates, many of which are imported via API. However, they are preserved here for reproduceability in case of changes to those repositories. 

QSS_RSiena_Models_T1.R imports the disambiguated matrixes and other covariate data found in the Covariates folder. It then conducts several preliminary analytical procedures, including Figure 1 and Table 2, conducts the primary rsiena models found Table 3 of the paper, and also conducts the GOF tests, represented in the appendix figure. It also includes some latex code for generating tables for the paper.

QSS_RSiena_Models_T2.R imports data then conducts rsiena models found in Table 4 of the paper

QSS_RSiena_Models_T3.R imports data then conducts rsiena models found in Table 5 of the paper

QSS_Robustness_Checks_Table_A1.R imports data then conducts a varietry of robustness checks, which are similar models with varying models specificiations, found in the first appendix table.

QSS_Gravity_Models_Table_A2.R imports data then conducts the alternative gravity model tests found in the second appendix table.

QSS_Network_Visualization_F2.R creates the network visualization represented in Figure 2
