# Habitat-Loss-Simulations

Code and associated data files for the simulations and analyses presented in Sandor et al. (in revision) Extinction of biotic interactions due to habitat loss could accelerate the current biodiversity crisis

"Scenario 1" is the code used for the area loss simulation for the exponential area loss in an empirical network scenario (scenario 1).
"Scenario 2" is the code used for the area loss simulation for the exponential area loss in synthetic networks scenario (scenario 2).
"Scenario 3" is the code used for the area loss simulation for the variable rate of linear area loss in synthetic networks scenario (scenario 3).
"Regression fitting for scenarios 2 and 3" is the code used for fitting regressions to the probability and number of orphaned species produced in scenarios 2 and 3.

Please note that there are stochastic elements to scenarios 2 and 3 and simply running "Scenario 2" and "Scenario 3" code will not produce identical results to those presented in the paper.


Data files associated with Scenario 1 code:

FullNetworkpresabs.csv: Full bipartite network for frugivorous birds and fruit-producing shrubs and small trees in the Sierra Nevada Mountains, California, USA. A "1" indicates that the given bird species (column) interacts with the given plant species (row). Bird species are listed by their standardized four-letter codes (https://www.birdpop.org/docs/misc/Alpha_codes_eng.pdf), the translation of which can be found in the "BirdNames.csv" file. Plant species are listed by the first three letters of their genus name combined with the first three letters of their species name (e.g. Amealn is Amelanchier alnifolia), the translation of which can be found in the "PlantNames.csv" file. 

BirdNames.csv: Common name and four letter name codes for all bird species in the full seed dispersal network.

BirdsGridsJoin.csv.zip: Large file, must be unzipped before using. All bird occurrences pulled from eBird. Some eBird columns were preserved. Beginning after "TIME.OBSER," columns list the left, top, right, and bottom coordinates and ID of the grid cell in which the occurrence is located, in each of the grid resolution sizes for the area loss simulations. These data are pared down to only species found in the seed dispersal network in the code presented in "Scenario 1." 

GridPathForAnalysis.csv: Left, top, right, and bottom coordinates, along with associated ID, of the grid cells set up for the area loss simulations. These constitute habitat loss pathways. Only 129 of these habitat loss pathways are used in the area loss simulations, due to patchy distribution of occurrence records across the study area.

PlantNames.csv: Scientific name and six letter name codes for all plant species in the full seed dispersal network.

PlantsGridsJoin.csv: Large file. All plant occurrences pulled from the Global Biodiversity Information Facility (GBIF), after cleaning. Many GBIF columns were preserved. Beginning after "optional," columns list the left, top, right, and bottom coordinates and ID of the grid cell in which the occurrence is located, in each of the grid resolution sizes for the area loss simulations. These data are pared down to only species found in the seed dispersal network in the code presented in "Scenario 1." 


Input data are not needed for running scenarios 2 and 3.


Input data for "Regression fitting for scenarios 2 and 3":

