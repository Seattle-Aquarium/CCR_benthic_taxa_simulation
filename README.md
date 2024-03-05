# benthic_taxa_simulation
## Introduction
This repo provides code to simulate data derived from the Seattle Aquarium's remotely operated vehicle (ROV) kelp forest survey program (see [here](https://github.com/zhrandell/Seattle_Aquarium_ROV_development)). Specifically, we include code to simulate data derived from downward- and forward-facing cameras mounted on the ROV. Our purpose here is to provide simulated data to feed to a bull kelp habitat suitability model (linked [here](https://experience.arcgis.com/experience/b11daaa83ff045f1a9d88b2b926e1f75/page/About/)) that is being modified by our team to further advance bull kelp conservation and restoration.  

The downward-facing camera is used to characterize percent-coverage of aggregate taxa such as fleshy red algae, brown algae, and substrate type. Imagery are analyzed in CoralNet to generate metrics of percent-coverage. Here, our code simulates data derived from CoralNet, where our columns all sum to `col_sum = 50`, mirroring the x50 percent-coverage data points classified in every image. 

Imagery from the forward-facing camera is used to classify discrete and individually specific objects encountered by the ROV such as bull kelp stipes, kelp crabs, and fishes. Here, our code provides the means to simulate and append these data to the simulated percent-coverage data. 

Finally, we simulate these data in a manner that approximates our real-world ROV surveys from the Urban Kelp Research Project with the Port of Seattle. That is to say, our simulated benthic community data have the exact structure (in terms of the number of rows representing sites and transects) as our summer 2023 Urban Kelp ROV surveys. 

In terms of content, we include the following: 
* [code](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/code) contains _R_ scripts to combine and clean our real-world ROV telemetry files in preparation to append it to our simulated data ([`wrangle_metadata.R`](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/wrangle_metadata.R)), as well as code necessary to simulate percent-coverage and abundance data ([`simulate_taxa.R`](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/CoralNet_simulation.R)).
* [data_input](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/data_input) contains our Urban Kelp ROV telemetry files from summer 2023.
* [data_output](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/data_output) contains all derived files from our analyses, including the combined metadata.csv file we append to our simulated data.    


Below we lay out a brief overview of our code and how to implement it. 


