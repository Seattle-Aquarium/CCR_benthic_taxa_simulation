# benthic_taxa_simulation
## Introduction
This repo provides code to simulate data derived from the Seattle Aquarium's remotely operated vehicle (ROV) kelp forest survey program (see [here](https://github.com/zhrandell/Seattle_Aquarium_ROV_development)). Specifically, we include code to simulate data derived from downward- and forward-facing cameras mounted on the ROV. Our purpose here is to provide simulated data to feed to a bull kelp habitat suitability model (linked [here](https://experience.arcgis.com/experience/b11daaa83ff045f1a9d88b2b926e1f75/page/About/)) that is being modified by our team to further advance bull kelp conservation and restoration.  

The downward-facing camera is used to characterize percent-coverage of aggregate taxa such as fleshy red algae, brown algae, and substrate type. Imagery are analyzed in CoralNet to generate metrics of percent-coverage. Here, our code simulates data derived from CoralNet, where our columns all sum to `col_sum = 50`, mirroring the x50 percent-coverage data points classified in every image. 

Imagery from the forward-facing camera is used to classify discrete and individually specific objects encountered by the ROV such as bull kelp stipes, kelp crabs, and fishes. Here, our code provides the means to simulate and append these data to the simulated percent-coverage data. 

Finally, we simulate these data in a manner that approximates our real-world ROV surveys from the Urban Kelp Research Project with the Port of Seattle. That is to say, our simulated benthic community data have the exact structure (in terms of the number of rows representing sites and transects) as our summer 2023 Urban Kelp ROV surveys. 

Below we lay out a brief overview of our code and how to implement it. 


