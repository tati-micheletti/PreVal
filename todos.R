# TODO

# 17.12.2025
# [X] Check why anthro disturbances are not working!
# [X] I AM CURRENTLY AT THE extractDistto --> This seems to have lots of NA's, although a functioning function. Rory is trying to figure out where the NA's can be coming from. I am waiting on info on what to do about those, but moved on to put to run for all years.
# [X] Rory said I shouldn't worry about it.... Moving on: trying to create sim$extractedLand. It seems it will take DAYS to create the tables... about 10 hours PER YEAR! This is like 2 years per day... Gee. I will be done on the 26th of December if everything works...
# [X] Julie says I do need to figure out why x2_ are NA... So I am currently in prepTracks. The problem is the function `random_steps`... But why I am currently investigating. 
# [X] RA! I know what happened:  In the original data there are still steps with 0 length. These are creating the problem. They can only be checked, however, once the tracks are created! So that's what I am doing now in prepTracks: I clean these up, and reconstruct the data for creating the random steps. It should be good to go now. 
# [X] Move on to preparing the data to use in a RSF or iSSA. Better have it in tables?! This is probably done in the iSSA module, but needs to be a separate module so can be used with another (i.e., RSF) model! --> No, it is done in extractLand! I just need to change the function, as I need the yearly, not 5 year version. --> This is actually done already in prepTracks. I implemented 3 different ways of adding the aggregated data
# [X] Run the modules and get the covariates!!!
# [X] Run the iSSA's as per table (Important: the model has to retain AT LEAST indv random effects. Interactions can be removed as per global models)
# No difference between iSSA and RSF... I decided to run NN.
# [X] Run global NN for ranking covariates --> RESULTS ARE SOOOO COOL! A paper on their own, probably...
# [X] Write the modules for running the experiment
# [X] Test experiment with 6 models (2 triplets)
# [X] Analize experiment
# [ ] Optimize the running of the experiment
#   [ ] cov2
#   [ ] cov5
#   [ ] cov10
#   [ ] covInf
# [ ] Run experiment
# [ ] Analyze experiment --> Important: when plotting, really make sure the difference across the triplets is shown. We have replicates of them, so we need to use them properly.
# [ ] Summarize the findings
# [ ] Write the paper!

# Desired improvements to the code:
# 1. Anthro disturbances 
# are harder, but maybe also possible to do? Check the anthropo data for years... 
# maybe we can see something. Something is better than nothing. Why not look at CA_forest_VLCE2_? 
# I think it has anthro disturbance and is annual! No, it doesn't. Give up on this.

# Talking to Eliot: the products available are not enough, so we need to use disturbance from ECCC and to the following:
# annual fitting/predicting, with anthro being: 
# 2008-2012 with layer 2010, from 2013-2017, layer 2015, 2018-2022, with layer 2020.
# So in the end I will use data from 2008 to 2020. 
# [X] Write to Julie/Rory about a considerably smaller version of the iSSA which still retains complexity (but is considerably easier to run), and which could still be more simplified and still be relatively realistic.
# 
