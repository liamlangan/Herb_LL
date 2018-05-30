# Author: Liam Langan
# Herbivory trade-offs

## Relationships taken from From - Normal Owen Smith (The influence of very large body size on ecology) unless otherwise stated.
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

## Chapter 5 (Body size and nutritional physiology) 

# daily food intake (organic matter) as percentage of body mass (function used = #2 below) 
# all options
# 1 - (grass hay, all species except pigmy hippo and giraffe) x <- function(mass){4.04*(mass^-0.184)}
# 2 - (grass hay, hindgut fermenters only) x <- function(mass){6.95*(mass^-0.236)} 
# 3 - (legume hay, all species except pigmy hippo and giraffe) x <- function(mass){7.31*(mass^-0.231)} 
# 4 - (legume hay, hindgut fermenters only) x <- function(mass){13.8*(mass^-0.315)} 
mass <- seq(1, 5000, by=1)
daily_food_intake <- function(mass){0.695*(mass^-0.236)} # Fig. 5.1 Normal Owen Smith
plot(mass, daily_food_intake(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))

# daily food intake as function of body mass (function used = #2 below) 
plot(mass, mass*daily_food_intake1(mass), ylab="Daily food intake (kg)", xlab="Body mass (kg)", ylim=c(0,1000))
lines(mass, mass*daily_food_intake1(mass), col="red", lwd=3)

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# daily food intake (dry mass) as percentage of body mass (Fig. 5.2 Normal Owen Smith) 
daily_food_intake2 <- function(mass){0.6*(mass^-0.191)} # Fig. 5.2 Normal Owen Smith
plot(mass, daily_food_intake2(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))
lines(mass, daily_food_intake2(mass), col="red", lwd=3)

# daily food intake as function of body mass 
plot(mass, mass*daily_food_intake2(mass), ylab="Daily food intake (kg)", xlab="Body mass (kg)", ylim=c(0,1000))
lines(mass, mass*daily_food_intake2(mass), col="red", lwd=3)

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Mean retention time (h) of digesta in relation to body mass (Fig. 5.3 Normal Owen Smith), original ref. (Foose, 1982) (function used = #2 below) 
# all options
# 1 - (grass hay, all species) x <- function(mass){46.10*(mass^0.048)}
# 2 - (grass hay, hindgut fermenters only) x <- function(mass){32.00*(mass^0.075)} 
# 3 - (grass hay, perissodactyls only) x <- function(mass){22.80*(mass^0.135)} 
# 4 - (legume hay, all species) x <- function(mass){36.60*(mass^0.061)} 
# 5 - (legume hay, hindgut fermenters only) x <- function(mass){23.4*(mass^0.106)} 
# 6 - (legume hay, perissodactyls only) x <- function(mass){12.8*(mass^0.177)} 

mean_retention_time_hindgut <- function(mass){32*(mass^0.135)} # Fig. 5.3 Normal Owen Smith
plot(mass, mean_retention_time_hindgut(mass), ylab="Mean retention time (h)", xlab="Body mass (kg)", ylim=c(0,120))

log_e_residual_digestable_cell_wall <- function(mean_retention_time_hindgut){ (4.95 - (0.034*mean_retention_time_hindgut))} # Fig. 5.5 Normal Owen Smith
# 0.7 scales to a maximum of 70% digestion
cell_wall_digestion_percent <- function(log_e_residual_digestable_cell_wall) { (100 - exp(log_e_residual_digestable_cell_wall(mean_retention_time_hindgut(mass))))*0.7} # Fig. 5.5 Normal Owen Smith
# I have no access to original paper (Waldo, D. R., Smith, L. W. & Cox, E. L. (1972). Model of cellulose disappearance from the rumen. J. Dairy Science, 55, 1 25-8.)
plot(mean_retention_time_hindgut(mass), cell_wall_digestion_percent(log_e_residual_digestable_cell_wall(mean_retention_time_hindgut(mass))), xlab="Mean retention time (h)", ylab="Cell wall digestion (%)" ) # this looks correct
plot(mass, cell_wall_digestion_percent(log_e_residual_digestable_cell_wall(mean_retention_time_hindgut(mass))), xlab="Body Mass (kg)", ylab="Cell wall digestion (%)" ) # this looks correct
#plot(mass, cell_wall_digestion_percent(log_e_residual_digestable_cell_wall(mean_retention_time_hindgut(mass))), xlab="Body Mass (kg)", ylab="Cell wall digestion (%)", xlim=c(1,30) ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

## Chapter 6 (Body size and nutritional physiology) 

# "Crude protein concentrations in the stomach contents of large herbivores during the dry season in relation to body size" (Owen Smith)
percent_crude_protein_stomach <- function(mass) {21.6*(mass^-0.23)} # Fig. 6.1 Normal Owen Smith (ruminants only excluding giraffe)
plot(mass, percent_crude_protein_stomach(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,20))

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Fermentation rate (umol gas g (dry matter)^-1 h^-1) in expressed as gas production in relation to body mass. 
fermentation_rate <- function(mass) {945*(mass^-0.22)} # Fig. 6.2 Normal Owen Smith
plot(mass, fermentation_rate(mass), xlab="Mass (kg)", ylab="Fermentation rate (umol gas g (dry matter)^-1 h^-1)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Proportion (%) of non-stem material (leaves, leaf sheaths, fruits) in rumen or stomach content of large herbivores in relation to body mass
# for elephants it's based on replicated food intake and hippo from fecal samples
non_stem_in_digesta <- function(mass) {pmin(100, (116*(mass^-0.118)))} # pmin as eq. extends above 100% # Fig. 6.3 Normal Owen Smith
plot(mass, non_stem_in_digesta(mass), xlab="Mass (kg)", ylab="(%) of non-stem material" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Daily foraging time (% of day) of large herbivores in relation to body mass. (function used = #1 below)
# all options
# 1 - (all species) x <- function(mass){24.20*(mass^0.12)}
# 2 - (ruminants only) x <- function(mass){27.90*(mass^0.08)} 
# 3 - (non-ruminants only) x <- function(mass){19.00*(mass^0.17)}

percent_foraging_time <- function(mass) {24.20*(mass^0.12)} # Fig. 6.4 Normal Owen Smith
plot(mass, percent_foraging_time(mass), xlab="Mass (kg)", ylab="(% of day) Foraging time" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Female home range area (km^2)(plot gives km2, eqn. gives ha) as a function of adult female body mass. Likely not needed but for completeness.
home_range <- function(mass) {1.35*(mass^1.25)} # Fig. 6.6 Normal Owen Smith
plot(mass, home_range(mass)/100, xlab="Female adult Mass (kg)", ylab="Home range area (km^2)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Home range extent per individual (km^2)(plot gives km2, eqn. gives ha) as a function of adult body mass. Likely not needed but for completeness.
# excludes mega herbivores. Calculated by dividing home range extent by size of social group. Social group size requires imformation about the
# behavioural ecology of species and as such is not useable/useful for what we need to do.  
home_range_per_individual <- function(mass) {1.07*(mass^0.83)} # Fig. 6.7 Normal Owen Smith
plot(mass, home_range_per_individual(mass), xlab="Female adult Mass (kg)", ylab="Home range extent per individual (ha)" , xlim=c(0,600), ylim=c(0,300)) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 9. deals with body-size and sociobiology. Noteworthy is that there is often a reduced probability of predation with increasing group size (higher fitness) 
# while there is reduced foraging efficiency with increasing group size (reduced fitness). This implies a there is a maximum fitness point in relation to group size, 
# foraging efficency and predation. These maximum fitness points will depend on mass, certainly predation will depend on mass of a prey individauls w.r.t. the mass of the predator.
# Fig. 9.1, Fig 9.2

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 10. Body-size and reproductive patterns. This chapter is crucial.

# Age at first conception (y) in relation to body mass (kg). I want to use this to determine the size an individual needs to reach before it can reproduce. 
# all options - (using option 1)
# 1 - (all species) x <- function(mass){0.41*(mass^0.32)}
# 2 - (mega herbivores excluded) x <- function(mass){0.54*(mass^0.25)} 

age_to_first_baby <- function(mass) {0.41*(mass^0.32)} # Fig. 10.1 Normal Owen Smith
plot(mass, age_to_first_baby(mass), xlab="Body mass (kg)", ylab="Age at first conception (y)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Birth interval (month) in relation to body mass (kg)
# all options - (using option 1)
# 1 - (all species) x <- function(mass){3.40*(mass^0.27)}
# 2 - (mega herbivores excluded) x <- function(mass){5.00*(mass^0.17)} 
birth_interval <- function(mass) {3.40*(mass^0.27)} # Fig. 10.2 Normal Owen Smith
plot(mass, birth_interval(mass)/12, xlab="Body mass (kg)", ylab="Birth interval (y)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Gestation time (y) in relation to body mass (female body mass).
# all options - (using option 1)
# 1 - (all species) x <- function(mass){3.15*(mass^0.20)}
# 2 - (mega herbivores excluded) x <- function(mass){3.49*(mass^0.17)} 
gestation_time <- function(mass) {3.15*(mass^0.20)} # Fig. 10.3 Normal Owen Smith
plot(mass, gestation_time(mass)/12, xlab="Body mass (kg)", ylab="Gestation time (y)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Neonatal mass (kg) in relation to maternal mass (female body mass).
# all options - (using option 1)
# 1 - (all species) x <- function(mass){0.23*(mass^0.79)}
# 2 - (mega herbivores excluded) x <- function(mass){0.17*(mass^0.87)} 
neonatal_mass <- function(mass) {0.23*(mass^0.79)} # Fig. 10.4 Normal Owen Smith
plot(mass, neonatal_mass(mass), xlab="Maternal body mass (kg)", ylab="neonatal_mass (kg)" ) # this looks correct

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Sex ratio, Fig. 10.5. I'm happy to assume even sex ratio. 

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 11. Demography ### should emerge based on trade-offs 

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 12. Community interactions 
# need equation which defines the probability an individual will kill/top-kill a tree based on body mass 

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 13. Body size and population regulation
# see appendix 2 for Owen Smiths model using scaling parameters
# "The only adjustment that can prevent or reduce episodic overexploitation of food resources during droughts is dispersal.". Seems to be apparent in Pachzelt model.

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 14. Body size and ecosystem processes
# see chapter 5 for basal metabolic rates, here it seems to be 
metabolic_cost <- function(mass) {mass^0.25} 
plot(mass, metabolic_cost(mass), xlab="Mass (kg)", ylab="Cost (?)" ) # this looks correct

# there are equations which related total population biomass to individual body mass (Fig. 14.1, Fig. 14.2). 
# it would be however nice if the model we design could reproduce this. 
# Fig. 14.3 gives herbivore biomass against rainfall - useful for benchmarking

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
# Chapter 15. Late pleistocence extinctions

































