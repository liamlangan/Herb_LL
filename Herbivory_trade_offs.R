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
# daily food intake (dry mass) as percentage of body mass (Fig. 5.2 Normal Owen Smith) 
daily_food_intake2 <- function(mass){0.6*(mass^-0.191)} # Fig. 5.2 Normal Owen Smith
plot(mass, daily_food_intake2(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))
lines(mass, daily_food_intake2(mass), col="red", lwd=3)

# daily food intake as function of body mass 
plot(mass, mass*daily_food_intake2(mass), ylab="Daily food intake (kg)", xlab="Body mass (kg)", ylim=c(0,1000))
lines(mass, mass*daily_food_intake2(mass), col="red", lwd=3)

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
percent_crude_protein_stomach <- function(mass) {21.6*(mass^-0.23)} # Fig. 6.1 Normal Owen Smith
plot(mass, percent_crude_protein_stomach(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,20))




























