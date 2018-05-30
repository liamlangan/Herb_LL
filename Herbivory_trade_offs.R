# Author: Liam Langan
# Herbivory trade-offs

## Relationships taken from From - Normal Owen Smith (The influence of very large body size on ecology) unless otherwise stated.
##------------------------------------------------------------------------------------------------
## Chapter 5 (Body size and nutritional physiology) 
mass <- seq(1, 5000, by=1)
daily_food_intake <- function(mass){0.695*(mass^-0.236)} # Fig. 5.1 Normal Owen Smith
plot(mass, daily_food_intake(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))

daily_food_intake2 <- function(mass){0.6*(mass^-0.191)} # Fig. 5.2 Normal Owen Smith
# plot(mass, daily_food_intake2(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))
lines(mass, daily_food_intake2(mass), col="red", lwd=3)

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
## Chapter 6 (Body size and nutritional physiology) 
