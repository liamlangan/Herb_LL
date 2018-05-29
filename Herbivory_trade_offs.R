# Author: Liam Langan
# Herbivory trade-offs

mass <- seq(1, 5000, by=1)
daily_food_intake <- function(mass){0.695*(mass^-0.236)} # Fig. 5.1 Normal Owen Smith
plot(mass, daily_food_intake(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))

daily_food_intake2 <- function(mass){0.6*(mass^-0.191)} # Fig. 5.2 Normal Owen Smith
# plot(mass, daily_food_intake2(mass), ylab="Daily food intake (% of body mass)", xlab="Body mass (kg)", ylim=c(0,1))
lines(mass, daily_food_intake2(mass), col="red", lwd=3)

mean_retention_time_hindgut <- function(mass){32*(mass^0.135)}
plot(mass, mean_retention_time_hindgut(mass), ylab="Mean retention time (h)", xlab="Body mass (kg)", ylim=c(0,120))


cell_wall_digestion_all_species <- function(mean_retention_time_hindgut){ (4.95 - (0.034*mean_retention_time_hindgut))}
plot(mean_retention_time_hindgut(mass), log(cell_wall_digestion_all_species(mean_retention_time_hindgut(mass))))
# not working