piercer_reroll_benefit <- function(weapon_ndice, weapon_dietype, sneakdice)
{
  D <- weapon_dietype
  offset <- (D - 6)/2
  weaponavg <- (D+1)/2
  sneakavg <- (6+1)/2
  weaponroll <- 0:D
  sneakroll <- 0:6
  max_weaponreroll = D/2
  max_sneakreroll = 6/2
  min_weaponroll_invcdf <- ((D - weaponroll)/D)^weapon_ndice
  min_weaponroll_pmf <- -diff(min_weaponroll_invcdf)
  min_sneakroll_invcdf <- ((6 - sneakroll)/6)^sneakdice
  min_sneakroll_pmf <- -diff(min_sneakroll_invcdf)
  joint_pmf <- outer(min_weaponroll_pmf, min_sneakroll_pmf)
  benefits <- pmax(outer(weaponavg - 1:D, sneakavg - 1:6, pmax),0)
  average_reroll_benefit <- sum(benefits * joint_pmf)
  return(average_reroll_benefit)
}

piercer_benefit <- function(base_tohit, base_tocrit, adv, sneakdice)
{
  average_crit_benefit <- 4.5
  tohit <- (1 - (1 - base_tohit)^(1+adv))
  tocrit <- (1 - (1 - base_tocrit)^(1+adv))
  average_reroll_benefit <- piercer_reroll_benefit(1, 8, sneakdice)
  total_benefit <- 
    (tohit-tocrit) * average_reroll_benefit + 
    tocrit * average_crit_benefit +
    tocrit * piercer_reroll_benefit(2, 8, 2*sneakdice)
  return(total_benefit)
}
