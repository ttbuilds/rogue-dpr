library(tidyverse)

source("piercer-benefit.R")

tactics <- c(
  "aim_hide_no_SS", 
  "aim_hide_SS", 
  "two_atks_no_SS", 
  "two_atks_SS", 
  "SS_first", 
  "SS_second")

attack_dpr <- function(
  enemyAC, 
  level, 
  DEX     = 3, 
  CBE     = 1,
  SS      = 1,
  ARCHERY = 0, 
  ADV1    = 0, 
  ADV2    = 0, 
  EA      = 0,
  PIERCER = 0)
{
  PROF            = 1 + floor((level+3)/4)
  BASE_TOCRIT     = 0.05
  ATK             = DEX + PROF
  BASE_TOHIT      = pmax(pmin((21-enemyAC+ATK+2*ARCHERY)/20, 0.95), 0.05)
  BASE_TOHIT_SS   = pmax(pmin((21-enemyAC+ATK+2*ARCHERY-5)/20, 0.95), 0.05)
  SNEAK_DICE      = ceiling(level/2)
  TOHIT1_NO_SS     = 1 - (1 - BASE_TOHIT)^(1+ADV1*(1+EA))
  TOHIT2_NO_SS     = 1 - (1 - BASE_TOHIT)^(1+ADV2*(1+EA))  
  TOHIT_NO_SS_ADV  = 1 - (1 - BASE_TOHIT)^(1+(1+EA))
  TOHIT1_SS        = 1 - (1 - BASE_TOHIT_SS)^(1+ADV1*(1+EA))
  TOHIT2_SS        = 1 - (1 - BASE_TOHIT_SS)^(1+ADV2*(1+EA))  
  TOHIT_SS_ADV     = 1 - (1 - BASE_TOHIT_SS)^(1+(1+EA))
  TOCRIT1          = (1 - (1 - BASE_TOCRIT)^(1+ADV1*(1+EA)))
  TOCRIT2          = (1 - (1 - BASE_TOCRIT)^(1+ADV2*(1+EA)))  
  TOCRIT_ADV       = (1 - (1 - BASE_TOCRIT)^(1+(1+EA)))
  BASE_DOH_NO_SS  = ifelse(CBE, 3.5, 4.5) + DEX
  BASE_DOH_SS     = BASE_DOH_NO_SS + 10  
  SNEAK_DMG       = SNEAK_DICE * 3.5
  BASE_CRIT_DMG   = ifelse(CBE, 3.5, 4.5)
  SNEAK_CRIT_DMG  = SNEAK_DMG
  SINGLE_ATK_NO_SS_DPR = 
    (BASE_DOH_NO_SS + SNEAK_DMG)     * TOHIT1_NO_SS + 
    (BASE_CRIT_DMG + SNEAK_CRIT_DMG) * TOCRIT1 +
    PIERCER * piercer_benefit(BASE_TOHIT, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)
  SINGLE_ATK_SS_DPR = 
    (BASE_DOH_SS + SNEAK_DMG)        * TOHIT1_SS + 
    (BASE_CRIT_DMG + SNEAK_CRIT_DMG) * TOCRIT1 +
    PIERCER * piercer_benefit(BASE_TOHIT_SS, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)    
  AIMHIDE_NO_SS = 
    (BASE_DOH_NO_SS + SNEAK_DMG)     * TOHIT_NO_SS_ADV + 
    (BASE_CRIT_DMG + SNEAK_CRIT_DMG) * TOCRIT_ADV +
    PIERCER * piercer_benefit(BASE_TOHIT, BASE_TOCRIT, 1+EA, SNEAK_DICE)        
  AIMHIDE_SS = 
    (BASE_DOH_SS + SNEAK_DMG)        * TOHIT_SS_ADV + 
    (BASE_CRIT_DMG + SNEAK_CRIT_DMG) * TOCRIT_ADV +
    PIERCER * piercer_benefit(BASE_TOHIT_SS, BASE_TOCRIT, 1+EA, SNEAK_DICE)    
  TWO_ATK_NO_SS_DPR = 
    BASE_DOH_NO_SS * (TOHIT1_NO_SS + TOHIT2_NO_SS) + 
    SNEAK_DMG      * (TOHIT1_NO_SS + (1-TOHIT1_NO_SS)*TOHIT2_NO_SS) +
    BASE_CRIT_DMG  * (TOCRIT1 + TOCRIT2) + 
    SNEAK_CRIT_DMG * (TOCRIT1 + (1-TOHIT1_NO_SS) * TOCRIT2) +
    PIERCER * piercer_benefit(BASE_TOHIT, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)
  TWO_ATK_SS_DPR = 
    BASE_DOH_SS    * (TOHIT1_SS + TOHIT2_SS) + 
    SNEAK_DMG      * (TOHIT1_SS + (1-TOHIT1_SS) * TOHIT2_SS) +
    BASE_CRIT_DMG  * (TOCRIT1 + TOCRIT2) + 
    SNEAK_CRIT_DMG * (TOCRIT1 + (1-TOHIT1_SS) * TOCRIT2) +
    PIERCER * piercer_benefit(BASE_TOHIT_SS, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)
  SS_FIRST_DPR = 
    (BASE_DOH_SS + SNEAK_DMG)    * TOHIT1_SS + 
    BASE_DOH_SS                  * TOHIT1_SS * TOHIT2_SS +
    (BASE_DOH_NO_SS + SNEAK_DMG) * (1 - TOHIT1_SS) * TOHIT2_NO_SS +
    BASE_CRIT_DMG                * (TOCRIT1 + TOCRIT2) +
    SNEAK_CRIT_DMG               * (TOCRIT1 + (1 - TOHIT1_SS) * TOCRIT2) +
    PIERCER * piercer_benefit(BASE_TOHIT_SS, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)
  SS_SECOND_DPR = 
    (BASE_DOH_NO_SS + SNEAK_DMG) * TOHIT1_NO_SS + 
    BASE_DOH_SS                  * TOHIT1_NO_SS * TOHIT2_SS +
    (BASE_DOH_NO_SS + SNEAK_DMG) * (1 - TOHIT1_NO_SS) * TOHIT2_NO_SS +
    BASE_CRIT_DMG                * (TOCRIT1 + TOCRIT2) +
    SNEAK_CRIT_DMG               * (TOCRIT1 + (1 - TOHIT1_NO_SS) * TOCRIT2) +
    PIERCER * piercer_benefit(BASE_TOHIT, BASE_TOCRIT, ADV1*(1+EA), SNEAK_DICE)    
  return(tibble(
    level = level,
    enemyAC = enemyAC,
    # single_atk_no_SS = SINGLE_ATK_NO_SS_DPR, 
    # single_atk_SS = SINGLE_ATK_SS_DPR,
    aim_hide_no_SS = AIMHIDE_NO_SS,
    aim_hide_SS = SS*AIMHIDE_SS,
    two_atks_no_SS = CBE*TWO_ATK_NO_SS_DPR,
    two_atks_SS = CBE*SS*TWO_ATK_SS_DPR,    
    SS_first = CBE*SS*SS_FIRST_DPR,
    SS_second = CBE*SS*SS_SECOND_DPR))
}