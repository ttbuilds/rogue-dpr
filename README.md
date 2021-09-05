# DPR Calculation and Visualization for Archer Rogue

## Dependencies

R

### R Packages on CRAN

```
tidyverse
RColorBrewer
remotes
colorspace
cowplot
colorblindr
```

### Package Installation

From an R prompt:

```
install.packages("tidyverse", "remotes")
remotes::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
remotes::install_github("clauswilke/colorblindr")
```

## Usage

Create a build file with the following columns:

- Level
- DEX
- CBE
- SS
- ARCHERY
- ADV1
- ADV2
- EA
- PIERCER

where, in each row, `LEVEL` is the character's rogue level, `DEX` is their Dexterity modifier, `CBE`, `SS`, `ARCHERY`, `EA` and `PIERCER` are flag which are 0 or 1 depending on whether the character has the corresponding feature (`EA` being Elven Accuracy, not Extra Attack),  `ADV1` and `ADV2` are 0 or 1 depending on whether there is an external source of advantage on the first and a potential second attack, respectively (not including Cunning Action Hide or Steady Aim).

Several example build files are provided.

To generate DPR graphs to compare tactics for different ACs, run the following from a terminal (Mac or Linux):

```
./rogue-ss-tactics-graphs.R <build_file>
```

The script will generate a plot for each level comparing DPR for each tactic for each enemy AC, as well as a plot showing the most damaging tactic at each level and AC.
