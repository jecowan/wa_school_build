
#############################################################################
# school_build.r
#
# Stack public-use school datasets from NCES & OSPI.
# 
# Author:  James Cowan
# Created: 2019-12-19
#############################################################################


library(tidyverse)
library(stringr)
library(haven)
library(forcats)

source("./code/ccd_school_stack.r")
source("./code/rc_school_stack.r")

load("./intermediate/ccd_school_1996_2007.RData")
load("./intermediate/rc_school_2008_2019.RData")

wa.schools <- bind_rows(ccd, rc.dat)
summary(wa.schools)

# Keep regular public schools.
wa.schools <- filter(wa.schools, 
  type == "Regular school", 
  gshi != "PK",
  gshi != "KG", 
  gshi != "00",
  enroll > 0)

# Encode grade levels using NCES conventions.
xtabs(~gslo + gshi, wa.schools)
wa.schools <- mutate(wa.schools, gshi = ifelse(gshi == "13", "12", gshi))
wa.schools <- mutate(wa.schools,
  level = 4,
  level = ifelse(gslo %in% c("PK", "KG", "01", "02", "03") &
      gshi %in% c("PK", "01", "02", "03", "04", "05", "06", "07", "08"), 1, level),
  level = ifelse(gslo %in% c("04", "05", "06", "07") & 
      gshi %in% c("04", "05", "06", "07", "08", "09"), 2, level),
  level = ifelse(gslo %in% c("07", "08", "09", "10", "11", "12") & gshi == "12",
    3, level)) %>%
  mutate(level = factor(level, levels = 1:4, 
    labels = c("Primary", "Middle", "High", "Other")))
xtabs(~syear + level, wa.schools)

save(wa.schools, file="./intermediate/wa_school_1996_2019.RData")
rm(list = ls())
