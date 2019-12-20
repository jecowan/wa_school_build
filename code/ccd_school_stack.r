
#############################################################################
# ccd_school_stack.r
#
# Stack annual school data from NCES Public School Universe (1996-2007).
# I use the SAS files constructed by NCES for 1998-2007 and the fixed
# width files for 1996-1997.
#
# Author:  James Cowan
# Created: 2019-12-19
#############################################################################


# Factor levels.
# Status.
status.labs <- c("Operational", "Closed", "Opened", "Not reported", 
  "Changed agency", "Temporarily closed", "Scheduled to open", "Reopened")
# Type.
type.labs <- c("Regular school", "Special education school", 
  "Vocational school", "Other/alternative school")

ccd.1996 <- read_fwf("./input/SCHL95OW.DAT", 
  fwf_cols(codistid = c(13, 26),
    bldg = c(57, 76),
    state = c(155, 156),
    type = c(176, 176),
    status = c(177, 177),
    gslo = c(184, 185),
    gshi = c(186, 187),
    enroll = c(263, 268),
    amind = c(275, 279),
    asian = c(280, 284),
    hisp = c(285, 289),
    black = c(290, 294),
    white = c(295, 299))) %>%
  filter(state == "WA") %>% select(-state) %>%
  mutate(syear = 1996,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    enroll = as.numeric(ifelse(enroll == "N", NA_character_, enroll)),
    amind = as.numeric(ifelse(amind == "N", NA_character_, amind)),
    asian = as.numeric(ifelse(asian == "N", NA_character_, asian)),
    hisp = as.numeric(ifelse(hisp == "N", NA_character_, hisp)),
    black = as.numeric(ifelse(black == "N", NA_character_, black)),
    white = as.numeric(ifelse(white == "N", NA_character_, white)))


ccd.1997 <- read_fwf("./input/sch96ow.dat", 
  fwf_cols(codistid = c(13, 26),
    bldg = c(57, 76),
    state = c(155, 156),
    type = c(176, 176),
    status = c(177, 177),
    gslo = c(184, 185),
    gshi = c(186, 187),
    enroll = c(263, 268),
    amind = c(275, 279),
    asian = c(280, 284),
    hisp = c(285, 289),
    black = c(290, 294),
    white = c(295, 299))) %>%
  filter(state == "WA") %>% select(-state) %>%
  mutate(syear = 1997,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    enroll = as.numeric(ifelse(enroll == "N", NA_character_, enroll)),
    amind = as.numeric(ifelse(amind == "N", NA_character_, amind)),
    asian = as.numeric(ifelse(asian == "N", NA_character_, asian)),
    hisp = as.numeric(ifelse(hisp == "N", NA_character_, hisp)),
    black = as.numeric(ifelse(black == "N", NA_character_, black)),
    white = as.numeric(ifelse(white == "N", NA_character_, white)))

ccd.1998 <- read_sas("./input/psu97ow.sas7bdat") %>%
  filter(FIPS == 53) %>%
  select(codistid = STID97, bldg = SEASCH97, type = TYPE97, status = STATUS97,
    gslo = GSLO97, gshi = GSHI97, enroll = MEMBER97, amind = IND97,
    asian = ASIAN97, hisp = HISP97, black = BLACK97, white = WHITE97) %>%
  mutate(syear = 1998,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs))

ccd.1999 <- read_sas("./input/sc981cow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID98, bldg = SEASCH98, type = TYPE98, status = STATUS98,
    gslo = GSLO98, gshi = GSHI98, enroll = MEMBER98, amind = AM98,
    asian = ASIAN98, hisp = HISP98, black = BLACK98, white = WHITE98) %>%
  mutate(syear = 1999,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs))
    
ccd.2000 <- read_sas("./input/sc991bow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID99, bldg = SEASCH99, type = TYPE99, status = STATUS99,
    gslo = GSLO99, gshi = GSHI99, enroll = MEMBER99, amind = AM99,
    asian = ASIAN99, hisp = HISP99, black = BLACK99, white = WHITE99) %>%
  mutate(syear = 2000,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs))

ccd.2001 <- read_sas("./input/sc001aow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID00, bldg = SEASCH00, type = TYPE00, status = STATUS00,
    gslo = GSLO00, gshi = GSHI00, enroll = MEMBER00, amind = AM00,
    asian = ASIAN00, hisp = HISP00, black = BLACK00, white = WHITE00) %>%
  mutate(syear = 2001,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs))

ccd.2002 <- read_sas("./input/sc011aow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID01, bldg = SEASCH01, type = TYPE01, status = STATUS01,
    gslo = GSLO01, gshi = GSHI01, frl = TOTFRL01, enroll = MEMBER01, amind = AM01,
    asian = ASIAN01, hisp = HISP01, black = BLACK01, white = WHITE01) %>%
  mutate(syear = 2002,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs))

ccd.2003 <- read_sas("./input/sc021aow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID02, bldg = SEASCH02, type = TYPE02, status = STATUS02,
    gslo = GSLO02, gshi = GSHI02, frl = TOTFRL02, enroll = MEMBER02, amind = AM02,
    asian = ASIAN02, hisp = HISP02, black = BLACK02, white = WHITE02) %>%
  mutate(syear = 2003,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    frl = ifelse(frl < 0, NA, frl),
    enroll = ifelse(enroll < 0, NA, enroll),
    amind = ifelse(amind < 0, NA, amind),
    asian = ifelse(asian < 0, NA, asian),
    hisp = ifelse(hisp < 0, NA, hisp),
    black = ifelse(black < 0, NA, black),
    white = ifelse(white < 0, NA, white))

ccd.2004 <- read_sas("./input/sc031aow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID03, bldg = SEASCH03, type = TYPE03, status = STATUS03,
    gslo = GSLO03, gshi = GSHI03, frl = TOTFRL03, enroll = MEMBER03, amind = AM03,
    asian = ASIAN03, hisp = HISP03, black = BLACK03, white = WHITE03) %>%
  mutate(syear = 2004,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    frl = ifelse(frl < 0, NA, frl),
    enroll = ifelse(enroll < 0, NA, enroll),
    amind = ifelse(amind < 0, NA, amind),
    asian = ifelse(asian < 0, NA, asian),
    hisp = ifelse(hisp < 0, NA, hisp),
    black = ifelse(black < 0, NA, black),
    white = ifelse(white < 0, NA, white))

ccd.2005 <- read_sas("./input/sc041bow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID04, bldg = SEASCH04, type = TYPE04, status = STATUS04,
    gslo = GSLO04, gshi = GSHI04, frl = TOTFRL04, enroll = MEMBER04, amind = AM04,
    asian = ASIAN04, hisp = HISP04, black = BLACK04, white = WHITE04) %>%
  mutate(syear = 2005,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    frl = ifelse(frl < 0, NA, frl),
    enroll = ifelse(enroll < 0, NA, enroll),
    amind = ifelse(amind < 0, NA, amind),
    asian = ifelse(asian < 0, NA, asian),
    hisp = ifelse(hisp < 0, NA, hisp),
    black = ifelse(black < 0, NA, black),
    white = ifelse(white < 0, NA, white))

ccd.2006 <- read_sas("./input/sc051aow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID05, bldg = SEASCH05, type = TYPE05, status = STATUS05,
    gslo = GSLO05, gshi = GSHI05, frl = TOTFRL05, enroll = MEMBER05, amind = AM05,
    asian = ASIAN05, hisp = HISP05, black = BLACK05, white = WHITE05) %>%
  mutate(syear = 2006,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    frl = ifelse(frl < 0, NA, frl),
    enroll = ifelse(enroll < 0, NA, enroll),
    amind = ifelse(amind < 0, NA, amind),
    asian = ifelse(asian < 0, NA, asian),
    hisp = ifelse(hisp < 0, NA, hisp),
    black = ifelse(black < 0, NA, black),
    white = ifelse(white < 0, NA, white))

ccd.2007 <- read_sas("./input/sc061cow.sas7bdat") %>%
  filter(FIPST == 53) %>%
  select(codistid = STID06, bldg = SEASCH06, type = TYPE06, status = STATUS06,
    gslo = GSLO06, gshi = GSHI06, frl = TOTFRL06, enroll = MEMBER06, amind = AM06,
    asian = ASIAN06, hisp = HISP06, black = BLACK06, white = WHITE06) %>%
  mutate(syear = 2007,
    type = factor(type, levels = 1:4, labels = type.labs),
    status = factor(status, levels = 1:8, labels = status.labs),
    frl = ifelse(frl < 0, NA, frl),
    enroll = ifelse(enroll < 0, NA, enroll),
    amind = ifelse(amind < 0, NA, amind),
    asian = ifelse(asian < 0, NA, asian),
    hisp = ifelse(hisp < 0, NA, hisp),
    black = ifelse(black < 0, NA, black),
    white = ifelse(white < 0, NA, white))

ccd <- bind_rows(ccd.1996, ccd.1997, ccd.1998, ccd.1999, ccd.2000, ccd.2001,
  ccd.2002, ccd.2003, ccd.2004, ccd.2005, ccd.2006, ccd.2007) %>%
  filter(status %in% c("Operational", "Opened", "Reopened")) %>% 
  select(-status) %>%
  mutate(
    frl = 100 * frl / enroll,
    amind = 100 * amind / enroll, 
    asian = 100 * asian / enroll,
    hisp = 100 * hisp / enroll,
    black = 100 * black / enroll,
    white = 100 * white / enroll)
summary(ccd)

save(ccd, file="./intermediate/ccd_school_1996_2007.RData")
rm(list = ls())
