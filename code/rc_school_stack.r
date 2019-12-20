
#############################################################################
# rc_school_stack.r
#
# Stack annual school data from school report card files (2002-2019).
#
# Author:  James Cowan
# Created: 2019-12-19
#############################################################################


rc.2008 <- read.csv("./input/Demographic_Information_by_School_2008.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2008,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2009 <- read.csv("./input/Demographic_Information_by_School_2009.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2009,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2010 <- read.csv("./input/Demographic_Information_by_School_2010.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2010,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2011 <- read.csv("./input/Demographic_Information_by_School_2011.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2011,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2012 <- read.csv("./input/Demographic_Information_by_School_2012.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2012,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2013 <- read.csv("./input/Demographic_Information_by_School_2013.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2013,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)

rc.2014 <- read.csv("./input/Demographic_Information_by_School_2014.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  mutate(syear = 2014,
    codistid = str_pad(CountyDistrictNumber, 5, pad = "0"),
    bldg = as.character(BuildingNumber)) %>%
  select(syear, codistid, bldg, type = SchoolTypeCode, gradespan = GradeSpan,
    enroll = TotalEnrollment, asian = PercentAsianPacificIslander, 
    amind = PercentAmericanIndianorAlaskanNative,
    black = PercentBlack, hisp = PercentHispanic, white = PercentWhite, 
    frl = PercentFreeorReducedPricedMeals)


rc.2015.2019 <- read.csv("./input/Report_Card_Enrollment_from_2014-15_to_Current_Year.csv",
  header=TRUE, skipNul=TRUE,
  stringsAsFactors=FALSE, strip.white=TRUE) %>%
  filter(OrganizationLevel == "School") %>%
  mutate(syear = as.numeric(paste(substr(SchoolYear, 1, 2), 
    substr(SchoolYear, 6, 7), sep = "")),
    codistid = str_pad(DistrictCode, 5, pad = "0"),
    bldg = as.character(SchoolCode)) %>%
  select(syear, codistid, bldg, type = CurrentSchoolType, Gradelevel,
    enroll = All.Students, amind = American.Indian..Alaskan.Native,
    asian = Asian, black = Black..African.American,
    hisp = Hispanic..Latino.of.any.race.s.,
    hpi = Native.Hawaiian..Other.Pacific.Islander,
    multi = Two.or.More.Races,
    white = White,
    frl = Low.Income)

# Code grade level.
rc.2015.2019 <- mutate(rc.2015.2019,
  grade = NA,
  grade = ifelse(Gradelevel == "Pre-Kindergarten", -1, grade),
  grade = ifelse(Gradelevel == "Half-day Kindergarten", 0, grade),
  grade = ifelse(Gradelevel == "Kindergarten", 0, grade),
  grade = ifelse(Gradelevel == "1st Grade", 1, grade),
  grade = ifelse(Gradelevel == "2nd Grade", 2, grade),
  grade = ifelse(Gradelevel == "3rd Grade", 3, grade),
  grade = ifelse(Gradelevel == "4th Grade", 4, grade),
  grade = ifelse(Gradelevel == "5th Grade", 5, grade),
  grade = ifelse(Gradelevel == "6th Grade", 6, grade),
  grade = ifelse(Gradelevel == "7th Grade", 7, grade),
  grade = ifelse(Gradelevel == "8th Grade", 8, grade),
  grade = ifelse(Gradelevel == "9th Grade", 9, grade),
  grade = ifelse(Gradelevel == "10th Grade", 10, grade),
  grade = ifelse(Gradelevel == "11th Grade", 11, grade),
  grade = ifelse(Gradelevel == "12th Grade", 12, grade)) %>%
  group_by(codistid, bldg, syear) %>%
  mutate(gslo = as.character(min(grade, na.rm = TRUE)),
    gshi = as.character(max(grade, na.rm = TRUE))) %>% ungroup() %>%
  mutate(gslo = ifelse(gslo == "-1", "PK", gslo),
    gslo = ifelse(gslo == "0", "KG", gslo),
    gshi = ifelse(gshi == "-1", "PK", gshi),
    gshi = ifelse(gshi == "0", "KG", gshi)) %>%
  filter(Gradelevel == "AllGrades") %>%
  select(syear, codistid, bldg, type, gslo, gshi, enroll, amind, asian, black,
    hisp, hpi, multi, white, frl) %>%
  mutate(amind = 100 * amind / enroll,
    asian = 100 * (asian + hpi) / enroll,
    black = 100 * black / enroll,
    hisp = 100 * hisp / enroll,
    multi = 100 * multi / enroll,
    white = 100 * white / enroll,
    frl = 100 * frl / enroll,
    amind = ifelse(is.na(amind), 0, amind),
    asian = ifelse(is.na(asian), 0, asian),
    black = ifelse(is.na(black), 0, black),
    hisp = ifelse(is.na(hisp), 0, hisp),
    multi = ifelse(is.na(multi), 0, multi),
    white = ifelse(is.na(white), 0, white),
    frl = ifelse(is.na(frl), 0, frl)) %>% select(-hpi) %>%
  arrange(codistid, bldg, syear)

dat <- group_by(rc.2015.2019, codistid, bldg, syear) %>% mutate(nobs=n())
xtabs(~nobs, dat)
xtabs(~type + syear, rc.2015.2019)


# Encode high and low grade levels in report card data.
rc.dat <- bind_rows(rc.2008, rc.2009, rc.2010, rc.2011, rc.2012, rc.2013, 
  rc.2014)
xtabs(~gradespan, rc.dat)
rc.dat <- mutate(rc.dat, dash = str_locate(gradespan, "-")[,1],
  end = nchar(gradespan)) %>%
  mutate(gslo = toupper(substr(gradespan, 1, (dash - 1))), 
    gshi = toupper(substr(gradespan, (dash + 1), end))) %>%
  select(-dash, -end, -gradespan)
xtabs(~gslo + gshi, rc.dat)
rc.dat <- mutate(rc.dat,
  gslo = ifelse(gslo == " ", "", gslo),
  gshi = ifelse(gshi == "-", "", gshi),
  gslo = ifelse(gslo == "" | gshi == "", NA_character_, gslo),
  gshi = ifelse(gslo == "" | gshi == "", NA_character_, gshi))


# Stack datasets and make codes consistent.
rc.dat <- bind_rows(rc.dat, rc.2015.2019)

rc.dat <- mutate(rc.dat,
  gslo = ifelse(gslo == "K", "KG", gslo),
  gshi = ifelse(gshi == "K", "KG", gshi),
  gslo = str_pad(gslo, 2, pad = "0"),
  gshi = str_pad(gshi, 2, pad = "0"))
xtabs(~gslo + gshi, rc.dat)

xtabs(~syear + type, rc.dat)
rc.dat <- mutate(rc.dat,
  type = ifelse(type %in% c("P"), "1", type),
  type = ifelse(type %in% c("S"), "2", type),
  type = ifelse(type %in% c("V", "X"), "3", type),
  type = ifelse(type %in% c("5", "A", "C", "I", "J", "Q", 
    "R", "Y", "Z"), "4", type)) %>%
  mutate(type = factor(type, levels = c("1", "2", "3", "4"), 
    labels = c("Regular school", "Special education school", 
      "Vocational school", "Other/alternative school")))
xtabs(~syear + type, rc.dat)

summary(rc.dat)

save(rc.dat, file="./intermediate/rc_school_2008_2019.RData")
rm(list = ls())
