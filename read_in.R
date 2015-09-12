######
# THIS SCRIPT READS IN OBESITY DATA,
# CLEANS IT, AND SAVES IT TO AN RDATA FILE
######

library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(RColorBrewer)
library(readxl)

#####
# DIRECTORIES
#####
set_directories <- function(){
  if(Sys.info()['user'] == 'BrewJR'){
    assign('private', 'E:/fdoh/private/obesity/data201415/', envir = .GlobalEnv)
    assign('public', 'C:/Users/BrewJR/Documents/obesity15/', envir = .GlobalEnv)
  } else if(Sys.info()['user'] == 'joebrew') {
    assign('private', '/media/joebrew/JB/fdoh/private/obesity/data201415/', envir = .GlobalEnv)
    assign('public', '/home/joebrew/Documents/obesity15/', envir = .GlobalEnv)
  } else {
    assign('private', '/media/benbrew/HP V100W/data201415/', envir = .GlobalEnv) # CHANGE THIS LINE
    assign('public', '/home/benbrew/Documents/obesity15/', envir = .GlobalEnv)
  }
}
set_directories()

#####
# READ IN DATA
#####
setwd(private)
if('temp.RData' %in% dir()){
  load(paste0(private, 'temp.RData'))
} else {
  
  
  # Read in 2014 data
  linkage <- read.csv(paste0(private, 'FDOH_Obesity_Linkage.csv'))
  df <- read.csv(paste0(private, 'BMI.csv'))
  names(df)[2] <- 'PersonID'
  
  # join together linkage with data
  screen14 <- left_join(x = df,
                        y = linkage[,c('PersonID', 'Homeroom_Teacher')])
  
  # Clean up column names
  names(screen14) <- c('School_Name', 'PersonID', 'Grade',
                       'Lunch_Status', 'Gender', 'Race', 'DOB',
                       'Health_Screening_Date', 'Age_Months', 'Growth_Height', 'Growth_Weight',
                       'BMI', 'BMI_Percentile', 'Teacher')
  
  screen14$DOB <- screen14$Teacher <- NULL
  
  ############################################
  ######### Read in the old data
  ############################################
  setwd(paste0(private, 'screen'))
  screen13 <- read_excel("screen2.xlsx", sheet=7)
  #Fix the column names of 2013 in order to match the previous years
  colnames(screen13)[13] <- "Lunch_Status"
  screen12 <- read_excel("screen2.xlsx", sheet=6)
  screen11 <- read_excel("screen2.xlsx", sheet=5)
  screen10 <- read_excel("screen2.xlsx", sheet=4)
  screen09 <- read_excel("screen2.xlsx", sheet=3)
  screen08 <- read_excel("screen2.xlsx", sheet=2)
  screen07 <- read_excel("screen2.xlsx", sheet=1)
  
  ############################################
  ######### Delete any student with multiple entries
  ############################################
  clean_up <- function(x){
    x <- x[!duplicated(x),]
    x <- x[,names(screen14)]
  }
  
  screen14 <- clean_up(screen14)
  screen13 <- clean_up(screen13)
  screen12 <- clean_up(screen12)
  screen11 <- clean_up(screen11)
  screen10 <- clean_up(screen10)
  screen09 <- clean_up(screen09)
  screen08 <- clean_up(screen08)
  screen07 <- clean_up(screen07)
  
  ############################################
  ######### Make one master df
  ############################################
  screen <- as.data.frame(rbind(screen07,
                                screen08,
                                screen09,
                                screen10,
                                screen11,
                                screen12,
                                screen13,
                                screen14))
  
  
  screen$Race <- factor(screen$Race, levels=c("W", "B", "M", "H", "A", "I"))
  
  
  ############################################
  ######### Fix missing decimals in screen$Growth_Height
  ############################################
  screen$Growth_Height <- as.numeric(screen$Growth_Height)
  screen$Growth_Height <- ifelse(screen$Growth_Height > 100,
                                 screen$Growth_Height/10,
                                 screen$Growth_Height)
  
  ############################################
  ######### Fix BMI
  ############################################
  screen$Growth_Weight <- as.numeric(screen$Growth_Weight)
  screen$bmi <- screen$Growth_Weight / ((screen$Growth_Height)^2) * 703
  
  
  ############################################
  ######### Read in data to calculate percentile values
  ############################################
  setwd(public)
  lms <- read.csv("lms.csv")
  lms <- lms[-c(1, 220),]
  lms$Agemos <- as.numeric(as.character(lms$Agemos)) - 0.5
  #convert all columns to numeric data
  for (i in colnames(lms)){
    lms[,which(colnames(lms) == i)] <-
      as.numeric(as.character(lms[,which(colnames(lms) == i)]))}
  #These data were downloaded on 6 February 2014 from 
  #http://www.cdc.gov/growthcharts/percentile_data_files.htm
  #good article for calculating this: http://www.cdc.gov/nchs/data/nhsr/nhsr063.pdf 
  # i think the pdf has an error in the placement of the -1
  #m = median 
  #s = generalized coefficient of variation
  #l = power in the Box-Cox transformation
  #z = z-score
  #x = percentile
  
  ############################################
  ######### Calculate percentile for age values
  ############################################
  
  #boys
  lms1 <- lms[which(lms$Sex == 1),]
  #girls
  lms2 <- lms[which(lms$Sex == 2),]
  
  #eliminate those older than 240 months
  screen <- screen[which(screen$Age_Months <= 240),]
  
  screen$X <- screen$bmi
  screen$id <- 1:nrow(screen)
  screen$Z <- NA
  
  #boys
  for (i in screen$id[which(screen$Gender == "M")]){
    screen$Z[which(screen$id == i &
                     screen$Gender == "M")] <-
      (  (    screen$X[which(screen$id == i)] /
                lms1$M[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                  lms1$L[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
      (lms1$L[which(lms1$Agemos==screen$Age_Months[which(screen$id == i)])]*
         lms1$S[which(lms1$Agemos == screen$Age_Months[which(screen$id == i)])])}
  
  #girls
  for (i in screen$id[which(screen$Gender == "F")]){
    screen$Z[which(screen$id == i &
                     screen$Gender == "F")] <-
      (  (    screen$X[which(screen$id == i)] /
                lms2$M[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]) ^ (
                  lms2$L[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])]  )-1) /
      (lms2$L[which(lms2$Agemos==screen$Age_Months[which(screen$id == i)])]*
         lms2$S[which(lms2$Agemos == screen$Age_Months[which(screen$id == i)])])}
  
  
  #Remove extreme values using CDC's guidelines
  #http://www.cdc.gov/pcd/issues/2009/jan/08_0007.htm
  screen <- screen[which(is.infinite(screen$Z)== FALSE &
                           screen$Z >= -4 &
                           screen$Z <= 5 ),]
  
  
  ##############################
  # Make a normal/oveweight/obese column
  #############################
  screen$cat <- factor(ifelse(screen$Z < 1.036436, 
                              "normal",
                              ifelse(screen$Z >=1.036436 & screen$Z <1.64485,
                                     "overweight",
                                     ifelse(screen$Z >=1.64485,
                                            "obese",
                                            NA))), levels=c("normal", "overweight", "obese"))
  ##############################
  # Make a normal/oveweight (binary) column
  #############################
  screen$catbi <- factor(ifelse(screen$Z < 1.036436, 
                                "normal",
                                ifelse(screen$Z >=1.036436,
                                       "overweight",
                                       NA)))
  
  # Get year
  screen$Health_Screening_Date <- as.Date(screen$Health_Screening_Date)
  screen$year <- as.numeric(format(screen$Health_Screening_Date, '%Y'))
  screen$day_number <- as.numeric(format(screen$Health_Screening_Date, '%j'))
  screen$year <- ifelse(screen$day_number <= 150, screen$year - 1, screen$year)
  
  # Fix grade
  screen$Grade <- ifelse(screen$Grade %in% c('PK', 'KG'),
                         as.character(screen$Grade),
                         as.character(as.numeric(as.character(screen$Grade))))
  
  
  
  #####
  # MANUALLY CLEAN SCHOOL NAMES
  #####
  screen$School_Name <- as.character(screen$School_Name)
  screen$school <- ifelse(screen$School_Name %in%
                            c('ALACHUA LEARNING CENTER ',
                              'ALACHUA LEARNING CENTER ELEMENTARY',
                              'ALACHUA LEARNING CENTER MIDDLE',
                              'ALACHUA LEARNING CNR ELE'),
                          'ALACHUA LEARNING CENTER',
                          ifelse(screen$School_Name == 'CARING & SHARING LEARNIN',
                                 'CARING & SHARING LEARNING',
                                 ifelse(screen$School_Name == 'DUVAL, CHARLES W. ELEMEN',
                                        'DUVAL, CHARLES W. ELEMENTARY',
                                        ifelse(screen$School_Name == 'FORT CLARKE MIDDLE SCHOO',
                                               'FORT CLARKE MIDDLE SCHOOL',
                                               ifelse(screen$School_Name == 'FOSTER, STEPHEN ELEMENTA',
                                                      'FOSTER, STEPHEN ELEMENTARY',
                                                      ifelse(screen$School_Name == 'HIGH SPRINGS COMMUNITY S',
                                                             'HIGH SPRINGS COMMUNITY SCHOOL',
                                                             ifelse(screen$School_Name == 'LINCOLN, ABRAHAM MIDDLE ',
                                                                    'LINCOLN, ABRAHAM MIDDLE SCHOOL',
                                                                    ifelse(screen$School_Name == 'MEADOWBROOK ELEMENTARY S',
                                                                           'MEADOWBROOK ELEMENTARY SCHOOL',
                                                                           ifelse(screen$School_Name == 'MEBANE, A.L. MIDDLE SCHO',
                                                                                  'MEBANE, A.L. MIDDLE SCHOOL',
                                                                                  ifelse(screen$School_Name == 'METCALFE, W.A. ELEMENTAR',
                                                                                         'METCALFE, W.A. ELEMENTARY',
                                                                                         ifelse(screen$School_Name == 'MICANOPY AREA COOP SCHOO',
                                                                                                'MICANOPY AREA COOP SCHOOL',
                                                                                                ifelse(screen$School_Name == 'RAWLINGS, MARJORIE K. EL',
                                                                                                       'RAWLINGS, MARJORIE K. ELEMENTARY',
                                                                                                       ifelse(screen$School_Name == 'SHELL, CHESTER ELEMENTAR',
                                                                                                              'SHELL, CHESTER ELEMENTARY',
                                                                                                              ifelse(screen$School_Name == 'SWEETWATER BRANCH ELEMENTARY',
                                                                                                                     'SWEETWATER BRANCH ACADEMY',
                                                                                                                     ifelse(screen$School_Name == 'TALBOT, WILLIAM S. ELEME',
                                                                                                                            'TALBOT, WILLIAM S. ELEMENTARY',
                                                                                                                            ifelse(screen$School_Name == 'TERWILLIGER, MYRA ELEMEN',
                                                                                                                                   'TERWILLIGER, MYRA ELEMENTARY',
                                                                                                                                   ifelse(screen$School_Name == 'WILES, KIMBALL ELEMENTAR',
                                                                                                                                          'WILES, KIMBALL ELEMENTARY',
                                                                                                                                          ifelse(screen$School_Name == 'WILLIAMS, JOSEPH ELEMENT',
                                                                                                                                                 'WILLIAMS, JOSEPH ELEMENTARY',
                                                                                                                                                 screen$School_Name))))))))))))))))))
  screen$School_Name <- NULL
  
  
  #PER HIDAHIS FIGUEROA MESA'S FEB 7 2014 EMAIL
  
  #race key %%%%%%%%%%%%%%%%%%%%%%
  #W White, Non Hispanic
  #B Black, Non Hispanic
  #H Hispanic
  #A Asian or Pacific Islander
  #I American Indian or Alaskan Native
  #M Multiracial
  screen$Race <- factor(screen$Race,
                        levels=c("W","I","A",  "B", "H", "M"),
                        labels=c("White",
                                 "Indian",
                                 "Asian",
                                 "Black",
                                 "Hispanic",
                                 "Multiracial"))
  
  
  #lunchStatus Key: %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #1  Applied Not Eligible
  #0  Did not apply
  #2  Eligible for free lunch
  #6	Eligible for free lunch/Direct Certified/Decline
  #9	Eligible for free lunch/Direct Certified
  #3	Eligible Reduced
  #4	Enrolled USDA approved Prov 2 school
  #Z	Unknown
  
  screen$lunch <- factor(Recode(screen$Lunch_Status,
                                "'1' = 'not_free';
                                '0' = 'not_free';
                                '2' = 'free';
                                '6' = 'free';
                                '9' = 'free';
                                '3' = 'free'"))
  screen$Lunch_Status <- NULL
  screen$lunch[which(screen$lunch == 'Z')] <- NA
  screen$lunch <- factor(screen$lunch)
  
  #####
  # 6. RECODE THE RACE VARIABLE INTO A BINARY
  #    WHITE / NON-WHITE VARIABLE
  #####
  screen$race <- Recode(screen$Race,
                        "'W' = 'White';
                        'B' =  'Black';
                        'H' = 'Hispanic';
                        'A' = 'Asian';
                        'I' = 'Native Amer.'; 
                        'M' =  'Multiracial'")
  screen$race_bi <- factor(ifelse(screen$race == "White",
                                  "white",
                                  "non_white"))
  screen$Race <- NULL
  
  
  # Get percentile
  screen$bmi_percentile <- pnorm(screen$Z) * 100
  
  # Get rid of 2015 data
  screen <- screen[screen$year != 2015,]
  
  #####
  # SAVE A CHECKPOINT
  #####
  save.image(paste0(private, 'temp.RData'))
}



#####
# RESET DIRECTORIES (in case image was saved on other system)
#####
set_directories()
