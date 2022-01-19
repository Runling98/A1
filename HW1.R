########################### 
# HW1: Data(1) 
# Runling Wu 
# Econ 613 
# Date: Jan.12/2022
###########################

setwd("~/Dropbox/Duke/Econ 613/Assignment/A1/Data/")
getwd()


install.packages("tidyverse")
library(tidyverse)

install.packages("crosstable")
librray("crosstable")

install.packages("remotes")
remotes :: install_github("jasonelaw/nrsa")
library(remotes)

install.packages("ineq")
library(ineq)

install.packages("fs")
library(fs)

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr, purrr)    # alternatively, this also loads %>%

install.packages("purrr")
library(purrr)

install.packages("readr")
library("readr")

install.packages("arsenal")
library("arsenal")

install.packages("dplyr")
library("dplyr")

library("dplyr","tidyverse")
library("crosstable", "remotes")
library("ineq", "fs")
library("magrittr" ,"readr")
library("arsenal", "purrr")

##############################
# Exercise 1: Basic Statistics
##############################

HH07 <- read.table("dathh2007.csv", sep = ",", header = TRUE)
HH05 <- read.table("dathh2005.csv", sep = ",", header = TRUE)
ID08 <- read.table("datind2008.csv", sep = ",", header = TRUE)
ID16 <- read.table("datind2016.csv", sep = ",", header = TRUE)
ID09 <- read.table("datind2009.csv", sep = ",", header = TRUE)
ID05 <- read.table("datind2005.csv", sep = ",", header = TRUE)
ID19 <- read.table("datind2019.csv", sep = ",", header = TRUE)
ID10 <- read.table("datind2010.csv", sep = ",", header = TRUE)
ID11 <- read.table("datind2011.csv", sep = ",", header = TRUE)
HD11 <- read.table("dathh2011.csv", sep = ",", header = TRUE)

# 1.1 Number of households surveyed in 2007.

nrow(HH07)  # 10498 : There are 10498 

# 1.2 Number of households with marital status “Couple with kids” in 2005.
nrow(filter(HH05, mstatus == "Couple, with Kids")) # 3374

# 1.3 Number of individuals surveyed in 2008.
nrow(ID08) # 25510 

# 1.4  Number of individuals aged between 25 and 35 in 2016.
nrow(filter(ID16, age >= 25 & age <=35)) # 2765 

# 1.5 Cross-table gender/profession in 2009.
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(ID09, row.vars = "profession", col.vars = "gender")

# 1.6 Distribution of wages in 2005 and 2019. Report the mean,
# the standard deviation, the inter-decile ratio D9/D1 and the Gini coefficient.

# we do not consider 0 wage in all calculation. 
summary(ID05$wage)

wage_earner <- ID05 %>% filter(wage != 0 & !is.na(wage) )

summary(wage_earner$wage) # mean 19603
sd(wage_earner$wage, na.rm = T) # sd 18076.71

quantile(wage_earner$wage, na.rm = TRUE, c(0.1, 0.9))

# 10%     90% 
# 4547.0 40452.5
40452.5/4547.0 

# use base R to calcualte gini coefficent 
sorted_wages <- sort(wage_earner$wage)
wage_count <- length(sorted_wages)
indices <- c(1: wage_count)

gini_numerator <- sorted_wages %*% (2*indices - wage_count - 1)
gini_denominator_1 <- sum(sorted_wages)
gini_coefficient <- (gini_numerator/gini_denominator_1)/wage_count

# just to verify again the results using base R 
ineq(wage_earner$wage, type = "Gini")

# (b) distribution of wages in 2019

ID19_wage <- ID19 %>% filter(wage != 0 & !is.na(wage) )

summary(ID19_wage$wage) # mean 27579
sd(ID19_wage$wage, na.rm = T) # sd 25107.19

quantile(ID19_wage$wage, na.rm = TRUE, c(0.1, 0.9))

# 10%     90% 
#  3634.0 50375.6 
50375.6/3634.0
# D9/D1 = 13.8623

# use base R to calcualte gini coefficent
sorted_wages19 <- sort(ID19_wage$wage)
wage_count19 <- length(sorted_wages19)
indices19 <- c(1: wage_count19)

gini_numerator19 <- sorted_wages19 %*% (2*indices19 - wage_count19 - 1)
gini_denominator_119 <- sum(sorted_wages19)
gini_coefficient19 <- (gini_numerator19/gini_denominator_119)/wage_count19

ineq(ID19_wage$wage, type = "Gini")

# 1.7 Distribution of age in 2010. Plot an histogram. 
# Is there any difference between men and women?
hist(ID10$age)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

hist(ID10[ID10$gender == "Male", ]$age)

hist(ID10[ID10$gender == "Female", ]$age) 

hg_male <- hist(ID10[ID10$gender == "Male", ]$age, breaks = 10, plot = FALSE)

hg_fe <- hist(ID10[ID10$gender == "Female", ]$age, breaks = 10, plot = FALSE)

plot(hg_male, col = c1)
plot(hg_fe, col = c2, add = TRUE)

# 1.8 Number of individuals in Paris in 2011.
IDHH11 <- merge(ID11, HD11, by = "idmen")
dim(ID11)
dim(HD11)
dim(IDHH11) # all matched 

IDHH11_paris <- filter(IDHH11, location == "Paris")
count(distinct(IDHH11_paris, idind)) 

# 1552

# 1552 individuals in Paris in 2011.

##############################
# Exercise 2: Merge Datasets
##############################

##############
# Part One 
##############

# 2.1 Read all individual datasets from 2004 to 2019. Append all these datasets.

data_path <- "~/Dropbox/Duke/Econ 613/Assignment/A1/Data/ID/"
files <- dir(data_path, pattern = "*.csv")

ID04_19 <- files %>%
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)
data_path

# 2.2 Read all household datasets from 2004 to 2019. Append all these datasets.]

data_path <- "~/Dropbox/Duke/Econ 613/Assignment/A1/Data/HD/"
files <- dir(data_path, pattern = "*.csv")

HD04_19 <- files %>%
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)
data_path

# 2.3 List the variables that are simultaneously present in the individual and 
# household datasets.

ID04_19[, names(ID04_19) %in% names(HD04_19)]

# variable names: idmen, year 

# 2.4 Merge the appended individual and household datasets.

IDHH <- merge(ID04_19, HD04_19, by = c("idmen", "year")) 

# write.csv(IDHH,"~/Dropbox/Duke/Econ 613/Assignment/A1/Data/IDHH.csv", 
#           row.names = FALSE)

##############
# Part Two 
##############

# 2.5 Number of households in which there are more than four family members

fam_count <- IDHH %>%
  group_by(idmen) %>%
  summarise(member_num = n_distinct(idind)) 

max(fam_count$member_num) #4

# since the maximum number of individuals is four, we have zero number of hou
# households in which there are more than four family members. 

# 2.6 Number of households in which at least one member is unemployed
# we compute the distinct number of households as long as one member has been 
# unemployed during 2004-2019. 
table(IDHH$empstat)

unemployed <- filter(IDHH, empstat == "Unemployed");
count(distinct(unemployed, idmen)) 

# 8161 households in which at least one member is unemployed. 

# 2.7 Number of households in which at least two members are of the 
# same profession

professionals <- filter(IDHH, !is.na(profession) & profession != 0);
coworkers <- professionals %>% group_by(idmen) %>% filter(duplicated(profession));
count(distinct(ungroup(coworkers), idmen))

# 21375 

# 2.8 Number of individuals in the panel that are from household-Couple with kids
Couple_wkids <- filter(IDHH, mstatus == "Couple, with Kids"); 
count(distinct(Couple_wkids, idind))

# 15646 individuals in the panel that are from household-Couple with kids. 

# 2.9 Number of individuals in the panel that are from Paris.
table(IDHH$location)

Paris <- filter(IDHH, location == "Paris"); 
count(distinct(Paris, idind)) #6186

# 6186 individuals in the panel that are from Paris.  

# 2.10 Find the household with the most number of family members. 
# Report its idmen.
most_mem <- subset(fam_count, member_num == 4)
table(most_mem$idmen)

# These households are 1500597023470100, 1512466015750100, 1607610011670100, 
# 1707764063150100, 2007953001000101, 2100493042350100, 
# 2303559016060102 2402178057590101 

# 2.11 Number of households present in 2010 and 2011.
# we consider the number of distinct household in 2010 and 2011. 

HH1011 <- filter(IDHH, year == "2010" | year == "2011"); 
count(distinct(HH1011, idmen)) #13424

# There are 13424 number of households present in 2010 and 2011. 

##############################
# Exercise 3: Migration
##############################

# 3.1 Find out the year each household enters and exit the panel. 
enter_exit <- IDHH %>%
  group_by(idmen) %>%
  summarise(exit = max(year),
            enter = min(year)) 

# Report the distribution of the time spent in the survey for each household.
enter_exit$duration <- enter_exit$exit - enter_exit$enter

hist(enter_exit$duration)
summary(enter_exit$duration)

# 3.2 based on datent, identify whether or not a household moved into its 
# current dwelling at the year of survey. 
table(IDHH$move)

IDHH$moved <- ifelse(IDHH$datent == IDHH$year, 1, 0)
newmover <- filter(IDHH, moved == 1)

# Report the first 10 rows 
head(newmover, 10)

# plot the share of individuals in that situation across years.
IDHH$number <- 1
table(IDHH$moved)

share <- IDHH %>%
  group_by(year) %>%
  summarise(total = sum(number),
           movers = sum(moved, na.rm = T),
           share = movers/total)

plot(share$year, share$share)
lines(share$year, share$share)

# 3.3 identify whether or not household migrated at the year of survey

IDHH$migrate <- (IDHH$myear == IDHH$year | IDHH$move == 2)


# Report the first 10 rows of your result and plot the share of individuals 
# in that situation across years.

head(IDHH, 10)

mig <- IDHH %>% filter(migrate == TRUE)

IDHH$migratedum <- ifelse(IDHH$migrate == TRUE, 1, 0)

share_m <- IDHH %>%
  group_by(year) %>%
  summarise(total = sum(number),
            migrant = sum(migratedum, na.rm = T),
            share_m = migrant/total)

plot(share_m$year, share_m$share_m)
lines(share_m$year, share_m$share_m)

# 3.4 Mix the two plots you created above in one graph, clearly label the graph.
# Do you prefer one method over the other? Justify.

plot(share$year, share$share, type = "b", pch = 19, ylim = c(0.01, 0.055), 
     xlim = c(2004, 2019),
     col = "red", xlab = "year", ylab = "share of fractions")
lines(share_m$year, share_m$share_m, pch = 18, col = "blue", lty = 2)
legend("bottomleft", legend = c("share of movers", "share of migrants"), 
       col = c("red", "blue"), lty = 1:2, cex = 0.8)

# 3.5 For household migrate, 
# how many households had at least one family member changed his/her 
# profession or employment status. 

# use a subset 

keeps <- c("idind", "idmen", "profession", "empstat", "year")
IDHH_up = IDHH[keeps]

# we dont want to consider missing value in either profession or empstat. 
# Thus we drop any rows with missing in ourdata. 
# The reason for that is we do not consider missing value as the same profession/
# empstat. 

IDHH_complete <- na.omit(IDHH_up)

migrate_diff <- IDHH_complete %>%
  group_by(idmen, idind) %>%
  summarise(diff_occ = n_distinct(profession),
            diff_emp = n_distinct(empstat)) 

mig_diff_op <- filter(migrate_diff, diff_occ > 1 | diff_emp >1)
mig_diff_op$distinc <- n_distinct(mig_diff_op$idmen)
table(mig_diff_op$distinc)


# 15645 

##########################
# Exercise 4: Attrition
#########################

colnames(IDHH)

table(IDHH$...1.x)

year_range <- IDHH %>% 
  group_by(idind) %>% 
  summarise(begin = min(year), end = max(year));

begin_count <- count(year_range, begin);
end_count <- count(year_range, end);

year_count <- left_join(begin_count, end_count,by = c("begin" = "end"));
colnames(year_count) = c("year", "begin", "end");

year_count_c <- year_count;
for (row in 2: nrow(year_count_c) )
{
  year_count_c[row, "begin"] 
  <- year_count_c[row-1, "begin"]  
  - year_count[row-1, "end"]
  + year_count[row, "begin"]
};
mutate(year_count_c, attrition_rate = end/begin)


########### end ###############
