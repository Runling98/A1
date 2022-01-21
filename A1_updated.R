########################### 
# HW1: Data(1) 
# Runling Wu 
# Date: Jan.12/2022
###########################

setwd("~/Dropbox/Duke/Econ 613/Assignment/A1/Data/")
getwd()

library(tidyverse)
library(crosstable)
install.packages("remotes")
remotes :: install_github("jasonelaw/nrsa")
library(remotes)
library(ineq)
library(fs)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(purrr)
library(readr)
library(arsenal)
library(dplyr)
library(xtable)

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

mean(wage_earner$wage)# mean 22443

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
duplicated(IDHH11_paris$idind)

# since it has duplicated idind, thus we just count the number of rows 
nrow(IDHH11_paris)

# 3514

# 3514 individuals in Paris in 2011.



##############################
# Exercise 2: Merge Datasets
##############################

##############
# Part One 
##############

# 2.1 Read all individual datasets from 2004 to 2019. Append all these datasets.

data_path1 <- "~/Dropbox/Duke/Econ 613/Assignment/A1/Data/ID/"
files <- dir(data_path1, pattern = "datind*")

ID04_19 <- files %>%
  map(~ paste(data_path1, .x, sep = "")) %>%
  map(read_csv, col_types = cols(.default = "c"), col_select = !c(1)) %>%
  bind_rows

# 2.2 Read all household datasets from 2004 to 2019. Append all these datasets.]

data_path2 <- "~/Dropbox/Duke/Econ 613/Assignment/A1/Data/HD/"
files <- dir(data_path2, pattern = "dathh*")

HD04_19 <- files %>%
  map(~ paste(data_path2, .x, sep = "")) %>%
  map(read_csv, col_types = cols(.default = "c"), col_select = !c(1)) %>%
  bind_rows

# 2.3 List the variables that are simultaneously present in the individual and 
# household datasets.

ID04_19[, names(ID04_19) %in% names(HD04_19)]

# variable names: idmen, year 

# 2.4 Merge the appended individual and household datasets.

IDHH <- merge(ID04_19, HD04_19, by = c("idmen", "year")) 

# write.csv(IDHH,"~/Dropbox/Duke/Econ 613/Assignment/A1/Data/IDHH.csv", 
#           row.names = FALSE)

# check possibilities of duplicates 

nrow(IDHH)- count(distinct(IDHH, idind, year)) #32 

# 32 duplicates in 2013, drop these duplicates 

IDHH_new <- distinct(IDHH, idind, year, .keep_all = T)

IDHH_old <- distinct(IDHH, idind, year, .keep_all = T)

sapply(IDHH_new, class) 

# data tidying work 
# transfer from strings to integers 

cols.num <- c("idmen","year", "idind", "datent", "wage", "myear")
IDHH_new[cols.num] <- sapply(IDHH_new[cols.num],as.numeric)

sapply(IDHH_new, class) 

################################################
# Part Two Version One 
# Version 1: we consider the dataset as a whole 
# We consider the distinct households or individuals 
################################################


# 2.5 Number of households in which there are more than four family members
# we see this question as households across all the survey years has more than
# four family members 

fam_count_v1 <- IDHH_new %>%
  group_by(idmen) %>%
  summarise(member_num = n()) %>% 
  filter(member_num > 4) %>% 
  summarise(count = n())

fam_count_v1 # 27195 

# 2.6 Number of households in which at least one member is unemployed
# we compute the number of households as long as one member has ever been 
# unemployed during 2004-2019. 

unemployed_year_v1 <- IDHH_new %>%
  group_by(idmen) %>% 
  summarise(unemployed = any(empstat == "Unemployed")) %>% 
  filter(unemployed) %>% 
  summarise(total_unemployed = n())

unemployed_year_v1 #8161 


# 2.7 Number of households in which at least two members are of the 
# same profession
# we compute the number of households as long as at least two members of the 
# same profession during 2004-2019. 

professionals_v1 <- IDHH_new %>%
  group_by(idmen) %>% 
  summarise(coworkers = any(duplicated(profession))) %>% 
  filter(coworkers) %>% 
  summarise(total_coworkers = n())

professionals_v1 # 36232


# 2.8 Number of individuals in the panel that are from household-Couple with kids
#  Union 

Couple_wkids_v1u <- IDHH_new %>%
  filter(mstatus == "Couple, with Kids") %>% 
  summarise(count = n()) 

Couple_wkids_v1u # 209371 

# intersection 

Couple_wkids_v1i <- IDHH_new %>%
  filter(mstatus == "Couple, with Kids") 

  count(distinct(Couple_wkids_v1i, idind))

Couple_wkids_v1u # 55094 


# 2.9 Number of individuals in the panel that are from Paris.
# Union

Paris_v1u <- IDHH_new %>%
  filter(location == "Paris") %>% 
  summarise(count = n()) 

Paris_v1u #51904 

# intersection 

Paris_v1i <- IDHH_new %>%
  filter(location == "Paris") 

count(distinct(Paris_v1i, idind)) #14563


# 2.10 Find the household with the most number of family members. 
# Report its idmen.

most_mem_v1 <- IDHH_new %>%
  group_by(year, idmen) %>%
  summarise(HH_num = n()) %>%
  filter(min_rank(desc(HH_num)) == 1)

xtable(most_mem_v1)

# idmen 2207811124040100 &  14 \\ 
# idmen 2010 & 2510263102990100 &  14 \\

# 2.11 Number of households present in 2010 and 2011.
# There are two possibilities 
# (1) union: we consider the number of household in 2010 and 2011. 

HH1011 <- filter(IDHH_new, year == "2010" | year == "2011"); 
count(distinct(HH1011, idmen)) #13424

# (2) intersection: we consider the number of household in both 2010 and 2011. 

HH1011 %>% 
  group_by(idmen, year) %>% 
  filter(year == 2010 | year == 2011) %>%
  summarise() %>% summarise(HH1011_num = n()) %>% 
  filter(HH1011_num == 2) %>% summarise(count = n())

# 8984


###########################################
# Part Two Version Two 
# Version 2: we consider for each year 
###########################################

# 2.5 Number of households in which there are more than four family members

fam_count_v2 <- IDHH_new %>%
  group_by(year, idmen) %>%
  summarise(member_num = n()) %>% 
  filter(member_num > 4) %>% 
  summarise(count = n())

fam_count_v2

# 2.6 Number of households in which at least one member is unemployed
# we compute the distinct number of households as long as one member has been 
# unemployed during 2004-2019. 
table(IDHH_new$empstat)

unemployed_year_v2 <- IDHH_new %>%
  group_by(year, idmen) %>% 
  summarise(unemployed = any(empstat == "Unemployed")) %>% 
  filter(unemployed) %>% 
  summarise(total_unemployed = n())

unemployed_year_v2

# 2.7 Number of households in which at least two members are of the 
# same profession

professionals_v2 <- IDHH_new %>%
  group_by(year, idmen) %>% 
  summarise(coworkers = any(duplicated(profession))) %>% 
  filter(coworkers) %>% 
  summarise(total_coworkers = n())

professionals_v2

# 2.8 Number of individuals in the panel that are from household-Couple with kids

Couple_wkids_v2 <- IDHH_new %>%
  group_by(year) %>% 
  filter(mstatus == "Couple, with Kids") %>% 
  summarise(count = n()) 

Couple_wkids_v2

# 2.9 Number of individuals in the panel that are from Paris.

Paris_v2 <- IDHH_new %>%
  group_by(year) %>% 
  filter(location == "Paris") %>% 
  summarise(count = n()) 

Paris_v2  

# 2.10 Find the household with the most number of family members. 
# Report its idmen.

most_mem <- IDHH_new %>%
  group_by(year, idmen) %>%
  summarise(HH_num = n()) %>%
  filter(min_rank(desc(HH_num)) == 1)

xtable(most_mem)

# 2.11 Number of households present in 2010 and 2011.
# There are two possibilities 
# (1) union: we consider the number of household in 2010 and 2011. 

HH1011 <- filter(IDHH_new, year == "2010" | year == "2011"); 
count(distinct(HH1011, idmen)) #13424

# (2) intersection: we consider the number of household in both 2010 and 2011. 

HH1011 %>% 
  group_by(idmen, year) %>% 
  filter(year == 2010 | year == 2011) %>%
  summarise() %>% summarise(HH1011_num = n()) %>% 
  filter(HH1011_num == 2) %>% summarise(count = n())

# 8984

##############################
# Exercise 3: Migration
##############################

# 3.1 Find out the year each household enters and exit the panel. 
enter_exit <- IDHH_new %>%
  group_by(idmen) %>%
  summarise(exit = max(year),
            enter = min(year)) 

# Report the distribution of the time spent in the survey for each household.
enter_exit$duration <- enter_exit$exit - enter_exit$enter + 1 

ggplot(enter_exit) + 
  geom_bar(mapping = aes( x = duration))

summary(enter_exit$duration)


# 3.2 based on datent, identify whether or not a household moved into its 
# current dwelling at the year of survey. 
table(IDHH_new$move)

IDHH_new$moved <- ifelse(IDHH_new$datent == IDHH_new$year, 1, 0)

newmover <- IDHH_new %>%
  group_by(idmen, year) %>%
  head(10) 

xtable(select(newmover, idmen, year, moved))

# plot the share of individuals in that situation across years.
table(IDHH_old$move)

IDHH_old$moved <- ifelse(IDHH_old$datent == IDHH_old$year, 1, 0)
newmover <- filter(IDHH_old, moved == 1)

IDHH_old$number <- 1
table(IDHH_old$moved)

share <- IDHH_old %>%
  group_by(year) %>%
  summarise(total = sum(number),
            movers = sum(moved, na.rm = T),
            share = movers/total)

plot(share$year, share$share)
lines(share$year, share$share)


# 3.3 identify whether or not household migrated at the year of survey
# I define migration as household either moved or migrated 

IDHH_old$migrate <- (IDHH_old$myear == IDHH_old$year | IDHH_old$move == 2)
                                                                                   
# Report the first 10 rows of your result and plot the share of individuals 
# in that situation across years.

migration <- IDHH_old %>%
  head(10) 

# Report the first 10 rows 
xtable(select(migration, idmen, year, migrate)) 

mig <- IDHH_old %>% filter(migrate == TRUE)

IDHH_old$migratedum <- ifelse(IDHH_old$migrate == TRUE, 1, 0)

share_m <- IDHH_old %>%
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

keeps <- c("idind", "idmen", "profession", "empstat", "year", "migrate")
IDHH_up = IDHH_old[keeps]

IDHH_mig <- IDHH_up %>% 
  filter(migrate == 1)

# we dont want to consider missing value in either profession or empstat. 
# Thus we drop any rows with missing in ourdata. 
# The reason for that is we do not consider missing value as the same profession/
# empstat. 

IDHH_complete <- na.omit(IDHH_mig)

migrate_diff <- IDHH_complete %>%
  group_by(idmen, idind) %>%
  summarise(diff_occ = n_distinct(profession),
            diff_emp = n_distinct(empstat)) 

mig_diff_op <- filter(migrate_diff, diff_occ > 1 | diff_emp >1)
mig_diff_op$distinc <- n_distinct(mig_diff_op$idmen)
table(mig_diff_op$distinc)

##########################
# Exercise 4: Attrition
#########################
# two ways of approaching this question: 
# Version I: create a list for all the idind, counting for reentry

year_att <- IDHH_new %>%
  group_by(idind) %>%
  mutate(year_list = list(unique(year))) %>%
  ungroup %>%
  mutate(att = !map2_lgl(year+1, year_list, is.element)) %>%
  filter(year < max(year)) %>%
  group_by(year) %>% 
  summarise(att_rate = mean(att) * 100)

xtable(year_att)
  
# Version II: year of entry and years of exit

year_range <- IDHH_new %>% 
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
