

library(tidyverse)
library(dplyr)

# load in the  QUALTRICS output files 
Qp2 <- read.csv("C:/Users/andyl/Downloads/ALLTMturkPart2.MainPrimary.round4and5_5.22 to 5.27_choicetext.csv")

Qp1 <- read.csv("C:/Users/andyl/Downloads/ALLTMturkPart1.MainPrimary.round4and5_5.22 to 5.27_choicetext.csv")
               


#batch               
batch <- read.csv("C:/Users/andyl/Downloads/ALLTMTurk Batch Main Round 4 and 5 Combined Link.csv")  




#email follow-up stuff 
Email_info <- read.csv("C:/Users/andyl/Downloads/ALLTEmails information.csv") #this is the information of the respondents that reached out


Email_follow_output1 <- read.csv("C:/Users/andyl/Downloads/ALLTEmailFollowUp_September+16,+2024_08.56.csv")
 

#Rejects for ttetst  and whatnot 
original_survey_rejects <- read.csv("C:/Users/andyl/Downloads/ALLTMturk_1539_CompleteRejects_9.16.csv")
#View(original_survey_rejects)
#-----------------------------------------------------------------------------------------------------------                
# Remove the first two rows 
p1_cleaned <- Qp1[-(1:2), ] 

ncol(p1_cleaned)
p2_cleaned <- Qp2[-(1:2), ] 


#View(p1_cleaned)

#View(p2_cleaned)




#---------------SURVEY PART 1 & 2 MERGE -----------------------------------------------------------------
#Merge through left join, keeps all rows from mp1
Q_Output_Merged <- merge(p1_cleaned, p2_cleaned, by = "Survey_ID", all.x = TRUE)

View(Q_Output_Merged)







#There are two partcipant IDs, so I figure it would be best to create a new variable 
#If they have a particpantId.y, then they completed prt 2 response, if .y is empty then we go with .x from prt 1
Q_Output_Merged$Participant.Link.ID <- ifelse(
  !is.na(Q_Output_Merged$Participant.ID.y), #  If Participant.ID.y (prt2) is not NA
  Q_Output_Merged$Participant.ID.y,         # Then: Use Participant.ID.y (prt 2)
  Q_Output_Merged$Participant.ID.x          # Else: Use Participant.ID.x (prt 1)
)

#View(Q_Output_Merged)


#--------CLEANING Q_OUTPUT AND BATCH RESPONDENTS--------------------------------------------------------------------------------

#These are 8/28 of the respondents from the completed email merge (info+ follow-up output)
#7/8 put their WorkerID as their end of survey code 
#1/8 had their email confirmation code end of survey code

incorrect_obs2 <- which(batch$Answer.surveycode == "A16NLR5V8OKO3K")
batch$Answer.surveycode[incorrect_obs2] <- "FFFF8102757"

incorrect_obs3 <- which(batch$Answer.surveycode == "A21C8LOB5W6QAF")
batch$Answer.surveycode[incorrect_obs3] <- "FFFF6191479"

incorrect_obs4 <- which(batch$Answer.surveycode == "A1SP14W5UKUIS2")
batch$Answer.surveycode[incorrect_obs4] <- "FFFF9365794"

incorrect_obs5 <- which(batch$Answer.surveycode == "A250VREHMWLHPT")
batch$Answer.surveycode[incorrect_obs5] <- "AAAA8014153"


incorrect_obs6 <- which(batch$Answer.surveycode == "A28SLGT2GKXKT7")
batch$Answer.surveycode[incorrect_obs6] <- "FFFF8082470"

incorrect_obs7 <- which(batch$Answer.surveycode == "A3Q1VHJCGDU1J8")
batch$Answer.surveycode[incorrect_obs7] <- "FFFF7981079"

incorrect_obs8 <- which(batch$Answer.surveycode == "A1V0ZQ68QCO8O4")
batch$Answer.surveycode[incorrect_obs8] <- "FFFF7684045"

incorrect_obs9 <- which(batch$Answer.surveycode == "397022")
batch$Answer.surveycode[incorrect_obs9] <- "AAAA6609392"


#---------This will be used to merge batch and outpout----------------------------------
# Extract numeric part from 'Answer.surveycode'
batch$code <- as.numeric(str_extract(batch$Answer.surveycode, "\\d+"))
#---------------------------------------------------------------------------------------
  
  
#Here I fix the 4/28 respondents that never submitted to MTurk
#I don't really like how I had to do this but I again basically "hard coded" the missing values into the batch 
#I plan to try the way that Dr. M & G suggested but this works for now 

#Define the new code and corresponding WorkerIds
new_code <- c("5891393", "5881303", "6872502", "7490677")
new_worker_ids <- c("A30F9C5YKMJ3BO", "A66YRRA681560", "A32ASBT42CHMV8", "A1FP7W6XPI31O6")

#data frame with both 'code' and 'WorkerId' values
new_rows <- data.frame(code = as.numeric(new_code), WorkerId = new_worker_ids)

# Set other columns in 'new_rows' as NA to match the structure of 'batch'
other_columns <- setdiff(names(batch), c("code", "WorkerId"))
for (col in other_columns) {
  new_rows[[col]] <- NA
}

#Appending the new rows to the original 'batch' data frame
batch <- rbind(batch, new_rows)




#---------FIXING ANOTHER RESPONDENT---------------------------------------------
#lets fix the respondent who put their email code for their end of suvrey code
# Locate the incorrect observation
incorrect_obs <- which(batch$code == 565606)

# Correct 
batch$code[incorrect_obs] <- 5654229





#View(batch)



#-------ORIGINAL SURVEY OUTPUT AND BATCH FILE MERGE-------------------------------------------------------------

#Checking for duplicate Participant.Link.ID in Q_Output_Merged
duplicates_Q_Output_Merged <- as.data.frame(table(Q_Output_Merged$Participant.Link.ID))
duplicates_Q_Output_Merged <- duplicates_Q_Output_Merged[duplicates_Q_Output_Merged$Freq > 1, ]
colnames(duplicates_Q_Output_Merged) <- c('Participant.Link.ID', 'Count')

if (nrow(duplicates_Q_Output_Merged) > 0) {
  cat("Duplicate Participant.Link.ID found in Q_Output_Merged:\n")
  print(duplicates_Q_Output_Merged)
} else {
  cat("No duplicate Participant.Link.ID found in Q_Output_Merged.\n")
}


#count(duplicates_Q_Output_Merged)   #2 

#Checking for duplicate Answer.surveycode in batch
duplicates_batch <- as.data.frame(table(batch$Answer.surveycode))
duplicates_batch <- duplicates_batch[duplicates_batch$Freq > 1, ]
colnames(duplicates_batch) <- c('Answer.surveycode', 'Count')

if (nrow(duplicates_batch) > 0) {
  cat("Duplicate Answer.surveycode found in batch:\n")
  print(duplicates_batch)
} else {
  cat("No duplicate Answer.surveycode found in batch.\n")
}
count(duplicates_batch)  #51 duplicates 


Q_Output_Merged$Participant.Link.ID <- as.numeric(Q_Output_Merged$Participant.Link.ID)



#finally merging batch and m_output (outer join)
batch_and_output_merge <- merge(Q_Output_Merged, batch, 
                                by.x = "Participant.Link.ID", 
                                by.y = "code", 
                                all = TRUE)

colnames(batch_and_output_merge)


#trying to figure out where my "code" variable went
sum(is.na(batch_and_output_merge$code))
class(batch$code)


View(batch_and_output_merge)

#print(dim(batch_and_output_merge))

#--------------EMAIL MERGE-----------------------------------------------------

colnames(Email_info)
colnames(Email_follow_output1)
Email_follow_output <- Email_follow_output1[-(1:2), ]

#fix the missing worker ID for mark lynch: 
# Find the row index where the email matches
markymark <- which(Email_info$recievedemail == "marklynch10002000@gmail.com")

# Update the 'workerid' at that specific row index
Email_info$workerid[markymark] <- "AK2YUQUFZN1L6"


#also need to fix mmircea@outlook.com missing MTurkWorkerID in email_followup_output
# Find the row index where the email matches
fixmmircea <- which(Email_follow_output$QualityCheckEmail == "mmircea@outlook.com")

# Update the 'MTurkWorkerID' at that specific row index
Email_follow_output$MTurkWorkerID[fixmmircea] <- "A1PHDT66U6IK4Q"




#ACTUAL MERGE

#get rid of duplicates 
email_info_nodup <- Email_info %>% distinct(workerid, recievedemail, .keep_all = TRUE)
email_follow_output_nodup <- Email_follow_output %>% distinct(MTurkWorkerID, QualityCheckEmail, .keep_all = TRUE)

# Merge by 'workerid' first, keeping all columns
email_merged <- left_join(email_info_nodup, email_follow_output_nodup, 
                          by = c("workerid" = "MTurkWorkerID"))
View(email_merged)

# Check if there are any rows where the 'workerid' merge failed
if (any(is.na(email_merged$MTurkWorkerID))) {
  email_merged <- email_merged %>%
    rows_update(
      left_join(
        filter(email_merged, is.na(MTurkWorkerID)), 
        email_follow_output_nodup, 
        by = c("recievedemail" = "QualityCheckEmail")
      ), 
      by = c("QualityCheckEmail", "recievedemail")
    )
} else 
  




#------------FINAL MERGE-------------------------------------------------------------------------------------------------------------------------------------

# Ensure both columns are in the same data type (e.g., character)
batch_and_output_merge$WorkerId <- as.character(batch_and_output_merge$WorkerId)
email_merged$workerid <- as.character(email_merged$workerid)





# Left join prioritizing WorkerId
final_merged_data <- left_join(email_merged, batch_and_output_merge, 
                               by = c("workerid" = "WorkerId"))




#View(final_merged_data)

#colnames(final_merged_data)







#-------DEMOGRAPHIC INCONSISTENCTY CHECKS----------------------------------------

#colnames(final_merged_data)

#Emailfollowup demo variables 
#ZIP_CODE.x 
#YR_BORN_1.x
#QualityCheckEmail.x

#Email Info 
#recievedemail

#Original survey demo variables 
#QualityCheckEmail.y
#YR_BORN_1.y
#ZIP_CODE.y



#I will first group thee respondents by whether or not they completed the follow-up survey
follow_completed <- final_merged_data[!is.na(final_merged_data$ZIP_CODE.x), ]  #completed the follow-up survey
follow_not_completed <- final_merged_data[is.na(final_merged_data$ZIP_CODE.x), ]  #Did not complete the follow-up survey

View(follow_not_completed)
## Check consistency of ZIP_CODE.x and ZIP_CODE.y
zip_check_follow_completed <- follow_completed$ZIP_CODE.x == follow_completed$ZIP_CODE.y


##Check consistency of YR_BORN_1.x and YR_BORN_1.y
yr_born_check_follow_completed <- follow_completed$YR_BORN_1.x == follow_completed$YR_BORN_1.y


#Check consistency for email
email_check_follow_completed <- follow_completed$QualityCheckEmail.x == follow_completed$QualityCheckEmail.y



# For completed follow-up respondents
completed_summary <- data.frame(
  Respondent = follow_completed$workerid,  
  ZIP_CODE.x = follow_completed$ZIP_CODE.x,  # ZIP from follow-up survey
  ZIP_CODE.y = follow_completed$ZIP_CODE.y,  # Original ZIP from first survey
  ZIP_Consistency = zip_check_follow_completed,  
  
  YR_BORN_1.x = follow_completed$YR_BORN_1.x,  # Year of birth from follow-up survey
  YR_BORN_1.y = follow_completed$YR_BORN_1.y,  #  Year of birth from first survey
  YR_BORN_Consistency = yr_born_check_follow_completed,  
  
  QualityCheckEmail.x = follow_completed$QualityCheckEmail.x,  # Email from follow-up survey
  QualityCheckEmail.y = follow_completed$QualityCheckEmail.y,  #  Email from first survey
  Email_Consistency = ifelse(tolower(follow_completed$QualityCheckEmail.x) == tolower(follow_completed$QualityCheckEmail.y), TRUE, FALSE) 
)


#----DID NOT DO FOLLOWUP----------------------
email_check_not_completed <- follow_not_completed$recievedemail == follow_not_completed$QualityCheckEmail.y


# For no follow-up respondents
not_completed_summary <- data.frame(
  Respondent = follow_not_completed$workerid,  
  recievedemail = follow_not_completed$recievedemail,              # Email from email information
  QualityCheckEmail.y = follow_not_completed$QualityCheckEmail.y,  # Email from original survey 
  Email_Consistency = ifelse(tolower(follow_not_completed$recievedemail) == tolower(follow_not_completed$QualityCheckEmail.y), TRUE, FALSE)
)



#--------------------------------------------------------------------------------------------------------------------





#summary tables
print(completed_summary)
View(completed_summary)


print(not_completed_summary)
view(not_completed_summary)

View(email_merged)




#-----------------------------------------------------------------------------------------------------------------------------------------------------





#Next step is investigating data quality check sucsess or failures from original survey based on the respondets who DID and DID NOT complete the email follow-up survey 


View(original_survey_rejects) 

colnames(original_survey_rejects)

original_survey_rejects$rejectcount<-  rowSums(original_survey_rejects[, c("Consent_Reject", "Age18_Reject", "Sight_Reject", "Sound_Reject", "Qualified_Reject", "Abandon_Reject", "Age_Birth_Reject", "State_Loc_Reject", 
                                                                           "State_Zip_Reject", "Email_Code_Reject", "FAALS_1_Reject", "FAALS_3_Reject", "FAALS_5_Reject", "Color_Reject", "QCH_Reject", "QCH_Txt_Reject", 
                                                                           "Ball_Plane_Reject", "QCK_Reject", "QCK_4_Reject", "Marathon_Reject", "TubMar_Reject", "TubEx_Reject", "Stair_Reject", "Exercise_Reject", "Duplicate_Reject", 
                                                                           "Honest_Sim_Reject", "Ankle_Sim_Reject", "Word_Reject", "BMI_Reject", "Email_NonUS_Reject", "Time_Reject", "Culture_NonUS_Reject", "IP_Dup_Reject", 

                                    
                                                                           
                                                                                                                                                     "Email_Dup_Reject", "QC_Reject", "LLM_BOT_Reject")], na.rm = TRUE)

#Assumptions:
# Histogram for visual inspection
hist(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0],
     main = "Histogram of Reject Count (Non-Completed)", xlab = "Reject Count")
hist(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1],
     main = "Histogram of Reject Count (Completed)", xlab = "Reject Count")

# Q-Q plot for each group
qqnorm(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0], main = "Q-Q Plot (Non-Completed)")
qqline(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0])
qqnorm(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1], main = "Q-Q Plot (Completed)")
qqline(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1])


# Shapiro-Wilk test for normality in each group
shapiro.test(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0])

#we fail to reject the null hypothesis that the data are normally distributed. In other words
#there is no evidence to suggest that the distribution of rejectcount for the individuals who did not complete the follow-up survey deviates from normality.

shapiro.test(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1])

#we reject the null hypothesis that the data are normally distributed.
#indicates that the distribution of rejectcount for the individuals who completed the follow-up survey significantly deviates from normality.


#Actual tests-------------------------------------------------------------------------------------------------------------
final_merged_data_t_test <- merge(final_merged_data, original_survey_rejects[, c("Survey_ID", "rejectcount")], 
                                  by = "Survey_ID", all.x = TRUE)

final_merged_data_t_test$completed_followup <- ifelse(!is.na(final_merged_data_t_test$ZIP_CODE.x), 1, 0)

t_test_results <- t.test(rejectcount ~ completed_followup, data = final_merged_data_t_test)
print(t_test_results)

#fail to reject the null hypothesis. 
#there is not enough evidence to conclude that there is a statistically significant difference in the mean rejectcount between those who completed the follow-up and those who did not.


#effect size
group1 <- final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1]  # Completers
group2 <- final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0]  # Non-completers

# Calculate means
mean1 <- mean(group1, na.rm = TRUE)
mean2 <- mean(group2, na.rm = TRUE)

# Calculate standard deviations
sd1 <- sd(group1, na.rm = TRUE)
sd2 <- sd(group2, na.rm = TRUE)

# Calculate sample sizes
n1 <- length(group1)
n2 <- length(group2)

# Calculate pooled standard deviation
s_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

# Calculate Cohen's d
cohen_d <- (mean1 - mean2) / s_pooled

# Print Cohen's d
cat("Cohen's d:", cohen_d, "\n") #-0.5987843 
#result (∣d∣=0.60)  indicates a medium effect size.

#View(final_merged_data_t_test)
#_________________________________________________________________________________________________________________________________________
#median & mean 
# For completers
median_completed <- median(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1], na.rm = TRUE)
mean_completed <- mean(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1], na.rm = TRUE)

# For non-completers
median_not_completed <- median(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0], na.rm = TRUE)
mean_not_completed <- mean(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0], na.rm = TRUE)


median_completed
mean_completed
median_not_completed
mean_not_completed


summary(final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1])

#Mann-Whitney U test--------------------------------------------------------------------------------------------------------------
# Mann-Whitney U test with approximate p-value due to ties
mann_whitney_test <- wilcox.test(
  rejectcount ~ completed_followup, 
  data = final_merged_data_t_test,
  exact = FALSE  # Use approximate p-value when there are ties
)

# Print the test results
print(mann_whitney_test)
#e fail to reject the null hypothesis at the 0.05 significance level. 
#There is not enough evidence to conclude that there is a statistically significant difference in rejectcount between those who completed the follow-up survey and those who did not.


# Calculate the mean of the sum of reject counts grouped by completed_followup
mean_rejectcount_by_followup <- final_merged_data_t_test %>%
  group_by(completed_followup) %>%
  summarize(mean_rejectcount_sum = mean(sum(rejectcount)))

# Print the result
print(mean_rejectcount_by_followup)
#not completed 77- 5.9
#completed 58 - 3.8

#------------------------------------------------------------------------------------------------------------------
#effect sizes for Mann Whitney U 
Mgroup1 <- final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 1]  # Completers
Mgroup2 <- final_merged_data_t_test$rejectcount[final_merged_data_t_test$completed_followup == 0]  # Non-completers

# Calculate sample sizes
Mn1 <- length(Mgroup1)
Mn2 <- length(Mgroup2)


# Calculate means and standard deviations for each group
Mmean1 <- mean(Mgroup1, na.rm = TRUE)
Mmean2 <- mean(Mgroup2, na.rm = TRUE)
Msd1 <- sd(Mgroup1, na.rm = TRUE)
Msd2 <- sd(Mgroup2, na.rm = TRUE)

# Pooled standard deviation
s_pooled <- sqrt(((Mn1 - 1) * Msd1^2 + (Mn2 - 1) * Msd2^2) / (Mn1 + Mn2 - 2))

# Cohen's d
cohen_d <- (Mmean1 - Mmean2) / s_pooled

# Print Cohen's d
cat("Cohen's d:", cohen_d, "\n")

#Cohen's d: -0.5987843 


# Run Mann-Whitney U
mw <- wilcox.test(Mgroup1, Mgroup2, exact = FALSE)
mw
# Extract U (statistic)
U <- mw$statistic

# Calculate rank-biserial correlation
r <- 1 - (2 * U) / (Mn1 * Mn2)

cat("Rank-Biserial Correlation (r):", r, "\n")
#---------------------------------------------------------------------------------------------------------------


#Z- test of proportions


#Summary of rejects for completed and non-completed groups
reject_summary <- final_merged_data_t_test %>%
  group_by(completed_followup) %>%
  summarize(
    total_respondents = n(),  # Total respondents in each group
    total_rejects = sum(rejectcount > 0, na.rm = TRUE)  # Count respondents with rejects
  )



# Extract counts for z-test
Zn1 <- reject_summary$total_respondents[reject_summary$completed_followup == 1]  # Completers
Zn2 <- reject_summary$total_respondents[reject_summary$completed_followup == 0]  # Non-completers
Zx1 <- reject_summary$total_rejects[reject_summary$completed_followup == 1]  # Rejects (completers)
Zx2 <- reject_summary$total_rejects[reject_summary$completed_followup == 0]  # Rejects (non-completers)

# Calculate proportions
Zp1 <- Zx1 / Zn1  # Proportion of rejects (completers)
Zp2 <- Zx2 / Zn2  # Proportion of rejects (non-completers)

# Pooled proportion
p_pooled <- (Zx1 + Zx2) / (Zn1 + Zn2)

# Standard error for difference in proportions
se_diff <- sqrt(p_pooled * (1 - p_pooled) * ((1 / Zn1) + (1 / Zn2)))

# Z-score
z_score <- (Zp1 - Zp2) / se_diff

# P-value (two-tailed)
p_value <- 2 * pnorm(-abs(z_score))

# Print results
cat("Z-Test of Proportions Results:\n")
cat("Proportion of Rejects (Completers):", Zp1, "\n")
cat("Proportion of Rejects (Non-Completers):", Zp2, "\n")
cat("Z-Score:", z_score, "\n")
cat("P-Value:", p_value, "\n")


#Z-Test of Proportions Results:
#Proportion of Rejects (Completers): 0.8666667 
#Proportion of Rejects (Non-Completers): 0.9230769 
#Z-Score: -0.4813088 


#-----------Looking into stuff for manuscript--------------------------------------------------------------------------------



final_merged_data$workerid


view(final_merged_data_t_test)


final_merged_data_manu <- merge(final_merged_data, original_survey_rejects, 
                                  by = "Survey_ID", all.x = TRUE)
view(final_merged_data_manu)


#This is a count of the number of Email_Code_Rejects for the 28
number_of_email_rejects <- sum(final_merged_data_manu$Email_Code_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_email_rejects))
#7 

#This is a count of the number of Email_Dup_Rejects for the 28(Provided email is identical to another response)
number_of_emaildup_rejects <- sum(final_merged_data_manu$Email_Dup_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_emaildup_rejects))
#7 

#count for age birth reject
number_of_age_birth_rejects <- sum(final_merged_data_manu$Age_Birth_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_age_birth_rejects))
#6

#count for ankle similarity reject 
number_of_anklesim_rejects <- sum(final_merged_data_manu$Ankle_Sim_Reject, na.rm = 1)
print(paste("Number of rejects:", number_of_anklesim_rejects))
#6


#count for State_Loc_reject (Provided state does not match state extracted from IP)
number_of_StateLoc_rejects <- sum(final_merged_data_manu$State_Loc_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_StateLoc_rejects))
#14

#count for State_Loc_reject (Provided state does not match state extracted from IP)
number_of_State_Zip_rejects <- sum(final_merged_data_manu$State_Zip_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_State_Zip_rejects))
#8

#count for QCH (Source of receiving survey is not “Other”)
number_of_QH_rejects <- sum(final_merged_data_manu$QCH_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_QH_rejects))
#12



#count for QCK (Strategy of responding to survey is not “Other”)
number_of_QK_rejects <- sum(final_merged_data_manu$QCK_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_QK_rejects))
#4

#count for TubMar reject (Getting out of tub more difficult than running a marathon)
number_of_TubMar_rejects <- sum(final_merged_data_manu$TubMar_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_TubMar_rejects))
#5

#count for TubEx reject (Getting out of tub more difficult than exercising for 1 hour)
number_of_TubEx_rejects <- sum(final_merged_data_manu$TubEx_Reject, na.rm = TRUE)
print(paste("Number of rejects:", number_of_TubEx_rejects))
#3


#count for Honest Similairity reject (First 7 words identical to another response)
number_of_HonestSim_rejects <- sum(final_merged_data_manu$Honest_Sim_Reject, na.rm = 1)
print(paste("Number of rejects:", number_of_HonestSim_rejects))
#14

# Data for the histogram
categories <- c("Email Code Rejects", "Email Dup Rejects", "Age Birth Rejects", 
                "Ankle Sim Rejects", "State Loc Rejects", "State Zip Rejects", 
                "QCH Rejects", "QCK Rejects", "TubMar Rejects", 
                "TubEx Rejects", "Honest Sim Rejects")
reject_counts <- c(7, 7, 6, 6, 14, 8, 12, 4, 5, 3, 14)

# Create the histogram
barplot(height = reject_counts,
        names.arg = categories,
        col = "skyblue",
        las = 2, # Rotate category names
        main = "Counts of Different Reject Categories",
        xlab = "Reject Categories",
        ylab = "Number of Rejects",
        cex.names = 0.8)


#_ __________________________________________________________________________
#Error graphics

library(ggplot2)
ggplot(final_merged_data_t_test, aes(x = factor(completed_followup), y = rejectcount)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(x = "Follow-up Survey Completion",
       y = "Reject Count",
       title = "Reject Counts by Follow-up Completion Status")



summary_stats <- final_merged_data_t_test %>%
  group_by(completed_followup) %>%
  summarise(mean_rejects = mean(rejectcount, na.rm = TRUE),
            sd_rejects = sd(rejectcount, na.rm = TRUE),
            n = n(),
            se = sd_rejects/sqrt(n))

# Relabel 0/1 as descriptive text
summary_stats$completed_followup <- factor(summary_stats$completed_followup,
                                           levels = c(0,1),
                                           labels = c("Did Not Complete", "Completed"))

ggplot(summary_stats, aes(x = completed_followup, y = mean_rejects)) +
  geom_col(fill = "#E8E8E8", width = 0.5) +  # light gray bars, thinner
  geom_errorbar(aes(ymin = mean_rejects - se, ymax = mean_rejects + se),
                width = 0.15) +
  labs(x = "Follow-up Survey Completion",
       y = "Mean Reject Count ± SE",
       title = "Mean Reject Counts with Standard Error Bars") +
  theme_minimal(base_size = 18) +  # bigger overall font
  theme(
    axis.text = element_text(size = 16),          # axis tick labels
    axis.title = element_text(size = 18, face = "bold"),  # axis titles
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # centered title
  )


