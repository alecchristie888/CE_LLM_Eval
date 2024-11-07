library(data.table)
library(tidyverse)

response <- fread(choose.files()) #humanresponses.csv
answers <- fread(choose.files()) #correctanswers.csv

head(response)
head(answers)


alldata <- merge(response,answers,by="Question")

#################################################################
######################### summary stats for human experts #######
#################################################################
sort(unique(alldata$Response))

alldata$correct <- sapply(1:nrow(alldata), function(x){
  if(alldata$Response[x]!=""){
    if(alldata$mc.answer[x]==alldata$Response[x]){
      return(1)
    } else{
      return(0)
    }
  } else{
    return(NA)
  }

  })

#overall
length(alldata[correct==1,correct])/length(alldata[!is.na(correct),correct]) 
#by participant
results.part <- data.table(alldata %>%
  group_by(Participant,correct) %>%
  summarise(n=n()))


results.part.all <- list()
i=1
for(i in 1:6){
results.part.all$correct[i] <- results.part[Participant==i&correct==1,n]/sum(results.part[Participant==i&!is.na(correct),n]) 
results.part.all$questions[i] <- sum(results.part[Participant==i&!is.na(correct),n])
}
results.part.all$Participant <- 1:6
results.part.all <- data.table(do.call(cbind,results.part.all))
results.part.all
mean(results.part.all$correct)
median(results.part.all$correct)
quantile(results.part.all$correct)


###############################################
############ llm comparisons ##################
###############################################

llmresponses <- fread(choose.files()) #llmresponses.csv
alldata_short_45 <- alldata[,c("Participant","Question","correct")]
questionnumbers <- fread(choose.files()) #questionnumbers.csv

llmresponses_hy <- merge(llmresponses[Method=="hybrid_retrieval"],questionnumbers,by.x="Question",by.y="question") #add question numbers to data with question title

str(llmresponses)

setdiff(llmresponses$Question,llmresponses_hy$Question)

setdiff(alldata_short$Question,llmresponses_hy$number)

# Set seed for reproducibility
set.seed(123)

num_llms <- 10
num_questions <- 45

data_widellm <- dcast(llmresponses_hy, number ~ Model, value.var = "Correct", fill = 0)
data_widehuman <- dcast(alldata_short_45, Question ~ Participant, value.var = "correct", fill = 0)

# View the wide format data
head(data_widellm)

# Extract LLM responses into a matrix
llm_responses <- as.matrix(data_widellm[, 2:11])  # Specify the LLM columns
human_responses <- as.matrix(data_widehuman[, !c("Question")])  # Exclude Question_ID

# Function to perform the permutation test for a single LLM
permutation_sign_test <- function(llm_data, human_data, num_permutations = 1000000) {
  # Step 1: Set observed difference to zero, which would be null hypothesis that LLM and human expert are equally good
  D_ref <- 0
  
  # Step 2: Permutation test
  D_perm <- numeric(num_permutations)  # Store permutation statistics
  
  for (i in 1:num_permutations) {
    # Randomly sample a human expert for each question
    random_human <- sapply(1:num_questions, function(x) sample(human_data[x, ], 1))
    
    # Calculate plus, equal, and minus counts for the shuffled data
    plus_count_perm <- rep(1,sum((random_human == 1) & (llm_data == 0))) # Human better
    minus_count_perm <- rep(-1,sum((random_human == 0) & (llm_data == 1))) # LLM better
    equal_count_perm <- rep(0,sum((random_human == 1) & (llm_data == 1)) + sum((random_human == 0) & (llm_data == 0))) #equal
      
    # Permutation statistic
    D_perm[i] <- mean(c(plus_count_perm, minus_count_perm, equal_count_perm))
  }
  
  # Step 3: Calculate p-value
  p_value <- 1-mean(abs(D_perm)> D_ref)
  
  mean_perm <- mean(D_perm)  # Mean of the permutation distribution
  sd_perm <- sd(D_perm)      # Standard deviation of the permutation distribution
  ci_lower <- quantile(D_perm, 0.025)  # 2.5% quantile
  ci_upper <- quantile(D_perm, 0.975)  # 97.5% quantile
  
  # Return all relevant statistics
  return(list(p_value = p_value, 
              D_perm = D_perm,
              mean_perm = mean_perm,
              sd_perm = sd_perm,
              ci_lower = ci_lower,
              ci_upper = ci_upper))
  
}

# repeat for each LLM and perform the permutation test
repeattest <- function(llm_number){
  test_result <- list()
  llm_data <- llm_responses[, llm_number]
  
  # Run permutation sign test
  test_result <- permutation_sign_test(llm_data, human_responses)
  
  # Store results
  p_value <- test_result$p_value
  D_perm_mean <- test_result$mean_perm
  D_perm_sd <- test_result$sd_perm
  ci_lower <- test_result$ci_lower
  ci_upper <- test_result$ci_upper 
  
  return(list(cbind(p_value,D_perm_mean,D_perm_sd,ci_lower,ci_upper)))
}

# Output results - run in parallel on windows laptop
library(parallel)
detectCores()
ncores = 12 ### put in the number of cores 
c1 <- makeCluster(ncores) ###create a cluster of cores
clusterExport(c1,c('llm_responses','num_questions','human_responses','permutation_sign_test')) ### import the data

system.time(
  alltests <- parSapplyLB(c1,1:num_llms,repeattest) #### time it with system.time if desired and run in parallel with load balancing (LB)
)

stopCluster(c1) ### make sure to stop cluster at the end
gc() ### clear memory


# Output results
alltestsdat <- data.table(do.call(rbind,alltests))

alltestsdat$p_valueadj <- p.adjust(alltestsdat$p_value, method = "holm")

#write.csv(alltestsdat,"permtestresults_1mil.csv")


###############################################
######################## retrieval ############
###############################################


#############retrieval summary stats for human experts
retrievalresponse_45 <- response[grep("[.]2",response$Question)] #questions numbered .2 are retrieval in survey
retrievalanswer<- answers[,list(Question2,page.answer)] #Question 2 and page.answer columns contain retrieval question and associated correct page

retrievalresponse_45$correct <- NA
i=1
for(i in 1:nrow(retrievalresponse_45)){
  quest <- retrievalresponse_45$Question[i]
  resp <- retrievalresponse_45$Response[i]
  
  if(length(grep(retrievalanswer[Question2==quest,page.answer],resp))>0){
    retrievalresponse_45$correct[i] <- 1
  } else{
    retrievalresponse_45$correct[i] <- 0
  }
}

#overall
length(retrievalresponse_45[correct==1,correct])/length(retrievalresponse_45[!is.na(correct),correct]) 
#by participant
ret.results.part <- data.table(retrievalresponse_45 %>%
                                 group_by(Participant,correct) %>%
                                 summarise(n=n()))


ret.results.part.all <- list()
i=1
for(i in 1:6){
  ret.results.part.all$correct[i] <- ret.results.part[Participant==i&correct==1,n]/sum(ret.results.part[Participant==i&!is.na(correct),n]) 
  ret.results.part.all$questions[i] <- sum(ret.results.part[Participant==i&!is.na(correct),n])
}
ret.results.part.all$Participant <- 1:6
ret.results.part.all <- data.table(do.call(cbind,ret.results.part.all))
ret.results.part.all
mean(ret.results.part.all$correct)
median(ret.results.part.all$correct)
quantile(ret.results.part.all$correct)


###########################################
############ llm comparisons ##############
###########################################

retr_strat_response <- fread(choose.files()) #llm_survey_retrieval_info.csv
retr_strat_response #llms
retrievalresponse_45 #humans

retr_strat_response_num <- merge(retr_strat_response,questionnumbers,by.x="Question",by.y="question") #add in question numbers alongside title to enable comparison

# Set seed for reproducibility
set.seed(123)

num_strat <- 3
num_questions <- 45

data_wideretr <- dcast(retr_strat_response_num, number ~ Method, value.var = "Correct", fill = 0)
data_widehumanretr <- dcast(retrievalresponse_45, Question ~ Participant, value.var = "correct", fill = 0)

# View the wide format data
head(data_wideretr)
head(data_widehumanretr)

# Extract LLM responses into a matrix
retr_responses <- as.matrix(data_wideretr[, 2:4])  # Specify the method columns
human_responses_retr <- as.matrix(data_widehumanretr[, !c("Question")])  # Exclude Question_ID

# Function to perform the permutation test for a single LLM
permutation_sign_test_retr <- function(retr_data, human_data_retr, num_permutations = 1000000) {
  # Step 1: Set observed difference to zero, which would be null hypothesis that LLM and human expert are equally good
  D_ref <- 0
  
  # Step 2: Permutation test
  D_perm <- numeric(num_permutations)  # Store permutation statistics
  
  for (i in 1:num_permutations) {
    # Randomly sample a human expert for each question
    random_human <- sapply(1:num_questions, function(x) sample(human_data_retr[x, ], 1))
    
    # Calculate plus, equal and minus counts for the shuffled data
    plus_count_perm <- rep(1,sum((random_human == 1) & (retr_data == 0))) # Human better
    minus_count_perm <- rep(-1,sum((random_human == 0) & (retr_data == 1))) # LLM better
    equal_count_perm <- rep(0,sum((random_human == 1) & (retr_data == 1)) + sum((random_human == 0) & (retr_data == 0))) #equal
    
    # Permutation statistic
    D_perm[i] <- mean(c(plus_count_perm, minus_count_perm, equal_count_perm))
  }
  
  # Step 3: Calculate p-value
  p_value <- 1-mean(abs(D_perm)> D_ref)
  
  mean_perm <- mean(D_perm)  # Mean of the permutation distribution
  sd_perm <- sd(D_perm)      # Standard deviation of the permutation distribution
  ci_lower <- quantile(D_perm, 0.025)  # 2.5% quantile
  ci_upper <- quantile(D_perm, 0.975)  # 97.5% quantile
  
  # Return all relevant statistics
  return(list(p_value = p_value, 
              D_perm = D_perm,
              mean_perm = mean_perm,
              sd_perm = sd_perm,
              ci_lower = ci_lower,
              ci_upper = ci_upper))
  
}

# Loop over each LLM and perform the permutation test
repeattest_retr <- function(retr_number){
  test_result <- list()
  retr_data <- retr_responses[, retr_number]
  
  # Run permutation sign test
  test_result_retr <- permutation_sign_test_retr(retr_data, human_responses_retr)
  
  # Store results
  p_value <- test_result_retr$p_value
  D_perm_mean <- test_result_retr$mean_perm
  D_perm_sd <- test_result_retr$sd_perm
  ci_lower <- test_result_retr$ci_lower
  ci_upper <- test_result_retr$ci_upper 
  
  return(list(cbind(p_value,D_perm_mean,D_perm_sd,ci_lower,ci_upper)))
}


library(parallel)
detectCores()
ncores = 12 ### put in the number of cores 
c1 <- makeCluster(ncores) ###create a cluster of cores
clusterExport(c1,c('retr_responses','num_questions','human_responses_retr','permutation_sign_test_retr')) ### import the data

system.time(
  alltests_retr <- parSapplyLB(c1,1:num_strat,repeattest_retr) #### time it with system.time if desired and run in parallel with load balancing (LB)
)

stopCluster(c1) ### make sure to stop cluster at the end
gc() ### clear memory


# Output results
alltestsdat_retr <- data.table(do.call(rbind,alltests_retr))

alltestsdat_retr$p_valueadj <- p.adjust(alltestsdat_retr$p_value, method = "holm")

#write.csv(alltestsdat_retr,"permtestresults_retr_1mil.csv")




################################################################
################################ time taken by human experts ###
################################################################

time <- response[grep("Submit",response$Question)]
time$Response <- as.numeric(time$Response)
mean(time$Response,na.rm=TRUE)
median(time$Response,na.rm=TRUE)
quantile(time$Response,na.rm=TRUE)

hist(time$Response)
