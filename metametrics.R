library(dplyr)
library(psych)

# =========================================================================================
# First, I randomly generate a dummy dataset assuming normally-distributed student ability
# and problem difficulty
set.seed(100)
prob_difficulty <- rnorm(10, mean=1, sd=0.3)
student_ids <- 1:1000
student_ability <- rnorm(1000, mean=0.75, sd=0.1)
success_probabilities <- student_ability %*% t(prob_difficulty)

cap_at_1 <- function(x){
  return(max(0, min(1, x)))
}

capped_success_probabilities <- structure(sapply(success_probabilities, cap_at_1), dim=dim(success_probabilities))

determine_problem_success <- function(x) {
  if(x==1){
    return(1)
  }
  if(x==0){
    return(0)
  }
  return(sample(c(1, 0), size=1, prob=c(x, 1-x)))
}

random_data <- structure(sapply(capped_success_probabilities, FUN=determine_problem_success), dim=dim(success_probabilities))
random_data <- cbind(student_ids, random_data)
colnames(random_data) <- c('Student ID', 'Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10')
# ==========================================================================================
# Next, I plot the distribution of test scores and compute metrics that will be helpful
# for evaluating test items. These include the item difficulty, discrimination, and Cronbach's
# alpha. In practical terms, in order to create a better test, we would consider replacing items
# with excessive difficulty or low discrimination. Meanwhile, Cronbach's alpha serves as a
# measure of the test's internal consistency. For a larger test, further steps could include
# factor analysis to confirm that the dimensionality of the test matches its intention.

responses <- random_data[ , -1]
# 1. Basic descriptive stats for the whole test
test_scores <- rowSums(responses, na.rm = TRUE)
summary(test_scores)
hist(test_scores, breaks = 10, main = "Distribution of Test Scores")

# 2. Item difficulty: proportion correct for each item
item_difficulty <- colMeans(responses, na.rm = TRUE)
item_difficulty

# 3. Item-total correlations (discrimination)
item_total_corr <- apply(responses, 2, function(x) cor(x, test_scores, use = "pairwise"))
item_total_corr

# 4. Reliability (Cronbach's alpha)
alpha_result <- psych::alpha(responses)
alpha_result$total$raw_alpha  # Cronbach’s alpha
alpha_result$item.stats       # includes item difficulty and discrimination

# 5. Quick item analysis summary
item_summary <- data.frame(
  Item = colnames(responses),
  Difficulty = item_difficulty,
  ItemTotalCorr = item_total_corr
)
item_summary


