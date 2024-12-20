---
  title: "Environmental Identities"
author: "Richard von Furstenberg"
output:    
  html_document:
  keep_tex: true
---

  

```{r include=FALSE}
## Load Packages

library(tidyverse)
library(nnet)
library(summarytools)
library(viridis)
library(crosstable)
library(pander)
library(multcompView)
library(officer)
library(flextable)
library(broom)

## Import data and key

dat<- read_csv("data/data.csv")
key<- read_csv("data/key.csv")

## Function(s)

# Create a eta squared function for ANOVAs
get_eta_squared <- function(anova_model_summary) {
  ss_model <- anova_model_summary[[1]]['Sum Sq'][1]  # First element typically refers to the model effect
  ss_total <- sum(anova_model_summary[[1]]['Sum Sq'])
  eta_squared <- ss_model / ss_total
  return(eta_squared)
}

```


```{r include=FALSE}
#### Relabeling factor levels

working_dat <- dat #making a data set to modify

#relabeling Race_6Cat factor levels 

working_dat$Race_6Cat<-as.factor(working_dat$Race_6Cat)

working_dat$Race_6Cat <- fct_recode(working_dat$Race_6Cat, White = "1", Hispanic = "2", Black ="3", Asian = "4", Am.Indian = "5", All_other = "6")

#relabeling Race_2Cat factor levels

working_dat$Race_2Cat<-as.factor(working_dat$Race_2Cat)

working_dat$Race_2Cat <- fct_recode(working_dat$Race_2Cat, Non_White = "0", White = "1")
#relabeling Gender_2Cat factor levels 
working_dat$Gender_2Cat<-as.factor(working_dat$Gender_2Cat)

working_dat$Gender_2Cat <- fct_recode(working_dat$Gender_2Cat, Women = "0", Men = "1")

#relabeling and making College Major_2Cat a factor
working_dat$CollegeMajor_2Cat<-as.factor(working_dat$CollegeMajor_2Cat)

working_dat$CollegeMajor_2Cat <- fct_recode(working_dat$CollegeMajor_2Cat, all_other_majors = "0", ag_or_nat_res = "1")

working_dat %>% 
  mutate(College_Major_2Cat = fct_relevel(CollegeMajor_2Cat, "ag_or_nat_res", "all_other_majors"))->working_dat

#relabeling and making ChildhoodLocation_2Cat a factor

working_dat$ChildhoodLocation_2Cat<-as.factor(working_dat$ChildhoodLocation_2Cat)

working_dat$ChildhoodLocation_2Cat <- fct_recode(working_dat$ChildhoodLocation_2Cat, small_town_rural = "0", city_urban = "1")

#relabeling HuntingParticipation_2cat to factor with yes and no instead of 1 and 0

working_dat$HuntingParticipation_2Cat <-as.factor(working_dat$HuntingParticipation_2Cat)

working_dat$HuntingParticipation_2Cat <- fct_recode(working_dat$HuntingParticipation_2Cat, yes = "1", no = "0")

#relabeling FISHING_Participation to factor with yes and no instead of 1 and 0

working_dat$FISHING_Participation <-as.factor(working_dat$FISHING_Participation)

working_dat$FISHING_Participation <- fct_recode(working_dat$FISHING_Participation, yes = "1", no = "0")

```

### Step 1: Creating identity groups 


**Derived from MultiState Survey Q 1.4:** "To what extent do you identify with each of the following groups?" Environmentalist, Conservationist

Response choices: Not at all (1), Slightly (2), Moderately (3), Strongly (4), Very Strongly (5)

**Four identities are formed using +/- valence with Environmentalist and Conservationist identities:**
  
  *Conservationist* = Conservationist (greater than or equal to 4) & Environmentalist (less than or equal to 3)

*Environmentalist* = Environmentalist (greater than or equal to 4) & Conservationist (less than or equal to 3)

*Pluralist* = Conservationist (greater than or equal to 3) & Environmentalist (greater than or equal to 3)

*Eco-Agnostic* = Conservationist (less than or equal to 2) & Environmentalist (less than or equal to 2)


```{r Identity Creation, include=FALSE}

######### Identity Variable

working_dat$identity <- ifelse(working_dat$Identity_Conservation >= 4 & working_dat$Identity_Environment <=3, "conservationist", 
                               ifelse(working_dat$Identity_Environment>= 4 & working_dat$Identity_Conservation <=3, "environmentalist", 
                                      ifelse(working_dat$Identity_Environment<= 2 & working_dat$Identity_Conservation <=2, "eco_agnostic", "pluralist")))

### Identity to factor

working_dat$identity <- as.factor(working_dat$identity)

# Reorder levels of the 'identity' variable 
working_dat$identity <- fct_relevel(working_dat$identity, "eco_agnostic", "pluralist", "environmentalist", "conservationist")
```



### Step 2: Demographic Attributes of Identity Groups



```{r echo=FALSE}

### Race

working_dat %>% 
  select(identity, Race_2Cat)-> id_race

na.omit(id_race)->id_race_nona

ct1 = crosstable(id_race_nona, by = Race_2Cat, total="both", 
                 percent_digits=0) %>%
  as_flextable()
ct1


### Gender

working_dat %>% 
  select(identity, Gender_2Cat)-> id_gender

na.omit(id_gender)->id_gender_nona

ct2 = crosstable(id_gender_nona, by = Gender_2Cat, total="both", 
                 percent_digits=0) %>%
  as_flextable()
ct2

### Childhood Location

working_dat %>% 
  select(identity, ChildhoodLocation_2Cat)-> id_loc

na.omit(id_loc)->id_loc_nona

ct3 = crosstable(id_loc_nona, by = ChildhoodLocation_2Cat, total="both", 
                 percent_digits=0) %>%
  as_flextable()
ct3

### College Major

working_dat %>% 
  select(identity, CollegeMajor_2Cat)-> id_coll

na.omit(id_coll)->id_coll_nona

ct4 = crosstable(id_coll_nona, by = CollegeMajor_2Cat, total="both", 
                 percent_digits=0) %>%
  as_flextable()
ct4




```


### Step 2: Contigency table of identity vs demographics (gender, race, childhod loc, college major)


```{r}

### Testing another crosstab package meant to emulate SPSS

library(expss)

# Create a vector of the categorical variables
categorical_vars <- working_dat %>% 
  select("identity", "Race_2Cat", "Gender_2Cat", "ChildhoodLocation_2Cat", "CollegeMajor_2Cat")

# Table column % with multiple banners
cross_cpct(categorical_vars, identity, list(total(), Race_2Cat, Gender_2Cat, ChildhoodLocation_2Cat, CollegeMajor_2Cat))


```



```{r contingency}

# Load the tidyverse library (assuming it's already loaded)
# If not loaded, make sure to load it using library(tidyverse) before running this code.

# Create a vector of the categorical variables
categorical_vars <- c("Race_2Cat", "Gender_2Cat", "ChildhoodLocation_2Cat", "CollegeMajor_2Cat", "HuntingParticipation_2Cat", "FISHING_Participation", "Rec_Birding", "Rec_Hiking", "Rec_WildlifePhotos")

# Create an empty data frame to store the results
chi_square_results <- data.frame()

# Iterate through the categorical variables and perform chi-square tests
for (var in categorical_vars) {
  # Create a contingency table
  contingency_table <- table(working_dat[[var]], working_dat$identity)
  
  # Perform chi-square test
  chi_square_test <- chisq.test(contingency_table)
  
  # Calculate Cramer's V
  n <- sum(contingency_table)
  phi <- sqrt(chi_square_test$statistic / (n * min(dim(contingency_table)) - 1))
  cramer_v <- phi / sqrt(min(dim(contingency_table)) - 1)
  
  # Store the results in the results data frame
  chi_square_results <- rbind(chi_square_results, cbind(Variable = var, Chi_Square_Statistic = chi_square_test$statistic, P_Value = chi_square_test$p.value, Cramer_V = cramer_v))
}

# Print the results
print(chi_square_results)


# Write the chi_square_results to a .csv file
write.csv(chi_square_results, file = "chi_square_results.csv", row.names = FALSE)

```


```{r eval=FALSE, include=FALSE}
# Load the tidyverse library (assuming it's already loaded)
# library(tidyverse)

# Assuming your data frame is named "working_dat"

# Create an empty data frame to store all the results
all_results <- data.frame()

# Get unique levels for "identity" and "Race_2Cat"
identity_levels <- unique(working_dat$identity)
race_levels <- unique(working_dat$Race_2Cat)

# Compare all combinations of levels between "identity" and "Race_2Cat"
for (level1 in identity_levels) {
  for (level2 in race_levels) {
    # Subset the data for the current combination of levels
    subset_dat <- subset(working_dat, identity == level1 & Race_2Cat == level2)
    
    # Create a contingency table
    contingency_table <- table(subset_dat$identity, subset_dat$Race_2Cat)
    
    # Perform Fisher's exact test
    fisher_result <- fisher.test(contingency_table)
    
    # Extract p-value (Cramer's V is not applicable for Fisher's exact test)
    p_value <- fisher_result$p.value
    
    # Create a table with the results for the current combination
    result_table <- data.frame(
      Identity_Level = level1,
      Race_2Cat_Level = level2,
      P_Value = p_value
    )
    
    # Append the result to the all_results data frame
    all_results <- rbind(all_results, result_table)
  }
}

# Print the combined result table
print(all_results)




```

### Step 2A. I: ANOVA comparing identity groups based on demographic variables

```{r}

working_dat %>% 
  select(identity, ChildhoodLocation_2Cat, CollegeMajor_2Cat, Race_2Cat, Gender_2Cat)->demos



```



### Step 2A.II: To what extent do demographics predict identity group?

**This code is being silenced. Not using in current draft 9-26-23**
  
  
  ```{r Demo MLR comparing to pluralist, eval=FALSE, include=FALSE}

### I will utilize a multinomal logistic regression to examine the predictive power of the demographic variables in determining identity group membership. The predictor vars are (race, gender, childhood loc, and college major)

# Relevel "identity" to compare identities to pluralist, the most typical group

working_dat$identity <- fct_relevel(working_dat$identity, "pluralist", "environmentalist","conservationist", "eco_agnostic")

# Relevel Childhood location to use reference "city_urban"
working_dat$ChildhoodLocation_2Cat <- relevel(working_dat$ChildhoodLocation_2Cat, ref ="city_urban")

# Fit the multinomial regression model

demo_model <- multinom(identity ~ Gender_2Cat+ Race_2Cat + ChildhoodLocation_2Cat + CollegeMajor_2Cat, data = working_dat)

summary(demo_model)

z_demo <- summary(demo_model)$coefficients/summary(demo_model)$standard.errors

#2-tailed z test

p <- (1 - pnorm(abs(z_demo), 0,1)) * 2
p

## extract the coefficients from the model and exponentiation
exp(coef(demo_model))



# Load the DescTools package for calculate the R square
library(DescTools)
#Calculate the R Square
PseudoR2(demo_model, which = c("CoxSnell","Nagelkerke","McFadden"))

```




```{r eval=FALSE, include=FALSE}

########################################

### After talking to Lincoln on 6/24/23 he suggested I compare all identity groups to the eco-agnostic for the MLR. I will do that below and have silenced the code chunk above which compared the groups against pluralist. 

### Note: I am also reworking code to export in a more friendly table to Word. The method in the above chunk has to be manually entered. 




# Fit the multinomial regression model
demo_model <- multinom(identity ~ Race_2Cat + Gender_2Cat + ChildhoodLocation_2Cat + CollegeMajor_2Cat, data = working_dat)

# Get the tidy summary of the model
tidy_summary <- summary(demo_model)

# Calculate the z-values and p-values
tidy_summary$z_value <- tidy_summary$estimate / tidy_summary$std.error
tidy_summary$p_value <- 2 * (1 - pnorm(abs(tidy_summary$z_value)))

# Calculate odds ratios and add them to the tidy_summary
tidy_summary$odds_ratio <- exp(as.numeric(tidy_summary$estimate))

# Remove the 'statistic' column
tidy_summary <- tidy_summary[, !names(tidy_summary) %in% "statistic"]


# Using 'mutate_if' and 'sprintf' to format all numerical columns to three decimal places
tidy_summary <- tidy_summary%>%
  mutate_if(is.numeric, function(x) sprintf("%.3f", x))

# Print the tidy summary
print(tidy_summary)

# Convert the table to a flextable object
flex_table <- flextable(tidy_summary)

# Create a Word document
doc <- read_docx()

# Add a title to the document
doc <- body_add_par(doc, "Multinomial Logistic Regression Results", style = "heading 1")

# Add the table to the document
doc <- body_add_flextable(doc, flex_table)

# Save the Word document
output_file <- "multinom_output.docx"
print(doc, target = output_file)


# Calculate the proportions of each level for each variable
demogs <- working_dat %>% 
  select(Race_2Cat, Gender_2Cat, ChildhoodLocation_2Cat, CollegeMajor_2Cat)

proportions_demo <- demogs %>%
  summarise_all(function(x) prop.table(table(x)))

print(proportions_demo)

```

#### Step 2B Behavioral Predictors (OLD and REDUNDANT NOT USING - RMEOVE)

Here I use variables for consumptive and non-consumptive outdoor recreation HuntingParticipation_2Cat, FISHING_Participation, Rec_Birding, Rec_Camping, Rec_Hiking, Rec_WildlifePhotos to predict identity membership

```{r eval=FALSE, include=FALSE}

# Convert numerical variables to factors with default levels (0 and 1)
working_dat$Rec_Birding <- factor(working_dat$Rec_Birding)
working_dat$Rec_Camping <- factor(working_dat$Rec_Camping)
working_dat$Rec_Hiking <- factor(working_dat$Rec_Hiking)
working_dat$Rec_WildlifePhotos <- factor(working_dat$Rec_WildlifePhotos)

# Change factor levels from 0 to "no" and 1 to "yes"
levels(working_dat$Rec_Birding) <- c("no", "yes")
levels(working_dat$Rec_Camping) <- c("no", "yes")
levels(working_dat$Rec_Hiking) <- c("no", "yes")
levels(working_dat$Rec_WildlifePhotos) <- c("no", "yes")

# Optional: You can check the updated levels of each variable
print(levels(working_dat$Rec_Birding))
print(levels(working_dat$Rec_Camping))
print(levels(working_dat$Rec_Hiking))
print(levels(working_dat$Rec_WildlifePhotos))



# Fit the multinomial regression model
behav_model <- multinom(identity ~ HuntingParticipation_2Cat + FISHING_Participation + Rec_Birding + Rec_Camping + Rec_Hiking + Rec_WildlifePhotos, data = working_dat)

# Get the tidy summary of the model
tidy_summary <- tidy(behav_model)

# Calculate the z-values and p-values
tidy_summary$z_value <- tidy_summary$estimate / tidy_summary$std.error
tidy_summary$p_value <- 2 * (1 - pnorm(abs(tidy_summary$z_value)))

# Calculate odds ratios and add them to the tidy_summary
tidy_summary$odds_ratio <- exp(as.numeric(tidy_summary$estimate))

# Remove the 'statistic' column
tidy_summary <- tidy_summary[, !names(tidy_summary) %in% "statistic"]


# Using 'mutate_if' and 'sprintf' to format all numerical columns to three decimal places
tidy_summary <- tidy_summary%>%
  mutate_if(is.numeric, function(x) sprintf("%.3f", x))

# Print the tidy summary
print(tidy_summary)

# Convert the table to a flextable object
flex_table <- flextable(tidy_summary)

# Create a Word document
doc <- read_docx()

# Add a title to the document
doc <- body_add_par(doc, "Behavioral Multinomial Logistic Regression Results", style = "heading 1")

# Add the table to the document
doc <- body_add_flextable(doc, flex_table)

# Save the Word document
output_file <- "behav_multinom_output.docx"
print(doc, target = output_file)


# making a subset of variables to generate level proportions
Rec_behav <- working_dat %>%
  select(HuntingParticipation_2Cat, FISHING_Participation, Rec_Birding, Rec_Camping, Rec_Hiking, Rec_WildlifePhotos)

# Calculate the proportions of each level for each variable
proportions <- Rec_behav %>%
  summarise_all(function(x) prop.table(table(x)))

print(proportions)




#z_demo <- summary(demo_model)$coefficients/summary(demo_model)$standard.errors

#2-tailed z test

#p <- (1 - pnorm(abs(z_demo), 0,1)) * 2
#p

## extract the coefficients from the model and exponentiation
#exp(coef(demo_model))


# Load the DescTools package for calculate the R square
library(DescTools)
#Calculate the R Square
#PseudoR2(demo_model, which = c("CoxSnell","Nagelkerke","McFadden"))

```


#### Step 2C How do Outdoor Behaviors predict group membership, a multinomial logistic regression model.

**Using this based on last Lab Mtg** 9-26-23

```{r echo=FALSE}

# Reorder levels of the 'identity' variable 
working_dat$identity <- fct_relevel(working_dat$identity, "eco_agnostic", "pluralist", "environmentalist", "conservationist")

# Fit the multinomial regression model
behaviors_model <- multinom(identity ~ HuntingParticipation_2Cat + FISHING_Participation + Rec_Birding + Rec_Camping + Rec_Hiking + Rec_WildlifePhotos, data = working_dat)

# Get the tidy summary of the model
tidy_summary <- tidy(behaviors_model)

# Calculate the z-values and p-values
tidy_summary$z_value <- tidy_summary$estimate / tidy_summary$std.error
tidy_summary$p_value <- 2 * (1 - pnorm(abs(tidy_summary$z_value)))

# Calculate odds ratios and add them to the tidy_summary
tidy_summary$odds_ratio <- exp(as.numeric(tidy_summary$estimate))

# Remove the 'statistic' column
tidy_summary <- tidy_summary[, !names(tidy_summary) %in% "statistic"]


# Using 'mutate_if' and 'sprintf' to format all numerical columns to three decimal places
tidy_summary <- tidy_summary%>%
  mutate_if(is.numeric, function(x) sprintf("%.3f", x))

# Print the tidy summary
print(tidy_summary)

# Convert the table to a flextable object
flex_table <- flextable(tidy_summary)

# Create a Word document
doc <- read_docx()

# Add a title to the document
doc <- body_add_par(doc, "Outdoor Behaviors-  Multinomial Logistic Regression Results", style = "heading 1")

# Add the table to the document
doc <- body_add_flextable(doc, flex_table)

# Save the Word document
output_file <- "behav_multinom_output.docx"
#print(doc, target = output_file)


# making a subset of variables to generate level proportions
Rec_behav <- working_dat %>%
  select(HuntingParticipation_2Cat, FISHING_Participation, Rec_Birding, Rec_Camping, Rec_Hiking, Rec_WildlifePhotos)

# Calculate the proportions of each level for each variable
proportions <- Rec_behav %>%
  summarise_all(function(x) prop.table(table(x)))

print(proportions)




```

### Step 2D: Types of outdoor recreation by identities

```{r}


# Converting variables to factors with "Yes" for 1 and "No" for 0
working_dat <- working_dat %>%
  mutate(across(c(Rec_Birding, Rec_Camping, Rec_Hiking, Rec_WildlifePhotos),
                ~factor(., levels = c(0, 1), labels = c("no", "yes"))))

working_dat %>% 
  select(identity, HuntingParticipation_2Cat, FISHING_Participation, Rec_Birding, Rec_Camping, Rec_Hiking, Rec_WildlifePhotos) %>%
  pivot_longer(cols = -identity, names_to = "activity", values_to = "participation") %>%
  group_by(identity, activity) %>%
  summarise(Participants = sum(participation == "yes", na.rm = TRUE), # Adjust based on your data
            Total = n(),
            Percent = Participants / Total * 100) %>%
  mutate(Percent = round(Percent)) %>%
  select(-Participants, -Total) # Keeping only the percent column

# Number of valid observations in above analysis

total_non_na_identity <- working_dat %>%
  filter(!is.na(identity)) %>%
  summarise(TotalNonNAIdentity = n())

print(total_non_na_identity)


```



### Step 3: Group Attitudes and Beliefs about Wildlife and Conservation


```{r}



#Subsetting vars of interest

working_dat %>% 
  select(WVO_Mutualism, WVO_Domination, ConservationCaringScale, identity) %>% 
  na.omit()->wildlife_beliefs

# Set the factor levels for the identity variable
wildlife_beliefs$identity <- factor(wildlife_beliefs$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

#### WVO Domination 

# Mean and SD 
mean_scores <- wildlife_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (WVO_Domination),
            StdDev = sd(WVO_Domination, na.rm = TRUE))  # Calculate standard deviation

print(mean_scores)


#Analysis of variance

anova2 <- aov(WVO_Domination ~ identity, data = wildlife_beliefs)
#summary(anova2)

#Tukey's test

tukey2 <- TukeyHSD(anova2)
#print(tukey2)


#Compact letter display
cld2 <- multcompLetters4(anova2, tukey2)
#print(cld2)

#table with factors and 3rd quantile
Tk2 <- group_by(wildlife_beliefs, identity) %>% 
  summarise(mean = mean(WVO_Domination), quant2 = quantile (WVO_Domination, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld2 <- as.data.frame.list(cld2$identity)
Tk2$cld2 <- cld2$Letters
#print(Tk2)

# Enhanced Boxplot with Jittered Points for Domination
domination_plot <- ggplot(wildlife_beliefs, aes(x = identity, y = WVO_Domination, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Enhanced Boxplot of Identity Groups' Alignment with Dominionistic WVO",
       x = "Identity",
       y = "Likert Rating",
       caption = "Figure 1. Enhanced boxplot of identity groups' alignment with dominionistic wildlife value orientation.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust = 0)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file = "domination_plot.svg", plot = domination_plot, width = 10, height = 10)

print(domination_plot)





# For WVO Domination
anova2_summary <- summary(anova2)
eta_squared_domination <- get_eta_squared(anova2_summary)


anova2_summary

eta_squared_domination


ggsave(file="domination_plot.svg", plot=domination_plot, width=10, height=10)



#### WVO Mutualism 

#Analysis of variance

anova3 <- aov(WVO_Mutualism ~ identity, data = wildlife_beliefs)
#summary(anova2)

#Tukey's test

tukey3 <- TukeyHSD(anova3)
#print(tukey2)

# ETA Squared For WVO Mutualism
anova3_summary <- summary(anova3)
eta_squared_mutualism <- get_eta_squared(anova3_summary)

anova3_summary

eta_squared_mutualism

#Compact letter display
cld3 <- multcompLetters4(anova3, tukey3)
#print(cld2)

#table with factors and 3rd quantile
Tk3 <- group_by(wildlife_beliefs, identity) %>% 
  summarise(mean = mean(WVO_Mutualism), quant3 = quantile (WVO_Mutualism, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld3 <- as.data.frame.list(cld3$identity)
Tk3$cld3 <- cld3$Letters
#print(Tk3)


################ Percent of scores 4 or better in WVO and Cons Caring across identities

# Function to calculate the percentage of scores >= 4
calc_percentage <- function(x) {
  sum(x >= 4) / length(x) * 100
}

# Apply both percentage calculation and mean computation grouped by 'identity'
result_1 <- wildlife_beliefs %>%
  group_by(identity) %>%
  summarise(
    WVO_Mutualism_Perc = calc_percentage(WVO_Mutualism),
    WVO_Mutualism_Mean = mean(WVO_Mutualism, na.rm = TRUE),
    WVO_Domination_Perc = calc_percentage(WVO_Domination),
    WVO_Domination_Mean = mean(WVO_Domination, na.rm = TRUE),
    ConservationCaringScale_Perc = calc_percentage(ConservationCaringScale),
    ConservationCaringScale_Mean = mean(ConservationCaringScale, na.rm = TRUE)
  )

# Print the resulting table
print(result_1)


```



```{r}


mean_scores <- wildlife_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (WVO_Mutualism),
            StdDev = sd(WVO_Mutualism, na.rm = TRUE))  # Calculate standard deviation

print(mean_scores)

# Example for Conservation Caring Scale
anova_cons_caring <- aov(ConservationCaringScale ~ identity, data = wildlife_beliefs)
summary(anova_cons_caring)
eta_squared_cons_caring <- summary(anova_cons_caring)[[1]][["Sum Sq"]][1] / sum(summary(anova_cons_caring)[[1]][["Sum Sq"]])

print(eta_squared_cons_caring)

library(ggplot2)
library(dplyr)

# Enhanced Boxplot with Jittered Points for Mutualism
mutualism_plot <- ggplot(wildlife_beliefs, aes(x = identity, y = WVO_Mutualism, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Enhanced Boxplot of Identity Groups' Alignment with Mutualistic WVO",
       x = "Identity",
       y = "Likert Rating",
       caption = "Figure 2. Enhanced boxplot of identity groups' alignment with mutualistic wildlife value orientation.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust = 0)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file = "mutualism_plot.svg", plot = mutualism_plot, width = 10, height = 10)

print(mutualism_plot)



#### Conservation Caring

#Analysis of variance

# Mean and SD 
mean_scores <- wildlife_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (ConservationCaringScale),
            StdDev = sd(ConservationCaringScale, na.rm = TRUE))  # Calculate standard deviation

print(mean_scores)

anova4 <- aov(ConservationCaringScale ~ identity, data = wildlife_beliefs)
#summary(anova4)

#Tukey's test

tukey4 <- TukeyHSD(anova4)
#print(tukey2)


# ETA Squared For Conservation Caring
anova4_summary <- summary(anova4)
eta_squared_cons_caring <- get_eta_squared(anova4_summary)

anova4_summary

eta_squared_cons_caring

#Compact letter display
cld4 <- multcompLetters4(anova4, tukey4)
#print(cld2)

#table with factors and 4rd quantile
Tk4 <- group_by(wildlife_beliefs, identity) %>% 
  summarise(mean = mean(ConservationCaringScale), quant4 = quantile (ConservationCaringScale, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld4 <- as.data.frame.list(cld4$identity)
Tk4$cld4 <- cld4$Letters
#print(Tk4)


#boxplot
# Enhanced Boxplot with Jittered Points for Conservation Caring
cons_caring_plot <- ggplot(wildlife_beliefs, aes(x = identity, y = ConservationCaringScale, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Enhanced Boxplot of Identity Groups' Conservation Caring",
       x = "Identity",
       y = "Likert Rating",
       caption = "Figure 3. Enhanced boxplot of identity groups' conservation caring.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust = 0)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file = "cons_caring_plot.svg", plot = cons_caring_plot, width = 10, height = 10)

print(cons_caring_plot)

```



### Step 4: Attitudes and Beliefs about Hunting


```{r echo=FALSE}
### Subsetting data
working_dat %>% 
  select(HuntingApproval, HuntApproval_Altruism, HuntApproval_Egoism, HuntBelief_HuntersCare, ConsFund_HuntFees, identity) %>% 
  na.omit()->hunt_beliefs

# Set the factor levels for the identity variable
hunt_beliefs$identity <- factor(hunt_beliefs$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))


############################################################### Overall Approval of Legal Regulated Hunting
### Means

# Calculate means
overall_approv <- hunt_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (HuntingApproval),
            StdDev = sd(HuntingApproval, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(overall_approv)

#Percent approve or strongly approve
approv_perc_summary <- hunt_beliefs%>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(HuntingApproval %in% c(4, 5)),
    percentage = (strongly_very_strongly / total) * 100
  )

print(approv_perc_summary)

#Analysis of variance

anova1_1 <- aov(HuntingApproval ~ identity, data = hunt_beliefs)
#summary(anova2)

#Tukey's test
p
tukey1_1 <- TukeyHSD(anova1_1)
#print(tukey2)

#ETA Squared
sum_anova1_1 <- summary(anova1_1)

eta_squared_overall_approval <- get_eta_squared(sum_anova1_1 )

#Compact letter display
cld1_1 <- multcompLetters4(anova1_1, tukey1_1)
#print(cld2)

#table with factors and 3rd quantile
Tk1_1 <- group_by(hunt_beliefs, identity) %>% 
  summarise(mean = mean(HuntingApproval), quant1_1 = quantile (HuntingApproval, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld5 <- as.data.frame.list(cld1_1$identity)
Tk1_1$cld5 <- cld5$Letters
#print(Tk1_1)


#boxplot
# Enhanced Boxplot with Jittered Points for Hunting Approval
hunting_approval_plot <- ggplot(hunt_beliefs, aes(x = identity, y = HuntingApproval, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title= "Identity Groups' Approval of Legal Regulated Hunting",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 4. Enhanced boxplot of identity groups' approval of legal regulated hunting.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk1_1, aes(label = cld5, x = identity, y = quant1_1), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="hunting_approval_plot.svg", plot=hunting_approval_plot, width=10, height=10)

print(hunting_approval_plot)




############################################# Hunt Approval EGOISTIC REASONS

### Means

# Calculate means
ego_means <- hunt_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (HuntApproval_Egoism),
            StdDev = sd(HuntApproval_Egoism, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(ego_means)

# View the results
print(overall_approv)

#Percent approve > 2.5 on 3 point scale for construct
ego_perc_summary <- hunt_beliefs%>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(HuntApproval_Egoism >= 2.5),
    percentage = (strongly_very_strongly / total) * 100
  )

print(ego_perc_summary)

#Analysis of variance

anova5 <- aov(HuntApproval_Egoism ~ identity, data = hunt_beliefs)
#summary(anova2)

#Tukey's test

tukey5 <- TukeyHSD(anova5)
#print(tukey2)

#ETA Squared
sum_anova5 <- summary(anova5)

eta_squared_ego_approval <- get_eta_squared(sum_anova5 )

#Compact letter display
cld5 <- multcompLetters4(anova5, tukey5)
#print(cld2)

#table with factors and 3rd quantile
Tk5 <- group_by(hunt_beliefs, identity) %>% 
  summarise(mean = mean(HuntApproval_Egoism), quant5 = quantile (HuntApproval_Egoism, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld5 <- as.data.frame.list(cld5$identity)
Tk5$cld5 <- cld5$Letters
#print(Tk5)


# Enhanced Boxplot with Jittered Points for Egoistic Hunting Approval
ego_approve_plot <- ggplot(hunt_beliefs, aes(x = identity, y = HuntApproval_Egoism, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title= "Identity Groups' Approval of Hunting for Egoistic Reasons",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 5. Enhanced boxplot of identity approval of hunting for egoistic reasons.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk5, aes(label = cld5, x = identity, y = quant5), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="ego_approve_plot.svg", plot=ego_approve_plot, width=10, height=10)

print(ego_approve_plot)


ggsave(file="ego_approve_plot.svg", plot=ego_approve_plot, width=10, height=10)

print(ego_approve_plot)

############################################# Hunt Approval ALTRUISTIC REASONS

### Means

# Calculate means
altru_means <- hunt_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (HuntApproval_Altruism),
            StdDev = sd(HuntApproval_Altruism, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(altru_means)

#Percent approve > 2.5 on 3 point scale for construct
alt_perc_summary <- hunt_beliefs%>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(HuntApproval_Altruism >= 2.5),
    percentage = (strongly_very_strongly / total) * 100
  )

print(alt_perc_summary)


#Analysis of variance

anova6 <- aov(HuntApproval_Altruism ~ identity, data = hunt_beliefs)
#summary(anova2)

#Tukey's test

tukey6 <- TukeyHSD(anova6)
#print(tukey2)

#ETA Squared
sum_anova6 <- summary(anova6)

eta_squared_altru_approval <- get_eta_squared(sum_anova6 )

#Compact letter display
cld6 <- multcompLetters4(anova6, tukey6)
#print(cld2)

#table with factors and 3rd quantile
Tk6 <- group_by(hunt_beliefs, identity) %>% 
  summarise(mean = mean(HuntApproval_Altruism), quant6 = quantile (HuntApproval_Altruism, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld6 <- as.data.frame.list(cld6$identity)
Tk6$cld6 <- cld6$Letters
#print(Tk6)


#boxplot
# Enhanced Boxplot with Jittered Points for Altruistic Hunting Approval
altruistic_approve_plot <- ggplot(hunt_beliefs, aes(x = identity, y = HuntApproval_Altruism, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title= "Identity Groups' Approval of Hunting for Altruistic Reasons",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 6. Enhanced boxplot of identity approval of hunting for altruistic reasons.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk6, aes(label = cld6, x = identity, y = quant6), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="altruistic_approve_plot.svg", plot=altruistic_approve_plot, width=10, height=10)

print(altruistic_approve_plot)


ggsave(file="altruistic_approve_plot.svg", plot=altruistic_approve_plot, width=10, height=10)

print(altruistic_approve_plot)

###################### Desire for Hunting to be contrib to conservation funding
### Means

# Calculate means
hunter_pay_means <- hunt_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (ConsFund_HuntFees),
            StdDev = sd(ConsFund_HuntFees, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(altru_means)

#Analysis of variance

anova7 <- aov(ConsFund_HuntFees ~ identity, data = hunt_beliefs)
#summary(anova2)

#Tukey's test

tukey7 <- TukeyHSD(anova7)
#print(tukey2)

#ETA Squared
sum_anova7 <- summary(anova7)

eta_squared_altru_approval <- get_eta_squared(sum_anova7)


#Compact letter display
cld7 <- multcompLetters4(anova7, tukey7)
#print(cld2)

#table with factors and 3rd quantile
Tk7 <- group_by(hunt_beliefs, identity) %>% 
  summarise(mean = mean(ConsFund_HuntFees), quant7 = quantile (ConsFund_HuntFees, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld7 <- as.data.frame.list(cld7$identity)
Tk7$cld7 <- cld7$Letters
#print(Tk7)


#boxplot
ggplot(hunt_beliefs, aes(identity, ConsFund_HuntFees)) +
  geom_boxplot()+
  labs(title= "Identity Groups' Support for Hunting as Source of Conservation Funding",x = "Identity", y = "Likert Rating", caption = "Figure 7. Boxplot of identity approval of hunting as a funding source for conservation. Letter above group denotes significant \n  difference if letter is not shared.") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", plot.caption = element_text(hjust = 0))+
  geom_text(data = Tk7, aes(label = cld7, x = identity, y = quant7),
            vjust = -0.8, hjust = -1, size = 3)+
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(limits = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

############################################### Hunters Care about conserving wildlife and natural resources

# Calculate means
hunter_care_means <- hunt_beliefs %>%
  group_by(identity) %>%
  summarise(Mean = mean (HuntBelief_HuntersCare),
            StdDev = sd(HuntBelief_HuntersCare, na.rm = TRUE))  # Calculate standard deviation

Set the factor levels for the identity variable
hunt_beliefs$identity <- factor(hunt_beliefs$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

# View the results
print(hunter_care_means)


#Percent approve or strongly approve
belief_perc_summary <- hunt_beliefs%>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(HuntBelief_HuntersCare %in% c(4, 5)),
    percentage = (strongly_very_strongly / total) * 100
  )

print(belief_perc_summary)

#Analysis of variance

anova8 <- aov(HuntBelief_HuntersCare ~ identity, data = hunt_beliefs)
#summary(anova2)

#Tukey's test

tukey8 <- TukeyHSD(anova8)
#print(tukey2)

#ETA Squared
sum_anova8 <- summary(anova8)

eta_squared_hunters_care<- get_eta_squared(sum_anova8)


#Compact letter display
cld8 <- multcompLetters4(anova8, tukey8)
#print(cld2)

#table with factors and 3rd quantile
Tk8 <- group_by(hunt_beliefs, identity) %>% 
  summarise(mean = mean(HuntBelief_HuntersCare), quant8 = quantile (HuntBelief_HuntersCare, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld8 <- as.data.frame.list(cld8$identity)
Tk8$cld8 <- cld8$Letters
#print(Tk8)



# Enhanced Boxplot with Jittered Points for Belief that Hunters Care about Conserving Natural Resources
hunters_care_plot <- ggplot(hunt_beliefs, aes(x = identity, y = HuntBelief_HuntersCare, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Belief that Hunters Care about Conserving Natural Resources",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 8. Enhanced boxplot of identity groups' belief that hunters care about conserving wildlife and natural resources. Letter above group denotes significant difference if letter is not shared.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk8, aes(label = cld8, x = identity, y = quant8), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="hunters_care_plot.svg", plot=hunters_care_plot, width=10, height=10)

print(hunters_care_plot)


```


### Step 5: Hunting behavior and Hunter identity


```{r echo=FALSE}
### Have you ever been hunting? Yes / No
working_dat %>% 
  select(identity, HuntingParticipation_2Cat) %>% 
  na.omit()-> hunt_connection_tbl

ct5 = crosstable(hunt_connection_tbl, by = HuntingParticipation_2Cat, total="both", 
                 percent_digits=0) %>%
  as_flextable()
ct5



working_dat %>% 
  select(identity, Identity_Hunter) %>% 
  na.omit() -> hunt_connection

# Set the factor levels for the identity variable
hunt_connection$identity <- factor(hunt_connection$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))


### To what extent do you identify as a Hunter
identity_hunter_summary <- hunt_connection %>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(Identity_Hunter %in% c(4, 5)),
    percentage = (strongly_very_strongly / total) * 100
  )

# Print the summary
print(identity_hunter_summary)


anova1cc <- aov(Identity_Hunter ~ identity, data = hunt_connection)
#summary(anova2)

#Tukey's test

tukey1cc <- TukeyHSD(anova1cc)
#print(tukey2)

#Compact letter display
cld1cc <- multcompLetters4(anova1cc, tukey1cc)
#print(cld1cc)

#table with factors and 3rd quantile
Tk1cc <- group_by(hunt_connection, identity) %>%
  summarise(mean = mean(Identity_Hunter), quant1cc = quantile (Identity_Hunter, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld1cc <- as.data.frame.list(cld1cc$identity)
Tk1cc$cld1cc <- cld1cc$Letters
#print(Tk1cc)

# ETA Squared For Conservation Caring
anova1cc_summary <- summary.aov(anova1cc)
eta_squared_hunter <- get_eta_squared(anova1cc_summary)

anova1cc_summary

print(eta_squared_hunter)



# Enhanced Boxplot with Jittered Points for Hunter Identity
hunt_id_plot <- ggplot(hunt_connection, aes(x = identity, y = Identity_Hunter, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Group Alignment with Hunter Identity",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 9. Enhanced boxplot showing groups' alignment with hunter identity. Letter above group denotes significant difference if letter is not shared.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk1cc, aes(label = cld1cc, x = identity, y = quant1cc), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="hunt_id_plot.svg", plot=hunt_id_plot, width=10, height=10)

print(hunt_id_plot)

```


### Step 6: Wedge Issues


```{r echo=FALSE}

### Trophy Hunting

working_dat %>%  ### Trophy hunting has a lot of NAs for some reason, so I'm isolating it in this subset so I don't eliminate too many rows in other variables
  select(identity, HuntApprove_Trophy) %>% 
  na.omit()->trophy

anova9 <- aov(HuntApprove_Trophy ~ identity, data = trophy)
#summary(anova2)

#Tukey's test

tukey9 <- TukeyHSD(anova9)
#print(tukey2)

#Compact letter display
cld9 <- multcompLetters4(anova9, tukey9)
#print(cld9)

#table with factors and 3rd quantile
Tk9 <- group_by(trophy, identity) %>%
  
  summarise(mean = mean(HuntApprove_Trophy), quant9 = quantile (HuntApprove_Trophy, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld9 <- as.data.frame.list(cld9$identity)
Tk9$cld9 <- cld9$Letters
#print(Tk9)


#boxplot
library(ggplot2)
library(dplyr)

# Set the factor levels for the identity variable
trophy$identity <- factor(trophy$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

# Enhanced Boxplot with Jittered Points for Trophy Hunting Approval
wedge_trophy <- ggplot(trophy, aes(x = identity, y = HuntApprove_Trophy, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Group Approval of Trophy Hunting",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 10. Enhanced boxplot showing groups' approval of hunting for trophy reasons. Letter above group denotes significant difference if letter is not shared.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk9, aes(label = cld9, x = identity, y = quant9), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="wedge_trophy.svg", plot=wedge_trophy, width=10, height=10)

print(wedge_trophy)


### Gun Rights



working_dat %>%  
  select(identity, Identity_GunRights, Identity_AnimalRights) %>% 
  na.omit()->wedge_id

# Set the factor levels for the identity variable
wedge_id$identity <- factor(wedge_id$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

### Means

# Calculate means
means <- wedge_id %>%
  group_by(identity) %>%
  summarise(Mean = mean (Identity_GunRights),
            StdDev = sd(Identity_GunRights, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(means)

gun_right_summary <- wedge_id %>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(Identity_GunRights %in% c(4, 5)),
    percentage = (strongly_very_strongly / total) * 100
  )

print(gun_right_summary)


anova1a <- aov(Identity_GunRights ~ identity, data = wedge_id)
#summary(anova2)

#Tukey's test

tukey1a <- TukeyHSD(anova1a)
#print(tukey2)

# ETA Squared For Gun Rights
anova1a_summary <- summary.aov(anova1a)
eta_squared_gr <- get_eta_squared(anova1a_summary)

anova1a_summary

print(eta_squared_gr)


#Compact letter display
cld1a <- multcompLetters4(anova1a, tukey1a)
#print(cld2)

#table with factors and 3rd quantile
Tk1a <- group_by(wedge_id, identity) %>%
  summarise(mean = mean(Identity_GunRights), quant1a = quantile (Identity_GunRights, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld1a <- as.data.frame.list(cld1a$identity)
Tk1a$cld1a <- cld1a$Letters
#print(Tk1a)




# Enhanced Boxplot with Jittered Points for Gun Rights Identity
wedge_gun <- ggplot(wedge_id, aes(x = identity, y = Identity_GunRights, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Group Alignment with Gun Rights Identity",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 11. Enhanced boxplot of identity groups' alignment with a gun rights identity. Letter above group denotes significant difference if letter is not shared.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk1a, aes(label = cld1a, x = identity, y = quant1a), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="wedge_gun.svg", plot=wedge_gun, width=10, height=10)

print(wedge_gun)



### Animal Rights

working_dat %>%  
  select(identity, Identity_GunRights, Identity_AnimalRights) %>% 
  na.omit()->wedge_id

# Calculate means
means <- wedge_id %>%
  group_by(identity) %>%
  summarise(Mean = mean (Identity_AnimalRights),
            StdDev = sd(Identity_AnimalRights, na.rm = TRUE))  # Calculate standard deviation


# View the results
print(means)

#####
anim_right_summary <- wedge_id %>%
  group_by(identity) %>%
  summarise(
    total = n(),
    strongly_very_strongly = sum(Identity_AnimalRights %in% c(4, 5)),
    percentage = (strongly_very_strongly / total) * 100
  )

print(anim_right_summary)
####

anova1b <- aov(Identity_AnimalRights ~ identity, data = wedge_id)
#summary(anova2)

#Tukey's test

tukey1b <- TukeyHSD(anova1b)
#print(tukey2)

#Compact letter display
cld1b <- multcompLetters4(anova1b, tukey1b)
#print(cld2)

#table with factors and 3rd quantile
Tk1b <- group_by(wedge_id, identity) %>%
  summarise(mean = mean(Identity_AnimalRights), quant1b = quantile (Identity_AnimalRights, probs =0.75))%>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table
cld1b <- as.data.frame.list(cld1b$identity)
Tk1b$cld1b <- cld1b$Letters
#print(Tk1b)

# ETA Squared For Gun Rights
anova1b_summary <- summary.aov(anova1b)
eta_squared_anim<- get_eta_squared(anova1b_summary)

anova1a_summary

print(eta_squared_anim)



# Set the factor levels for the identity variable
wedge_id$identity <- factor(wedge_id$identity, levels = c("conservationist", "environmentalist", "eco_agnostic", "pluralist"))

# Enhanced Boxplot with Jittered Points for Animal Rights Identity
wedge_anim_rights <- ggplot(wedge_id, aes(x = identity, y = Identity_AnimalRights, fill = identity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5) +
  geom_jitter(aes(color = identity), width = 0.15, size = 1, alpha = 0.6) +
  labs(title = "Group Alignment with Animal Rights Identity",
       x = "Identity", 
       y = "Likert Rating", 
       caption = "Figure 12. Enhanced boxplot of identity groups' alignment with an animal rights identity. Letter above group denotes significant difference if letter is not shared.") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        plot.caption = element_text(hjust = 0)) +
  geom_text(data = Tk1b, aes(label = cld1b, x = identity, y = quant1b), 
            vjust = -0.8, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(file="wedge_anim_rights.svg", plot=wedge_anim_rights, width=10, height=10)

print(wedge_anim_rights)




## 
## Test - trying to find the right directory for overleaf 


##
```

#### Looking for statistical similarity in the visual similarity between approval of legal reg hunting and egoistic reasons. I think overall approval might be from conception of hunting for egoistic reasons. 

No strong relationship found across identities and the two approvals - some moderate

```{r}
# Convert 'identity' to dummy variables excluding the intercept
identity_dummies <- model.matrix(~ identity - 1, data = hunt_beliefs)

# Subset the specific approval columns
approval_data <- hunt_beliefs[, c("HuntingApproval", "HuntApproval_Egoism")]

# Perform Canonical Correlation Analysis using base R's cancor function
cancor_results <- cancor(approval_data, identity_dummies)

# Print the results of the canonical correlation analysis
print(cancor_results)


```


Methods: Survey demographics

```{r}

working_dat %>% 
  select(University, StateName, Gender_2Cat, YearBorn, Race_6Cat, ChildhoodLocation_2Cat) ->demos2

summary <- dfSummary(demos2,
                     max.distinct.values = Inf, # No limit on distinct values shown
                     max.items = NULL,          # Do not truncate the list
                     style = "grid"             # Choose a style (grid, plain, rmarkdown, etc.)
)

# To view the summary
view(summary)
```


### Block Regression of WVO, Identities, Demographics predicting Animal Rights

Idea being that Identities should be significant contributor to model if more salient because of polarizing issues.


```{r}
# df = working_dat 

# wvo block 1 "WVO_Mutualism" "WVO_Domination" 

# identity block 2 "identity"

# demographics block 3 "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat"

# first predict "Identity_AnimalRights"




# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_AnimalRights) %>%
  na.omit()


# Block 1 (Wildlife Value Orientations)
block1_vars <- c("WVO_Mutualism", "WVO_Domination")
block1_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1 )
summary(block1_model)

# Block 2 (Add Identity to the model)
block2_vars <- c(block1_vars, "identity")
block2_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1 )
summary(block2_model)

# Block 3 (Add Demographic Variables)
block3_vars <- c(block2_vars, "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block3_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1 )
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)


######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Wildlife Value Orientations)", 
            "Block 2 (Add Identity)", 
            "Block 3 (Add Demographics)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

nrow(complete_data_1)

```

### Block Regression of WVO, Identities, Demographics predicting Hunter identity

Idea being that Identities should be significant contributor to model if more salient because of polarizing issues.


```{r}
# df = working_dat 

# wvo block 1 "WVO_Mutualism" "WVO_Domination" 

# identity block 2 "identity"

# demographics block 3 "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat"

# first predict "Identity_Hunter"

# Filter to keep only complete cases for all variables used in any of the models
complete_data_2 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_Hunter) %>%
  na.omit()


# Block 1 (Wildlife Value Orientations)
block1_vars <- c("WVO_Mutualism", "WVO_Domination")
block1_formula <- as.formula(paste("Identity_Hunter ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_2 )
summary(block1_model)

# Block 2 (Add Identity to the model)
block2_vars <- c(block1_vars, "identity")
block2_formula <- as.formula(paste("Identity_Hunter ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_2 )
summary(block2_model)

# Block 3 (Add Demographic Variables)
block3_vars <- c(block2_vars, "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block3_formula <- as.formula(paste("Identity_Hunter ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_2)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)


######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Wildlife Value Orientations)", 
            "Block 2 (Add Identity)", 
            "Block 3 (Add Demographics)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

nrow(complete_data_2)






```
### Block Regression of WVO, Identities, Demographics predicting Gun rights advocate identity

Idea being that Identities should be significant contributor to model if more salient because of polarizing issues.


```{r}
# df = working_dat 

# wvo block 1 "WVO_Mutualism" "WVO_Domination" 

# identity block 2 "identity"

# demographics block 3 "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat"

# first predict "Identity_GunRights"

# Filter to keep only complete cases for all variables used in any of the models
complete_data_2 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_GunRights) %>%
  na.omit()


# Block 1 (Wildlife Value Orientations)
block1_vars <- c("WVO_Mutualism", "WVO_Domination")
block1_formula <- as.formula(paste("Identity_GunRights ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_2 )
summary(block1_model)

# Block 2 (Add Identity to the model)
block2_vars <- c(block1_vars, "identity")
block2_formula <- as.formula(paste("Identity_GunRights ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_2 )
summary(block2_model)

# Block 3 (Add Demographic Variables)
block3_vars <- c(block2_vars, "CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block3_formula <- as.formula(paste("Identity_GunRights ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_2)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)


######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Wildlife Value Orientations)", 
            "Block 2 (Add Identity)", 
            "Block 3 (Add Demographics)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

nrow(complete_data_2)





```


Animal Rights - Reordered blocks (demos, WVO, environmental identities) 


```{r}

# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_AnimalRights) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("Identity_AnimalRights ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)



```


Hunting - Reordered blocks (demos, WVO, environmental identities) 


```{r}
# Filter to keep only complete cases for all variables used in any of the models
complete_data_2 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_Hunter) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("Identity_Hunter ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_2)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("Identity_Hunter ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_2)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("Identity_Hunter ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_2)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_2)




```


Gun Rights - Reordered blocks (demos, WVO, environmental identities)


```{r}

# Filter to keep only complete cases for all variables used in any of the models
complete_data_2 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, Identity_GunRights) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("Identity_GunRights ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_2)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("Identity_GunRights ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_2)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("Identity_GunRights ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_2)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_2)



```


### Experimenting with block regression to predict other vars


```{r}
### predict Conservation Caring


# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, ConservationCaringScale) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("ConservationCaringScale ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("ConservationCaringScale ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("ConservationCaringScale ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)


#################### Belief that Hunters Care about Cons Wildlife

# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, HuntBelief_HuntersCare) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("HuntBelief_HuntersCare ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("HuntBelief_HuntersCare ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("HuntBelief_HuntersCare ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)

############################################ Approval of Hunting


# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, HuntingApproval) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("HuntingApproval ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("HuntingApproval ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("HuntingApproval ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)


######## Approval _ Trophy


# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, HuntApprove_Trophy) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("HuntApprove_Trophy ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("HuntApprove_Trophy ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("HuntApprove_Trophy ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)


######### Egoistic approval

# Filter to keep only complete cases for all variables used in any of the models
complete_data_1 <- working_dat %>%
  dplyr::select(WVO_Mutualism, WVO_Domination, identity, CollegeMajor_2Cat, ChildhoodLocation_2Cat, Race_2Cat, Gender_2Cat, HuntApproval_Egoism) %>%
  na.omit()

# Block 1 (Demographics)
block1_vars <- c("CollegeMajor_2Cat", "ChildhoodLocation_2Cat", "Race_2Cat", "Gender_2Cat")
block1_formula <- as.formula(paste("HuntApproval_Egoism ~", paste(block1_vars, collapse = " + ")))

# Fit the first block
block1_model <- lm(block1_formula, data = complete_data_1)
summary(block1_model)

# Block 2 (Wildlife Value Orientations)
block2_vars <- c(block1_vars, "WVO_Mutualism", "WVO_Domination")
block2_formula <- as.formula(paste("HuntApproval_Egoism ~", paste(block2_vars, collapse = " + ")))

# Fit the second block
block2_model <- lm(block2_formula, data = complete_data_1)
summary(block2_model)

# Block 3 (Add Identity)
block3_vars <- c(block2_vars, "identity")
block3_formula <- as.formula(paste("HuntApproval_Egoism ~", paste(block3_vars, collapse = " + ")))

# Fit the third block
block3_model <- lm(block3_formula, data = complete_data_1)
summary(block3_model)

# Compare models by examining R-squared or using an ANOVA
anova(block1_model, block2_model, block3_model)

######

# Extract AIC values
block1_aic <- AIC(block1_model)
block2_aic <- AIC(block2_model)
block3_aic <- AIC(block3_model)

# Extract R-squared values
block1_r2 <- summary(block1_model)$r.squared
block2_r2 <- summary(block2_model)$r.squared
block3_r2 <- summary(block3_model)$r.squared

# Calculate Delta R-squared (Change in R²)
delta_r2_block2 <- block2_r2 - block1_r2
delta_r2_block3 <- block3_r2 - block2_r2

# Extract residual sum of squares from ANOVA
anova_results <- anova(block1_model, block2_model, block3_model)

# Extract residual sum of squares
block1_rss <- sum(residuals(block1_model)^2)
block2_rss <- sum(residuals(block2_model)^2)
block3_rss <- sum(residuals(block3_model)^2)

# Create the data frame for the metrics
block_comparison <- data.frame(
  Block = c("Block 1 (Demographics)",
            "Block 2 (Wildlife Value Orientations)",
            "Block 3 (Add Identity)"),
  R2 = c(block1_r2, block2_r2, block3_r2),
  Delta_R2 = c(NA, delta_r2_block2, delta_r2_block3),
  AIC = c(block1_aic, block2_aic, block3_aic),
  RSS = c(block1_rss, block2_rss, block3_rss),
  F_Statistic = c(NA, anova_results$F[2], anova_results$F[3]),
  p = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

# Round all numeric columns to three decimal places
numeric_columns <- sapply(block_comparison, is.numeric)
block_comparison[numeric_columns] <- lapply(block_comparison[numeric_columns], function(x) round(x, 3))

# Ensure p-values are formatted to show three decimal places, even for small values
block_comparison$p <- formatC(block_comparison$p, format = "f", digits = 3)

# Print the comparison table using knitr
library(knitr)
kable(block_comparison, caption = "Comparison of Predictive Power Across Models")

# Check the number of complete cases
nrow(complete_data_1)


#######
###
```





