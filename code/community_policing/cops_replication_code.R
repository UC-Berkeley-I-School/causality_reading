# Obtained from https://isps.yale.edu/research/data/d152 (January 2023)
# with small fixes applied (see notes from Tilman below)
# ========================================================================

# This file replicates the main results presented in the manuscript, 
# "A field experiment on community policing and police legitimacy". 
# It also replicates the analyses reported in Table S1, S4, S5, S7-S16, and Fig S10
# in the Supplementary Materials. https://doi.org/10.1073/pnas.1910157116 


# Note: The data provided here are sufficient for replicating the results reported 
# in the  paper. Due to privacy concerns of both officers and residents, we are
# unable to make some data publicly available. This includes, 
# 1) The officer-level covariate information that we use to present descriptive 
#    statistics as these data would identify individual officers.
# 2) The officer level canvassing survey data that recorded their perceptions of 
#    interactions w/ residents.
# 3) Geographic data on membership in each of the city's 10 police districts as 
#    these data are indirect identifiers that, in combination w/ covariates, 
#    could be used to identify individuals that participated in the study. 
# 4) Individual-level data from the voter file in order to protect the 
#    confidentiality of research participants. 


# Load packages necessary for replication. 
suppressMessages({
  library(estimatr)
  library(knitr)
  library(kableExtra)
  library(scales)
  library(tidyverse)
  library(ggplot2)
  library(lemon)
  library(caret)
  library(haven)
  library(ggbeeswarm)
})

# Helper functions for table creation 
add_parens <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_entry <- function(est, se, p, alpha, digits = 2) {
  entry <- paste0(format_num(est, digits = digits), " ", 
                  add_parens(se, digits = digits))
  entry[p < alpha] <- paste0(entry[p < alpha], "*")
  return(entry)
}

# Theme for pretty plots
devtools::source_url("https://kylepeyton.github.io/assets/theme_kyle.R")

theme_cops <- function () { 
  theme_kyle(font_size = 18) + #%+replace%
    theme(legend.position = "bottom",
          legend.justification = "center",
          text = element_text(family = "serif", size = 18),
          plot.title = element_text(margin = margin(b = 0, l = -10), 
                                    size = 24, face = "bold", family = "serif"), 
          legend.key.size = unit(3, "line"),
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(-30, -10, -10, -10),
          strip.text.x = element_text(size = 18, family = "serif"),
          axis.title.y = element_text(size = 19, family = "serif"),
          legend.title = element_blank(),
          axis.text.x = element_text(size = 19, family = "serif"),
          strip.background = element_rect(fill = "grey90"),
          axis.line.y = element_line(size = I(4/12), colour = "black"),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.y = element_line(size = I(4/12), colour = "black"))
}

# Read in dataset. 
cops_df <- 
#  read_rds("cops_replication_data_isps_2019.rds")
  read_rds("cops_replication_data_july2019.rds")
# (Tilman, January 2023:) fixed file name to the one 
# currently used at https://isps.yale.edu/research/data/d152

##------Outcome Scaling------

# This section implements the standardization procedures detailed in the PAP
# and the SI. 

# This function subsets a dataframe (df) to respondents (R) and the relevant
# columns (col_names), performs a principal components analysis, and extracts 
# the first component from the PCA to use an an index. It returns a dataframe
# with this new index added to the dataframe as a column with the desired label
# (new_name). PCA is performed using the prcomp() function in base R, which 
# used singular value decomposition of the data matrix, which is standardized 
# to have mean 0 (center = TRUE) and sd 1 (scale = TRUE) by default, but it
# allows for custom transformations. It also allows for a user-specified 
# rotation matrix of variable loadings (Q). The default constructs the rotation
# matrix using the subset of columns (col_names) in the dataframe (df).
scale_outcome <- function(df = NULL, R = NULL, Q = NULL, scale = TRUE, 
                          center = TRUE, col_names = NULL, new_name = NULL){
  # Response index
  index <- as.logical(R)
  index[is.na(index)] <- FALSE
  
  # Subset to desired columns. 
  X <- df[index, col_names]
  
  # Unless a custom rotation matrix is provided, use prcomp function to 
  # perform PCA using singular value decomposition of the data matrix 
  if(is.null(Q))
    Q <- prcomp(X, scale. = scale, center = center)$rotation
  
  # Multiply centered data by the rotation matrix to generate the rotated
  # data matrix (P), then extract first column (y) or "principal component"
  Z <- scale(as.matrix(X), scale = scale, center = center) 
  P <- Z %*% Q
  y <- as.vector(P[, 1])
  
  # The sign of the columns of the rotation matrix (Q) are arbitrary; this
  # ensures more positive values indicate more positive attitudes 
  if(cor(y, Z[, 1], use = "complete.obs") < 0) 
    y <- -y
  
  # Add the index, with the desired label, and export the new dataframe, the
  # rotation matrix, and the rotated data matrix
  df[[paste(new_name)]] <- NA
  df[index, ][[paste(new_name)]] <- y
  return(list(df = df, Q = Q, P = P))
}


# This function subsets a dataframe (df) to respondents (R) and the relevant
# columns (col_names), then scales the columns to have mean 0 (center = TRUE) 
# and sd 1 (scale = TRUE) and sums across the rows to create a standardized
# index. It returns a dataframe with this new index added to the dataframe as a 
# column with the desired label (new_name).
scale_sum <- function(df = NULL, R = NULL, col_names = NULL, new_name = NULL, 
                      center = TRUE, scale = TRUE){
  # Response index
  index <- as.logical(R)
  index[is.na(index)] <- FALSE
  
  # subset to relevant columns in dataframe
  tmp <- df[index, col_names] 
  
  # apply procedure, create new column vector w/ the appropriate name
  df[[paste(new_name)]] <- NA
  df[index, ][[paste(new_name)]]  <- 
    as.numeric(
      rowSums(
        scale(tmp, center = center, scale = scale)
      )
    )
  return(df)
}


##------Legitimacy----
legit <- 
  cops_df %>%
  dplyr::select(starts_with("itrust_care"), starts_with("fair_decision"),
                starts_with("norm_action"), starts_with("norm_values"),
                starts_with("norm_stand"), starts_with("conf_lawful"),
                starts_with("fair_lawful"), starts_with("itrust_honest")) %>%
  names()

legit_t0 <- tidyselect::vars_select(legit, ends_with("_t0"))
legit_t1 <- tidyselect::vars_select(legit, ends_with("_t1"))
legit_t2 <- tidyselect::vars_select(legit, ends_with("_t2"))


cops_df <- 
  scale_sum(df = cops_df, col_names = legit_t0, new_name = "legit_index_t0",
            R = cops_df$R_t0)

cops_df <- 
  scale_sum(df = cops_df, col_names = legit_t1, new_name = "legit_index_t1",
            R = cops_df$R_t1)

cops_df <- 
  scale_sum(df = cops_df, col_names = legit_t2, new_name = "legit_index_t2",
            R = cops_df$R_t2)

##------Cooperation----
coop <- 
  cops_df %>%
  dplyr::select(starts_with("coop_")) %>%
  names()

coop_t0 <- tidyselect::vars_select(coop, ends_with("_t0"))
coop_t1 <- tidyselect::vars_select(coop, ends_with("_t1"))
coop_t2 <- tidyselect::vars_select(coop, ends_with("_t2"))


cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t0, col_names = coop_t0,
            new_name = "coop_index_t0")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t1, col_names = coop_t1,
            new_name = "coop_index_t1")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t2, col_names = coop_t2,
            new_name = "coop_index_t2")

##------Compliance-----
comply <- 
  cops_df %>%
  dplyr::select(starts_with("legit_comply"), starts_with("legit_ignore"),
                starts_with("legit_disagree"), starts_with("legit_mistreat")) %>%
  names()


comply_t0 <- tidyselect::vars_select(comply, ends_with("_t0"))
comply_t1 <- tidyselect::vars_select(comply, ends_with("_t1"))
comply_t2 <- tidyselect::vars_select(comply, ends_with("_t2"))


cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t0, col_names = comply_t0,
            new_name = "comply_index_t0")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t1, col_names = comply_t1,
            new_name = "comply_index_t1")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t2, col_names = comply_t2,
            new_name = "comply_index_t2")

##------Performance-----
perform <- 
  cops_df %>%
  dplyr::select(starts_with("itrust_confident"), 
                starts_with("perform_crimefight"),
                starts_with("perform_helping"), 
                starts_with("perform_response")) %>%
  names()


perform_t0 <- tidyselect::vars_select(perform, ends_with("_t0"))
perform_t1 <- tidyselect::vars_select(perform, ends_with("_t1"))
perform_t2 <- tidyselect::vars_select(perform, ends_with("_t2"))


cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t0, col_names = perform_t0,
            new_name = "perform_index_t0")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t1, col_names = perform_t1,
            new_name = "perform_index_t1")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t2, col_names = perform_t2,
            new_name = "perform_index_t2")

##------Primary DVs Index------
primary_dvs <- c(legit, coop, comply, perform)

primary_dvs_t0 <- tidyselect::vars_select(primary_dvs, ends_with("_t0"))
primary_dvs_t1 <- tidyselect::vars_select(primary_dvs, ends_with("_t1"))
primary_dvs_t2 <- tidyselect::vars_select(primary_dvs, ends_with("_t2"))


cops_df <- 
  scale_outcome(df = cops_df, R = cops_df$R_t0, col_names = primary_dvs_t0,
                new_name = "primary_dvs_index_t0")$df

cops_df <- 
  scale_outcome(df = cops_df, R = cops_df$R_t1, col_names = primary_dvs_t1,
                new_name = "primary_dvs_index_t1")$df

cops_df <- 
  scale_outcome(df = cops_df, R = cops_df$R_t2, col_names = primary_dvs_t2,
                new_name = "primary_dvs_index_t2")$df


##------Community Support-----
csi <- 
  cops_df %>%
  dplyr::select(starts_with("community_safe"), starts_with("community_equal"),
                starts_with("community_better"), starts_with("community_respect"),
                starts_with("community_perform"), starts_with("community_gallup")) %>%
  names()

csi_t0 <- tidyselect::vars_select(csi, ends_with("_t0"))
csi_t1 <- tidyselect::vars_select(csi, ends_with("_t1"))
csi_t2 <- tidyselect::vars_select(csi, ends_with("_t2"))

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t0, col_names = csi_t0,
            new_name = "csi_index_t0")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t1, col_names = csi_t1,
            new_name = "csi_index_t1")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t2, col_names = csi_t2,
            new_name = "csi_index_t2")


##------Stereotypes----
stype <- 
  cops_df %>%
  dplyr::select(starts_with("stereotype_")) %>%
  names()

stype_t0 <- tidyselect::vars_select(stype, ends_with("_t0"))
stype_t1 <- tidyselect::vars_select(stype, ends_with("_t1"))
stype_t2 <- tidyselect::vars_select(stype, ends_with("_t2"))

# Police:
cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t0, 
            col_names = stype_t0[str_detect(stype_t0, "police")],
            new_name = "stereotype_police_index_t0")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t1, 
            col_names = stype_t1[str_detect(stype_t1, "police")],
            new_name = "stereotype_police_index_t1")

cops_df <- 
  scale_sum(df = cops_df, R = cops_df$R_t2, 
            col_names = stype_t2[str_detect(stype_t2, "police")],
            new_name = "stereotype_police_index_t2")


##------Trust Index-----
trust_t0 <- c("trustgov_bigintrst_nh_t0", "trustgov_corrpt_nh_t0",
              "trustgov_trustgrev_nh_t0", "trustgov_waste_nh_t0")

trust_t1 <- c("trustgov_bigintrst_nh_t1", "trustgov_corrpt_nh_t1",
              "trustgov_trustgrev_nh_t1", "trustgov_waste_nh_t1")

cops_df <- 
  scale_outcome(df = cops_df, R = cops_df$R_t0, col_names = trust_t0,
                new_name = "trustgov_local_index_t0")$df

cops_df <- 
  scale_outcome(df = cops_df, R = cops_df$R_t1, col_names = trust_t1,
                new_name = "trustgov_local_index_t1")$df

###-----Standardize Primary/Secondary Outcomes to 0-100------

# Pre-registered primary DVs
primary_dvs <- c("coop_index", "comply_index", "legit_index",
                 "perform_index", "primary_dvs_index")

# Pre-registered secondary DVs
secondary_dvs <- c("csi_index", "therm_police", "stereotype_police_index",
                   "policy_bodycam", "policy_increase")


# Standardize all outcomes to 0-100
cops_df <- 
  cops_df %>% 
  mutate_at(.vars = c(paste0(primary_dvs, "_t0"),
                      paste0(primary_dvs, "_t1"), 
                      paste0(primary_dvs, "_t2"),
                      paste0(secondary_dvs, "_t0"),
                      paste0(secondary_dvs, "_t1"), 
                      paste0(secondary_dvs, "_t2"),
                      "trustgov_local_index_t0",
                      "trustgov_local_index_t1"),
            .funs = list( ~ scales::rescale(., to = c(0, 100))))


###-----Table S1, S4, S5, S7-S9------

# Note: Tables S2-S3 use predicted probabilities for race/ethnicity that are
# unique to each individual. These indirect identifiers could be used in
# combination with covariates to identify individuals that participated in
# the study and these data are not provided w/ replication materials. 

# Note: Table S6 is proprietary data from NHPD employement records. These 
# data are not provided w/ replication materials. 

# Note: Tables S7-S9 do not include district level identifiers, which are 
# indirect identifiers that, in combination w/ covariates, could be used to 
# identify individuals that participated in the study. 


##------Table S1: Survey Respondents by Experimental Condition and Wave----
rr_t0 <- cops_df %>% group_by(Z) %>% tally(R_t0) %>% na.omit()

rr_t1 <- cops_df %>% group_by(Z) %>% tally(R_t1) %>%  na.omit()

rr_t2 <- cops_df %>% group_by(Z) %>% tally(R_t2) %>% na.omit()

rr_df <- 
  bind_cols(rr_t0, rr_t1, rr_t2) %>% 
  # dplyr::select(-Z1, -Z2) %>%
  # (Tilman, January 2023:) fixed bug apparently resulting from 
  # bind_cols() now autogenerating differing column names  
  dplyr::select(-"Z...3", -"Z...5") %>%
  # mutate(Z = ifelse(Z == 1, "Treatment", "Control"))
  mutate(Z...1 = ifelse(Z...1 == 1, "Treatment", "Control"))

colnames(rr_df) <- c("Condition", "Baseline (T0)", "3 Day (T1)", "3 Week (T2)")

kable(rr_df, "latex", booktabs = TRUE,
      caption = "\\label{tab:assignment}Survey Respondents by Experimental Condition and Wave") %>%
  add_header_above(c(" ", "Survey Wave" = 3), bold = T, italic = T) %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") 

##------Table S4: Reported confidence in the police department by level and group-----
# Baseline confidence in police 

# (Tilman, January 2023:)
# Commenting out the following code for generating S4,
# which fails with "object 'pew_matrix_police_t0' not found",
# as the replication dataset retrieved via https://isps.yale.edu/research/data/d152
# does not contain a column with that name
# (or a similar name - I checked with "cops_df %>% select(contains('police')) %>% names()" 
# todo?: figure out how the "Confidence in Police" column in est_secondary_t1 etc.
# is generated below) 
# 
# conf_overall <- 
#   cops_df %>%
#   filter(R_t0 == 1) %>%
#   summarise(pew_alot = mean(pew_matrix_police_t0 == 4, na.rm = TRUE),
#             pew_some = mean(pew_matrix_police_t0 == 3, na.rm = TRUE),
#             pew_little = mean(pew_matrix_police_t0 == 2, na.rm = TRUE),
#             pew_none = mean(pew_matrix_police_t0 == 1, na.rm = TRUE)) %>%
#   mutate(group = "Overall") %>%
#   dplyr::select(group, everything())
# 
# # Baseline confidence in police by race/ethnicity 
# conf_race <- 
#   cops_df %>%
#   filter(R_t0 == 1) %>%  
#   mutate(dem_race4 = factor(dem_race4, levels = c("White", "Black", "Hispanic",
#                                                   "Other"))) %>%
#   group_by(dem_race4) %>%
#   summarise(pew_alot = mean(pew_matrix_police_t0 == 4, na.rm = TRUE),
#             pew_some = mean(pew_matrix_police_t0 == 3, na.rm = TRUE),
#             pew_little = mean(pew_matrix_police_t0 == 2, na.rm = TRUE),
#             pew_none = mean(pew_matrix_police_t0 == 1, na.rm = TRUE)) %>%
#   rename(group = dem_race4)
# 
# # Compare to data from 2016 Pew survey:
# #http://www.pewsocialtrends.org/2016/09/29/the-racial-confidence-gap-in-police-performance/
# 
# # Compare to Pew data
# conf_pew <- data.frame(group = c("Overall", "White", "Black"),
#                        pew_alot = c(0.36, 0.42, 0.14),
#                        pew_some = c(0.41, 0.39, 0.41),
#                        pew_little = c(0.13, 0.12, 0.20),
#                        pew_none = c(0.09, 0.06, 0.24))
# 
# conf_table <- bind_rows(conf_overall, conf_race, conf_pew)
# col_labs <- c("Group", "A Lot", "Some", "Only a little", "None at all")
# 
# kable(conf_table, "latex", booktabs = TRUE, digits = 2, col.names = col_labs,
#       caption = "\\label{tab:conf}Reported confidence in the police department by level and group") %>%
#   kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
#   kableExtra::group_rows("Baseline (T0 Survey)", 1, 5) %>%
#   kableExtra::group_rows("Pew Survey (2016)", 6, 8) %>%
#   footnote(general = c("Pew data from Aug.16-Sept. 12, 2016 Survey of U.S. adults.")) 


##------Table S5: Feeling thermometer ratings of police by level and group-----

# Police feeling themometer scores at baseline
therm_overall <- 
  cops_df %>%
  filter(R_t0 == 1) %>%
  summarise(very_cold = mean(therm_police_t0 %in% c(0:24), na.rm = TRUE),
            somewhat_cold = mean(therm_police_t0 %in% c(25:49), na.rm = TRUE),
            neutral = mean(therm_police_t0 %in% c(50), na.rm = TRUE),
            somewhat_warm = mean(therm_police_t0 %in% c(51:75), na.rm = TRUE),
            very_warm = mean(therm_police_t0 %in% c(76:100), na.rm = TRUE)) %>%
  mutate(group = "Overall") %>%
  dplyr::select(group, everything())

# Police feeling themometer at baseline, by race/ethnicity
therm_race <- 
  cops_df %>%
  filter(R_t0 == 1) %>%
  mutate(dem_race4 = factor(dem_race4, levels = c("White", "Black", "Hispanic",
                                                  "Other"))) %>%
  group_by(dem_race4) %>%
  summarise(very_cold = mean(therm_police_t0 %in% c(0:24), na.rm = TRUE),
            somewhat_cold = mean(therm_police_t0 %in% c(25:49), na.rm = TRUE),
            neutral = mean(therm_police_t0 %in% c(50), na.rm = TRUE),
            somewhat_warm = mean(therm_police_t0 %in% c(51:75), na.rm = TRUE),
            very_warm = mean(therm_police_t0 %in% c(76:100), na.rm = TRUE)) %>%
  rename(group = dem_race4)

# Compare to data from 2016 Pew survey:
# https://www.pewresearch.org/fact-tank/2017/09/15/deep-racial-partisan-divisions-in-americans-views-of-police-officers/
therm_pew <- data.frame(group = c("Overall", "White", "Black", "Hispanic"),
                        very_cold= c(0.10, 0.07, 0.30, 0.08),
                        somewhat_cold = c(0.08, 0.07, 0.08, 0.09),
                        neutral = c(0.16, 0.11, 0.28, 0.25),
                        somewhat_warm = c(0.19, 0.20, 0.08, 0.18),
                        very_warm = c(0.45, 0.53, 0.22, 0.37))

therm_table <- bind_rows(therm_overall, therm_race, therm_pew)
col_labs <- c("Group", "Very cold", "Somewhat cold", "Neutral", "Somewhat warm",
              "Very warm")

kable(therm_table, "latex", booktabs = TRUE, digits = 2, col.names = col_labs,
      caption = "\\label{tab:therm}Feeling thermometer ratings of police by level and group") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  kableExtra::group_rows("Baseline (T0 Survey)", 1, 5) %>%
  kableExtra::group_rows("Pew Survey (2017)", 6, 9) %>%
  footnote(general = c("Pew data from Aug.8-21, 2017 Survey of U.S. adults."))  


##------Table S7: Background Covariates by Treatment Assignment in T0 Survey-----

# Subset to individuals who responded to baseline survey to create tables of 
# background covariates by treatment assignment.
full_df <- 
  cops_df %>% 
  filter(R_t0 == 1) %>% 
  mutate(dem_black = as.numeric(dem_race4 == "Black"),
         dem_white = as.numeric(dem_race4 == "White"),
         dem_hisp = as.numeric(dem_race4 == "Hispanic"),
         dem_other = as.numeric(dem_race4 == "Other"))

# Baseline means:
cov_means_t0 <- 
  full_df %>%
  group_by(Z) %>%
  summarise(dvs_index = mean(primary_dvs_index_t0),
            age = mean(dem_age),
            female = mean(dem_female),
            hh_size = mean(hh_size),
            inc_high = mean(dem_inc_high),
            spanish = mean(spanish),
            pid = mean(dem_pid7),
            white = mean(dem_white),
            black = mean(dem_black), 
            hisp = mean(dem_hisp),
            other = mean(dem_other),
            contact = mean(contact_f2f_t0),
            arrest = mean(contact_arrest_t0),
            unfair = mean(contact_unfair_t0)) 

# Baseline SDs:
cov_sds_t0 <- 
  full_df %>%
  filter(R_t0 == 1) %>%
  group_by(Z) %>%
  summarise(dvs_index = sd(primary_dvs_index_t0),
            age = sd(dem_age),
            female = sd(dem_female),
            hh_size = sd(hh_size),
            inc_high = sd(dem_inc_high),
            spanish = sd(spanish),
            pid = mean(dem_pid7),
            white = sd(dem_white),
            black = sd(dem_black), 
            hisp = sd(dem_hisp),
            other = sd(dem_other),
            contact = sd(contact_f2f_t0),
            arrest = sd(contact_arrest_t0),
            unfair = sd(contact_unfair_t0)) 

row_labs <- c("Index of Primary Outcomes at T0", "Age", "Female", "Household size", 
              "Annual income exceeds 50,000-59,999", "Spanish speaker",
              "Party Identification",
              "White", "Black", "Hispanic", "Other", 
              "Face-to-face contact, last 12 Mos.",
              "Any prior arrest", 
              "Any prior unfair treatment")

col_labs <- c("Treat Mean", "Treat SD", "Ctrl Mean", "Ctrl SD")

# Bring together and export via kable
cov_bal_t0 <- 
  bind_rows(cov_means_t0, cov_sds_t0) %>% 
  arrange(desc(Z)) %>% 
  dplyr::select(-Z) %>% 
  t() %>%
  data.frame()

rownames(cov_bal_t0) <- row_labs
colnames(cov_bal_t0) <- col_labs

kable(cov_bal_t0, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:cov_bal_t0}Background Covariates by Treatment Assignment in T0 Survey") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  column_spec(1, width = "7cm") %>%
  kableExtra::group_rows("Race/Ethnicity:", 8, 11) %>%
  kableExtra::group_rows("Prior Police Contact:", 12, 14) %>%
  footnote(general = c("Individual-level data from treatment (N = 1,007) and control (N = 1,006) groups."))  

##------Table S8: Background Covariates by Treatment Assignment in T1 Survey-----
# Repeat process for generating balance table for in background covariates in 
# 1st post-treatment survey wave. 
cov_means_t1 <- 
  full_df %>%
  filter(R_t1 == 1) %>%
  group_by(Z) %>%
  summarise(dvs_index = mean(primary_dvs_index_t0),
            age = mean(dem_age),
            female = mean(dem_female),
            hh_size = mean(hh_size),
            inc_high = mean(dem_inc_high),
            spanish = mean(spanish),
            pid = mean(dem_pid7),
            white = mean(dem_white),
            black = mean(dem_black), 
            hisp = mean(dem_hisp),
            other = mean(dem_other),
            contact = mean(contact_f2f_t0),
            arrest = mean(contact_arrest_t0),
            unfair = mean(contact_unfair_t0)) 

cov_sds_t1 <- 
  full_df %>%
  filter(R_t1 == 1) %>%
  group_by(Z) %>%
  summarise(dvs_index = sd(primary_dvs_index_t0),
            age = sd(dem_age),
            female = sd(dem_female),
            hh_size = sd(hh_size),
            inc_high = sd(dem_inc_high),
            spanish = sd(spanish),
            pid = mean(dem_pid7),
            white = sd(dem_white),
            black = sd(dem_black), 
            hisp = sd(dem_hisp),
            other = sd(dem_other),
            contact = sd(contact_f2f_t0),
            arrest = sd(contact_arrest_t0),
            unfair = sd(contact_unfair_t0)) 

cov_bal_t1 <- 
  bind_rows(cov_means_t1, cov_sds_t1) %>% 
  arrange(desc(Z)) %>% 
  dplyr::select(-Z) %>% 
  t() %>%
  data.frame()

rownames(cov_bal_t1) <- row_labs
colnames(cov_bal_t1) <- col_labs

kable(cov_bal_t1, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:cov_bal_t1}Background Covariates by Treatment Assignment in T1 Survey") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  column_spec(1, width = "7cm") %>%
  kableExtra::group_rows("Race/Ethnicity:", 8, 11) %>%
  kableExtra::group_rows("Prior Police Contact:", 12, 14) %>%
  footnote(general = c("Individual-level data from treatment (N = 749) and control (N = 735) groups."))  

##------Table S9: Background Covariates by Treatment Assignment in T2 Survey-----
# Repeat process for generating balance table for in background covariates in 
# 2nd post-treatment survey wave. 
cov_means_t2 <- 
  full_df %>%
  filter(R_t2 == 1) %>%
  group_by(Z) %>%
  summarise(dvs_index = mean(primary_dvs_index_t0),
            age = mean(dem_age),
            female = mean(dem_female),
            hh_size = mean(hh_size),
            inc_high = mean(dem_inc_high),
            spanish = mean(spanish),
            party = mean(dem_pid7),
            white = mean(dem_white),
            black = mean(dem_black), 
            hisp = mean(dem_hisp),
            other = mean(dem_other),
            contact = mean(contact_f2f_t0),
            arrest = mean(contact_arrest_t0),
            unfair = mean(contact_unfair_t0)) 

cov_sds_t2 <- 
  full_df %>%
  filter(R_t2 == 1) %>%
  group_by(Z) %>%
  summarise(dvs_index = sd(primary_dvs_index_t0),
            age = sd(dem_age),
            female = sd(dem_female),
            hh_size = sd(hh_size),
            inc_high = sd(dem_inc_high),
            spanish = sd(spanish),
            party = mean(dem_pid7),
            white = sd(dem_white),
            black = sd(dem_black), 
            hisp = sd(dem_hisp),
            other = sd(dem_other),
            contact = sd(contact_f2f_t0),
            arrest = sd(contact_arrest_t0),
            unfair = sd(contact_unfair_t0)) 

cov_bal_t2 <- 
  bind_rows(cov_means_t2, cov_sds_t2) %>% 
  arrange(desc(Z)) %>% 
  dplyr::select(-Z) %>% 
  t() %>%
  data.frame()

rownames(cov_bal_t2) <- row_labs
colnames(cov_bal_t2) <- col_labs


kable(cov_bal_t2, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:cov_bal_t2}Background Covariates by Treatment Assignment in T2 Survey") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  column_spec(1, width = "7cm") %>%
  kableExtra::group_rows("Race/Ethnicity:", 8, 11) %>%
  kableExtra::group_rows("Prior Police Contact:", 12, 14) %>%
  footnote(general = c("Individual-level data from treatment (N = 546) and control (N = 523) groups."))  

##------Randomization Inference (Tables S10-S12)-----

# Note that the district level variable used for blocking is not included
# in the public release dataset. Hence the datasets of simulated test 
# statistics used to assess balance in the SM for the paper are provided instead. 

# (Tilman, January 2023:) I assume the above means we also can't replicate
# code that relies on the design declaration below - commenting out these parts

## Make design declaration
# library(randomizr)
# declaration <- 
#   with(full_df, {
#     declare_ra(
#       blocks = block_id,
#       clusters = hh_id)
#   })
# 
# # Report experimental design
# declaration


# Obtain permutation matrix for 10000 random assignments based on the 
# experimental design. Notes: this takes several minutes to finish, easier 
# to load it. This code was written using R version 3.5.3 (2019-03-11). 
# The default random number generator changed from "Rounding" to "Rejection" 
# in R version 3.6.0. If using R 3.6.0 or later you must run 
# RNGkind(sample.kind = "Rounding") prior to set.seed(1) to ensure 
# cross-platform consistency. See ?RNGkind for additional details. 
#set.seed(1)
#perm_mat <- obtain_permutation_matrix(declaration)
perm_mat <-
  read_rds("ri_permutation_matrix.rds")


##------Table S10: Randomization Inference (RI) for covariate balance-----
# Read in dataset of 10000 simulated test statistics under the null.
# Note that the district level variable used for blocking is not included
# in the public release dataset. Hence the dataset of simulated test 
# statistics used to assess balance in the SM for the paper is provided instead. 
null_dist_df <- 
  read_rds("ri_balance_null_dist.rds")

ri_table <- 
  null_dist_df %>%
  group_by(wave) %>%
  summarise(obs = mean(obs), 
            ci_lower = quantile(est, probs = 0.025),
            ci_upper = quantile(est, probs = 0.975),
            ri_pvalue = mean(est >= obs)
  ) 

colnames(ri_table) <- c("Survey Wave", "Observed F-Statistic", 
                        "0.025th Quantile", "0.975th Quantile", "RI P-value")
kable(ri_table, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:bal_ri}Randomization Inference (RI) for covariate balance") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  footnote(general = c("Quantiles of null distribution and RI P-values from 10,000 permutations of the experimental design."))  

##------Table S11: Randomization Inference (RI) for the sharp null hypothesis-----

# Read in dataset of 10000 simulated test statistics under the null.
# Alternatively, implement the procedure directly using the code for 
# ri2 package below.
null_dist_df <- 
  read_rds("ri_attrit_sharp_dist.rds")

ri_table <- 
  null_dist_df %>%
  group_by(wave) %>%
  summarise(obs = mean(obs), 
            ci_lower = quantile(est, probs = 0.025),
            ci_upper = quantile(est, probs = 0.975),
            ri_pvalue = mean(abs(est) >= abs(obs))
  ) 

colnames(ri_table) <- c("Survey Wave", "Observed Estimate", 
                        "0.025th Quantile", "0.975th Quantile", "RI P-value")
kable(ri_table, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:attrit_ri_sharp}Randomization Inference (RI) for the sharp null hypothesis of no effect on response for any unit") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  footnote(general = c("Quantiles of null distribution and RI P-values from 10,000 permutations of the experimental design."))  

# Test the sharp null (no covariates): no effect of Z on R for any unit.
# We use the ri2 package for convenience (https://github.com/acoppock/ri2)
library(ri2)
set.seed(1)

# RI for first post-treatment wave
ri_sharp_t1 <- 
  conduct_ri(
    R_t1 ~ Z, 
    # (Tilman, January 2023:) remove declaration as it depends on the 
    # redacted block variable, see above)
    #declaration = declaration,
    sharp_hypothesis = 0,
    data = full_df,
    permutation_matrix = perm_mat,
    progress_bar = TRUE
  )

# Recode non-response from NA to 0 in order to avoid ri2 warning
full_df$R_t2[is.na(full_df$R_t2)] <- 0 

# RI for second post-treatment wave
ri_sharp_t2 <- 
  conduct_ri(
    R_t2 ~ Z, 
    # (Tilman, January 2023:) remove declaration as it depends on the 
    # redacted block variable, see above)
    # declaration = declaration,
    sharp_hypothesis = 0,
    data = full_df,
    permutation_matrix = perm_mat,
    progress_bar = TRUE
  )

##------Table S12: Randomization Inference (RI) for differential attrition----

# Read in dataset of 10000 simulated test statistics under the null.
# Note that the district level variable used for blocking is not included
# in the public release dataset. Hence the dataset of simulated test 
# statistics used to assess differential attrition in the SM for the paper is 
# provided instead. 
null_dist_df <- 
  read_rds("ri_attrit_null_dist.rds")

ri_table <- 
  null_dist_df %>%
  group_by(wave) %>%
  summarise(obs = mean(obs), 
            ci_lower = quantile(est, probs = 0.025),
            ci_upper = quantile(est, probs = 0.975),
            ri_pvalue = mean(abs(est) >= abs(obs))
  ) 

colnames(ri_table) <- c("Survey Wave", "Observed F-Statistic", 
                        "0.025th Quantile", "0.975th Quantile", "RI P-value")
kable(ri_table, "latex", booktabs = TRUE, digits = 2,
      caption = "\\label{tab:attrit_ri_covs}Randomization Inference (RI) for differential attrition") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  footnote(general = c("Quantiles of null distribution and RI P-values from 10,000 permutations of the experimental design."))  


##------Fig. S10, Fig. 1-3 in manuscript, Table S13-S16 --------


# Pre-registered covariates for adjustment 
covariates <- c("dem_age", "dem_female", "dem_race4", "dem_pid7", 
                "dem_inc_high", "contact_arrest_t0", "contact_unfair_t0",
                "contact_f2f_t0", "spanish")

# Code below for replicating in-text results presented in manuscript.

# Print sample sizes for text: individuals
cops_df %>% 
  filter(R_t0 == 1) %>% 
  tally()

# Print sample sizes for text: households
cops_df %>% 
  filter(R_t0 == 1) %>% 
  distinct(hh_id, .keep_all = TRUE) %>% 
  tally()

# Count of individuals by treatment status:
cops_df %>% 
  filter(R_t0 == 1) %>% 
  group_by(Z)  %>% 
  tally()

# Count of households by treatment status:
cops_df %>% 
  filter(R_t0 == 1) %>% 
  distinct(hh_id, .keep_all = TRUE) %>% 
  group_by(Z)  %>% 
  tally()

# Count of individuals reached at the door. Z is treatment assignment and
# D is treatment receipt. 
cops_df %>% 
  filter(R_t0 == 1) %>% 
  group_by(Z, D)  %>% 
  tally()

# Count of individuals completing each followup survey:
cops_df %>% tally(R_t0) # T0 survey
cops_df %>% tally(R_t1) # T1 survey 
cops_df %>% tally(R_t2) # T2 survey 


# Estimate effects on index of primary DVs reported in text,
X <- c(paste0("primary_dvs_index_t0"), covariates)


# ITT in T1
lm_lin(primary_dvs_index_t1 ~ Z, 
       covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
       clusters = hh_id, se_type = "stata", alpha = 0.08,
       data = cops_df)   %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3) %>%
  filter(term %in% c("(Intercept)", "Z"))

# ATT in T1
iv_robust(as.formula(paste("primary_dvs_index_t1", "~ D + ", paste(X, collapse = "+"),
                           "| Z + ", paste(X, collapse = "+"))),
          clusters = hh_id, se_type = "stata", alpha = 0.08,
          data = cops_df) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3) %>%
  filter(term %in% c("(Intercept)", "D"))

# ITT in T2
lm_lin(primary_dvs_index_t2 ~ Z, 
       covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
       clusters = hh_id, se_type = "stata", alpha = 0.08,
       data = cops_df)   %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3) %>%
  filter(term %in% c("(Intercept)", "Z"))

# ATT in T2
iv_robust(as.formula(paste("primary_dvs_index_t2", "~ D + ", paste(X, collapse = "+"),
                           "| Z + ", paste(X, collapse = "+"))),
          clusters = hh_id, se_type = "stata", alpha = 0.08,
          data = cops_df) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3) %>%
  filter(term %in% c("(Intercept)", "D"))


# What are baseline means by race/ethnicity?
cops_df %>% 
  filter(R_t1 == 1) %>% 
  group_by(dem_race4) %>% 
  summarise(mean(primary_dvs_index_t0))

X <- c(paste0("therm_police_t0"), covariates)

# What was impact on police feeling thermometer?
iv_robust(as.formula(paste("therm_police_t1", "~ D + ", paste(X, collapse = "+"),
                           "| Z + ", paste(X, collapse = "+"))),
          clusters = hh_id, se_type = "stata", alpha = 0.08,
          data = cops_df) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3) %>%
  filter(term %in% c("(Intercept)", "D"))

# Baseline means
summary(cops_df$therm_police_t0)
summary(cops_df$therm_police_t1)


##------Fig. S10: Distribution of Primary Outcome Measures in Baseline Survey----

baseline_plot <- 
  cops_df %>%
  filter(R_t0 == 1) %>% 
  mutate(Z = factor(ifelse(Z == 1, "Treatment", "Control"), 
                    levels = c("Control", "Treatment"))) %>%
  dplyr::select(Z, perform_index_t0, legit_index_t0, coop_index_t0, 
                comply_index_t0) %>% 
  gather(Outcome, Y, perform_index_t0:comply_index_t0) %>%
  mutate(Outcome = factor(Outcome),
         Outcome = recode_factor(Outcome, 
                                 perform_index_t0 = "Performance",
                                 legit_index_t0 = "Legitimacy",
                                 coop_index_t0 = "Cooperation",
                                 comply_index_t0 = "Compliance")) %>%
  ggplot(., aes(y = Y, x = Z, fill = Z, color = Z, group = Z)) + 
  geom_quasirandom(shape = 21, size = 2, alpha = 0.25, varwidth = TRUE,
                   bandwidth = 1, show.legend = FALSE, groupOnX = TRUE) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, varwidth = TRUE, notch = TRUE,
               color = "black") + 
  scale_color_manual(name = "", values = c("gray30","#027ce7")) + 
  scale_fill_manual(name = "", values = c("gray30","#027ce7")) + 
  facet_wrap(~ Outcome) + 
  theme_cops() +
  labs(x = "", y = "", caption = "") 

# Print
baseline_plot

ggsave(filename = paste0("baseline_plot.png"), baseline_plot, height = 10, 
       width = 9, device = "png")

##------Estimate effects on Primary Outcome Measures (Fig. 1, Table S13)----
# First Wave
gg_itt <- list()
gg_att <- list()

for(i in 1:length(primary_dvs)) {
  
  Y <- paste0(primary_dvs[i], "_t1")
  X <- c(paste0(primary_dvs[i], "_t0"), covariates)
  
  gg_itt[[i]] <- 
    lm_lin(as.formula(paste0(Y, " ~ Z")), 
           covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
           clusters = hh_id, se_type = "stata", alpha = 0.08,
           data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
  gg_att[[i]] <- 
    iv_robust(as.formula(paste(Y, "~ D + ", paste(X, collapse = "+"),
                               "| Z + ", paste(X, collapse = "+"))),
              clusters = hh_id, se_type = "stata", alpha = 0.08,
              data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
}

est_primary_t1 <- 
  bind_rows(
    (gg_itt %>% bind_rows()),
    (gg_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = factor(outcome, levels = rev(paste0(primary_dvs, "_t1")), 
                          labels = rev(c("Cooperation", "Compliance",
                                         "Legitimacy", "Performance", 
                                         "Index of Primary DVs"))),
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T1 Survey")

# Second Wave
gg_itt <- list()
gg_att <- list()

for(i in 1:length(primary_dvs)) {
  
  Y <- paste0(primary_dvs[i], "_t2")
  X <- c(paste0(primary_dvs[i], "_t0"), covariates)
  
  gg_itt[[i]] <- 
    lm_lin(as.formula(paste0(Y, " ~ Z")), 
           covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
           clusters = hh_id, se_type = "stata", alpha = 0.08,
           data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
  gg_att[[i]] <- 
    iv_robust(as.formula(paste(Y, "~ D + ", paste(X, collapse = "+"),
                               "| Z + ", paste(X, collapse = "+"))),
              clusters = hh_id, se_type = "stata", alpha = 0.08,
              data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
}

est_primary_t2 <- 
  bind_rows(
    (gg_itt %>% bind_rows()),
    (gg_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = factor(outcome, levels = rev(paste0(primary_dvs, "_t2")), 
                          labels = rev(c("Cooperation", "Compliance",
                                         "Legitimacy", "Performance", 
                                         "Index of Primary DVs"))),
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T2 Survey")

# Combine for presentation in Tables/Figures
est_primary <- 
  bind_rows(est_primary_t1, est_primary_t2)

# Combine results into dataframe 
plot_primary <- 
  bind_rows(est_primary_t1, est_primary_t2) %>%
  mutate(outcome = factor(outcome, levels = c("Index of Primary DVs",
                                              "Legitimacy",
                                              "Performance",
                                              "Cooperation",
                                              "Compliance")), 
         estimator = factor(estimator, levels = c("OLS", "IV"),
                            labels = c("Intent-to-Treat Effect", 
                                       "Treatment Effect on Treated")),
         Wave = factor(Wave,  
                       levels = c("T1 Survey", "T2 Survey"),
                       labels = c("\n 3 + days", "\n 21 + days"))) 

# Create the plot for Fig. 2 in the manuscript. 
primary_plot_component <- 
  plot_primary %>%
  filter(outcome != "Index of Primary DVs") %>%
  ggplot(aes(y = estimate, x = Wave, colour = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, lwd = I(4/12), alpha = I(7/12), colour = "black") + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                 size = 1.25, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                size = 1.25, width = 0, show.legend = FALSE,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(name = "", values = c("darkgoldenrod1", "navyblue")) +
  #scale_y_continuous(limits = c(-0.1, 0.6)) + 
  facet_wrap(~ outcome) + 
  labs(color = "", shape = "", x = "", 
       y = "Effect of Community Policing Treatment",
       title = "") +
  coord_capped_cart(left = 'none', 
                    bottom = brackets_horizontal(direction = "up", 
                                                 length = unit(0.10, "npc"))) +
  theme_cops() 


# Print 
primary_plot_component

ggsave(filename = paste0("component_dvs_plot.png"),
       primary_plot_component, height = 8, width = 9, device = "png")

# Table of output:
primary_table <- 
  est_primary %>%
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value, 
                            alpha = 0.08, digits = 2),
         outcome = factor(outcome, levels = c("Index of Primary DVs", 
                                              "Legitimacy",
                                              "Cooperation", 
                                              "Compliance", 
                                              "Performance"))) %>%
  dplyr::select(outcome, entry, estimator, Wave) %>%
  spread(outcome, entry) %>%
  arrange(Wave) %>%
  dplyr::select(estimator, everything(), -Wave) %>%
  mutate(estimator = ifelse(estimator == "OLS", "ITT", "ATT"))

colnames(primary_table)[1] <- ""
kable(primary_table, "latex", booktabs = TRUE,
      caption = "\\label{tab:est_primary}Estimated treatment effects on primary outcome measures") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position") %>%
  kableExtra::group_rows("T1 Survey:", 1, 2) %>%
  kableExtra::group_rows("T2 Survey:", 3, 4) %>%
  footnote(general = c("Covariate-adjusted point estimates with standard errors in parentheses.", "* denotes statistical significance at pre-registered level"))



##------Estimate effects among Race/Ethnicity Sub-groups (Fig. 2, Table S14) -----
race_itt <- list()
race_att <- list()

race <- c("White", "Black", "Hispanic", "Other")

# Note that messages re: "coefficient  not defined because the design matrix is 
# rank deficient" is due to Hispanic respondents being the only subgroup w/
# variation on the spanish indicator.
for(i in 1:length(race)){
  race_itt[[i]] <- 
    lm_lin(primary_dvs_index_t1 ~ Z, covariates = ~ primary_dvs_index_t0 + 
             dem_age + dem_female + dem_pid7 + dem_inc_high + 
             contact_arrest_t0 + contact_unfair_t0 + contact_f2f_t0 +
             spanish, clusters = hh_id, se_type = "stata", 
           data = cops_df, subset = dem_race4 == race[i]) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           race = race[i])
  
  race_att[[i]] <-
    iv_robust(primary_dvs_index_t1 ~ D + primary_dvs_index_t0 + dem_age + 
                dem_female + dem_pid7 + dem_inc_high + contact_arrest_t0 + 
                contact_unfair_t0 + contact_f2f_t0 + spanish | Z + 
                primary_dvs_index_t0 + dem_age + dem_female + dem_pid7 +
                dem_inc_high + contact_arrest_t0 + contact_unfair_t0 + 
                contact_f2f_t0 + spanish, clusters = hh_id, se_type = "stata", 
              data = cops_df, subset = dem_race4 == race[i])  %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           race = race[i])
}

est_race_t1 <- 
  bind_rows(
    (race_itt %>% bind_rows()),
    (race_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = "Index of Primary DVs",
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T1 Survey")

# Second Wave
race_itt <- list()
race_att <- list()

race <- c("White", "Black", "Hispanic", "Other")

for(i in 1:length(race)){
  race_itt[[i]] <- 
    lm_robust(primary_dvs_index_t2 ~ Z + primary_dvs_index_t0 + 
                dem_age + dem_female + dem_pid7 + dem_inc_high + 
                contact_arrest_t0 + contact_unfair_t0 + contact_f2f_t0 +
                spanish, clusters = hh_id, se_type = "stata", 
              data = cops_df, subset = dem_race4 == race[i]) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           race = race[i])
  
  race_att[[i]] <-
    iv_robust(primary_dvs_index_t2 ~ D + primary_dvs_index_t0 + dem_age + 
                dem_female + dem_pid7 + dem_inc_high + contact_arrest_t0 + 
                contact_unfair_t0 + contact_f2f_t0 + spanish | Z + 
                primary_dvs_index_t0 + dem_age + dem_female + dem_pid7 +
                dem_inc_high + contact_arrest_t0 + contact_unfair_t0 + 
                contact_f2f_t0 + spanish, clusters = hh_id, se_type = "stata", 
              data = cops_df, subset = dem_race4 == race[i])  %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           race = race[i])
}

est_race_t2 <- 
  bind_rows(
    (race_itt %>% bind_rows()),
    (race_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = "Index of Primary DVs",
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T2 Survey")


# Combine estimates from each wave
est_race <- 
  bind_rows(est_race_t1, est_race_t2) 


# Combine estimates from each wave
plot_race <- 
  bind_rows(est_race_t1, est_race_t2) %>%
  mutate(estimator = factor(estimator, levels = c("OLS", "IV"),
                            labels = c("Intent-to-Treat Effect", 
                                       "Treatment Effect on the Treated")),
         Wave = factor(Wave,  
                       levels = c("T1 Survey", "T2 Survey"),
                       labels = c("\n 3 + days", "\n 21 + days")),
         race = factor(race, levels = c("Black", "Hispanic", "White", "Other")))

# Create Fig 2. in manuscript. 
race_plot <- 
  plot_race %>%
  ggplot(aes(y = estimate, x = Wave, colour = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, lwd = I(4/12), alpha = I(7/12), colour = "black") + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                 size = 1.25, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                size = 1.25, width = 0, show.legend = FALSE,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(name = "", values = c("darkgoldenrod1", "navyblue")) +
  facet_wrap(~ race)  +
  labs(color = "", shape = "", x = "", 
       y = "Effect of Community Policing Treatment",
       title = "") +
  coord_capped_cart(left = 'none', 
                    bottom = brackets_horizontal(direction = "up", 
                                                 length = unit(0.10, "npc"))) +
  theme_cops()


ggsave(filename = paste0("race_plot.png"),
       race_plot, height = 8, width = 9, device = "png")

# Generate table S14: 
race_table <- 
  est_race %>%
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value, 
                            alpha = 0.05, digits = 2),
         outcome = factor(race, 
                          levels = c("White", "Black", "Hispanic", "Other"))) %>%
  dplyr::select(outcome, entry, estimator, Wave) %>%
  spread(outcome, entry) %>%
  arrange(Wave) %>%
  dplyr::select(estimator, everything(), -Wave) %>%
  mutate(estimator = ifelse(estimator == "OLS", "ITT", "ATT"))


# Are these still "quantiles" when we take missingness into account? Pretty 
# much, but get the exact percentages for reporting in table. 
p_t1 <- 
  sort(
    round(
      table(cops_df$dem_race4[cops_df$R_t1 == 1])/sum(cops_df$R_t1, na.rm = T), 2
    )
  )

p_t2 <- 
  sort(
    round(
      table(cops_df$dem_race4[cops_df$R_t2 == 1])/sum(cops_df$R_t2, na.rm = T), 2
    )
  )


race_table <- 
  rbind(race_table[1:2,], 
        c("Pct. Sample", 
          sprintf(p_t1[c("White", "Black", "Hispanic", "Other")], fmt = "%#.2f")), 
        race_table[3:4,], 
        c("Pct. Sample", 
          sprintf(p_t2[c("White", "Black", "Hispanic", "Other")], fmt = "%#.2f"))
  )

colnames(race_table)[1] <- ""
kable(race_table , "latex", booktabs = TRUE, row.names = FALSE,
      caption = "\\label{tab:racecut}Estimated treatment effects by Race/Ethnicity") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position",
                font_size = 10) %>%
  kableExtra::group_rows("T1 Survey:", 1, 3) %>%
  kableExtra::group_rows("T2 Survey:", 4, 6) %>%
  footnote(general = c("Covariate-adjusted point estimates with standard errors in parentheses.", "* denotes statistical significance at 0.05."))


##------Estimate effects on Secondary Outcomes (Fig. 3, Table S15)----
secondary_dvs <- c("csi_index", "therm_police", "stereotype_police_index",
                   "policy_bodycam", "policy_increase", "trustgov_local_index")

# First Wave
gg_itt <- list()
gg_att <- list()

# pre-registered p-values
alphas <- c(rep(0.08, 3), 0.05, 0.08, 0.08)

for(i in 1:length(secondary_dvs)) {
  
  Y <- paste0(secondary_dvs[i], "_t1")
  X <- c(paste0(secondary_dvs[i], "_t0"), covariates)
  
  gg_itt[[i]] <- 
    lm_lin(as.formula(paste0(Y, " ~ Z")), 
           covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
           clusters = hh_id, se_type = "stata", alpha = alphas[i],
           data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
  gg_att[[i]] <- 
    iv_robust(as.formula(paste(Y, "~ D + ", paste(X, collapse = "+"),
                               "| Z + ", paste(X, collapse = "+"))),
              clusters = hh_id, se_type = "stata", alpha = alphas[i],
              data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
}

est_secondary_t1 <- 
  bind_rows(
    (gg_itt %>% bind_rows()),
    (gg_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = factor(outcome, 
                          levels = paste0(secondary_dvs, "_t1"), 
                          labels = c("Confidence in Police", 
                                     "Police Feeling Thermometer",
                                     "Police Stereotypes Index",
                                     "Support Body Cameras",
                                     "Support Funding Increase",
                                     "Trust in City Government")),
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T1 Survey")

# Second Wave (note we did not measure local trust in Wave 2)
secondary_dvs <- c("csi_index", "therm_police", "stereotype_police_index",
                   "policy_bodycam", "policy_increase")

gg_itt <- list()
gg_att <- list()

# Update pre-registered pvalues. Note that this is necessary because the 
# trust in government question was not asked in T2 survey wave. 
alphas <- c(rep(0.08, 3), 0.05, 0.08)

for(i in 1:length(secondary_dvs)) {
  
  Y <- paste0(secondary_dvs[i], "_t2")
  X <- c(paste0(secondary_dvs[i], "_t0"), covariates)
  
  gg_itt[[i]] <- 
    lm_lin(as.formula(paste0(Y, " ~ Z")), 
           covariates = as.formula(paste("~", paste(X, collapse = "+"))), 
           clusters = hh_id, se_type = "stata", alpha = alphas[i],
           data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
  gg_att[[i]] <- 
    iv_robust(as.formula(paste(Y, "~ D + ", paste(X, collapse = "+"),
                               "| Z + ", paste(X, collapse = "+"))),
              clusters = hh_id, se_type = "stata", alpha = alphas[i],
              data = cops_df) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"))
  
}

est_secondary_t2 <- 
  bind_rows(
    (gg_itt %>% bind_rows()),
    (gg_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = factor(outcome, 
                          levels = paste0(secondary_dvs, "_t2"), 
                          labels = c("Confidence in Police", 
                                     "Police Feeling Thermometer",
                                     "Police Stereotypes Index",
                                     "Support Body Cameras",
                                     "Support Funding Increase")),
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T2 Survey")

# Combine for presentation in Tables/Figures
est_secondary <- 
  bind_rows(est_secondary_t1, est_secondary_t2) %>%
  mutate(outcome = factor(outcome, 
                          levels = c("Confidence in Police", 
                                     "Police Feeling Thermometer",
                                     "Police Stereotypes Index",
                                     "Support Body Cameras",
                                     "Support Funding Increase",
                                     "Trust in City Government"),
                          labels = c("Confidence in Police", 
                                     "Police Feeling Thermometer",
                                     "Negative Beliefs about Police",
                                     "Support Body Cameras",
                                     "Increase Police by 10%",
                                     "Trust in City Gov.")),
         estimator = factor(estimator, levels = c("OLS", "IV"),
                            labels = c("Intent-to-Treat Effect", 
                                       "Treatment Effect on the Treated")),
         Wave = factor(Wave,  
                       levels = c("T1 Survey", "T2 Survey"),
                       labels = c("\n 3 + days", "\n 21 + days"))) 


#  Create Fig 3. in manuscript. 
secondary_plot <- 
  est_secondary %>%
  ggplot(aes(y = estimate, x = Wave, colour = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, lwd = I(4/12), alpha = I(7/12), colour = "black") + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                 size = 1.25, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                size = 1.25, width = 0, show.legend = FALSE,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) + 
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(name = "", values = c("darkgoldenrod1", "navyblue")) +
  facet_wrap(~ outcome)  +
  labs(color = "", shape = "", x = "", 
       y = "Effect of Community Policing Treatment",
       title = "") +
  coord_capped_cart(left = 'none', 
                    bottom = brackets_horizontal(direction = "up", 
                                                 length = unit(0.10, "npc"))) +
  theme_cops()

# print
secondary_plot

ggsave(filename = paste0("secondary_plot.pdf"),
       secondary_plot, height = 9, width = 11, device = "pdf")

# Generate table: 
secondary_table <- 
  est_secondary %>%
  mutate(alpha = ifelse(outcome == "Support Body Cameras", 0.05, 0.08)) %>%
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value, 
                            alpha = alpha, digits = 2),
         outcome = factor(outcome, levels = c("Confidence in Police", 
                                              "Police Feeling Thermometer",
                                              "Negative Beliefs about Police",
                                              "Support Body Cameras",
                                              "Increase Police by 10%",
                                              "Trust in City Gov."))) %>%
  dplyr::select(outcome, entry, estimator, Wave) %>%
  spread(outcome, entry) %>%
  arrange(Wave) %>%
  dplyr::select(estimator, everything(), -Wave) %>%
  mutate(estimator = ifelse(estimator == "OLS", "ITT", "ATT"))

colnames(secondary_table)[1] <- ""
kable(secondary_table, "latex", booktabs = TRUE,
      caption = "\\label{tab:est_secondary}Estimated treatment effects on secondary outcome measures") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position",
                font_size = 10) %>%
  kableExtra::group_rows("T1 Survey:", 1, 2) %>%
  kableExtra::group_rows("T2 Survey:", 3, 4) %>%
  footnote(general = c("Covariate-adjusted point estimates with standard errors in parentheses.", "* denotes statistical significance at pre-registered level."))

##------Estimate effects among Quantiles of Baseline Support (Table S16)----

# Estimate covariate-adjusted ITT/ATT by quantiles of baseline support
cops_df <- 
  cops_df %>%
  mutate(quantcut = cut(primary_dvs_index_t0, 
                        quantile(primary_dvs_index_t0, na.rm = T)))


# Note that messages re: "coefficient not defined because the design matrix is 
# rank deficient" is due to no spanish speaking respondents falling in the 
# bottom 25% of the distribution w.r.t. attitudes toward police. 

quant_itt <- list()
quant_att <- list()

quant <- levels(cops_df$quantcut)

for(i in 1:length(quant)){
  quant_itt[[i]] <- 
    lm_lin(primary_dvs_index_t1 ~ Z, covariates = ~ dem_race4 +
             dem_age + dem_female + dem_pid7 + dem_inc_high + 
             contact_arrest_t0 + contact_unfair_t0 + contact_f2f_t0 +
             spanish, clusters = hh_id, se_type = "stata", 
           data = cops_df, subset = quantcut == quant[i]) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           quantcut = quant[i])
  
  quant_att[[i]] <-
    iv_robust(primary_dvs_index_t1 ~ D + dem_race4 + dem_age + 
                dem_female + dem_pid7 + dem_inc_high + contact_arrest_t0 + 
                contact_unfair_t0 + contact_f2f_t0 + spanish | Z + 
                dem_race4 + dem_age + dem_female + dem_pid7 +
                dem_inc_high + contact_arrest_t0 + contact_unfair_t0 + 
                contact_f2f_t0 + spanish, clusters = hh_id, se_type = "stata", 
              data = cops_df, subset = quantcut == quant[i])  %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           quantcut = quant[i])
}

est_quantcut_t1 <- 
  bind_rows(
    (quant_itt %>% bind_rows()),
    (quant_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = "Index of Primary DVs",
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T1 Survey")

# Repeat for Second Wave Index
quant_itt <- list()
quant_att <- list()

for(i in 1:length(quant)){
  quant_itt[[i]] <- 
    lm_lin(primary_dvs_index_t2 ~ Z, covariates = ~ dem_race4 +
             dem_age + dem_female + dem_pid7 + dem_inc_high + 
             contact_arrest_t0 + contact_unfair_t0 + contact_f2f_t0 +
             spanish, clusters = hh_id, se_type = "stata", 
           data = cops_df, subset = quantcut == quant[i]) %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "Z")) %>%
    mutate(estimator = "OLS",
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           quantcut = quant[i])
  
  quant_att[[i]] <-
    iv_robust(primary_dvs_index_t2 ~ D + dem_race4 + dem_age + 
                dem_female + dem_pid7 + dem_inc_high + contact_arrest_t0 + 
                contact_unfair_t0 + contact_f2f_t0 + spanish | Z + 
                dem_race4 + dem_age + dem_female + dem_pid7 +
                dem_inc_high + contact_arrest_t0 + contact_unfair_t0 + 
                contact_f2f_t0 + spanish, clusters = hh_id, se_type = "stata", 
              data = cops_df, subset = quantcut == quant[i])  %>%
    tidy() %>%
    mutate_if(is.numeric, round, 3) %>%
    filter(term %in% c("(Intercept)", "D")) %>%
    mutate(estimator = "IV", 
           term = ifelse(term == "(Intercept)", "Control", "Treatment"),
           quantcut = quant[i])
}

est_quantcut_t2 <- 
  bind_rows(
    (quant_itt %>% bind_rows()),
    (quant_att %>% bind_rows())) %>% 
  filter(term != "Control") %>%
  mutate(outcome = "Index of Primary DVs",
         estimator = factor(estimator, levels = c("OLS", "IV")),
         Wave = "T2 Survey")


# Combine estimates from each wave
est_quantcut <- 
  bind_rows(est_quantcut_t1, est_quantcut_t2)

# Generate table: 
quant_table <- 
  est_quantcut %>%
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value, 
                            alpha = 0.05, digits = 2),
         outcome = factor(quantcut, labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  dplyr::select(outcome, entry, estimator, Wave) %>%
  spread(outcome, entry) %>%
  arrange(Wave) %>%
  dplyr::select(estimator, everything(), -Wave) %>%
  mutate(estimator = ifelse(estimator == "OLS", "ITT", "ATT"))

# Are these still "quantiles" when we take missingness into account? Pretty 
# much, but get the exact percentages for reporting in table. 
q_t1 <- 
  paste(
    round(
      table(cops_df$quantcut[cops_df$R_t1 == 1])/sum(cops_df$R_t1, na.rm = T), 2
    )
  )

q_t2 <- 
  paste(
    round(
      table(cops_df$quantcut[cops_df$R_t2 == 1])/sum(cops_df$R_t2, na.rm = T), 2
    )
  )

names(q_t1) <- paste0("Q", 1:4)
names(q_t2) <- paste0("Q", 1:4)

quant_table <- 
  rbind(quant_table[1:2,], 
        c("Pct. Sample", q_t1), 
        quant_table[3:4,], 
        c("Pct. Sample", q_t2))


colnames(quant_table)[1] <- ""
kable(quant_table , "latex", booktabs = TRUE, row.names = FALSE, 
      caption = "\\label{tab:quantcut}Estimated treatment effects by Quantile of Baseline Support") %>%
  kable_styling(full_width = TRUE, latex_options = "HOLD_position",
                font_size = 10) %>%
  kableExtra::group_rows("T1 Survey:", 1, 3) %>%
  kableExtra::group_rows("T2 Survey:", 4, 6) %>%
  footnote(general = c("Covariate-adjusted point estimates with standard errors in parentheses.", "* denotes statistical significance at 0.05."))

