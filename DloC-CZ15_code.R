# Víteček is my bejb
# Data wrangling
#

# Packages ----------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", 
              "dplyr", 
              "tidyverse", 
              "psych", 
              "lavaan", 
              "readxl", 
              "plyr",
              "lavaan",
              "corrplot")
ipak(packages)

# Data --------------------------------------------------------------------

# Load Data
DLoC_Dataset <- read_excel("DLoC-CZ15_data.xlsx", 
                           sheet = 1)

# Load codebook
DLoC_Codebook <- read_excel("DLoC-CZ15_data.xlsx", 
                            sheet = 2)

# Descriptive statistics --------------------------------------------------

## Driving licence

### Driving licence for descriptive statistics
DLoC_Driving_Licence_descriptive <- DLoC_Dataset %>%
  select(Q33_1:Q33_2) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  select(Driving_Licence) %>%
  group_by(Driving_Licence) %>%
  dplyr::rename(`Driving licence?` = Driving_Licence) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,1))

### Driving licence as a variable
Driving_Licence_variable <- DLoC_Dataset %>%
  select(Q33_1:Q33_2) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  select(Driving_Licence) %>%
  dplyr::rename(`Driving licence?` = Driving_Licence)

## DLoC

### Prepare list of variables for descriptive statistics
DLoC_list <- as.matrix(DLoC_Codebook[5:34, 3])

# Create labels for factor levels
labels_DLoC <- c("Disagree very much", "Disagree quite a bit", "Disagree some",
                 "Agree a little", "Agree quite a bit", "Agree very much")

# Force to factors
DLoC <- DLoC_Dataset %>%
  select(Q22_1:Q22_30) %>%
  mutate_all(as.ordered)

# Label values
DLoC <- as.data.frame(lapply(DLoC, function(x) { revalue(x, c("0"="Disagree very much", 
                                                              "1"="Disagree quite a bit", 
                                                              "2"="Disagree some",
                                                              "3"="Agree a little", 
                                                              "4"="Agree quite a bit", 
                                                              "5"="Agree very much")) }))

DLoC <- as.data.frame(lapply(DLoC, function(x) { ordered(x, levels = c("Disagree very much",
                                                                       "Disagree quite a bit", 
                                                                       "Disagree some",
                                                                       "Agree a little", 
                                                                       "Agree quite a bit", 
                                                                       "Agree very much")) }))

# Name variables
names(DLoC) <- DLoC_list

DLoC_summary <- DLoC %>% 
  select_all() %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "Variables") %>%
  spread(names, x) %>%
  select(-Variables, -`NA's`) %>%
  mutate(RowSumsValid = rowSums(.)) %>%
  mutate_all(., funs("percent"= round((./RowSumsValid)*100,1))) %>%
  select(`Disagree very much`, `Disagree very much_percent`,
         `Disagree quite a bit`, `Disagree quite a bit_percent`,
         `Disagree some`, `Disagree some_percent`,
         `Agree a little`, `Agree a little_percent`,
         `Agree quite a bit`, `Agree quite a bit_percent`,
         `Agree very much`, `Agree very much_percent`,
         RowSumsValid)

DLoC_variables <- DLoC %>% 
  select_all() %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "Variables") %>%
  spread(names, x) %>%
  select(Variables)

DLoC_Summary_Final <- cbind(DLoC_variables, DLoC_summary)

#
# CFA, SEM
#

# Data ------------------------------------------------------------------------------------------------------------------------------------------

DLoC_CFA_Model <- DLoC_Dataset %>%
  select(id:Q22_30, Q33_1:Q33_2) %>%
  mutate(Driving_Licence = case_when(Q33_1 == 1 | Q33_2 == 1 ~ 'Driving Licence',
                                     TRUE ~ 'No driving Licence')) %>%
  select(id, 
         Driving_Licence, 
         Q4:Q22_30)

DLoC_CFA_Model[, 2:36] <- as.data.frame(lapply(DLoC_CFA_Model[, 2:36], 
                                               ordered))

lavCor <- names("DLoC_E_1")

DLoC_CFA_Model <- na.omit(DLoC_CFA_Model)

# Descriptives ----------------------------------------------------------------------------------------------------------------------------------

# Model -----------------------------------------------------------------------------------------------------------------------------------------
## CFA


# Two-factor model --------------------------------------------------------

modelDLoC = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25
IDLoC =~  Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30'

modelDLoC_orthogonal = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25
IDLoC =~  Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30

EDLoC ~~ 0*IDLoC' # orthogonal factors


DLoC_No_Grouping = cfa(modelDLoC, DLoC_CFA_Model, estimator = "WLSMV")
summary(DLoC_No_Grouping, fit.measures = TRUE, standardized = TRUE)


## Estimating factor scores

## Modification indices

modind = modificationIndices(DLoC_No_Grouping, sort = TRUE)
modind[order(-modind$mi), ]

head(modind [order(modind$mi, decreasing=TRUE), ], 10)

# McDonald's omega
DLoC_CFA_Omega <- DLoC_CFA_Model %>% dplyr::select(Q22_1:Q22_30)

MBESS::ci.reliability(data=DLoC_CFA_Omega, 
                      type="categorical", 
                      conf.level = 0.95, 
                      interval.type="bca",
                      B=1000)


# One-factor model --------------------------------------------------------

modelDLoC_One_Factor = '
EDLoC =~  Q22_1 + Q22_2 + Q22_3 + Q22_4 + Q22_5 +
          Q22_11 + Q22_12 + Q22_13 + Q22_14 + Q22_15 +
          Q22_21 + Q22_22 + Q22_23 + Q22_24 + Q22_25 + 
          Q22_6 + Q22_7 + Q22_8 + Q22_9 + Q22_10 +
          Q22_16 + Q22_17 + Q22_18 + Q22_19 + Q22_20 +
          Q22_26 + Q22_27 + Q22_28 + Q22_29 + Q22_30'

DLoC_One_Factor = cfa(modelDLoC_One_Factor, DLoC_CFA_Model, estimator = "WLSMV")
summary(DLoC_One_Factor, fit.measures = TRUE, standardized = TRUE)


# Corrplot ----------------------------------------------------------------

lavCor <- lavCor(DLoC_No_Grouping, ordered = TRUE, group = NULL,
                 output = "cor")

# lavCorTable <- as.data.frame(lavCor) %>%
#   select(Q22_1:Q22_5, Q22_11:Q22_15, Q22_21:Q22_25) %>%
#   filter_all(., any_vars(. > 0.5 & . !=1))

rownames(lavCor) <- c("DE_01","DE_02","DE_03","DE_04","DE_05",
                      "DE_06","DE_07","DE_08","DE_09","DE_10",
                      "DE_11","DE_12","DE_13","DE_14","DE_15",
                      "DI_01","DI_02","DI_03","DI_04","DI_05",
                      "DI_06","DI_07","DI_08","DI_09","DI_10",
                      "DI_11","DI_12","DI_13","DI_14","DI_15")

colnames(lavCor) <- c("DE_01","DE_02","DE_03","DE_04","DE_05",
                      "DE_06","DE_07","DE_08","DE_09","DE_10",
                      "DE_11","DE_12","DE_13","DE_14","DE_15",
                      "DI_01","DI_02","DI_03","DI_04","DI_05",
                      "DI_06","DI_07","DI_08","DI_09","DI_10",
                      "DI_11","DI_12","DI_13","DI_14","DI_15")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(lavCor, 
                   col=col(200), 
                   method="color", 
                   addCoef.col = "black", 
                   diag=FALSE, type="upper", 
                   tl.col="black", 
                   tl.srt=45, 
                   tl.cex = 0.8, 
                   number.cex= 0.8, 
                   order="alphabet",
                   addshade = "all",
                   addrect = 4
)
