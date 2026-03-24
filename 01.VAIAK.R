#rm(list=ls()) 

library(tidyverse)
library(ggdist)

#-----------------1. Read raw data-----------------

VAIAK = read.csv(file = "C:/Users/matte/OneDrive/Desktop/VAIAK/Italian VAIAK/Data italian VAIAK/VAIAK+ITA_21+dicembre+2025_14.55.csv", 
                 header=T,sep=",")

VAIAK_1 = read.csv(file = "C:/Users/matte/OneDrive/Desktop/VAIAK/Italian VAIAK/Data italian VAIAK/VAIAK+ITA+-+Copia_21+dicembre+2025_14.57.csv", 
                   header=T,sep=",")

VAIAK = bind_rows(VAIAK, VAIAK_1)

colnames(VAIAK)[ncol(VAIAK)] = "Participant_ID"

rm(VAIAK_1)

#-----------------2. Clean data-----------------
#    eliminate noisy rows and columns, fix age, gender, art history and art job
#    ensure that Part A, B and C are numeric


VAIAK = VAIAK %>%
  mutate(n_na = rowSums(is.na(.))) %>%          # order based on NA
  arrange(n_na) %>%
  dplyr::select(-n_na) %>%
  distinct(Participant_ID,.keep_all = TRUE) %>%                # To remove same ID
  
  slice(-c(1:2))  %>%                   # drop first row
  dplyr::select(-c(1:17)) %>%                # drop first 17 columns
  
  mutate(
    D_Expertise = factor(case_when(
      D4_ArtHist == 1 & D5_ArtJob == 1 ~ "Lay person",
      D5_ArtJob == 2 | D4_ArtHist %in% c(2,3)  ~ "Expert",
      TRUE     ~ NA_character_))) %>%
  
  mutate(
    D1_Age     = as.numeric(D1_Age),       # age as numeric      
    D2_Gender      = factor(case_when(              # gender
      D2_Gender == 1 ~ "Male", 
      D2_Gender == 2 ~ "Female", 
      D2_Gender == 3 ~ "Other", 
      TRUE     ~ NA_character_)),                    
    D4_ArtHist = factor(case_when(                  # art history
      D4_ArtHist == 1 ~ "Lay person", 
      D4_ArtHist == 2 ~ "Art student", 
      D4_ArtHist == 3 ~ "Art graduate",
      TRUE     ~ NA_character_)), 
    D5_ArtJob = factor(case_when(                    # art job
      D5_ArtJob == 1 ~ "Lay person", 
      D5_ArtJob == 2 ~ "Art worker",
      TRUE     ~ NA_character_))) %>%
  
  mutate(
    across(starts_with("AI"), as.numeric),              # Part A as numeric
    across(starts_with("B"), as.numeric),               # Part B as numeric
    across(starts_with("C") & ends_with("B"), as.numeric),                  # Part C as numeric 
    
    Participant_ID = as.numeric(Participant_ID)) %>% 
  
  mutate(
    Language = factor("Italian"))  %>% 

  filter(Participant_ID != "865964") %>%  # Spanish speaker
  filter(!is.na(D_Expertise))  # NA expertise

VAIAK$D_Expertise

#-----------------3. Score part B-----------------
# Give 1 to the correct answers of Part B


VAIAK = VAIAK %>%
  mutate(
    B1_Numeric = case_when(
      B1 == 3  ~ 1, TRUE ~ 0), 
    B2_Numeric = case_when(
      B2 == 3  ~ 1, TRUE ~ 0), 
    B3_Numeric = case_when(
      B3 == 2  ~ 1, TRUE ~ 0), 
    B4_Numeric = case_when(
      B4 == 2  ~ 1, TRUE ~ 0), 
    B5_Numeric = case_when(
      B5 == 3  ~ 1, TRUE ~ 0), 
    B6_Numeric = case_when(
      B6 == 1  ~ 1, TRUE ~ 0)
  )

#-----------------4. Score Part C-----------------
# Give 1 to the correct answers of Part C 

VAIAK = VAIAK %>%
  mutate(
    C1_1_Numeric = ifelse(str_detect(C1_1,regex("M.[ounc][nchk]", ignore_case=TRUE))==TRUE |
                            str_detect(C1_2,regex("M.[ounc][nchk]", ignore_case=TRUE))==TRUE, 1,  0),
    C1_2_Numeric = ifelse(str_detect(C1_2, regex("E\\s?[xps][pre][res].", ignore_case=TRUE))==TRUE &
                            str_detect(C1_2, regex("^Pre", ignore_case=TRUE))==FALSE &
                            str_detect(C1_2, regex("Post", ignore_case=TRUE))==FALSE &
                            str_detect(C1_2, regex("Proto", ignore_case=TRUE))==FALSE &
                            str_detect(C1_2, regex("A[bs]tr.", ignore_case=TRUE))==FALSE,  1,  0),
    
    
    C2_1_Numeric = ifelse(str_detect(C2_1, regex("Gi?ott", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("Gi?ott", ignore_case=TRUE))==TRUE, 1,  0), 
    C2_2_Numeric = ifelse(str_detect(C2_2, regex("Pre\\s?ri[na][inaesc].", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("Proto\\s?ri[na][inaesc].", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("Got.", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("M[e][td][edi].", ignore_case=TRUE))==TRUE |  
                            str_detect(C2_2, regex("trec.", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("1?300?", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("XIV", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("prerin", ignore_case=TRUE))==TRUE |
                            str_detect(C2_2, regex("precurs", ignore_case=TRUE)) == TRUE, 1,  0),
    
    
    C3_1_Numeric = ifelse(str_detect(C3_1, regex("Van\\s?g", ignore_case=TRUE))==TRUE |
                            str_detect(C3_1, regex("Go[ghc]", ignore_case=TRUE))==TRUE |
                            str_detect(C3_2, regex("Van\\s?g", ignore_case=TRUE))==TRUE |
                            str_detect(C3_2, regex("Go[ghc]", ignore_case=TRUE))==TRUE,  1,  0), 
    C3_2_Numeric = ifelse(str_detect(C3_2, regex("Post\\simpr.", ignore_case=TRUE))==TRUE |
                            str_detect(C3_2, regex("Post\\s-\\simpr", ignore_case=TRUE))==TRUE,  1,  0), 
    
    
    C4_1_Numeric = ifelse(str_detect(C4_1, regex("K[li][ilmn][mnt]", ignore_case=TRUE))==TRUE |
                            str_detect(C4_2, regex("K[li][ilmn][mnt]", ignore_case=TRUE))==TRUE,  1,  0), 
    C4_2_Numeric = ifelse(str_detect(C4_2, regex("Jugen.", ignore_case=TRUE))==TRUE |
                            str_detect(C4_2,regex("S.[mn]b[ol].", ignore_case=TRUE))==TRUE|
                            str_detect(C4_2,regex("Art N.", ignore_case=TRUE))==TRUE |
                            str_detect(C4_2,regex("nouveau", ignore_case=TRUE))==TRUE |
                            str_detect(C4_2,regex("S[eu][cs][ces].", ignore_case=TRUE))==TRUE |
                            str_detect(C4_2,regex("Lib[er].", ignore_case=TRUE))==TRUE | 
                            str_detect(C4_2,regex("Wiener s[eu][zc]", ignore_case=TRUE))==TRUE,  1,  0),
    
    
    C5_1_Numeric = ifelse(str_detect(C5_1, regex("Mi[ck].", ignore_case=TRUE))==TRUE | 
                            str_detect(C5_1, regex("Bu?onar", ignore_case=TRUE))==TRUE |
                            str_detect(C5_2, regex("Mi[ck].", ignore_case=TRUE))==TRUE | 
                            str_detect(C5_2, regex("Bu?onar", ignore_case=TRUE))==TRUE, 1,  0),  
    C5_2_Numeric = ifelse(str_detect(C5_2, regex("R[ie]na[snuce]", ignore_case=TRUE))==TRUE |
                            str_detect(C5_2, regex("Manier", ignore_case=TRUE))==TRUE,  1,  0), 
    
    
    C6_1_Numeric = ifelse(str_detect(C6_1, regex("Rub[eèé]", ignore_case=TRUE))==TRUE |
                            str_detect(C6_2, regex("Rub[eèé]", ignore_case=TRUE))==TRUE,  1,  0), 
    C6_2_Numeric = ifelse(str_detect(C6_2, regex("Barr?o.", ignore_case=TRUE))==TRUE |
                            str_detect(C6_2, regex("Fi?am.", ignore_case=TRUE))==TRUE,  1,  0), 
    
    
    C7_1_Numeric = ifelse(str_detect(C7_1, regex("R[e?o]n[oairu].", ignore_case=TRUE))==TRUE |
                            str_detect(C7_2, regex("R[e?o]n[oairu].", ignore_case=TRUE))==TRUE ,  1,  0), 
    C7_2_Numeric = ifelse(str_detect(C7_2, regex("I[mn]p[ert].", ignore_case=TRUE))==TRUE &
                            str_detect(C7_2, regex("Post.", ignore_case=TRUE))==FALSE,  1,  0), 
    
    
    C8_1_Numeric = ifelse(str_detect(C8_1, regex("D[?ahe]h?l.", ignore_case=TRUE))==TRUE |
                            str_detect(C8_2, regex("D[?ahe]h?l.", ignore_case=TRUE))==TRUE |
                            str_detect(C8_1, regex("Salvad.", ignore_case=TRUE))==TRUE, 1,  0), 
    C8_2_Numeric = ifelse(str_detect(C8_2, regex("S[ui][rue][reai].", ignore_case=TRUE))==TRUE,  1,  0),
    
    
    C9_1_Numeric = ifelse(str_detect(C9_1, regex("D[uo]\\s?[cs][hcau][hau].", ignore_case=TRUE))==TRUE |
                            str_detect(C9_2, regex("D[uo]\\s?[cs][hcau][hau].", ignore_case=TRUE))==TRUE, 1,  0), 
    C9_2_Numeric = ifelse(str_detect(C9_2, regex("[ck]on[ck]et.", ignore_case=TRUE))==TRUE |
                            str_detect(C9_2,regex("Dad.", ignore_case=TRUE))==TRUE |
                            str_detect(C9_2,regex("Da[:blank:]da", ignore_case=TRUE))==TRUE |
                            str_detect(C9_2,regex("Re[ad][da]y.", ignore_case=TRUE))==TRUE | 
                            str_detect(C9_2,regex("obje.", ignore_case=TRUE))==TRUE,  1, 0),
    
    
    C10_1_Numeric = ifelse(str_detect(C10_1, regex("W[aho][rthao][rho].", ignore_case=TRUE))==TRUE |
                             str_detect(C10_2, regex("W[aho][rthao][rho].", ignore_case=TRUE))==TRUE |
                             C10_1 == "Walrhol",  1,  0), 
    C10_2_Numeric = ifelse(str_detect(C10_2, regex("Pop", ignore_case=TRUE))==TRUE,  1,  0)) 



#-----------------5. Additional variables-----------------
# Add sums of Part A, Part B, Part C and Art Knoweldge

VAIAK = VAIAK %>%
  mutate(
    Part_A = rowSums(across(starts_with("AI"))), 
    Part_B = rowSums(across(starts_with("B") & ends_with("Numeric"))), 
    Part_C = rowSums(across(starts_with("C") & ends_with("Numeric")))
  ) %>%
  mutate(
    Art_knowledge = rowSums(across(c(Part_B, Part_C)))
  )

#-----------------6. Export data set-----------------

write.csv(VAIAK,
          "C:\\Users\\matte\\OneDrive\\Desktop\\VAIAK.csv")


