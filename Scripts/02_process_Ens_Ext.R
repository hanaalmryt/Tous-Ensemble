# Dependencies
  source("C:/Users/isaintsu/Desktop/Tous-Ensemble/Scripts/01_load_libraries.R")


# Excel sheet reading
  ens <- read_excel("C:/Users/isaintsu/Desktop/Tous-Ensemble/Initial_Phase/Cleaning/Enseignants/V1/Fichiers_xlsx/Enseignants_V0.xlsx",
                    sheet = "DataforR",
                    na = "N/A")

  ext <- read_excel("C:/Users/isaintsu/Desktop/Tous-Ensemble/Initial_Phase/Cleaning/Intervenants/V1/Intervenants_V1.xlsx",
                    na = "N/A")


  num_val_assign <- function(df) {
    # Numerical value assignation
    
    PTRSIIens_num <- colnames(df)[5:28]   # Identify columns needing to be re coded
    zarit_num <- colnames(df)[29:40]
    BPNSFS_num <- colnames(df)[41:63]
    MBI_num <- colnames(df)[64:85]
    WHOQOL_num <- colnames(df)[86:111]
    HAQ_num <- colnames(df)[112:122]
    MATIES_num <- colnames(df)[123:140]
    fiveD_num <- colnames(df)[141:145]
    CUQ_num <- colnames(df)[146:162]
    UEQ_num <- colnames(df)[163:170]
    TENS_num <- colnames(df)[171:197]
    
    # Define the code-to-score mapping
    code_map_1 <- c(                        
      "A1" = 1,
      "A2" = 2,
      "A3" = 3,
      "A4" = 4,
      "A5" = 5,
      "N/A" = "N/A")
    
    code_map_2 <- c(
      "A1" = 0,
      "A2" = 1,
      "A3" = 2,
      "A4" = 3,
      "A5" = 4,
      "N/A" = "N/A")
    
    code_map_3 <- c(
      "A1" = 0,
      "A2" = 1,
      "A3" = 2,
      "A4" = 3,
      "A5" = 4,
      "A6" = 5,
      "A7" = 6,
      "N/A" = "N/A")
    
    code_map_4 <- c(
      "A1" = 1,
      "A2" = 2,
      "A3" = 3,
      "A4" = 4,
      "A5" = 5,
      "A6" = 6,
      "N/A" = "N/A")
    
    code_map_5 <- c(
      "A2" = 1,
      "A3" = 2,
      "A4" = 3,
      "A5" = 4,
      "A13" = 5,
      "N/A" = "N/A")
    
    code_map_6 <- c(
      "A1" = 1,
      "A2" = 2,
      "A3" = 3,
      "A4" = 4,
      "A5" = 5,
      "A6" = 6,
      "A7" = 7,
      "N/A" = "N/A")
    
    code_map_7 <- c(
      "A1" = -3,
      "A2" = -2,
      "A3" = -1,
      "A4" = 0,
      "A5" = 1,
      "A6" = 2,
      "A7" = 3,
      "N/A" = "N/A")
    
    df <- df %>%                              # Sanitize code (removing of line breaks)
      mutate(across(everything(), ~ trimws(.)))
    
    recode_column_by_map <- function(column, col_name) {
      if (all(is.na(column)) || all(column == "0" | column == 0)) {
        return(column)  # Skip columns with only NAs or 0s
      }
      case_when(
        col_name %in% PTRSIIens_num ~ recode(column, !!!code_map_1),
        col_name %in% zarit_num ~ recode(column, !!!code_map_2),
        col_name %in% BPNSFS_num ~ recode(column, !!!code_map_1),
        col_name %in% MBI_num ~ recode(column, !!!code_map_3),
        col_name %in% WHOQOL_num ~ recode(column, !!!code_map_1),
        col_name %in% HAQ_num ~ recode(column, !!!code_map_4),
        col_name %in% MATIES_num ~ recode(column, !!!code_map_5),
        col_name %in% fiveD_num ~ recode(column, !!!code_map_6),
        col_name %in% CUQ_num ~ recode(column, !!!code_map_1),
        col_name %in% UEQ_num ~ recode(column, !!!code_map_7),
        col_name %in% TENS_num ~ recode(column, !!!code_map_1),
        TRUE ~ NA                          # Fallback
      )
    }
    
    recode_cols <- c(                      # Vector for all columns to transform
      PTRSIIens_num,
      zarit_num,
      BPNSFS_num,
      MBI_num,
      WHOQOL_num,
      HAQ_num,
      MATIES_num,
      fiveD_num,
      CUQ_num,
      UEQ_num,
      TENS_num)
    
    df[recode_cols] <- map2_dfc(df[recode_cols],
                                names(df[recode_cols]),
                                ~ recode_column_by_map(.x, .y))
    
    return(df)
  }


# Columns to reverse, organised by category
  PTRSIIens_reverse <- c("PTRSIIens1-2",
                         "PTRSIIens1-4",
                         "PTRSIIens1-8",
                         "PTRSIIens2-1",
                         "PTRSIIens2-3",
                         "PTRSIIens2-4",
                         "PTRSIIens3-2",
                         "PTRSIIens3-4",
                         "PTRSIIens4-2")
  
  BPNSFS_reverse <- c("BPNSFS-5",
                      "BPNSFS-6",
                      "BPNSFS-7",
                      "BPNSFS-8",
                      "BPNSFS-13",
                      "BPNSFS-14",
                      "BPNSFS-15",
                      "BPNSFS-16",
                      "BPNSFS-21",
                      "BPNSFS-22",
                      "BPNSFS-23")
  
  MBI_reverse <- c("MBIpros-17",
                   "MBIpros-18",
                   "MBIpros-19",
                   "MBIpros-20",
                   "MBIpros-21",
                   "MBIpros-23")
  
  WHOQOL_reverse <- c("reste-3",
                      "reste-4",
                      "negatif")
  
  HAQ_reverse <- c("HAQens-2",
                   "HAQens-6",
                   "HAQens-8")
  
  MATIES_reverse <- c("MATIESens-6",
                      "MATIESens-7",
                      "MATIESens-8",
                      "MATIESens-10",
                      "MATIESens-11",
                      "MATIESens-12",
                      "MATIESens-13",
                      "MATIESens-14",
                      "MATIESens-16",
                      "MATIESens-18")
  
  TENS_reverse <- c("TENScomp-3",
                    "TENScomp-4",
                    "TENSauto-1",
                    "TENSauto-2",
                    "TENSauto-3",
                    "TENSauto-4",
                    "TENSapp-3",
                    "TENSapp-4",
                    "tenscomp-3",
                    "tenscomp-4",
                    "tenscomp-5",
                    "tensauto-3",
                    "tensauto-4",
                    "tensauto-5",
                    "tensapp-4",
                    "tensapp-5")
  
  reversed_cols_ens <- c(PTRSIIens_reverse,
                         BPNSFS_reverse,
                         MBI_reverse,
                         WHOQOL_reverse,
                         MATIES_reverse,
                         TENS_reverse)
  
  reversed_cols_ext <- c(BPNSFS_reverse,
                         MBI_reverse,
                         WHOQOL_reverse,
                         TENS_reverse)
  
  
  rev_columns <- function(df,
                          columns_to_reverse,
                          min_val,
                          max_val) {
    # Reversal of specific columns
    
    df <- df %>%
      mutate(across(all_of(columns_to_reverse), ~ {
        if (all(is.na(.)) || all(. == 0)) {
          return(.)
        }
        
        clean_val <- gsub("[^0-9]", "", .)
        num_val <- as.numeric(clean_val)
        if_else(!is.na(num_val),
                (min_val + max_val) - num_val,
                NA_real_)
      }))
    
    return(df)
  }
  
  
  build_df <- function(df) {
    # Updates data frame with the numerically assigned and reversed columns
    
    df_num <- num_val_assign(df)
    HAQ_reversed <- rev_columns(df_num, HAQ_reverse, 1, 6)
    df_num[HAQ_reverse] <- HAQ_reversed[HAQ_reverse]
    
    if (identical(df, ens)) {              # PTRSII and MATIES only in Pro dataframe
      df_rev <- rev_columns(df_num, reversed_cols_ens, 1, 5)
      df_num[reversed_cols_ens] <- df_rev[reversed_cols_ens]
    }
    
    if (identical(df, ext)){
      df_rev <- rev_columns(df, reversed_cols_ext, 1, 5)
      df_num[reversed_cols_ext] <- df_rev[reversed_cols_ext]
    }
    
    return(df_num)
  }


# Building the data frames before computing scores
  ens_data <- build_df(ens)
  ext_data <- build_df(ext)


  process_df <- function(df) {
    # Creation of subsets of data frames to prepare for calculi
    # Uses the build_df generated data frame
    
    PTRSII <- colnames(df)[5:28]
    zarit <- colnames(df)[29:40]
    BPNSFS <- colnames(df)[41:63]
    MBI <- colnames(df)[64:85]
    WHOQOL <- colnames(df)[86:111]
    HAQ <- colnames(df)[112:122]
    MATIES <- colnames(df)[123:140]
    fiveD <- colnames(df)[141:145]
    CUQ <- colnames(df)[146:162]
    UEQ <- colnames(df)[163:170]
    TENST <- colnames(df)[171:182]
    TENSI <- colnames(df)[183:197]
    
    # Subsubsets
    PTRSII_join <- PTRSII[1:19]
    PTRSII_com <- PTRSII[20:24]
    BPNSFS_auto <- BPNSFS[1:8]
    BPNSFS_affi <- BPNSFS[9:16]
    BPNSFS_comp <- BPNSFS[17:23]
    MBI_ti <- MBI[1:8]
    MBI_di <- MBI[9:16]
    MBI_acc <- MBI[17:22]
    WHOQOL_hq <- WHOQOL[1:2]
    
    WHOQOL_phy <- c("reste-3",
                    "reste-4",
                    "reste-10",
                    "reste-15",
                    "reste-16",
                    "reste-17",
                    "reste-18")
    
    WHOQOL_psy <- c("reste-5",
                    "reste-6",
                    "reste-7",
                    "reste-11",
                    "reste-19",
                    "negatif")
    
    WHOQOL_soc <- c("reste-20",
                    "reste-21",
                    "reste-22")
    
    WHOQOL_env <- c("reste-8",
                    "reste-9",
                    "reste-12",
                    "reste-13",
                    "reste-14",
                    "reste-23",
                    "reste-24",
                    "reste-25")
    
    MATIES_inc <- c("MATIESens-1",
                    "MATIESens-2",
                    "MATIESens-3",
                    "MATIESens-4",
                    "MATIESens-5",
                    "MATIESens-13",
                    "MATIESens-14")
    
    MATIES_spe <- c("MATIESens-6",
                    "MATIESens-7",
                    "MATIESens-8",
                    "MATIESens-11",
                    "MATIESens-12")
    
    MATIES_sou <- c("MATIESens-9",
                    "MATIESens-10",
                    "MATIESens-16",
                    "MATIESens-18")
    
    MATIES_dro <- c("MATIESens-15",
                    "MATIESens-17")
    
    often <- CUQ[1:9]
    perf <- CUQ[10:17]
    UEQPrag <- UEQ[1:4]
    UEQHedo <- UEQ[5:8]
    TENSTcomp <- TENST[1:4]
    TENSTauto <- TENST[5:8]
    TENSTapp <- TENST[9:12]
    TENSIcomp <- TENSI[1:5]
    TENSIauto <- TENSI[6:10]
    TENSIapp <- TENSI[11:15]
    
    # Check for PTRSII and MATIES (only in ens)
    has_MATIES <- all(MATIES %in% colnames(df)) && any(rowSums(sapply(df[, MATIES, drop=FALSE], as.numeric)) != 0)
    has_PTRSII <- all(PTRSII %in% colnames(df)) && any(rowSums(sapply(df[, PTRSII, drop=FALSE], as.numeric)) != 0)
    
    # General calculi
    df_sc <- df %>%
      mutate(
        # Normalisation
        m_WHOQOL_hq    = rowMeans(across(all_of(WHOQOL_hq), ~ as.numeric(.)), na.rm = TRUE),
        m_WHOQOL_phy   = rowMeans(across(all_of(WHOQOL_phy), ~ as.numeric(.)), na.rm = TRUE),
        m_WHOQOL_psy   = rowMeans(across(all_of(WHOQOL_psy), ~ as.numeric(.)), na.rm = TRUE),
        m_WHOQOL_soc   = rowMeans(across(all_of(WHOQOL_soc), ~ as.numeric(.)), na.rm = TRUE),
        m_WHOQOL_env   = rowMeans(across(all_of(WHOQOL_env), ~ as.numeric(.)), na.rm = TRUE),
        s_HAQ          = rowSums(across(all_of(HAQ), ~ as.numeric(.)), na.rm = TRUE),

        # Score calculi
        PTRS_T = if (has_PTRSII) {
          rowMeans(across(all_of(PTRSII), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
              PTRS_Joint = if (has_PTRSII) {
          rowMeans(across(all_of(PTRSII_join), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
        PTRS_Comm  = if (has_PTRSII) {
          rowMeans(across(all_of(PTRSII_com), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
        MATIES_Inc = if (has_MATIES) {
          rowMeans(across(all_of(MATIES_inc), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
        MATIES_Spe = if (has_MATIES) {
          rowMeans(across(all_of(MATIES_spe), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
        MATIES_Sou = if (has_MATIES) {
          rowMeans(across(all_of(MATIES_sou), ~ as.numeric(.)), na.rm = TRUE)
        } else {
          NA_real_
        },
        
        Zarit          = rowSums(across(all_of(zarit), ~ as.numeric(.)), na.rm = TRUE),
        BPNSFS_Auto    = rowMeans(across(all_of(BPNSFS_auto), ~ as.numeric(.)), na.rm = TRUE),
        BPNSFS_Aff     = rowMeans(across(all_of(BPNSFS_affi), ~ as.numeric(.)), na.rm = TRUE),
        BPNSFS_Comp    = rowMeans(across(all_of(BPNSFS_comp), ~ as.numeric(.)), na.rm = TRUE),
        MBI_T          = rowSums(across(all_of(MBI_ti), ~ as.numeric(.)), na.rm = TRUE),
        MBI_Dist       = rowSums(across(all_of(MBI_di), ~ as.numeric(.)), na.rm = TRUE),
        MBI_Acc        = rowSums(across(all_of(MBI_acc), ~ as.numeric(.)), na.rm = TRUE),
        WQOL_G         = ((m_WHOQOL_hq - 1) / 6) * 100,
        WQOL_Phy       = ((m_WHOQOL_phy - 1) / 6) * 100,
        WQOL_Psy       = ((m_WHOQOL_psy - 1) / 6) * 100,
        WQOL_Rel       = ((m_WHOQOL_soc - 1) / 6) * 100,
        WQOL_Env       = ((m_WHOQOL_env - 1) / 6) * 100,
        WQOL_T         = ((WQOL_G + WQOL_Phy + WQOL_Psy + WQOL_Rel + WQOL_Env - 26) / 126) * 100,
        HAQ_Total      = s_HAQ * (10/9),
        HAQ_AllT       = s_HAQ * (100/78),
        HAQ_Pat        = s_HAQ * (100/12),
        fiveD_Cur      = rowMeans(across(all_of(fiveD), ~ as.numeric(.)), na.rm = TRUE),
        CUQ            = rowMeans(across(all_of(CUQ), ~ as.numeric(.)), na.rm = TRUE),
        Often          = rowMeans(across(all_of(often), ~ as.numeric(.)), na.rm = TRUE),
        Perf           = rowMeans(across(all_of(perf), ~ as.numeric(.)), na.rm = TRUE),
        UEQ_Total      = rowMeans(across(all_of(UEQ), ~ as.numeric(.)), na.rm = TRUE),
        UEQ_Prag       = rowMeans(across(all_of(UEQPrag), ~ as.numeric(.)), na.rm = TRUE),
        UEQ_Hedo       = rowMeans(across(all_of(UEQHedo), ~ as.numeric(.)), na.rm = TRUE),
        TENS_T_Total   = rowMeans(across(all_of(TENST),~ as.numeric(.)), na.rm = TRUE),
        TENS_T_Comp    = rowMeans(across(all_of(TENSTcomp), ~ as.numeric(.)), na.rm = TRUE),
        TENS_T_Auto    = rowMeans(across(all_of(TENSTauto), ~ as.numeric(.)), na.rm = TRUE),
        TENS_T_App     = rowMeans(across(all_of(TENSTapp), ~ as.numeric(.)), na.rm = TRUE),
        TENS_I_Total   = rowMeans(across(all_of(TENSI),~ as.numeric(.)), na.rm = TRUE),
        TENS_I_Comp    = rowMeans(across(all_of(TENSIcomp), ~ as.numeric(.)), na.rm = TRUE),
        TENS_I_Auto    = rowMeans(across(all_of(TENSIauto), ~ as.numeric(.)), na.rm = TRUE),
        TENS_I_App     = rowMeans(across(all_of(TENSIapp), ~ as.numeric(.)), na.rm = TRUE),
      ) %>%
      
      # Displaying the 5 first original columns with participant information and then the mutated columns
      select(1:4,
             PTRS_T,
             PTRS_Joint,
             PTRS_Comm,
             Zarit,
             BPNSFS_Auto,
             BPNSFS_Aff,
             BPNSFS_Comp,
             MBI_T,
             MBI_Dist,
             MBI_Acc,
             WQOL_G,
             WQOL_Phy,
             WQOL_Psy,
             WQOL_Rel,
             WQOL_Env,
             WQOL_T,
             HAQ_Total,
             HAQ_AllT,
             HAQ_Pat,
             MATIES_Inc,
             MATIES_Spe,
             MATIES_Sou,
             fiveD_Cur,
             CUQ,
             Often,
             Perf,
             UEQ_Total,
             UEQ_Prag,
             UEQ_Hedo,
             TENS_T_Total,
             TENS_T_Comp,
             TENS_T_Auto,
             TENS_T_App,
             TENS_I_Total,
             TENS_I_Comp,
             TENS_I_Auto,
             TENS_I_App)
  }


# Saving cleaned files
  Ens_sc <- process_df(ens_data)
  write_xlsx(Ext_sc, "Enseignants.xlsx")
  
  Ext_sc <- process_df(ext_data)
  write_xlsx(Ext_sc, "Intervenants.xlsx")

