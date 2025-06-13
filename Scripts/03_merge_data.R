# Dependencies
  source("C:/Users/isaintsu/Desktop/Tous-Ensemble/Scripts/01_load_libraries.R")


# Clean dataset loading
  Eleves <- read_excel("Data/Cleaned/Eleves.xlsx")
  Parents <- read_excel("Data/Cleaned/Parents.xlsx")
  Enseignants <- read_excel("Data/Cleaned/Enseignants.xlsx")
  Intervenants <- read_excel("Data/Cleaned/Intervenants.xlsx")


# Variables
  IV <- c("Group", "Questionnaire")           # Independent
  
  # Needing z-scoring
  vars_par_ens <- c("PTRS_T",                 # Relations
                    "PTRS_Joint",
                    "PTRS_Comm",
                    
                    "HAQ_Total",
                    "HAQ_AllT",
                    "HAQ_Pat",
                    
                    "ATIM_T",                # Attitude
                    "ATIM_Benef",
                    "ATIM_Satis",
                    "ATIM_Ens",
                    "ATIM_Droits",
                    
                    "MATIES_Inc",
                    "MATIES_Spe",
                    "MATIES_Sou")
  
  vars_all <- c("HAQ_Total",                  # Relations
                "HAQ_AllT",
                "HAQ_Pat",
                
                "BPNSFS_T",                   # Self-efficacy 
                "BPNSFS_Auto",
                "BPNSFS_Aff",
                "BPNSFS_Comp",
                
                "Zarit",                      # Burden
                
                "PBI_T",                      # Burnout
                "PBI_Fatigue",
                "PBI_Dist",
                "PBI_Acc",
                "MBI_T",
                "MBI_Dist",
                "MBI_Acc",
                
                "WQOL_T",                     # Adult's QOL
                "WQOL_G",
                "WQOL_Phy",
                "WQOL_Psy",
                "WQOL_Rel",
                "WQOL_Env")
  
  optional_vars <- c("CUQ",
                     "UEQ_Total",
                     "UEQ_Prag",
                     "UEQ_Hedo",
                     
                     "TENS_T_Total",
                     "TENS_T_Comp",
                     "TENS_T_Auto",
                     "TENS_T_App",
                     "TENS_I_Total",
                     "TENS_I_Comp",
                     "TENS_I_Auto",
                     "TENS_I_App")
  

# Preparation for team analysis
  # Group labelling
    assign_group <- function(dataset) {
      # Determining if a participant is in the equipped or control group
      dataset <- dataset %>%
        mutate(
          # Replace "-" with NA in both ID columns
          `ID-Pilote` = na_if(`ID-Pilote`, "-"),
          `ID-Phase2` = na_if(`ID-Phase2`, "-"),
          
          # Combine IDs safely
          id_combined = coalesce(as.character(`ID-Pilote`), as.character(`ID-Phase2`)),
          id_combined = str_trim(id_combined),  # Remove leading/trailing spaces
          
          # Group assignment based on first character
          Group = case_when(
            str_sub(id_combined, 1, 1) %in% c("1", "7") ~ "Equip",
            str_sub(id_combined, 1, 1) %in% c("2", "8") ~ "Control",
            TRUE ~ NA_character_
          )
        ) %>%
        select(1:2, Group, everything(), -id_combined)
    }
  
  # Applying labels
    Ele <- assign_group(Eleves)
    Par <- assign_group(Parents)
    Ens <- assign_group(Enseignants)
    Int <- assign_group(Intervenants)
    
  
  # Removing metadata from Excel
    remove_cols <- c("NB_Valide")
    
    Ele_ <- Ele %>% select(-any_of(remove_cols))
    Par_ <- Par %>% select(-any_of(remove_cols))
    Ens_ <- Ens %>% select(-any_of(remove_cols))
    Int_ <- Int %>% select(-any_of(remove_cols))

  # Converting columns to numeric in all data frames
    make_numeric <- function(df, cols) {
      cols <- cols[cols %in% colnames(df)]            # Filtering existing columns
      df %>%
        mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.))))
  }
  
  # Defining all numeric vars that might have type mismatch
    numeric_vars <- c(vars_ele_par, vars_par_ens, vars_all, optional_vars)
  
  # Applying it across all the data frame columns
    Ele__ <- make_numeric(Ele_, numeric_vars)
    Par__ <- make_numeric(Par_, numeric_vars)
    Ens__ <- make_numeric(Ens_, numeric_vars)
    Int__ <- make_numeric(Int_, numeric_vars)
  
  
  # Dataset binding
    z_data <- bind_rows(Ele__, Par__, Ens__, Int__)

  # Create unified ID
    unify <- function(df) {
      # ID-Pilote, ID-Phase2 or NA in the resulting data frame
      df <- df %>%
        mutate(TeamID = coalesce(`ID-Pilote`, `ID-Phase2`))
    }
    
    z_data <- unify(z_data)
    Ele_p <- unify(Ele__)
    Par_p <- unify(Par__)
  
  
  # Z-scoring
    zscore_global <- function(df, vars_to_scale) {
      df %>%
        mutate(across(all_of(vars_to_scale), ~ scale(.)[,1], .names = "z_{.col}"))
    }
    
    vars_to_zscore <- unique(c(vars_par_ens, vars_all, optional_vars))                # Assembles all the variable names, removing duplicates
    
    vars_to_zscore <- vars_to_zscore[vars_to_zscore %in% colnames(z_data)]            # Ensures only valid column names are kept and avoids errors
    
    full_data_z <- zscore_global(z_data, vars_to_zscore)                              # Apply z-scoring

    
  # First two datasets : T1 -> T2 (PEDSQL + all DVs)
    analysis_prep <- function(data, ord_factor = c("T1", "T2", "T3")) {
      # Dataframe organisation before analysis (with z-scored DVs or not)
      data_clean <- data %>%
        mutate(                                                                       # Turning Questionnaire into ordering factor and sorting
      Questionnaire = factor(Questionnaire, levels = ord_factor, ordered = TRUE),
      TeamID = substr(TeamID, 1, 3)                                                   # Team ID extraction
    ) %>%
      arrange(Questionnaire)
    }
    
    # # All z data
    # full_data_z_clean <- analysis_prep(full_data_z)
    # 
    # 
    # team_summary <- full_data_z_clean %>%                                           # Collapse to team-level average per wave
    #   group_by(TeamID, Questionnaire) %>%
    #   summarise(across(starts_with("z_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    # 
    # 
    # team_T1_T2_all <- team_summary %>%
    #   filter(Questionnaire %in% c("T1", "T2")) %>%
    #   pivot_wider(names_from = Questionnaire, values_from = starts_with("z_"), names_sep = "_")
    # 
    # 
    # writexl::write_xlsx(team_T1_T2_all, "Data/Processed/T1_T2/teams_T1_T2_all.xlsx")
    # 
    # # PEDSQL
    #   # Cleaning each individual dataset
    #   Ele_cl <- analysis_prep(Ele_p)
    #   Par_cl <- analysis_prep(Par_p)
    #   
    #   # DV name standardisation
    #   Ele_clean <- Ele_cl %>%
    #     rename(
    #       PEDSQL_T = PEDSQL,
    #       PEDSQL_SP = PEDSQL_SP,
    #       PEDSQL_PS = PEDSQL_PS,
    #       PEDSQL_Em = PEDSQL_Em,
    #       PEDSQL_R = PEDSQL_R,
    #       PEDSQL_Ec = PEDSQL_Ec
    #     ) %>%
    #     select(TeamID, Questionnaire, PEDSQL_T, PEDSQL_SP, PEDSQL_PS, PEDSQL_Em, PEDSQL_R, PEDSQL_Ec) %>%
    #     group_by(TeamID, Questionnaire) %>%
    #     summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    #     mutate(Role = "Eleve")
    #   
    #   # Standardize names in Par
    #   Par_clean <- Par_cl %>%
    #     rename(
    #       PEDSQL_T = PedsQL_T,
    #       PEDSQL_SP = PedsQL_SP,
    #       PEDSQL_PS = PedsQL_PS,
    #       PEDSQL_Em = PedsQL_Em,
    #       PEDSQL_R = PedsQL_R,
    #       PEDSQL_Ec = PedsQL_Ec
    #     ) %>%
    #     select(TeamID, Questionnaire, PEDSQL_T, PEDSQL_SP, PEDSQL_PS, PEDSQL_Em, PEDSQL_R, PEDSQL_Ec) %>%
    #     group_by(TeamID, Questionnaire) %>%
    #     summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    #     mutate(Role = "Parent")
    # 
    #   Par_ready <- analysis_prep(Par_clean)
    #   Ele_ready <- analysis_prep(Ele_clean)
    # 
    #   ele_par <- bind_rows(Ele_ready, Par_ready)
    #   
    #   PEDSQL_data <- ele_par %>%
    #     group_by(TeamID, Questionnaire) %>%
    #     summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    #   
    #   PEDSQL_T1_T2 <- PEDSQL_data %>%
    #     filter(Questionnaire %in% c("T1", "T2")) %>%
    #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
    #   
    #   PEDSQL_ele <- Ele_clean %>%
    #     group_by(TeamID, Questionnaire) %>%
    #     summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    #   
    #   PEDSQL_ele_T1_T2 <- PEDSQL_ele %>%
    #     filter(Questionnaire %in% c("T1", "T2")) %>%
    #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
    #   
    #   PEDSQL_par <- Par_clean %>%
    #     group_by(TeamID, Questionnaire) %>%
    #     summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    #   
    #   PEDSQL_par_T1_T2 <- PEDSQL_par %>%
    #     filter(Questionnaire %in% c("T1", "T2")) %>%
    #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
    #   
    #   writexl::write_xlsx(PEDSQL_ele_T1_T2, "Data/Processed/T1_T2/PEDSQL_ele_T1_T2.xlsx")
    #   writexl::write_xlsx(PEDSQL_par_T1_T2, "Data/Processed/T1_T2/PEDSQL_par_T1_T2.xlsx")
    #   writexl::write_xlsx(PEDSQL_T1_T2, "Data/Processed/T1_T2/teams_T1_T2_PEDSQL.xlsx")
    # 
    
  # # Second two datasets : T1 -> T2 - > T3 (PEDSQL + all DVs)  
  #   
  #   # All data
  #   team_T1_T2_T3_all <- team_summary %>%
  #     filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
  #     pivot_wider(names_from = Questionnaire, values_from = starts_with("z_"), names_sep = "_")
  # 
  #   writexl::write_xlsx(team_T1_T2_T3_all, "Data/Processed/T1_T2_T3/teams_T1_T2_T3_all.xlsx")
  #   
  #   
  #   # PEDSQL  
  #   PEDSQL_T1_T2_T3 <- PEDSQL_data %>%
  #     filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
  #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
  #   
  #   PEDSQL_ele_T1_T2_T3 <- PEDSQL_ele %>%
  #     filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
  #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
  #   
  #   PEDSQL_par_T1_T2_T3 <- PEDSQL_par %>%
  #     filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
  #     pivot_wider(names_from = Questionnaire, values_from = starts_with("PEDSQL"), names_sep = "_")
  #   
  #   writexl::write_xlsx(PEDSQL_ele_T1_T2_T3, "Data/Processed/T1_T2_T3/PEDSQL_ele_T1_T2_T3.xlsx")
  #   writexl::write_xlsx(PEDSQL_par_T1_T2_T3, "Data/Processed/T1_T2_T3/PEDSQL_par_T1_T2_T3.xlsx")
  #   writexl::write_xlsx(PEDSQL_T1_T2_T3, "Data/Processed/T1_T2_T3/teams_T1_T2_T3_PEDSQL.xlsx")
  #   
    
# Preparation for individual analysis
  # Suppressing NB_valid
  E <- Eleves %>% select(-any_of(remove_cols))
  P <- Parents %>% select(-any_of(remove_cols))
  En <- Enseignants %>% select(-any_of(remove_cols))
  I <- Intervenants %>% select(-any_of(remove_cols))
    
  # Applying numericalisation across all the data frame columns
  El <- make_numeric(E, numeric_vars)
  Pa <- make_numeric(P, numeric_vars)
  Ens <- make_numeric(En, numeric_vars)
  In <- make_numeric(I, numeric_vars)

  # Dataset binding
  full_df <- bind_rows(El, Pa, Ens, In)

  # Group assignation
  full_grouped_df <- assign_group(full_df)

  # Z-scoring
  vars_to_zscore_ind <- unique(c(vars_par_ens, vars_all, optional_vars))                
  
  vars_to_zscore_ind <- vars_to_zscore_ind[vars_to_zscore_ind %in% colnames(full_grouped_df)]
  
  full_data_ind_z <- zscore_global(full_grouped_df, vars_to_zscore_ind)

  full_data_ind_z_clean  <- full_data_ind_z %>%
    select(everything(), Group) %>% 
    mutate(             
      Questionnaire = factor(Questionnaire, levels = c("T1", "T2", "T3"), ordered = TRUE),
    ) %>%
    arrange(Questionnaire)

    
  # First two datasets : T1 -> T2 (PEDSQL + all DVs)
  
    # All z data
      ind_summary_u <- full_data_ind_z_clean %>%
        group_by(Group, `ID-Pilote`, `ID-Phase2`, Questionnaire) %>%
        summarise(across(starts_with("z_"), function(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          `ID-Pilote` = na_if(`ID-Pilote`, "-"),
          `ID-Phase2` = na_if(`ID-Phase2`, "-"),
          PersonID = coalesce(`ID-Pilote`, `ID-Phase2`)
        )
      
      # Sanity check: duplicates before pivoting
      ind_summary_u %>%
        count(PersonID, Group, Questionnaire) %>%
        filter(n > 1)  # Already confirmed to exist — fix this below.

      # T1–T2 all z-scores
      ind_T1_T2_all <- ind_summary_u %>%
        filter(Questionnaire %in% c("T1", "T2")) %>%
        group_by(PersonID, Group, Questionnaire) %>%
        summarise(across(starts_with("z_"), function(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        pivot_wider(
          names_from = Questionnaire,
          values_from = starts_with("z_"),
          names_sep = "_"
        )
    
    writexl::write_xlsx(ind_T1_T2_all, "Data/Processed/Ind/T1_T2/ind_T1_T2_all.xlsx")
    
    # PEDSQL
      # Eleve
      cols_ele <- c("Questionnaire",
                    "Group",
                    "ID-Pilote",
                    "ID-Phase2",
                    "PEDSQL",               # Child's QOL
                    "PEDSQL_PS",
                    "PEDSQL_SP",
                    "PEDSQL_Em",
                    "PEDSQL_R",
                    "PEDSQL_Ec")
      
      PEDSQL_ele_ind <- full_grouped_df %>% select(all_of(cols_ele))
      
      PEDSQL_ele_ind_i <- PEDSQL_ele_ind %>%
        mutate(
          `ID-Pilote` = na_if(`ID-Pilote`, "-"),
          `ID-Phase2` = na_if(`ID-Phase2`, "-"),
          
          # Unified individual identifier creation
          PersonID = coalesce(`ID-Pilote`, `ID-Phase2`))
      
      
      PEDSQL_ele_ind_i <- PEDSQL_ele_ind_i %>%
        filter(if_any(starts_with("PEDSQL"), ~ !is.na(.x)))
      
      
      PEDSQL_ind_summary <- PEDSQL_ele_ind_i %>%
        group_by(PersonID, Group, Questionnaire) %>%
        summarise(across(starts_with("PEDSQL"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

      
      PEDSQL_ele_ind_T1_T2 <- PEDSQL_ind_summary %>%
        filter(Questionnaire %in% c("T1", "T2")) %>%
        pivot_wider(
          names_from = Questionnaire,
          values_from = starts_with("PEDSQL"),
          names_sep = "_"
        )
      writexl::write_xlsx(PEDSQL_ele_T1_T2, "Data/Processed/Ind/T1_T2/PEDSQL_ele_ind_T1_T2.xlsx")
      

      # Parent
      cols_par <- c("Questionnaire",
                    "Group",
                    "ID-Pilote",
                    "ID-Phase2",
                    "PedsQL_T",           # Parent's QOL
                    "PedsQL_PS",
                    "PedsQL_SP",
                    "PedsQL_Em",
                    "PedsQL_R",
                    "PedsQL_Ec")

      
      PEDSQL_par_ind <- full_grouped_df %>% select(all_of(cols_par))
      
      
      PEDSQL_par_ind_i <- PEDSQL_par_ind %>%
        mutate(
          `ID-Pilote` = na_if(`ID-Pilote`, "-"),
          `ID-Phase2` = na_if(`ID-Phase2`, "-"),
          
          # Unified individual identifier creation
          PersonID = coalesce(`ID-Pilote`, `ID-Phase2`))
      
      
      PEDSQL_par_ind_i <- PEDSQL_par_ind_i %>%
        filter(if_any(starts_with("PedSQL"), ~ !is.na(.x)))
      
      
      PedSQL_ind_summary <- PEDSQL_par_ind_i %>%
        group_by(PersonID, Group, Questionnaire) %>%
        summarise(across(starts_with("PedSQL"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
      
      
      PEDSQL_par_ind_T1_T2 <- PedSQL_ind_summary %>%
        filter(Questionnaire %in% c("T1", "T2")) %>%
        pivot_wider(
          names_from = Questionnaire,
          values_from = starts_with("PedSQL"),
          names_sep = "_"
        )
  
      writexl::write_xlsx(PEDSQL_par_T1_T2, "Data/Processed/Ind/T1_T2/PEDSQL_par_ind_T1_T2.xlsx")


  # Second two datasets : T1 -> T2 -> T3 (PEDSQL + all DVs)
    # All z data
      ind_T1_T2_T3_all <- ind_summary_u %>%
        filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
        group_by(PersonID, Group, Questionnaire) %>%
        summarise(across(starts_with("z_"), function(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        pivot_wider(
          names_from = Questionnaire,
          values_from = starts_with("z_"),
          names_sep = "_"
        )
      
      writexl::write_xlsx(ind_T1_T2_T3_all, "Data/Processed/Ind/T1_T2_T3/ind_T1_T2_T3_all.xlsx")
      
    
  # Eleve
      PEDSQL_ele_ind_T1_T2_T3 <- PEDSQL_ind_summary %>%
        filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
        pivot_wider(
          id_cols = c(PersonID, Group),
          names_from = Questionnaire,
          values_from = starts_with("PEDSQL"),
          names_sep = "_"
        )
      writexl::write_xlsx(PEDSQL_ele_ind_T1_T2_T3, "Data/Processed/Ind/T1_T2_T3/PEDSQL_ele_ind_T1_T2_T3.xlsx")
      
      
    #Parent
      PEDSQL_par_ind_T1_T2_T3 <- PedSQL_ind_summary %>%
        filter(Questionnaire %in% c("T1", "T2", "T3")) %>%
        pivot_wider(
          id_cols = c(PersonID, Group),
          names_from = Questionnaire,
          values_from = starts_with("PedSQL"),
          names_sep = "_"
        )
      
      writexl::write_xlsx(PEDSQL_par_ind_T1_T2_T3, "Data/Processed/Ind/T1_T2_T3/PEDSQL_par_ind_T1_T2_T3.xlsx")
  