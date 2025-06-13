# Dependencies
  source("C:/Users/isaintsu/Desktop/Tous-Ensemble/Scripts/01_load_libraries.R")


# Data set loading
  full_T1_T2 <- readxl::read_excel("Data/Processed/Team/T1_T2/teams_T1_T2_all.xlsx")
  ind_full_T1_T2 <- readxl::read_excel("Data/Processed/Ind/T1_T2/ind_T1_T2_all.xlsx")
  
# Useful functions for analysis later
  # Group column addition
  add_group <- function(df) {
    df <- df %>%
      mutate(
        Group = case_when(
          substr(TeamID, 1, 1) %in% c("1", "7") ~ "Equip",
          substr(TeamID, 1, 1) %in% c("2", "8") ~ "Control",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Group))
  }
  
  # Adding groups for the "Team" datasets
  full_T1_T2 <- add_group(full_T1_T2)
  PEDSQL_ele_T1_T2 <- add_group(PEDSQL_ele_T1_T2)
  PEDSQL_par_T1_T2 <- add_group(PEDSQL_par_T1_T2)
  PEDSQL_T1_T2 <- add_group(PEDSQL_T1_T2)
  
  
  saveRDS(full_T1_T2, "Data/Processed/Team/T1_T2/full_T1_T2_grouped.rds")
  write_xlsx(full_T1_T2, "Data/Processed/Team/T1_T2/full_T1_T2_grouped.xlsx")


  # Function to reshape to long format for ANOVA
  reshape_long <- function(df) {
    # Defining possible names for group and ID
    group_col <- intersect(c("Group", "group"), colnames(df))
    id_col <- intersect(c("TeamID", "PersonID"), colnames(df))
    
    if (length(group_col) == 0) stop("Missing required column: Group or group")
    if (length(id_col) == 0) stop("Missing required column: TeamID or PersonID")
    
    # Using the first match in case of multiple
    group_col <- group_col[1]
    id_col <- id_col[1]
    
    # Pivot longer on all except these two columns
    df_long <- df %>%
      pivot_longer(
        cols = -all_of(c(group_col, id_col)),
        names_to = c("DV", "Time"),
        names_pattern = "(.*)_T([123])",
        values_to = "Score",
        names_transform = list(Time = function(x) paste0("T", x))
      ) %>%
      mutate(
        DV = factor(DV),
        !!group_col := factor(.data[[group_col]]),
        !!id_col := factor(.data[[id_col]])
      ) %>%
      drop_na(Score)
    
    return(df_long)
  }
  
  
  # Same but for ANCOVA
  reshape_long_with_cuq <- function(df, cuq_time = "T1", covariate_prefix = "z_CUQ") {
    group_col <- intersect(c("Group", "group"), colnames(df))
    id_col <- intersect(c("TeamID", "PersonID"), colnames(df))
    if (length(group_col) == 0) stop("Missing required column: Group or group")
    if (length(id_col) == 0) stop("Missing required column: TeamID or PersonID")
    group_col <- group_col[1]
    id_col <- id_col[1]
    
    cuq_colname <- paste0(covariate_prefix, "_", cuq_time)
    if (!(cuq_colname %in% colnames(df))) stop("CUQ column for specified timepoint not found.")
    
    # Baseline CUQ extraction
    cuq_df <- df %>% select(all_of(c(id_col, cuq_colname))) %>%
      rename(CUQ = all_of(cuq_colname))
    
    # Reshaping the rest (exclude CUQ columns)
    df_wo_cuq <- df %>% select(-matches(paste0("^", covariate_prefix, "_T[123]$")))
    df_long <- df_wo_cuq %>%
      pivot_longer(
        cols = -all_of(c(group_col, id_col)),
        names_to = c("DV", "Time"),
        names_pattern = "(.*)_T([123])",
        values_to = "Score",
        names_transform = list(Time = function(x) paste0("T", x))
      ) %>%
      left_join(cuq_df, by = id_col) %>%
      mutate(
        DV = factor(DV),
        !!group_col := factor(.data[[group_col]]),
        !!id_col := factor(.data[[id_col]])
      ) %>%
      drop_na(Score)
    
    return(df_long)
  }
  
  
  # Domain aggregation helper
  aggregate_domains_clean <- function(df, timepoints = c("T1", "T2", "T3")) {
    # Dynamically detecting group and ID columns
    group_col <- intersect(c("Group", "group"), colnames(df))
    id_col <- intersect(c("TeamID", "PersonID"), colnames(df))
    
    if (length(group_col) == 0) stop("Missing required column: Group or group")
    if (length(id_col) == 0) stop("Missing required column: TeamID or PersonID")
    
    group_col <- group_col[1]
    id_col <- id_col[1]
    
    id_vars <- c(id_col, group_col)
    
    # DVs to keep across timepoints
    keep_vars <- c(
      "z_CUQ",
      "z_BPNSFS_T", "z_BPNSFS_Auto", "z_BPNSFS_Aff", "z_BPNSFS_Comp",
      "z_WQOL_T", "z_WQOL_G", "z_WQOL_Phy", "z_WQOL_Psy", "z_WQOL_Rel", "z_WQOL_Env",
      "z_UEQ_Total", "z_UEQ_Prag", "z_UEQ_Hedo",
      "z_TENS_T_Total", "z_TENS_T_Comp", "z_TENS_T_Auto", "z_TENS_T_App"
    )
    
    for (tp in timepoints) {
      # Variables to aggregate for each domain (Relations, Burden/Burnout)
      rel_patterns <- c("z_PTRS_T_", "z_HAQ_Total_")
      bur_den_out_patterns <- c("z_Zarit_", "z_PBI_T_", "z_MBI_T_")
      att_patterns <- c("z_ATIM_T_", "z_ATIM_Benef_", "z_ATIM_Satis_", "z_ATIM_Ens_", "z_ATIM_Droits_",
                        "z_MATIES_Inc_", "z_MATIES_Spe_", "z_MATIES_Sou_")
      
      # Matching columns for each pattern
      rel_vars <- unlist(lapply(rel_patterns, function(p) grep(paste0("^", p, tp, "$"), names(df), value = TRUE)))
      bur_den_out_vars <- unlist(lapply(bur_den_out_patterns, function(p) grep(paste0("^", p, tp, "$"), names(df), value = TRUE)))
      att_vars <- unlist(lapply(att_patterns, function(p) grep(paste0("^", p, tp, "$"), names(df), value = TRUE)))
      
      # Compute means (use if length > 0, else fill NA)
      df[[paste0("Relations_", tp)]] <- if (length(rel_vars) > 0) rowMeans(df[, rel_vars, drop = FALSE], na.rm = TRUE) else NA
      df[[paste0("Burden_Burnout_", tp)]] <- if (length(bur_den_out_vars) > 0) rowMeans(df[, bur_den_out_vars, drop = FALSE], na.rm = TRUE) else NA
      df[[paste0("Attitude_", tp)]] <- if (length(att_vars) > 0) rowMeans(df[, bur_den_out_vars, drop = FALSE], na.rm = TRUE) else NA
    }
    
    # Expanding DV names to include timepoints
    keep_timepoint_vars <- unlist(lapply(timepoints, function(tp) paste0(keep_vars, "_", tp)))
    
    # Also including new aggregate columns
    domain_vars <- unlist(lapply(timepoints, function(tp) c(paste0("Relations_", tp), paste0("Burden_Burnout_", tp), paste0("Attitude_", tp))))
    
    # Final list of columns to return (- )only those actually present in df)
    kept_columns <- intersect(c(id_vars, keep_timepoint_vars, domain_vars), names(df))
    
    return(df[, kept_columns, drop = FALSE])
  }

  
  # Shapiro-Wilk + Levene + ANOVA
    run_anova <- function(data, analysis_prefix, output_prefix, ds_prefix, time_levels) {
      # Identifying columns to use
      group_col <- intersect(c("Group", "group"), colnames(data))
      id_col <- intersect(c("TeamID", "PersonID"), colnames(data))
      time_col <- "Time"
      
      if (length(group_col) == 0) stop("Missing required column: Group or group")
      if (length(id_col) == 0) stop("Missing required column: TeamID or PersonID")
      
      group_col <- group_col[1]
      id_col <- id_col[1]
      
      # Output table folder creation
      table_path <- glue("Output/{analysis_prefix}/Tables/{output_prefix}/Mixed/{ds_prefix}")
      if (!dir.exists(table_path)) {
        dir.create(table_path, recursive = TRUE)
      }
      
      # Output plot folder creation
      plot_path <- glue("Output/{analysis_prefix}/Plots/{output_prefix}/Mixed/{ds_prefix}")
      if (!dir.exists(plot_path)) {
        dir.create(plot_path, recursive = TRUE)
      }
      
      results_list <- list()
      plots <- list()
      
      # Factor conversions with dynamic column names:
      data[[ "Time" ]] <- factor(data$Time, levels = time_levels)
      data[[ group_col ]] <- factor(data[[group_col]])
      data[[ id_col ]] <- factor(data[[id_col]])
      data$DV <- factor(data$DV)
      
      unique_dvs <- unique(data$DV)
      
      for (dv in unique_dvs) {
        cat("Processing DV:", dv, "\n")
        
        df <- data %>% filter(DV == dv)
        
        cell_counts <- table(df[[group_col]], df[[time_col]])
        
        # Checking if the design is fully crossed and has ≥ 2 per cell
        if (any(cell_counts < 2)) {
          warning(paste("DV", dv, ": Not fully crossed or some Group × Time cells have < 2 participants."))
          fully_crossed <- FALSE
        } else {
          fully_crossed <- TRUE
        }
        
        group_levels <- nlevels(factor(df[[group_col]]))

        # Levene's test:
        if (fully_crossed && nlevels(factor(df[[group_col]])) > 1 && nlevels(factor(df[[time_col]])) > 1) {
          levene <- tryCatch({
            df %>% levene_test(reformulate(c(group_col, time_col), response = "Score"))
          }, error = function(e) {
            warning(paste("Levene's test failed for", dv, ":", e$message))
            return(data.frame())
          })
        } else {
          warning(paste("Skipping Levene's test for", dv, "- design not fully crossed or only one group/time level."))
          levene <- data.frame()
        }
        
        # Shapiro-Wilk test:
        shapiro <- tryCatch({
          df %>% group_by(across(all_of(c(group_col, "Time")))) %>% shapiro_test(Score)
        }, error = function(e) {
          warning(paste("Shapiro test failed for", dv, ":", e$message))
          return(data.frame())
        })
        
        # ANOVA:
        anova_res <- tryCatch({
          if (group_levels > 1) {
            df %>%
              anova_test(dv = Score, wid = .data[[id_col]], within = Time, between = .data[[group_col]]) %>%
              get_anova_table()
          } else {
            message(paste("Only one Group level for", dv, "- running within-subjects ANOVA only."))
            df %>%
              anova_test(dv = Score, wid = .data[[id_col]], within = Time) %>%
              get_anova_table()
          }
        }, error = function(e) {
          warning(paste("ANOVA failed for", dv, ":", e$message))
          return(data.frame())
        })
        
        
        # ANCOVA: Adding CUQ as covariate
        ancova_res <- data.frame()
        if ("CUQ" %in% colnames(df)) {
          if (is.numeric(df$CUQ)) {
            cuq_non_na <- sum(!is.na(df$CUQ))
            if (cuq_non_na >= 10) {
              ancova_res <- tryCatch({
                if (group_levels > 1) {
                  anova_test(data = df, dv = Score, wid = .data[[id_col]], 
                             between = .data[[group_col]], within = Time, covariate = CUQ) %>%
                    get_anova_table()
                } else {
                  anova_test(data = df, dv = Score, wid = .data[[id_col]], 
                             within = Time, covariate = CUQ) %>%
                    get_anova_table()
                }
              }, error = function(e) {
                warning(paste("ANCOVA failed for", dv, ":", e$message))
                return(data.frame())
              })
            } else {
              warning(paste("CUQ has too few non-NA values (", cuq_non_na, 
                            ") for", dv, "- skipping ANCOVA."))
            }
          } else {
            warning(paste("CUQ is not numeric for", dv, "- skipping ANCOVA."))
          }
        } else {
          warning(paste("CUQ not found for", dv, "- skipping ANCOVA."))
        }
        
        
        results_list[[dv]] <- list(
          LeveneTest = levene,
          ShapiroTest = shapiro,
          ANOVA = anova_res,
          ANCOVA = ancova_res
        )

        
        # QQ plot
        qq_plot <- ggqqplot(df, "Score", facet.by = c(group_col, "Time")) +
          labs(title = paste("QQ Plot for", dv))
        ggsave(filename = file.path(plot_path, paste0(ds_prefix, output_prefix, "_", dv, "_qqplot.png")),
               plot = qq_plot, width = 6, height = 4)
        
        # Interaction plot
        interaction_plot <- df %>%
          group_by(Time, .data[[group_col]]) %>%
          summarise(mean_score = mean(Score, na.rm = TRUE),
                    se = sd(Score, na.rm = TRUE) / sqrt(n()), 
                    .groups = "drop") %>%
          ggplot(aes(x = Time, y = mean_score, color = .data[[group_col]], group = .data[[group_col]])) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.2) +
          theme_minimal() +
          labs(title = paste("Interaction Plot for", dv),
               x = "Time",
               y = "Mean Score") +
          theme(legend.position = "bottom")
        
        ggsave(filename = file.path(plot_path, paste0(ds_prefix, output_prefix, "_", dv, "_interaction.png")),
               plot = interaction_plot, width = 6, height = 4)
        
      }

      
      # Saving all results to Excel
      wb <- createWorkbook()
      for (dv in names(results_list)) {
        addWorksheet(wb, paste0(dv, "_Levene"))
        writeData(wb, paste0(dv, "_Levene"), results_list[[dv]]$LeveneTest)
        
        addWorksheet(wb, paste0(dv, "_Shapiro"))
        writeData(wb, paste0(dv, "_Shapiro"), results_list[[dv]]$ShapiroTest)
        
        addWorksheet(wb, paste0(dv, "_ANOVA"))
        writeData(wb, paste0(dv, "_ANOVA"), results_list[[dv]]$ANOVA)
        
        addWorksheet(wb, paste0(dv, "_ANCOVA"))
        writeData(wb, paste0(dv, "_ANCOVA"), results_list[[dv]]$ANCOVA)
      }
      
      saveWorkbook(wb, file.path(table_path, paste0(ds_prefix, output_prefix, "_anova_results_tabs.xlsx")), overwrite = TRUE)    
      cat("All results and plots saved with prefix:", analysis_prefix, ds_prefix, output_prefix, "\n")
    }


# T1 → T2 analysis

  # ANOVA + Levene
  
    # Dataset preparation
    
      # Team
      full_T1_T2_agg <- aggregate_domains_clean(full_T1_T2, timepoints = c("T1", "T2"))
      
      long_T1_T2 <- reshape_long(full_T1_T2_agg) %>% filter(Time %in% c("T1", "T2"))
      long_PEDSQL_ele_T1_T2 <- reshape_long(PEDSQL_ele_T1_T2)
      long_PEDSQL_par_T1_T2 <- reshape_long(PEDSQL_par_T1_T2)
      long_PEDSQL_T1_T2 <- reshape_long(PEDSQL_T1_T2)
      
      # Individual
      ind_full_T1_T2_agg <- aggregate_domains_clean(ind_full_T1_T2, timepoints = c("T1", "T2"))
      
      ind_long_T1_T2 <- reshape_long(ind_full_T1_T2_agg) %>% filter(Time %in% c("T1", "T2"))
      ind_long_PEDSQL_ele_T1_T2 <- reshape_long(PEDSQL_ele_ind_T1_T2)
      ind_long_PEDSQL_par_T1_T2 <- reshape_long(PEDSQL_par_ind_T1_T2)

      
#     # Running analysis
#       # Team
#       run_anova(long_T1_T2, analysis_prefix = "Team", output_prefix = "T1_T2", ds_prefix = "z_scored", time_levels = c("T1", "T2"))
#       run_anova(long_PEDSQL_ele_T1_T2, analysis_prefix = "Team", output_prefix = "T1_T2", ds_prefix = "PEDSQL_ele", time_levels = c("T1", "T2"))
#       run_anova(long_PEDSQL_par_T1_T2, analysis_prefix = "Team", output_prefix = "T1_T2", ds_prefix = "PEDSQL_par", time_levels = c("T1", "T2"))
#       run_anova(long_PEDSQL_T1_T2, analysis_prefix = "Team", output_prefix = "T1_T2", ds_prefix = "PEDSQL", time_levels = c("T1", "T2"))

      # Individual
      # run_anova(ind_long_T1_T2, analysis_prefix = "Ind", output_prefix = "T1_T2", ds_prefix = "z_scored", time_levels = c("T1", "T2"))
      # run_anova(ind_long_PEDSQL_ele_T1_T2, analysis_prefix = "Ind", output_prefix = "T1_T2", ds_prefix = "PEDSQL_ele", time_levels = c("T1", "T2"))
      # run_anova(ind_long_PEDSQL_par_T1_T2, analysis_prefix = "Ind", output_prefix = "T1_T2", ds_prefix = "PEDSQL_par", time_levels = c("T1", "T2"))


  # Descriptive stats
      
      # DVs definition
      descriptive_dvs <- c(
        "z_CUQ",
        "z_UEQ_Total", "z_UEQ_Prag", "z_UEQ_Hedo",
        "z_TENS_T_Total", "z_TENS_T_Comp", "z_TENS_T_Auto", "z_TENS_T_App",
        "z_TENS_I_Total", "z_TENS_I_Comp", "z_TENS_I_Auto", "z_TENS_I_App")
      
      # Cleaning and preparing
      long_desc <- long_T1_T2 %>%
        filter(grepl(paste(descriptive_dvs, collapse = "|"), DV)) %>%
        mutate(
          Time = gsub(".*_T([123])$", "T\\1", DV),
          DV = gsub("_T[123]$", "", DV)
        ) %>%
        filter(!is.na(Score))
      
      # Summary table - by Group
      desc_stats_by_group <- long_desc %>%
        group_by(DV, Time, Group) %>%
        summarise(
          N = n(),
          Mean = mean(Score),
          SD = sd(Score),
          SE = SD / sqrt(N),
          Median = median(Score),
          Min = min(Score),
          Max = max(Score),
          .groups = "drop")
      
      # Summary table - collapsed across groups
      desc_stats_overall <- long_desc %>%
        group_by(DV, Time) %>%
        summarise(
          N = n(),
          Mean = mean(Score),
          SD = sd(Score),
          SE = SD / sqrt(N),
          Median = median(Score),
          Min = min(Score),
          Max = max(Score),
          .groups = "drop")
      
      # Saving to Excel
      write_xlsx(
        list(
          ByGroup = desc_stats_by_group,
          Overall = desc_stats_overall
        ),
        path = "Output/Ind/Tables/T1_T2_T3/Descriptive_stats.xlsx")
      
      # Plot
      plot_list <- list()
      
      for (dv in unique(long_desc$DV)) {
        p <- long_desc %>%
          filter(DV == dv) %>%
          ggplot(aes(x = Time, y = Score, fill = Group)) +
          stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +
          stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.7), width = 0.2) +
          labs(title = paste("Mean ± SE for", dv), x = "Time", y = "Score") +
          theme_minimal(base_size = 14) +
          theme(legend.position = "top")
        
        plot_list[[dv]] <- p
      }
      
      # Combining and saving all plots
      ggexport(
        plotlist = plot_list,
        filename = "Output/Ind/Plots/T1_T2_T3/Descriptive_plots.pdf",
        width = 10, height = 6
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
