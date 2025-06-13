# Dependencies
  source("C:/Users/isaintsu/Desktop/Tous-Ensemble/Scripts/04_analysis_T1_T2.R")
  

# Z-scored datasets loading
  full_T1_T2_T3 <- readxl::read_excel("Data/Processed/Team/T1_T2_T3/teams_T1_T2_T3_all.xlsx")
  ind_full_T1_T2_T3 <- readxl::read_excel("Data/Processed/Ind/T1_T2_T3/ind_T1_T2_T3_all.xlsx")
    
# Group column addition
  full_T1_T2_T3 <- add_group(full_T1_T2_T3)
  PEDSQL_ele_T1_T2_T3 <- add_group(PEDSQL_ele_T1_T2_T3)
  PEDSQL_par_T1_T2_T3 <- add_group(PEDSQL_par_T1_T2_T3)
  PEDSQL_T1_T2_T3 <- add_group(PEDSQL_T1_T2_T3)
  
# Domain aggregation (Relations, Burden/Burnout)
  # Team
  full_T1_T2_T3 <- aggregate_domains_clean(full_T1_T2_T3, timepoints = c("T1", "T2", "T3"))
  
  # Individual
  ind_full_T1_T2_T3_agg <- aggregate_domains_clean(ind_T1_T2_T3_all, timepoints = c("T1", "T2", "T3"))

  
# # Saving T1 -> T2 -> T3 dataset  
#   saveRDS(full_T1_T2_T3, "Data/Processed/full_T1_T2_T3_agg_grouped.rds")
#   write_xlsx(full_T1_T2_T3, "Data/Processed/full_T1_T2_T3_agg_grouped.xlsx")
#   

# Reshaping to long format
  # Team
  long_T1_T2_T3 <- reshape_long(full_T1_T2_T3)
  long_PEDSQL_ele_T1_T2_T3 <- reshape_long(PEDSQL_ele_T1_T2_T3)
  long_PEDSQL_par_T1_T2_T3 <- reshape_long(PEDSQL_par_T1_T2_T3)
  long_PEDSQL_T1_T2_T3 <- reshape_long(PEDSQL_T1_T2_T3)
  
  # Individual
  ind_long_T1_T2_T3 <- reshape_long(ind_full_T1_T2_T3_agg)
  ind_long_PEDSQL_ele_T1_T2_T3 <- reshape_long(PEDSQL_ele_ind_T1_T2_T3)
  ind_long_PEDSQL_par_T1_T2_T3 <- reshape_long(PEDSQL_par_ind_T1_T2_T3)
  
  ind_CUQ_adj_long <- reshape_long_with_cuq(ind_full_T1_T2_T3, cuq_time = "T1", covariate_prefix = "z_CUQ")
  ind_CUQ_adj_long_agg <- reshape_long_with_cuq(ind_full_T1_T2_T3_agg, cuq_time = "T1", covariate_prefix = "z_CUQ")

  
# ANOVA + Levene
  # Team
  # run_anova(long_T1_T2_T3, analysis_prefix = "Team", output_prefix = "T1_T2_T3", ds_prefix = "z_scored", time_levels = c("T1", "T2", "T3"))
  # run_anova(long_PEDSQL_ele_T1_T2_T3, analysis_prefix = "Team", output_prefix = "T1_T2_T3", ds_prefix = "PEDSQL_ele", time_levels = c("T1", "T2", "T3"))
  # run_anova(long_PEDSQL_par_T1_T2_T3, analysis_prefix = "Team", output_prefix = "T1_T2_T3", ds_prefix = "PEDSQL_par", time_levels = c("T1", "T2", "T3"))
  # run_anova(long_PEDSQL_T1_T2_T3, analysis_prefix = "Team", output_prefix = "T1_T2_T3", ds_prefix = "PEDSQL", time_levels = c("T1", "T2", "T3"))
  # 
  # Individual
  # run_anova(ind_long_T1_T2_T3, analysis_prefix = "Ind", output_prefix = "T1_T2_T3", ds_prefix = "z_scored", time_levels = c("T1", "T2", "T3"))
  # run_anova(ind_long_PEDSQL_ele_T1_T2_T3, analysis_prefix = "Ind", output_prefix = "T1_T2_T3", ds_prefix = "PEDSQL_ele", time_levels = c("T1", "T2", "T3"))
  # run_anova(ind_long_PEDSQL_par_T1_T2_T3, analysis_prefix = "Ind", output_prefix = "T1_T2_T3", ds_prefix = "PEDSQL_par", time_levels = c("T1", "T2", "T3"))


# ANCOVA
  # Individual
  write_xlsx(ind_CUQ_adj_long, "Data/Processed/Ind/T1_T2_T3/ind_CUQ_adj_long.xlsx")
  run_anova(ind_CUQ_adj_long_agg, analysis_prefix = "Ind", output_prefix = "T1_T3", ds_prefix = "CUQ_adjusted_agg", time_levels = c("T1", "T2", "T3"))


  # Running the post hoc tests for z_BPNSFS_Comp, z_WQOL_T, z_WQOL_G, z_WQOL_Phy
    significant_anova_dvs <- c("z_BPNSFS_Comp",
                         "z_WQOL_T",
                         "z_WQOL_G",
                         "z_WQOL_Phy")

    ind_long_T1_T2_T3 <- ind_long_T1_T2_T3 %>%
      mutate(
        Score = as.numeric(gsub(",", ".", Score)))
    
    plot_dir <- "Output/Ind/Plots/T1_T2_T3/Posthoc_ANOVA/"

    wb <- createWorkbook()
    
    for (dv in significant_anova_dvs) {
      message("Processing DV: ", dv)
      
      # Filtering dataset for current DV
      df_dv <- ind_long_T1_T2_T3 %>%
        filter(DV == dv, !is.na(Score), !is.na(Group), !is.na(Time))
      
      model <- aov(Score ~ Group * Time + Error(PersonID/Time), data = df_dv)
      
      # Intra analysis
      emm_time <- emmeans(model, ~ Time | Group)
      pairs_time <- contrast(emm_time, method = "pairwise", adjust = "bonferroni")
      df_time <- as.data.frame(pairs_time)
      

      # Inter
      emm_group <- emmeans(model, ~ Group | Time)
      pairs_group <- contrast(emm_group, method = "pairwise", adjust = "bonferroni")
      df_group <- as.data.frame(pairs_group)

      # Combining results and writing to Excel
      sheetname_time <- paste0(dv, "_Time_Post_hoc")
      sheetname_group <- paste0(dv, "_Group_Post_hoc")
      
      addWorksheet(wb, sheetname_time)
      addWorksheet(wb, sheetname_group)
      
      writeData(wb, sheetname_time, df_time)
      writeData(wb, sheetname_group, df_group)
      
      # Plot generation
      plot <- ggplot(df_dv, aes(x = Time, y = Score, color = Group, group = Group)) +
        stat_summary(fun = mean, geom = "line", size = 1.1) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Mean ± SE for", dv), y = "Score", x = "Time") +
        theme(legend.position = "top")
      
      ggsave(file.path(plot_dir, paste0("Plot_", dv, ".jpg")),
             plot = plot, width = 8, height = 5)
    }
    
    # Saving Excel workbook
    saveWorkbook(wb, "Output/Ind/Tables/T1_T2_T3/Posthoc_ANOVA/all_results.xlsx", overwrite = TRUE)
    
    