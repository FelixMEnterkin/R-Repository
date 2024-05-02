#  imbalance analysis short bandwidth

var_list <-
  c(
    "agea",
    "gndr",
    "eduyrs",
    "blgetmg"
  )

#create a list to store the results
tdv_list <- list()

#loop through the variable names and run the t-test
for(i in var_list){
  tdv <- t.test(as.formula(paste(i,"~ Treatment")), data = ESSRU)
  tdv_list[[i]] <- tdv
}
#print results
for(i in var_list){
  print(tdv_list[[i]])
}

summary_list <- list()

#loop through the variable names and run the t-test
for(i in var_list){
  tdv <- t.test(as.formula(paste(i,"~ Treatment")), data = ESSRU_with_NA)
  summary_result <- tibble(
    Variable = i,
    estimate = tdv$estimate,
    statistic = tdv$statistic,
    p.value = tdv$p.value
  )
  summary_list[[i]] <- summary_result
}


# Repeat for each bandwidth 



## GLM on imbalances

iba <-
  glm(
    formula = Treatment ~ agea + gndr + eduyrs  + blgetmg,
    family = binomial(link = "logit"),
    data = ESSRU
  )

summary(iba)

stargazer(iba,  type="text",
          covariate.labels = 
            c("Age", "Gender", "Education Years", "Ethnic Minority"),
          dep.var.labels="Treatment status", keep.stat=c("aic", "n"), 
          notes = "russia, treatment = interviewed before January 24, 2011, Two tailed tests, Standard Errors in parentheses",
          out="Imbalance_2009.doc")
# Trust
trust_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(Trust))/n())

# Obligation
obligation_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    bplcdc_missing_pct = sum(is.na(bplcdc))/n(),
    doplcsy_missing_pct = sum(is.na(doplcsy))/n(),
    dpcstrb_missing_pct = sum(is.na(dpcstrb))/n()
  )

# Moral alignment
moral_alignment_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrgwr_missing_pct = sum(is.na(plcrgwr))/n(),
    plcipvl_missing_pct = sum(is.na(plcipvl))/n(),
    gsupplc_missing_pct = sum(is.na(gsupplc))/n()
  )

# Lawfulness
lawfulness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plciplt))/n())

# Procedural fairness
procedural_fairness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrspc_missing_pct = sum(is.na(plcrspc))/n(),
    plcfrdc_missing_pct = sum(is.na(plcfrdc))/n(),
    plcexdc_missing_pct = sum(is.na(plcexdc))/n()
  )

# Police effectiveness
police_effectiveness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plcpvcr))/n())

# Combine all the results into a single table
combined_results <- full_join(trust_results, obligation_results, by = "Treatment") |>
  full_join(moral_alignment_results, by = "Treatment") |>
  full_join(lawfulness_results, by = "Treatment") |>
  full_join(procedural_fairness_results, by = "Treatment") |>
  full_join(police_effectiveness_results, by = "Treatment")

# Round the results to 3 decimal places
rounded_results <- combined_results |>
  mutate(across(everything(), ~ round(., 3)))

# Pivot to long data table
long_table <- rounded_results |>
  pivot_longer(cols = -Treatment, names_to = "Category", values_to = "Missing_Pct") |>
  mutate(Group = ifelse(Treatment == "Treatment", "Treatment", "Control"))

# Spread the table to have separate columns for Treatment 0 and Treatment 1
spread_table <- long_table |>
  spread(key = Treatment, value = Missing_Pct)

flextable(spread_table)

