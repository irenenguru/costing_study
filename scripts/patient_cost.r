library(dplyr)
library(purrr)
library(tibble)
library(gtsummary)

# Minimum monthly wage in 2025 survey 14427
mon_min_wage <- 14427

# Monthly work hrs
mon_work_hrs <- 8 * 22

# Minimum hourly wage
hr_min_wage_ksh <- mon_min_wage / mon_work_hrs

# Minimum hourly wage in USD
exchange_rate <- 128  # KSH to USD exchange rate
hr_min_wage_usd <- hr_min_wage_ksh / exchange_rate

# ----------------------- Begin analysis ----------------------- #
# List columns with their respective col number
# for (i in seq_len(ncol(patient_costing))) {
#   cat(i, names(patient_costing)[i], "\n")
# }

# Create groups of costing
groups <- list(
  initial_visit = list(
    direct_costs = 22:26,
    # Changed non direct other (42) to transport based on data exploration
    non_health_transport = c(20, 42, 46),
    non_health_food = c(39, 47),
    non_health_other = 40:41,
    `indirect_costs_trans(min)` = c(15, 18),
    `indirect_costs_wait(min)` = c(16, 19)
  ),
  outpatient_visit = list(
    direct_costs = c(212:235, 245:268),
    non_health_transport = c(281:284, 286:289),
    non_health_food = 290:293,
    non_health_other = 294:297,
    `indirect_costs(min)` = c(208:211, 277:280, 306:309)
  ),
  inpatient_visit = list(
    direct_costs = 482:505,
    non_health_transport = c(518:521, 523:526),
    non_health_food = c(527:530),
    non_health_other = c(531:538),
    `indirect_costs(min)` = c(478:481, 514:517, 547:550)
  ),
  diagnosis = list(
    direct_costs = 511
  )
)

# Quality control for non-numeric columns
# Subset the group columns
cost_cols <- groups |>
  unlist(recursive = TRUE, use.names = FALSE) |>
  unique()

# Capture conversion issues
conversion_issues <- patient_costing |>
  select(all_of(cost_cols)) |>
  mutate(
    row_id = row_number()
  ) |>
  pivot_longer(
    -row_id,
    names_to = "column",
    values_to = "original_value"
  ) |>
  mutate(
    numeric_value = suppressWarnings(as.numeric(original_value)),
    failed = is.na(numeric_value) & !is.na(original_value)
  ) |>
  filter(failed)

# Log the issues
# There are none so far
# write.csv(
#   conversion_issues,
#   file = "./logs/numeric_conversion_failures.csv",
#   row.names = FALSE
# )

# Convert non-numeric columns to numeric
patient_costing_clean <- patient_costing |>
  mutate(
    across(
      all_of(cost_cols),
      ~ as.numeric(.x)
    )
  )


# Sum the costs for each group
patient_costing_summary <- patient_costing_clean |>
  bind_cols(
    imap_dfc(groups, \(visit_groups, visit_name) {

      imap_dfc(visit_groups, \(cols, leaf_name) {

        tibble(
          !!paste0("sum_", visit_name, "_", leaf_name) :=
            rowSums(
              patient_costing_clean[, cols, drop = FALSE],
              na.rm = TRUE
            )
        )
      })
    })
  )

# Subset the relevant columns
patient_costing_1 <- patient_costing_summary |>
  select(record_id, gender, p_age, name_of_facility, starts_with("sum_"))

# write.csv(
#   patient_costing_1,
#   file = "./results/patient_costing_summary.csv",
#   row.names = FALSE
# )

# Presenting the data in gtsummary table
patient_costing_2 <- patient_costing_1 |>
  select(-contains(c("outpatient_visit", "inpatient_visit", "diagnosis"))) |>
  mutate(
    across(
      .cols = c(
        sum_initial_visit_direct_costs,
        sum_initial_visit_non_health_transport,
        sum_initial_visit_non_health_food,
        sum_initial_visit_non_health_other
      ),
      .fns = ~ (.x * 13) / exchange_rate, # Annualize the costs based on 13 visits per year
      .names = "{.col}_usd"
    ),
    subtotal_direct_non_health_usd = (
      sum_initial_visit_direct_costs_usd +
      sum_initial_visit_non_health_transport_usd +
      sum_initial_visit_non_health_food_usd +
      sum_initial_visit_non_health_other_usd
    ),
    total_indirect_cost_usd = (
      ((`sum_initial_visit_indirect_costs_trans(min)` * 2) +
       `sum_initial_visit_indirect_costs_wait(min)`) / 60 * hr_min_wage_usd),
    grand_total_usd = subtotal_direct_non_health_usd + total_indirect_cost_usd
  )


# Define labels
cost_labels <- list(
  sum_initial_visit_direct_costs_usd ~ "Direct health care costs",
  sum_initial_visit_non_health_transport_usd ~ "Transport",
  sum_initial_visit_non_health_food_usd ~ "Food",
  sum_initial_visit_non_health_other_usd ~ "Other non-health costs",
  subtotal_direct_non_health_usd ~ "Subtotal (Direct Costs)",
  total_indirect_cost_usd ~ "Indirect costs",
  grand_total_usd ~ "Direct + Indirect costs"
)

# Check for normality of the cost variables
# Extract column names from the formula list
col_names <- sapply(cost_labels, function(f) as.character(f)[2])

normality_results <- patient_costing_2 |>
  select(all_of(col_names)) |>
  summarise(
    across(
      everything(),
      ~ shapiro.test(.x)$p.value,
      .names = "{.col}_p_value"
    )
  )

# 1. Mean + CI Table
table_mean <- patient_costing_2 |>
  filter(!is.na(p_age)) |>
  select(
    sum_initial_visit_direct_costs_usd,
    sum_initial_visit_non_health_transport_usd,
    sum_initial_visit_non_health_food_usd,
    sum_initial_visit_non_health_other_usd,
    subtotal_direct_non_health_usd,
    total_indirect_cost_usd,
    grand_total_usd
  ) |>
  tbl_summary(
    type = ends_with("usd") ~ "continuous",
    statistic = all_continuous() ~ "{mean}",
    label = cost_labels
  ) |>
  add_ci(pattern = "{stat} ({ci})") |>
  modify_header(stat_0 ~ "**Mean US$ (95% CI)**")

# 2. Median + IQR Table
table_median <- patient_costing_2 |>
  filter(!is.na(p_age)) |>
  select(all_of(names(table_mean$inputs$data))) |> # Keep columns identical
  tbl_summary(
    type = ends_with("usd") ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    label = cost_labels
  ) |>
  modify_header(stat_0 ~ "**Median US$ (IQR)**")

# 3. Merge and Style
final_costing_table <- tbl_merge(
  tbls = list(table_mean, table_median),
  tab_spanner = c(" ", " ")
) |>
  modify_table_styling(
    column = label,
    rows = variable %in% c("subtotal_direct_non_health_usd", "grand_total_usd"),
    text_format = "bold"
  ) |>
  modify_table_styling(
    column = label,
    rows = !variable %in% c("subtotal_direct_non_health_usd", "grand_total_usd", "total_indirect_cost_usd"),
    text_format = "bold"
  )

  gt::gtsave(
    as_gt(final_costing_table),
    filename = "./results/patient_costing_summary.docx"
  )
