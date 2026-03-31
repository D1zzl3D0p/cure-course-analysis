library(tidyverse)
library(broom)

# reading in data
raw_labels_df <- 
  readxl::read_excel(
    paste0(data_dir,"Version 2 RQ- Adult Survey OO_March 24, 2026_11.53.xlsx"))
raw_values_df <- 
  read_csv(
    paste0(data_dir,"Version 2 RQ- Adult Survey OO_March 24, 2026_12.46.csv"))

# headers is the same for values and labels
headers <- raw_labels_df[1,] |> t()

# for reference so you can see what the survey takers saw on their question labels
labels_df <- raw_labels_df |> 
  slice(-1) |> 
  filter(!is.na(QID1) & QID1 != '.') |> 
  select(-`VBQ-Y`, -`VBQ-N`, -`Q-SIB-N`,
         -`Q-TBQ-Y`, -`Q-TBQ-N`, -`Q-OP-2`,
         -ends_with("TEXT"))

values_df <- raw_values_df |> 
  slice(-c(1,2)) |> 
  # remove test data, non-responses, and one guy who didn't finish the survey
  filter(!is.na(QID1) & QID1 != '.') |> 
  filter(!is.na(`Q-CE-2-1`)) |> 
  # remove free response survey data (not demographics though)
  select(-`VBQ-Y`, -`VBQ-N`, -`Q-SIB-N`,
         -`Q-TBQ-Y`, -`Q-TBQ-N`, -`Q-OP-2`,
         -ends_with("TEXT")) |> 
  # remove questions that weakly predict outcomes
  select(-`Q-DC-1-1`, -`Q-DC-1-2`, -`Q-DC-2`, 
         -`Q-DC-3`, -`Q-IC-1`, -`Q-IC-3`, -`Q-CE-1`, 
         -`Q-CE-2-2`, -`Q-T-4`, -`Q-T-5-1`, -`Q-T-5-2`) |> 
  #type_convert() |> 
  # have to flip this one because the text is wrong
  mutate(`Q-T-11-1` = case_match(`Q-T-11-1`,
    '5' ~ '1',
    '4' ~ '2',
    '3' ~ '3',
    '2' ~ '4',
    '1' ~ '5')) |>
  # Turn Q2 into a one-hot encoding
  mutate(row_id = row_number(),
         Q2 = as.character(Q2)) |>  
  separate_rows(Q2, sep = ",") |> 
  filter(Q2 != "") |> 
  mutate(Q2 = case_match(
    Q2,
    "1" ~ "American_Indian",
    "2" ~ "Asian",
    "3" ~ "African",
    "4" ~ "Hispanic",
    "5" ~ "Pacific",
    "6" ~ "White",
    "7" ~ "Other"
  )) |>  
  pivot_wider(
    names_from   = Q2,
    values_from  = Q2,
    values_fn    = length,
    values_fill  = 0,
    names_prefix = "Q2_",
    names_sort   = TRUE
  ) |>  
  select(-row_id) |> 
  mutate(Q1 = case_match(Q1,
                         '1' ~ 'Male',
                         '2' ~ 'Female',
                         '4' ~ 'Prefer not to say',
                         .default = Q1)) |>
  mutate(Q3 = case_match(Q3,
                         '1' ~ 'Salt Lake City',
                         '2' ~ 'Utah',
                         '3' ~ 'Davis',
                         '4' ~ 'Summit',
                         '5' ~ 'Washington',
                         '6' ~ 'Wasatch',
                         '7' ~ 'Other',
                         .default = Q3)) |>
  mutate(Q5 = case_match(Q5,
                         '1' ~ 'House',
                         '2' ~ 'Apartment',
                         '3' ~ 'Townhome',
                         '4' ~ 'Trailer',
                         '5' ~ 'Other',
                         .default = Q5)) |>
  mutate(Q7 = case_match(Q7,
                         '1' ~ 'English',
                         '2' ~ 'Spanish',
                         '3' ~ 'Other',
                         .default = Q7)) |>
  mutate(Q9 = case_match(Q9,
                         '2' ~ '0',
                         .default = Q9)) |>
  mutate(Q10 = case_match(Q10,
                          '2' ~ '0',
                          .default = Q10)) |>
  mutate(Q11 = case_match(Q11,
                          '1' ~ 'Yes',
                          '2' ~ 'No',
                          '3' ~ "I don't know",
                          .default = Q11)) |>
  mutate(Q12 = case_match(Q12,
                          '1' ~ 'Yes',
                          '2' ~ 'No',
                          '3' ~ "I don't know",
                          .default = Q12)) |>
  mutate(Q13 = case_match(Q13,
                          '1' ~ '< High School',
                          '2' ~ 'High School',
                          '3' ~ 'Some College',
                          '4' ~ 'Associates Degree',
                          '5' ~ 'Bachelors Degree',
                          '6' ~ 'Masters/Doctorate Degree',
                          '7' ~ "I don't know",
                          .default = Q13)) |>
  mutate(Q14 = case_match(Q14,
                          '1' ~ '< High School',
                          '2' ~ 'High School',
                          '3' ~ 'Some College',
                          '4' ~ 'Associates Degree',
                          '5' ~ 'Bachelors Degree',
                          '6' ~ 'Masters/Doctorate Degree',
                          '7' ~ "I don't know",
                          .default = Q14)) |>
  mutate(Q15 = case_match(Q15,
                          '1' ~ '< High School',
                          '2' ~ 'High School',
                          '3' ~ 'Some College',
                          '4' ~ 'Associates Degree',
                          '5' ~ 'Bachelors Degree',
                          '6' ~ 'Masters/Doctorate Degree',
                          '7' ~ "I don't know",
                          .default = Q15)) |>
  mutate(Q16 = case_match(Q16,
                          '2' ~ '0',
                          .default = Q16)) |>
  mutate(Q508 = case_match(Q508,
                           '1' ~ '< $30,000',
                           '2' ~ '$30,000 - $50,000',
                           '3' ~ '$50,000 - $100,000',
                           '4' ~ '$100,000 - $200,000',
                           '5' ~ '> $200,000',
                           .default = Q508)) |>
  mutate(across(matches("^Q[0-9]"), as.factor)) |> 
  #type_convert() |> 
  # remove people who fail the tests
  # this test basically fails everyone it comes in contact with, so we probably
  # need to change it. By the very fact that everybody put their names in right
  # I would assume that everyone answered in good faith.
  # mutate(
  #   failed_on = case_when(
  #     (`Q-T-5-1` > 3 & `Q-T-5-2` < 3) | (`Q-T-5-1` < 3 & `Q-T-5-2` > 3) ~ "Q-T-5",
  #     (`Q-T-11-1` > 3 & `Q-T-11-2` < 3) | (`Q-T-11-1` < 3 & `Q-T-11-2` > 3) ~ "Q-T-11",
  #     (`Q-V-2-1` > 3 & `Q-V-2-2` < 3) | (`Q-V-2-1` < 3 & `Q-V-2-2` > 3) ~ "Q-V-2",
  #     (`Q-V-11-1` > 3 & `Q-V-11-2` < 3) | (`Q-V-11-1` < 3 & `Q-V-11-2` > 3) ~ "Q-V-11",
  #     (`Q-SI-7-1` > 3 & `Q-SI-7-2` < 3) | (`Q-SI-7-1` < 3 & `Q-SI-7-2` > 3) ~ "Q-SI-7",
  #     (`Q-PH-1-1` > 3 & `Q-PH-1-2` < 3) | (`Q-PH-1-1` < 3 & `Q-PH-1-2` > 3) ~ "Q-PH-1",
  #     (`Q-PH-2-1` > 3 & `Q-PH-2-2` < 3) | (`Q-PH-2-1` < 3 & `Q-PH-2-2` > 3) ~ "Q-PH-2",
  #     (`Q-P-1-1` > 3 & `Q-P-1-2` < 3) | (`Q-P-1-1` < 3 & `Q-P-1-2` > 3) ~ "Q-P-1",
  #     (`Q-P-8-1` > 3 & `Q-P-8-2` < 3) | (`Q-P-8-1` < 3 & `Q-P-8-2` > 3) ~ "Q-P-8",
  #     (`Q-DC-1-1` > 3 & `Q-DC-1-2` < 3) | (`Q-DC-1-1` < 3 & `Q-DC-1-2` > 3) ~ "Q-DC-1",
  #     (`Q-CE-2-1` > 3 & `Q-CE-2-2` < 3) | (`Q-CE-2-1` < 3 & `Q-CE-2-2` > 3) ~ "Q-CE-2",
  #     .default = NA
  #   )
  # ) |> 
  
  # aggregate values
  mutate(
    t_avg = rowMeans(across(starts_with("Q-T-"), as.numeric), na.rm = TRUE),
    v_avg = rowMeans(across(starts_with("Q-V-"), as.numeric), na.rm = TRUE),
    si_avg = rowMeans(across(starts_with("Q-SI-"), as.numeric), na.rm = TRUE),
    ph_avg = rowMeans(across(starts_with("Q-PH-"), as.numeric), na.rm = TRUE),
    p_avg = rowMeans(across(starts_with("Q-P-"), as.numeric), na.rm = TRUE),
    ic_avg = rowMeans(across(starts_with("Q-IC-"), as.numeric), na.rm = TRUE),
    ce_avg = rowMeans(across(starts_with("Q-CE-"), as.numeric), na.rm = TRUE)
  )

# 1. Identify the predictors based on the regex and outcomes based on the suffix
predictors <- names(values_df) |> str_subset("^Q[0-9].*")
outcomes   <- names(values_df) |> str_subset("_avg$")

# See which predictors lose levels for each outcome
outcomes |> set_names() |> map(function(y) {
  complete <- values_df |> filter(!is.na(.data[[y]]))
  complete |> 
    select(all_of(predictors)) |> 
    select(where(is.factor)) |> 
    select(where(~ nlevels(droplevels(.x)) < 2)) |> 
    names()
})

# A more complex "glm" object that automatically filters out problematic collumns
# by factor
glms <- outcomes |> 
  set_names() |> 
  map(function(y) {
    complete <- values_df |> filter(!is.na(.data[[y]]))
    valid_preds <- predictors |> keep(~ {
      col <- complete[[.x]]
      !is.factor(col) || nlevels(droplevels(col)) >= 2
    })
    glm(reformulate(valid_preds, response = y), data = complete)
  })
# get the summaries
model_summaries <- map_dfr(glms, tidy, .id = "outcome") |> 
  filter(p.value <= 0.05 & term != "(Intercept)")

