library(tidyverse)

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
  type_convert() |> 
  # have to flip this one because the text is wrong
  mutate(`Q-T-11-1` = case_match(`Q-T-11-1`,
    5 ~ 1,
    4 ~ 2,
    3 ~ 3,
    2 ~ 4,
    1 ~ 5)) |> 
  # remove people who fail the tests
  # this test basically fails everyone it comes in contact with, so we probably
  # need to change it. By the very fact that everybody put their names in right
  # I would assume that everyone answered in good faith.
  mutate(
    failed_on = case_when(
      (`Q-T-5-1` > 3 & `Q-T-5-2` < 3) | (`Q-T-5-1` < 3 & `Q-T-5-2` > 3) ~ "Q-T-5",
      (`Q-T-11-1` > 3 & `Q-T-11-2` < 3) | (`Q-T-11-1` < 3 & `Q-T-11-2` > 3) ~ "Q-T-11",
      (`Q-V-2-1` > 3 & `Q-V-2-2` < 3) | (`Q-V-2-1` < 3 & `Q-V-2-2` > 3) ~ "Q-V-2",
      (`Q-V-11-1` > 3 & `Q-V-11-2` < 3) | (`Q-V-11-1` < 3 & `Q-V-11-2` > 3) ~ "Q-V-11",
      (`Q-SI-7-1` > 3 & `Q-SI-7-2` < 3) | (`Q-SI-7-1` < 3 & `Q-SI-7-2` > 3) ~ "Q-SI-7",
      (`Q-PH-1-1` > 3 & `Q-PH-1-2` < 3) | (`Q-PH-1-1` < 3 & `Q-PH-1-2` > 3) ~ "Q-PH-1",
      (`Q-PH-2-1` > 3 & `Q-PH-2-2` < 3) | (`Q-PH-2-1` < 3 & `Q-PH-2-2` > 3) ~ "Q-PH-2",
      (`Q-P-1-1` > 3 & `Q-P-1-2` < 3) | (`Q-P-1-1` < 3 & `Q-P-1-2` > 3) ~ "Q-P-1",
      (`Q-P-8-1` > 3 & `Q-P-8-2` < 3) | (`Q-P-8-1` < 3 & `Q-P-8-2` > 3) ~ "Q-P-8",
      (`Q-DC-1-1` > 3 & `Q-DC-1-2` < 3) | (`Q-DC-1-1` < 3 & `Q-DC-1-2` > 3) ~ "Q-DC-1",
      (`Q-CE-2-1` > 3 & `Q-CE-2-2` < 3) | (`Q-CE-2-1` < 3 & `Q-CE-2-2` > 3) ~ "Q-CE-2",
      .default = NA
    )
  ) |> 
  # aggregate values
  mutate(
    t_avg = rowMeans(pick(starts_with("Q-T-")), na.rm = TRUE),
    v_avg = rowMeans(pick(starts_with("Q-V-")), na.rm = TRUE),
    si_avg = rowMeans(pick(starts_with("Q-SI-")), na.rm = TRUE),
    ph_avg = rowMeans(pick(starts_with("Q-PH-")), na.rm = TRUE),
    p_avg = rowMeans(pick(starts_with("Q-P-")), na.rm = TRUE),
    dc_avg = rowMeans(pick(starts_with("Q-DC-")), na.rm = TRUE),
    ic_avg = rowMeans(pick(starts_with("Q-IC-")), na.rm = TRUE),
    ce_avg = rowMeans(pick(starts_with("Q-CE-")), na.rm = TRUE)
    )
