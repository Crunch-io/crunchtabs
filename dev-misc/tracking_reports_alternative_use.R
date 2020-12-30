library(crunchtabs)
login()

places <- sort(c("St Louis", "Memphis", "Detroit", "Charlotte", "Baltimore", "Birmingham"))
ds_names <- sprintf("Campaign Zero %s October 2020 testing fork", places)
ds_names[3] <- "Campaign Zero Charlotte November 2020"
ds_list <- lapply(ds_names, loadDataset)
# civil_assets_
vars <- "noknock_supop, asset_forfeiture_reask, compliance_window, officers_visible"
vars <- unlist(strsplit(vars, ", "))

ct <- tracking_report(ds_list, wave_label = places, 
                      vars = vars)

writeLatex(ct, pdf = TRUE)

# - When data are available for a single wave only, present that wave
# - When data are avilable for multiple waves (but not all n waves) present those t

# Jimmy's datasets
library(crunchtabs)
login()

pre <- loadDataset("Oregon 2020 Election Polling - Pre-Election BEB Fork")
post <- loadDataset("Oregon 2020 Election Polling - Post-Election BEB Fork")
vars <- c(
  "RIGHTTRACK", 
  "RETURNMODE", 
  "RETURNPROBLEMS", 
  #  "RETURNPROBLEMSOPEN", 
  "BALLOTPROBLEMS", 
  #  "BALLOTPROBLEMSOPEN", 
  "PAMPHLET", 
  "PAMPHLETUSE",
  "PAMPHLETNOUSE", 
  #  "PAMPHLETNOUSE_other", 
  "TIMEOFVOTE", 
  "WHYEARLY_m", 
  #  "WHYEARLY_other", 
  "VBMCONCERNS", 
  "VOTERCONFIDENCE", 
  "FRAUDOR", 
  "FRAUDUS"
)

toplines_theme <- themeNew(
  default_theme = themeDefaultLatex(),
  # logo = list(file = "yougov-logo"),
  format_title = list(decoration = "bold"),
  format_subtitle = list(decoration = "bold", font_size = 14),
  format_var_description = list(include_q_number = TRUE, background_color = "gray"),
  format_var_filtertext = list(decoration = "italic", font_size = 8),
  latex_headtext = "tbc", 
  latex_foottext = "tbc",
  latex_round_percentages = FALSE,
  latex_max_lines_for_tabular = 15,
  latex_table_align = 'g',
  format_totals_row = NULL,
  format_unweighted_n = NULL,
  format_weighted_n = NULL,
  one_per_sheet = FALSE,
  latex_flip_grids = TRUE
)

ct <- tracking_report(list(pre,post), wave_label = c("Pre", "Post"), vars = c("RIGHTTRACK", "PAMPHLETUSE","WHYEARLY_m"))
writeLatex(ct, theme=toplines_theme, pdf = TRUE)

# Malecki, look at 
