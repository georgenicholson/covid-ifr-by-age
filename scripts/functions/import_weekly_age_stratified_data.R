####################################
# Import weekly age-stratifed data
####################################
import_weekly_age_stratified_data <- function(control, react_age_strat_dir = "data/REACT_region_age_week_aggregated") {
  testing <- T
  if (testing) {
    react_age_strat_dir = "data/REACT_region_age_week_aggregated"
    control$Pillar2_age_strat_N_file_in = "data/P1_P2_age_stratified_weekly_test_counts_UTLA_to_2021-03-10.csv"
    control$Pillar2_age_strat_n_file_in = "data/P1_P2_age_stratified_weekly_positive_test_counts_UTLA_to_2021-03-10.csv"
  }

  #########################################
  # Import REACT
  # Source : https://github.com/mrc-ide/reactidd/tree/master/inst/extdata/region_age_week_aggregated
  #########################################

  react_age_files <- list.files(react_age_strat_dir)
  react_age_files <- react_age_files[grepl("_go", react_age_files)]
  d_react <- data.frame()
  for (react_file in react_age_files) {
    add <- read.csv(paste0(react_age_strat_dir, "/", react_file), stringsAsFactors = F)
    d_react <- rbind(d_react, add)
  }
  d_react$region <- gsub("Yorkshire and The Humber", "Yorkshire and Humber", d_react$region)

  d_react$nr <- d_react$number_positive
  d_react$Nr <- d_react$number_samples
  d_react$mid_week_in <- as.character(as.Date(d_react$react_week_start_date) + 3)
  d_react$name <- d_react$phe_region <- d_react$region
  # shifting to coincide with Pillar 1+2 weeks
  d_react$mid_week <- as.character(as.Date(d_react$react_week_start_date) + 6)

  #########################################
  # Import Pillar 2
  # Source: https://www.gov.uk/government/publications/weekly-statistics-for-nhs-test-and-trace-england-4-march-to-10-march-2021
  #########################################
  Nin <- read.csv(control$Pillar2_age_strat_N_file_in, stringsAsFactors = F)
  nin <- read.csv(control$Pillar2_age_strat_n_file_in, stringsAsFactors = F)
  Nin$Region.Name <- gsub("Yorkshire and The Humber", "Yorkshire and Humber", Nin$Region.Name)
  nin$Region.Name <- gsub("Yorkshire and The Humber", "Yorkshire and Humber", nin$Region.Name)
  if(!all(dim(nin) == dim(Nin)))
    stop("Dimensions of input matrices for N and n differ")
  data_col_indices <- which(grepl("X", colnames(Nin)) & (grepl("20", colnames(Nin)) | grepl("21", colnames(Nin))))
  N <- as.matrix(Nin[, data_col_indices])
  n <- as.matrix(nin[, data_col_indices])
  N <- gsub("\\*", "0", gsub(",", "", gsub(" ", "", N)))
  n <- gsub("\\*", "0", gsub(",", "", gsub(" ", "", n)))
  mode(N) <- "integer"
  mode(n) <- "integer"
  if(!all(Nin$UTLA.Name == nin$UTLA.Name))
    stop("UTLA rows differ between input files")

  week_intervals_in <- gsub("X", "", colnames(Nin[, data_col_indices]))
  n_weeks <- length(week_intervals_in)
  start_week_one <- sapply(strsplit(week_intervals_in, split = "\\."), function(v) paste(v[1:3], collapse = "-"))[1]
  start_week_one_as_date <- as.Date(strptime(start_week_one, format = "%d-%m-%y"))
  mid_week_one_as_date <- start_week_one_as_date + 3
  all_mid_weeks <- mid_week_one_as_date + 0:(n_weeks - 1) * 7
  dimnames(N) <- dimnames(n) <- list(paste0(Nin$UTLA.Name, "_", Nin$Agegrp), all_mid_weeks)
  age_grp_unique <- unique(Nin$Agegrp)
  n_age_grp <- length(age_grp_unique)
  dweek_utla <- data.frame(utla = rep(Nin$UTLA.Name, each = n_weeks),
                           phe_region = rep(Nin$Region.Name, each = n_weeks),
                           Nt = c(t(N)), nt = c(t(n)), stringsAsFactors = F,
                           mid_week = rep(as.character(all_mid_weeks), times = nrow(Nin)),
                           age_group = rep(Nin$Agegrp, each = n_weeks))
  dweek_utla$name <- dweek_utla$utla
  phe_region_all <- unique(Nin$Region.Name)

  ##########################################################
  # Grouping age bands
  #################################################################
  age_grp_unique_pillar12 <- unique(dweek_utla$age_group)
  age_grp_unique_react <- unique(d_react$age_group)
  age_band_map <- list('0-19' = list(react = c("5-12", "13-17"), p12 = c("0-9", "10-19"), react_range = c(5, 17), p12_range = c(0, 19)),
                       '20-29' = list(react = c("18-24"), p12 = c("20-29"), react_range = c(18, 24), p12_range = c(20, 29)),
                       '30-39' = list(react = c("25-34"), p12 = c("30-39"), react_range = c(25, 34), p12_range = c(30, 39)),
                       '40-49' = list(react = c("35-44"), p12 = c("40-49"), react_range = c(35, 44), p12_range = c(40, 49)),
                       '50-59' = list(react = c("45-54"), p12 = c("50-59"), react_range = c(45, 54), p12_range = c(50, 59)),
                       '60-69' = list(react = c("55-64"), p12 = c("60-69"), react_range = c(55, 64), p12_range = c(60, 69)),
                       '70-101' = list(react = c("65+", "(64,101]"), p12 = c("70-79", "80-89", "90+"), react_range = c(65, 101), p12_range = c(70, 101)))
  # ,
  #                      '0-101' = list(react = c("5-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "(64,101]"),
  #                                     p12 = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
  #                                     react_range = c(0, 101), p12_range = c(0, 101)))
  dweek_utla$age_grp_merged <- d_react$age_grp_merged <- NA
  for (j in 1:length(age_band_map)) {
    dweek_utla$age_grp_merged[dweek_utla$age_group %in% age_band_map[[j]]$p12] <- names(age_band_map)[j]
    d_react$age_grp_merged[d_react$age_group %in% age_band_map[[j]]$react] <- names(age_band_map)[j]
  }

  dweek_utla$name_date_agemerged <- paste(dweek_utla$name, dweek_utla$mid_week, dweek_utla$age_grp_merged, sep = "_")
  dweek_utla$name_date_agemerged_code <- match(dweek_utla$name_date_agemerged, unique(dweek_utla$name_date_agemerged))
  d_react$name_date_agemerged <- paste(d_react$name, d_react$mid_week, d_react$age_grp_merged, sep = "_")
  dweek_utla_age_merged <- dweek_utla
  dweek_utla_age_merged[, c("Nt", "nt", "age_group")] <- NA
  dweek_utla_age_merged <- unique(dweek_utla_age_merged)
  dweek_utla_age_merged[, "Nt"] <- sapply(dweek_utla_age_merged$name_date_agemerged_code, function(x)
        sum(dweek_utla[which(dweek_utla$name_date_agemerged_code == x), "Nt"]))
  dweek_utla_age_merged[, "nt"] <- sapply(dweek_utla_age_merged$name_date_agemerged_code, function(x)
    sum(dweek_utla[which(dweek_utla$name_date_agemerged_code == x), "nt"]))
  d_react_age_merged <- d_react
  d_react_age_merged[, c("Nr", "nr", "age_group")] <- NA
  d_react_age_merged <- unique(d_react_age_merged)
  d_react_age_merged[, "Nr"] <- sapply(d_react_age_merged$name_date_agemerged, function(x)
    sum(d_react[which(d_react$name_date_agemerged == x), "Nr"]))
  d_react_age_merged[, "nr"] <- sapply(d_react_age_merged$name_date_agemerged, function(x)
    sum(d_react[which(d_react$name_date_agemerged == x), "Nr"]))
  dweek_utla_age_merged$phe_midweek_agemerged <- paste(dweek_utla_age_merged$phe_region, dweek_utla_age_merged$mid_week, dweek_utla_age_merged$age_grp_merged, sep = "_")
  dweek_phe_region <- dweek_utla_age_merged
  dweek_phe_region$utla <- dweek_phe_region$Nt <- dweek_phe_region$nt <- dweek_phe_region$name <- dweek_phe_region$name_date_agemerged <-
    dweek_phe_region$name_date_agemerged_code <- NA
  dweek_phe_region <- unique(dweek_phe_region)
  dweek_phe_region$Nt <- sapply(dweek_phe_region$phe_midweek_agemerged, function(x) sum(dweek_utla_age_merged[dweek_utla_age_merged$phe_midweek_agemerged == x, "Nt"]))
  dweek_phe_region$nt <- sapply(dweek_phe_region$phe_midweek_agemerged, function(x) sum(dweek_utla_age_merged[dweek_utla_age_merged$phe_midweek_agemerged == x, "nt"]))
  dweek_phe_region$name <- dweek_phe_region$phe_region

  dweek_phe_region[dweek_phe_region$name == "London", ]

  ######################################################################################
  # Bring dweek_utla_age_merged, dweek_utla_age_merged and d_react together
  ######################################################################################
  d_comb <- rbind(dweek_utla_age_merged, dweek_phe_region)
  d_comb <- d_comb[!is.na(d_comb$age_grp_merged), ] # Removing NAs arising from age_group "Not Stated" category
  d_comb$name_date_agemerged <- paste(d_comb$name, d_comb$mid_week, d_comb$age_grp_merged, sep = "_")
  d_comb <- d_comb[order(d_comb$mid_week), ]
  mid_week_unique <- unique(d_comb$mid_week)
  d_comb <- d_comb[, c("utla", "phe_region", "Nt", "nt", "mid_week",
                       "name", "age_grp_merged", "name_date_agemerged")]
  d_comb[, c("Nr", "nr")] <- d_react[match(d_comb$name_date_agemerged, d_react$name_date_agemerged), c("Nr", "nr")]

  ######################################################################################
  # Source: https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls
  ######################################################################################
  unique_regions <- unique(d_comb$name)
  pop_size_by_age <- read.csv("data/ons_pop_sizes_by_age.csv", stringsAsFactors = F)
  colnames(pop_size_by_age) <- gsub("X90.", "X90", colnames(pop_size_by_age))
  pop_size_by_age$Name <- gsub("YORKSHIRE AND THE HUMBER", "YORKSHIRE AND HUMBER", pop_size_by_age$Name)
  pop_size_by_age$Name <- gsub("EAST", "EAST OF ENGLAND", pop_size_by_age$Name)
  pop_size_by_age$Name <- gsub("SOUTH EAST OF ENGLAND", "SOUTH EAST", pop_size_by_age$Name)
  pop_size_by_age$Name <- gsub("NORTH EAST OF ENGLAND", "NORTH EAST", pop_size_by_age$Name)
  pop_size_by_age$Name <- gsub("EAST OF ENGLAND MIDLANDS", "EAST MIDLANDS", pop_size_by_age$Name)
  pop_size_by_age$Name_fixed <- pop_size_by_age$Name
  for (phe_region in control$phe_region_unique) {
    pop_size_by_age$Name_fixed[toupper(phe_region) == pop_size_by_age$Name] <- phe_region
  }
  d_comb$age_lower <- as.numeric(sapply(strsplit(d_comb$age_grp_merged, split = "-"), function(x) x[1]))
  d_comb$age_upper <- as.numeric(sapply(strsplit(d_comb$age_grp_merged, split = "-"), function(x) x[2]))
  d_comb$age_lower_react <- sapply(age_band_map[na.omit(d_comb$age_grp_merged)], function(x) x$react_range[1])
  d_comb$age_upper_react <- sapply(age_band_map[na.omit(d_comb$age_grp_merged)], function(x) x$react_range[2])
  d_comb$age_lower_p12 <- sapply(age_band_map[na.omit(d_comb$age_grp_merged)], function(x) x$p12_range[1])
  d_comb$age_upper_p12 <- sapply(age_band_map[na.omit(d_comb$age_grp_merged)], function(x) x$p12_range[2])
  d_comb$age_upper <- as.numeric(sapply(strsplit(d_comb$age_grp_merged, split = "-"), function(x) x[2]))
  d_comb$M <- d_comb$M_react <- d_comb$M_p12 <- 0
  for (age in 0:90) {
    popsize_of_age_curr <- pop_size_by_age[match(tolower(d_comb$name), tolower(pop_size_by_age$Name_fixed)), paste0("X", age)]
    in_age_band_logical <- d_comb$age_lower <= age & d_comb$age_upper >= age
    d_comb$M <- d_comb$M + ifelse(in_age_band_logical, popsize_of_age_curr, 0)
    in_age_band_logical_p12 <- d_comb$age_lower_p12 <= age & d_comb$age_upper_p12 >= age
    d_comb$M_p12 <- d_comb$M_p12 + ifelse(in_age_band_logical_p12, popsize_of_age_curr, 0)
    in_age_band_logical_react <- d_comb$age_lower_react <= age & d_comb$age_upper_react >= age
    d_comb$M_react <- d_comb$M_react + ifelse(in_age_band_logical_react, popsize_of_age_curr, 0)
  }

  d_comb$name_agemerged <- paste0(d_comb$name, "_", d_comb$age_grp_merged)
  d_comb$phe_region_agemerged <- paste0(d_comb$phe_region, "_", d_comb$age_grp_merged)
  check_double_count <- F
  if (check_double_count) {
    ############################################
    # Check double counting
    ############################################
    d_lon <- d_comb[d_comb$name == "London", ]
    age_grp_unique <- unique(d_lon$age_grp_merged)
    pop_size_check <- read.csv("data/population_size_from_ons.csv", stringsAsFactors = F)
    pop_size_check[pop_size_check$Name == "LONDON", ]
    sum(d_lon[match(age_grp_unique, d_lon$age_grp_merged), "M"])
    sum(d_lon[match(age_grp_unique, d_lon$age_grp_merged), "M_p12"])
    sum(d_lon[match(age_grp_unique, d_lon$age_grp_merged), "M_react"])
  }

  region_unique <- unique(d_comb$phe_region)
  utla_unique <- na.omit(unique(d_comb$utla))
  age_grp_unique <- unique(d_comb$age_grp_merged)

  d_coarse <- d_comb[d_comb$name %in% region_unique &
                             d_comb$age_grp_merged %in% age_grp_unique, ]
  d_coarse$utla <- NA
  d_coarse$name <- d_coarse$phe_region# <- paste0(phe_region_of_utla_to_debias, "_", age_grp_to_debias)
  d_coarse$phe_region_namdat <- paste0(d_coarse$phe_region, "_", d_coarse$mid_week)
  d_coarse <- d_coarse[, !names(d_coarse) == "utla"]
  d_fine <- d_comb[d_comb$name %in% utla_unique &
                           d_comb$age_grp_merged %in% age_grp_unique, ]
  d_fine$name <- d_fine$utla# <- paste0(utla_to_debias, "_", age_grp_to_debias)
  # d_fine$phe_region <- paste0(phe_region_of_utla_to_debias, "_", age_grp_to_debias)
  d_fine$phe_region_namdat <- paste0(d_fine$phe_region, "_", d_fine$mid_week)

  names(d_coarse)[names(d_coarse) == "age_grp_merged"] <- "age_bin"
  names(d_fine)[names(d_fine) == "age_grp_merged"] <- "age_bin"
  d_coarse$location <- sapply(strsplit(d_coarse$name, split = "_"), function(x) x[1])
  d_fine$location <- sapply(strsplit(d_fine$name, split = "_"), function(x) x[1])
  d_fine$phe_region_of_location <- sapply(strsplit(d_fine$phe_region, split = "_"), function(x) x[1])

  d_coarse <- d_coarse[, c("location", "age_bin", "mid_week", "M", "Nr", "nr", "Nt", "nt")]
  d_coarse$nr[is.na(d_coarse$nr)] <- 0
  d_coarse$Nr[is.na(d_coarse$Nr)] <- 0
  d_fine <- d_fine[, c("location", "phe_region", "age_bin", "mid_week", "M", "Nt", "nt")]
  return(list(d_fine = d_fine, d_coarse = d_coarse))
}




























