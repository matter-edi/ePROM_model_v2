# SETUP ------------------------------------------------------------------------

# Data manipulation
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(arrayhelpers)
library(matrixStats)
library(data.table) # https://raw.githubusercontent.com/rstudio/cheatsheets/master/datatable.pdf 
library(car)
library(ascii)

# Plotting & graphs
library(ggplot2)
library(plotly)
library(ggplotify)

# Shiny & other graphical/UI packages
library(progress)
library(shiny)
library(shinythemes)
library(bslib)
library(purrr)
library(progressr)
library(shinycssloaders)
library(shinyBS)
library(shinyWidgets)
library(shinyfullscreen)

# Plotting & visualisation
library(scales)
library(ggplot2) # https://rstudio.github.io/cheatsheets/data-visualization.pdf 
library(shadowtext)
library(plotly)

## GLOBAL SETTINGS -------------------------------------------------------------

# GENERAL
set.seed(123)
options(scipen = 999)

# WTP
wtp_v <- 0:20 * 5000 # 0-100K possible threshold range

## LOAD DATA -------------------------------------------------------------------

# MODEL PARAMETER TABLE
par_df <-  read.csv("data/parameters.csv", row.names = "Label") # (MONTHLY RATES)
# Dictionary: _r = rates, _hr = hazard ratios

# SURVIVAL SIMULATION PARAMETERS
drates_dt <- fread("data/cancerdeaths.csv", stringsAsFactors=T) %>% select( # death rates
  Site, Condition, Age_lb, Age_ub, Death_y_r) # monthly death rates by age band
# Variables: 
#   - Site: cancer site, string (e.g. "Breast)
#   - Condition: int = 0, 1, or 5, years prior survival since diagnosis
#   - Age_lb & Age_ub: int, lower and upper bounds of each age group, same for all cancers
#   - Death_y_r: ANNUALISED rate of mortality by cancer site, conditional survival, and age band

# SHINY MODEL INSTRUCTIONS
# uicsv_df <- read.csv("data/shiny_ui.csv") 

# VARIABLE NAMES & IDs
par_v <- seq_len(length(rownames(par_df))) # vector with variable ID names
parl_v <- rownames(par_df) # labels
names(par_v) <- parl_v # clarify variable name & number
var_v <-  rownames(par_df) # input variable vector
var_n <- length(var_v) # number of variables

## USEFUL FUNCTIONS ------------------------------------------------------------

# Define useful functions
mag_f <- function(x) log(abs(x), 10) %>% round # detect magnitude
tocurr_f <- function(x, currency = "£", digits = 1) {
  
  # sign + absolute
  sign <- ifelse(x < 0, "-", "")
  v <- abs(x)
  
  # < 1000
  small <- formatC(v, format = "f", big.mark = ",", digits = 0)
  
  # >= 1000 (K format with trimmed decimals)
  k_raw <- formatC(v / 1000, format = "f", digits = digits, big.mark = ",")
  k_trim <- sub("\\.?0+$", "", k_raw)
  
  out <- ifelse(v < 1000, paste0(
    sign, currency, small), paste0(sign, currency, k_trim, "K"))
  
  out[is.na(x)] <- NA_character_
  out
} # format numbers to currency
rmna_f <- function(sv){ # Simple function to remove `£NA` `NA%` string anomalies
  sv[str_detect(sv, "NA")] <- ""
  return(sv)
}

## SHINY FUNCTIONS -------------------------------------------------------------

widget_f <- function(i){ # i is row number in the CSV instructions
  
  # DEBUG
  #print(i)
  
  # Read instructions from CSV
  fun_s <- get(ui_df$widget[i]) # extract shiny widget type
  args_v <- names(formals(ui_df$widget[i])) # extract valid arguments
  args_v <- args_v[args_v %in% colnames(ui_df)] # select defined arguments
  arg_l <- as.list(rep(NA, length(args_v))) # create empty list
  names(arg_l) <- args_v # list names = argument names
  tip_s <- paste("tip", i, sep = "_")
  for(v in args_v){arg_l[v] <- ui_df[i,][[v]]} # fill list with argument values
  
  # Format HTML
  arg_l[["label"]] <- lapply(
    arg_l[["label"]], function(s) HTML(s))
  if("choiceNames" %in% args_v) arg_l[["choiceNames"]] <- lapply(
    arg_l[["choiceNames"]], function(s) HTML(s))
  
  # Define funtion to place widget
  placewidget_f <- function(){
    tooltip(
      do.call(fun_s, arg_l), HTML(ui_df$tooltip[i]), 
      title = "Tip:",
      id = tip_s, placement = "right", options = list(
        animation = T, trigger = "hover focus", 
        customClass = "wide-tooltip"
      )
    )
  }
  
  # Format input boxes horizontally (label next to box)
  if(ui_df$LeftLabel[i] == T){
    label_s <- arg_l[["label"]]
    # arg_l[["label"]] <- character(0)
    edit_s <- str_replace( # remove default label above box
      ".control-label[for='x'] {display: none;}", "x", ui_df$inputId[i])
    
    # Generate Shiny Widget based on CSV instructions
    conditionalPanel(
      condition = ui_df$condition[i], 
      
      tags$style(edit_s),
      fluidRow(column(
        7,  style = "display: flex; align-items: center; position: relative; 
        top: -6px;", label_s), column(5, placewidget_f()))
    )
  } else conditionalPanel(condition = ui_df$condition[i], placewidget_f())
  
}

tab_f <- function(tab){tagList(lapply(as.list( # Generate widgets
  tabs_l[[tab]]), function(i){ widget_f(i) }))} # by section ID

section_f <- function(section) nav_panel(section, tab_f(section)) # h3(section), 

input_f <- function(input){list(
  
  ### BASE SETTINGS ----------------------------------------------------------
  seed_n = input$seed_n, # random seed
  psa_n = input$psa_n, # PSA samples
  th_n = input$th_n * 12, # time horizon (months)
  p_n = input$p_n, # number of patients
  au_n = input$au_n / 100, # Artificial uncertainty
  
  ### WTP & CURRENCY ---------------------------------------------------------
  base_wtp = input$base_wtp,
  wtprange_v = input$wtprange_v,
  
  ### PATIENT POPULATION SETTINGS --------------------------------------------
  age_v = min(input$age_v):max(input$age_v), # min &  max age bands
  survcond_v = survcond_v[as.numeric(input$survcond_v)], # allowed prior survival with cancer (years)
  sites_v = sites_v[as.numeric(input$sites_v)], # vector of included cancer sites
  
  ### SCENARIO SETTINGS ------------------------------------------------------
  
  # INCLUDE/EXCLUDE PARAMETERS
  sc_surv = 1 %in% input$scen_v, # Use HR for survival benefit
  sc_lower_ed = 2 %in% input$scen_v, # Do ePROMs lower ED visits?
  sc_lower_amb = 3 %in% input$scen_v, # Do ePROMs reduce ambulance use?
  sc_utility = 4 %in% input$scen_v, # Do ePROMs improve QoL?
  sc_custom_adh = input$sc_custom_adh == 2, # input/change adherence
  sc_monitor = input$sc_monitor == 1, # default monitoring cost
  
  # INPUT CUSTOM PARAMETERS
  sc_monitor_c = input$sc_monitor_c, # Custom per patient-month monitoring cost option
  # sc_alert_c = 0, # Cost per alert [NOT IN USE]
  sc_adh_pat = input$sc_adh_pat / 100, # patients' adherence rate
  sc_adh_hcp = input$sc_adh_hcp / 100, # HCP engagement rate
  sc_adh_eff = c(0, 1, input$sc_adh_eff / 100 )[as.numeric(input$sc_adh)], # Missing alerts x adherence
  
  ### PROGRESS BAR -----------------------------------------------------------
  pb_rcon = F, # show progress bar in R console
  pb_shiny = F # show progress bar in shiny
)}

md_f <- function() {
  chunk_current <- chunk_n 
  chunk_n <<- chunk_n + 1
  div(style = "color: black", markdown(md_l[[chunk_current]]))
} # black markdown text

pad_f <- function(...) div( # pad/centre widgets
  style = "max-width: 70%; width: 70%; margin: 0 auto;", ...)

tbl_f <- function(wdg) div(style = "color: black; align-items: center", wdg)


## REFORMAT MORTALITY DATA -----------------------------------------------------

# Cumulative Hazard at End of each Band
dedge_v <- c(unique(drates_dt$Age_lb), 100) # Vector of Band Edges
# setnames(drates_dt, "Death_y_r", "Rate" )
# drates_dt[, `:=`(Width = Age_ub - Age_lb + 1, Edge = Age_lb)][, `:=`(
#   AgeBand = 1:.N, CumHaz = shift(cumsum(Rate), 1, fill = 0)
# ), by = .(Site, Condition)]

bandrates_dt <- drates_dt %>% select(!Age_lb) %>% pivot_wider( # reformat to wide
  names_from = Age_ub, values_from = Death_y_r, names_prefix = "Band_")
drates_m <- bandrates_dt %>% select(!c(Site, Condition)) %>% as.matrix() # reformat to matrix
rownames(drates_m) <- paste(
  bandrates_dt$Site, bandrates_dt$Condition, sep = "_")

band_min_v <-  unique(drates_dt$Age_lb)
band_max_v <- unique(drates_dt$Age_ub) %>% sort # Max age at each bound
band_len_v <- band_max_v - band_min_v + 1 # Assuming band starts on a birthday and ends on the eve of the last year's birthday
bands_n <- length(band_max_v) # number of bands
bands_s_v <- paste(band_min_v, band_max_v, sep = "-")


## SHINY MODEL INSTRUCTIONS ----------------------------------------------------

# UI Builder
uicsv_df <- read.csv("data/shiny_ui.csv")

# Embedded text
model_html <- readLines("data/model_embed.txt") %>% HTML() # model flowchart
eprom_html <- readLines("data/eprom_embed.txt") %>% HTML() # what are ePROMs

ui_v <- readLines("UI text/uitext.Rmd")

## SHINY UI TEXT (markdown) ----------------------------------------------------

chunks_v <- ui_v %>% str_which("next_chunk") # Row numbers for start and end of sections

md_l <- sapply(1:(length(chunks_v)-1), function(i){
  ui_v[(chunks_v[i]+2):(chunks_v[i+1]-2)]
}) # list of Markdown chunks to be pasted into the Shiny UI


## GLOBALS (Shiny) -------------------------------------------------------------

# GRAPHICS
grey_v <- c("#e5e5e5")
pal_m <- matrix( # palette matrix
  c("#4F8A3A","#007f9c","#cdb900","#d96200","#b20009",
    "#b9dcac","#a3d3de","#faf3b0","#ffd6b5","#ff777e"), 
  byrow = T, nrow = 2, dimnames = list(
    "Shade" = c("Dark", "Light"),
    "Col" = c("Green", "Blue", "Gold", "Orange", "Red")
  )
)
grad_v <- c("#007f9c", "#4e98b0", "#7db1c3", "#a9cbd7") # gradient

# INPUTS
survcond_v <- c(0, 1, 5) # allowed prior survival with cancer (years)
sites_v <- c("Breast", "Colorectal", "Lung", "Prostate") # vector of included cancer sites

## HTML FORMATTING -------------------------------------------------------------

htmltags_v <- c( # HTML / CSS
  "<g" = "<span style='color: white; background-color: #ABABAB;
  padding: 2px 4px; border-radius: 3px;'>", "g>" = "</span>" # link box
)

## FORMAT INSTRUCTIONS ---------------------------------------------------------

ui_df <- uicsv_df %>% mutate(
  label = str_replace_all(label, htmltags_v),
  choiceNames = str_replace_all(choiceNames, htmltags_v),
  choiceNames = str_split(choiceNames, ", "), 
  choiceValues = sapply(choiceValues, function(i) if(is.na(i)) NA else seq(i)),
  selected = lapply(str_split(selected, ", "), as.numeric),
  value = lapply(str_split(value, ", "), as.numeric),
  order = seq_len(nrow(uicsv_df))
) # `choices` strings -> vector

tabs_l <- lapply(unique(ui_df$tab), function(s) { 
  filter(ui_df, tab == s)$order
}) # Extract tab IDs
names(tabs_l) <- unique(ui_df$tab) # avoid simplifying to matrix

section_l <- sapply(unique(ui_df$section), function(s){ 
  unique(filter(ui_df, section == s)$tab)
}) # Extract tags per section


# SIMULATION FUNCTION ==========================================================


sim_f <- function(  
    
  ## BASE SETTINGS -------------------------------------------------------------
  seed_n = 123, # random seed
  psa_n = 1000, # PSA samples
  th_n = 12, # time horizon (months)
  p_n = 200, # number of patients
  au_n = 0.2, # Artificial uncertainty
  
  ## WTP & CURRENCY ------------------------------------------------------------
  base_wtp = 20000,
  wtprange_v = 2:3 * 10^4,
  
  ## PATIENT POPULATION SETTINGS -----------------------------------------------
  age_v = 55:64, # min &  max age bands
  survcond_v = c(0, 1, 5), # allowed prior survival with cancer (years)
  sites_v = c("Breast", "Colorectal", "Lung", "Prostate"), # vector of included cancer sites
  
  ## SCENARIO SETTINGS ---------------------------------------------------------
  
  # INCLUDE/EXCLUDE PARAMETERS
  sc_surv = F, # Use HR for survival benefit
  sc_lower_ed = T, # Do ePROMs lower ED visits
  sc_lower_amb = T, # Do ePROMs reduce ambulance use
  sc_custom_adh = T, # input/change adherence
  sc_monitor = T, # default monitoring cost
  sc_utility = T, # default: use difference in utility values
  
  # INPUT CUSTOM PARAMETERS
  sc_monitor_c = 0, # Custom per patient-month monitoring cost option
  sc_alert_c = 0, # Cost per alert
  sc_adh_pat = 0.67, # patients' adherence rate
  sc_adh_hcp = 0.44, # HCP engagement rate
  sc_adh_eff = 0.5, # Missing alerts x adherence
  
  ## PROGRESS BAR --------------------------------------------------------------
  pb_rcon = F, # show progress bar in R console
  pb_shiny = F # show progress bar in shiny
  
  ## FUNCTION ENGINE -----------------------------------------------------------
){
  
  ## FUNCTION SETUP & KEY VARIABLES --------------------------------------------
  
  # random seed & progress bar
  set.seed(seed_n)
  if(pb_rcon) pb <- progress_bar$new(
    total = 10, format = "Section: :what :bar :current / :total")
  if(pb_shiny) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Initiating model...", value = 0)
  }
  if(pb_rcon) pb$tick(tokens = list(what = "Initiating"))
  if(pb_shiny) progress$set(value = 0.05, message = "Initiating...")
  
  
  # Vectors for labels, useful objects, local copies
  lpar_df <- par_df
  psa_v <- paste("PSA", seq_len(psa_n), sep="") # PSA labels
  ci_v <- c("CIl", "CIu") # CI labels, lower & upper bounds
  owsa_v <- paste(rep(c("CIl", "CIu"), each = var_n), var_v, sep = "_")
  owsa_n <- length(owsa_v)
  it_v <- c("Mu", ci_v, psa_v, owsa_v) # Iteration labels: base case, upper & lower, PSA
  it_n <- length(it_v) # number of iterations: Mean, CI, PSA, OWSA
  # lab_v <- paste(rep(it_v, each = p_n), "_p", rep(1:p_n, psa_n), sep = "") # patients by iteration
  strat_v <- c("UC", "ePROM")
  event_v <- c("Symptom", "Hospital", "Ambulance", "End", "Death")
  # it_strat_v <- paste(rep(strat_v, each = it_n), rep(it_v, 2), sep = "_") %>% 
  #   as.factor
  
  # WTP extent
  
  
  # patients across all iterations, copied into 2 treatment strategies
  # pat_v <- paste(rep(strat_v, each = it_n * p_n), lab_v, sep = "_")
  
  ## CUSTOM PARAMETERS ---------------------------------------------------------
  
  # Patient adherence & Clinician engagement
  lpar_df[c("sim_padh", "sim_review"),"Value"] <- c(sc_adh_pat, sc_adh_hcp)
  lpar_df[c("sim_padh", "sim_review"), c("SE", "CIL", "CIU")] <- NA # reset
  
  # Adherence effect
  lpar_df["adh_effect", "Value"] <- sc_adh_eff
  
  if(sc_monitor==F){ # Custom monitoring cost
    lpar_df["eprom_m_c",c("Value", "SE", "CIL", "CIU")] <- c(
      sc_monitor_c, NA, NA, NA
    )
  }
  
  ## SIMULATION ITERATIONS (base case, PSA, etc.) ------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Variable Sampling"))
  if(pb_shiny) progress$set(value = 0.15, message = "Sampling variables...")
  
  # Remove variables from beta if =0 or =1 excactly
  lpar_df[(lpar_df$Dist == "beta") & (lpar_df$Value %in% c(0, 1)),
          "Dist"] <- "none"
  lpar_df[(lpar_df$Dist == "gamma") & (lpar_df$Value == 0),
          "Dist"] <- "none"
  
  # GAMMA & BETA DISTRIBUTIONS
  gamma_v <- filter(lpar_df, Dist == "gamma") %>% rownames()
  beta_v <- filter(lpar_df, Dist == "beta") %>% rownames()
  lnorm_v <- filter(lpar_df, Dist == "lognormal") %>% rownames()
  nodist_v <- filter(lpar_df, Dist == "none") %>% rownames()
  
  
  # ARTIFICIAL UNCERTAINTY
  lpar_df[is.na(lpar_df$SE), "SE"] <- with(
    lpar_df[is.na(lpar_df$SE),], Value * au_n)
  
  # PSA DISTRIBUTION PARAMETERS
  lpar_df[gamma_v, c("Alpha", "Beta")] <- with( # shape & rate
    lpar_df[gamma_v,], c((Value ^ 2) / (SE ^ 2), Value / (SE ^ 2)))
  lpar_df[beta_v, c("Alpha", "Beta")] <- with( # alpha & beta
    lpar_df[beta_v,], c(
      Value * ((Value * (1-Value)/(SE^2)) - 1), # x * ((x * (1-x)/(y^2)) - 1)
      (1 - Value) * ((Value * (1-Value)/(SE^2)) - 1)))
  lpar_df[lnorm_v, c("Alpha", "Beta")] <- with(
    lpar_df[lnorm_v,], c(
      log(Value) - (SE^2)/2,
      sqrt(log(1 + (SE^2)/(Value^2)))
    )
  )
  # lpar_df[owsa_v] <- NA # placeholder
  
  # Matrix for storing base case, CI, and PSA parameters
  par_m <- matrix(NA, nrow = length(par_v), ncol = it_n, dimnames = list(
    "Variable" = parl_v, "Iteration" = it_v))
  par_m[,"Mu"] <- lpar_df[["Value"]] # Base case
  
  # DRAW PARAMETERS FOR EACH PSA ITERATION
  par_m[gamma_v, psa_v] <- with(lpar_df[gamma_v,], rgamma( # Gamma distributed variables
    length(Dist) * psa_n, shape = rep(Alpha, psa_n), rate = rep(Beta, psa_n)))
  par_m[beta_v, psa_v] <- with(lpar_df[beta_v,], rbeta( # Beta distributed variables
    length(Dist) * psa_n, shape1 = rep(Alpha,psa_n), shape2 = rep(Beta,psa_n)))
  par_m[lnorm_v, psa_v] <- with(lpar_df[lnorm_v,], rlnorm( # Beta distributed variables
    length(Dist) * psa_n, meanlog = rep(Alpha,psa_n), sdlog = rep(Beta,psa_n)))
  par_m[nodist_v, psa_v] <- lpar_df[nodist_v,]$Value
  
  # UPPER & LOWER BOUNDS (calculate or import confidence bounds)
  par_m[gamma_v, ci_v] <- with(lpar_df[gamma_v,], qgamma( # from Gamma
    rep(c(0.025, 0.975), each = length(Dist)), shape = Alpha, rate = Beta))
  par_m[beta_v, ci_v] <- with(lpar_df[beta_v,], qbeta( # from Beta
    rep(c(0.025, 0.975), each = length(Dist)), shape1 = Alpha, shape2 = Beta))
  par_m[lnorm_v, ci_v] <- with(lpar_df[lnorm_v,], qlnorm( # from Beta
    rep(c(0.025, 0.975), each = length(Dist)), meanlog = Alpha, sdlog = Beta))
  par_m[nodist_v, ci_v] <- par_m[nodist_v, "Mu"] # # No variation
  par_m[rownames(lpar_df[!is.na(lpar_df$CIL),]), "CIl"] <- lpar_df[
    !is.na(lpar_df$CIL),]$CIL # From source (lower)
  par_m[rownames(lpar_df[!is.na(lpar_df$CIU),]), "CIu"] <- lpar_df[
    !is.na(lpar_df$CIU),]$CIU # From source (upper)
  
  # OWSA ITERATIONS
  par_m[,owsa_v] <- par_m[,rep("Mu", owsa_n)] # Copy means, then replace diagonals with lower/upper
  par_m[,owsa_v[1:(owsa_n/2)]][diag(1, nrow = var_n)==1] <- par_m[,"CIl"]
  par_m[,owsa_v[(1+owsa_n/2):owsa_n]][diag(1, nrow = var_n)==1] <- par_m[,"CIu"]
  
  # CORRECTIONS: `qbeta` struggles with adherence/engagement values close to 1
  # par_m["hcp_review", is.nan(
  #   par_m["hcp_review", ]) | is.na(par_m["hcp_review", ])] <- par_m[
  #   "hcp_review", "Mu"] 
  # par_m["sim_padh", is.nan(
  #   par_m["sim_padh", ]) | is.na(par_m["sim_padh", ])] <- par_m[
  #   "sim_padh", "Mu"] 
  
  ## SCENARIO ADJUSTMENTS ------------------------------------------------------
  
  # User input vs Base case adherence & engagement parameters
  if(!sc_custom_adh){  
    lpar_df[ # If base case is used, copy rows such that they are identical
      c("sim_padh", "sim_review"), ] <- lpar_df[c("obs_padh", "obs_review"), ]
    par_m[ # If base case is used, copy rows such that they are identical
      c("sim_padh", "sim_review"), ] <- par_m[c("obs_padh", "obs_review"), ]
  }
  
  par_m["death_hr",] <- par_m["death_hr",] ^ sc_surv # death HR
  par_m["hosp_hr",] <- par_m["hosp_hr",] ^ sc_lower_ed
  par_m["amb_hr",] <- par_m["amb_hr",] ^ sc_lower_amb
  
  # Which variables to exclude from OWSA
  excl_owsa_v <- (par_m[,"Mu"] == par_m[,"CIl"]) & 
    (par_m[,"Mu"] == par_m[,"CIu"])
  excl_owsa_v[c("sim_padh", "sim_review")] <- !sc_custom_adh
  
  
  ## CREATE SIMULATED PATIENTS -------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Simulating Patients"))
  if(pb_shiny) progress$set(value = 0.2, message = "Simulating patients...")
  
  pat_dt <- data.table(
    "ID" = 1:p_n,
    "SiteCond" = NA_character_, # Patient ID
    "Site" = factor(sample(sites_v, p_n, T)), # draw cancer type
    "Condition" = sample(survcond_v, p_n, T), # draw prior survival with cancer
    "Age" = sample(age_v, p_n, T) # draw age
  )[,SiteCond := factor(paste(Site, Condition, sep = "_"))]
  setorder(pat_dt, ID) # reorder
  p_all_n <- p_n * it_n * 2 # mumber of patients x iterations x strategies
  
  pcat_v <- pat_dt$SiteCond
  mdim_l <- list("Patient" = pcat_v, "Band" = colnames(drates_m)) # mort matrix labels
  
  # IDs acress iterations
  catid_v <- rep(pat_dt$SiteCond, 2 * it_n) # Patient list repeated
  pid_v <- rep(1:p_n, 2 * it_n)
  # Order: Strategy > Iteration > Patient number
  
  
  ## MORTALITY -----------------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Simulating Mortality"))
  if(pb_shiny) progress$set(
    value = 0.25, message = "Running survival analysis...")
  
  # TIME TO NEXT BAND: Matrix of time available in each age band from each patient's starting age
  ttnb_m <- matrix( # subtract start age from the next band's minimum 
    band_max_v + 1 - rep(pat_dt$Age, each = bands_n), # current band max + 1 = next band's minimum
    byrow = T, ncol = bands_n, dimnames = mdim_l)
  
  # Each patient's available/possible time in each band based on start age and band length
  bandtime_m <- pmax(0, pmin(band_len_v, c(t(ttnb_m)))) %>% matrix(
    ncol = bands_n, dimnames = mdim_l, byrow = T)
  cumtime_m <- rowCumsums(bandtime_m)
  
  # CUMULATIVE HAZARD PER ROW, ACROSS BANDS (replaces rexp matrix + ttd_bstart_m + survband_m + ttd_m)
  bandhaz_m <- drates_m[pcat_v,] * bandtime_m # hazard contributed by each band (p_all_n x bands_n)
  cumhaz_m <- cbind("Band_0" = 0, rowCumsums(bandhaz_m)) # cumulative hazard / reached by END of each band
  #starthaz_m <- cbind(0, cumhaz_m[, -bands_n]) # cumulative hazard at START of each band
  bandE_v <- colnames(bandhaz_m) # End cumulative hazard band column names
  bandS_v <- c("Band_0", bandE_v[-bands_n]) # Start cumulative hazard band column names
  totalhaz_v  <- cumhaz_m[, bandE_v[bands_n]] # total hazard available before hitting age 100
  
  # Exponential draw
  exp_v <- -log(runif(p_n * it_n * 2)) / 
    c(rep(1, it_n * p_n), rep(par_m["death_hr",], each = p_n)) # Hazard ratio
  names(exp_v) <- catid_v
  censored_v <- exp_v >= totalhaz_v[pid_v]
  
  # Target bands& indices
  targid_v <- rowSums(cumhaz_m[pid_v, bandE_v] <= exp_v) + 1
  targid_v[censored_v] <- NA
  id_m <- cbind(pid_v, targid_v)
  
  # TIME CALCULATIONS
  tinto_v <- (exp_v - cumhaz_m[, bandS_v][id_m]) / drates_m[catid_v,][id_m] # into band
  tostart_v <- ifelse(
    targid_v == 1, 0,
    cumtime_m[cbind(pid_v, pmax(targid_v - 1, 1))])
  
  # TIME TO DEATH
  ttd_v <- tinto_v + tostart_v
  # names(ttd_v) <- pid_v
  ttd_v[censored_v] <- Inf
  ttdm_v <- 12 * ttd_v
  
  # Time to end
  tend_v  <- pmin(ttd_v, 100 - pat_dt$Age, th_n/12)
  tend_m_v <- tend_v * 12
  
  # Deaths (months): excluding survivors past 100 OR past time horizon
  died_v <- !(censored_v | (ttdm_v > th_n)) # Deaths *within* time horizon
  
  ## EVENT TABLE ---------------------------------------------------------------
  
  # Create event table and log deaths
  ev_dt <-  data.table(
    "Strategy" = factor(rep(strat_v, each = it_n * p_n), levels = strat_v),
    "Run" = factor(rep(it_v, 2, each = p_n), levels = it_v),
    "Patient" = rep(1:p_n, it_n * 2),
    "Event" = factor("End", levels = event_v),
    "Died" = died_v,
    "Time" = round(tend_m_v, 1)
  ) 
  
  ## COMPOSITE VARIABLES -------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Parameter Calculations"))
  if(pb_shiny) progress$set(value = 0.3, message = "Calculating parameters...")
  
  newv_v <- c( # Variables calculated from starting inputs
    "symp_r", "adj_adh_obs", "adj_adh_sim", "ep_hosp_r", "ep_amb_r")
  
  # Empty matrix
  npar_m <- matrix(NA, nrow = length(newv_v), ncol = it_n, dimnames = list(
    "Variable" = newv_v, "Iteration" = it_v))
  
  # Adherence / ePROM completion rates adjusted by assumption of missing data
  # ρ + (1-ρ)(1-ε)     = 1+ρε-ε
  npar_m[c("adj_adh_obs", "adj_adh_sim"), ] <- par_m[
    c("obs_padh", "sim_padh"), ] + 
    (1 - par_m[c("obs_padh", "sim_padh"), ]) * 
    (1 - par_m[rep("adh_effect", 2), ])
  
  npar_m[c("adj_adh_obs", "adj_adh_sim"), ][ # Correction: R returns Nan where we expect 0 in this expression
    is.nan(npar_m[c("adj_adh_obs", "adj_adh_sim"), ])] <- 0
  
  # Symptom rate adjusted by assumption of missing data in Girgis
  # λ_s' = λ_S / [ρ + (1-ρ)(1-ε)]
  npar_m["symp_r",] <- par_m["symp_unadj_r",] / npar_m["adj_adh_obs",]
  
  # ED Visits: APPLY HRs & ADHERENCE/ENGAGEMENT FILTER: λ_E * (HR_E – 1 + ρɣ) / ρɣ
  ρɣ <- npar_m[rep("adj_adh_sim",),] * par_m[rep("sim_review",),] # Simulated
  ρɣ_bc <- npar_m[rep("adj_adh_obs",),] * par_m[rep("obs_review",),] # base case (Girgis)
  npar_m["ep_hosp_r",] <- pmax( # this will only be applied when ρ=1 & ɣ=1
    0, par_m["hosp_r",] * (par_m["hosp_hr",] - 1 + ρɣ) / ρɣ)
  
  # Ambulance HR
  npar_m["ep_amb_r",] <- par_m["amb_r",] * par_m["amb_hr",] # applied globally
  
  # combine new variables to old variables in a matrix
  # par_all_m <- rbind(par_m, npar_m)
  
  ## SYMPTOM SIMULATION --------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Simulating Symptom Events"))
  if(pb_shiny) progress$set(value = 0.4, message = "Simulating symptom events...")
  
  # SAMPLE TOTAL SYMPTOMS FROM POISSON DISTRIBUTION
  # Sum of symptoms per patient/iteration
  sympn_v <- rpois(p_all_n, rep(npar_m["symp_r",], each = p_n, 2) * tend_m_v) 
  symp_n <- sum(sympn_v) # Total symptoms simulated, across patients and iterations
  e_n <- max(sympn_v) # Max symptoms (events) per patient
  
  # Update event table
  ev_dt <- data.table(
    "Strategy" = rep(rep(strat_v, each = it_n * p_n), sympn_v),
    "Run" = rep(rep(it_v, 2, each = p_n), sympn_v),
    "Patient" = rep(rep(1:p_n, it_n * 2), sympn_v),
    "Event" = "Symptom",
    "Died" = NA,
    "Time" = runif(symp_n, min = 0, max = rep(tend_m_v, sympn_v)) %>%
      round(1) # Uniform-sampled times between events
  ) %>% rbind(ev_dt)
  
  setorder(ev_dt, Strategy, Run, Patient, Time) # Arrange by event time
  
  # GROUP EVENTS BY SYMPTOM & CALCULATE TIME TO NEXT EVENT
  ev_dt[, EventGroup := 1:.N, by = .(Strategy, Run, Patient)]
  ev_dt[, TN := c(ev_dt$Time[2:.N], NA)][Event == "End", TN := NA] # Time from current to next symptom
  
  
  ## PATIENT ADHERENCE & HCP ENGAGEMENT ----------------------------------------
  
  # Set default symptom reporting and monitoring status to 0
  ev_dt[Event == "Symptom", `:=`("Report" =  F, "Review" = F)]
  
  # Calculate reviewing and monitoring for ePROM arm symptoms
  ev_dt[(Strategy == "ePROM") & (Event == "Symptom"), `:=`(
    Report = 1 == rbinom(.N, 1, npar_m["adj_adh_sim", Run]),
    Review = 1 == rbinom(.N, 1, par_m["sim_review", Run])
  )]
  
  # QC = check report/review rates are aligned
  # ev_dt[(Strategy == "ePROM") & (Event == "Symptom") &
  #         (Run %in% psa_v), c("Report", "Review")] %>% colMeans()
  
  # Calculate Monitoring
  ev_dt[, Monitored := Report & Review][, `:=`(Report = NULL, Review = NULL)]
  
  ## TRIGGER EVENT SIMULATION --------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(
    what = "Simulating ED Visit & Ambulance Events"))
  if(pb_shiny) progress$set(
    value = 0.5, message = "Simulating ED visits & ambulances...")
  
  # `TrigEvents` variable captures number of hospital events following a symptom
  # OR whether an ambulance is required for a given hospital event
  
  # MATRIX of hospital-symptom rate ratios
  hsrr_m <- rbind(par_m["hosp_r",], npar_m["ep_hosp_r",]) / 
    npar_m[rep("symp_r", 2),]
  rownames(hsrr_m) <- strat_v
  
  # Calculate Hospital Events (NON-MONITORED)
  ev_dt[(Event == "Symptom") & !Monitored, "TrigEvents" := rpois(
    .N, hsrr_m["UC", Run]
  )]
  
  # Calculate Hospital Events (MONITORED)
  ev_dt[(Event == "Symptom") & Monitored, "TrigEvents" := rpois(
    .N, hsrr_m["ePROM", Run]
  )]
  
  # HOSPITAL EVENT TIMES
  
  phosp_m <- ppois(0, hsrr_m, lower.tail = F) # Probability (per run) that a given event leads to hospitalisation
  hgs_v <- hsrr_m / phosp_m # Conditional expected hospitalisations E[H|S>0] = λ/P(x>0)
  
  ev_dt <- rbind( # Add rows for hospital events
    ev_dt, ev_dt[(Event == "Symptom") & (TrigEvents > 0),][
      rep(seq_along(TrigEvents), TrigEvents),][, Event := "Hospital"]
  )
  setorder(ev_dt, Strategy, Run, Patient, Time, -Event)
  
  # AMBULANCE (TrigEvent)
  ambp_m <- rbind(par_m["amb_r",], npar_m["ep_amb_r",]) / # probability of amb given hosp
    rbind(par_m["hosp_r",], npar_m["ep_hosp_r",])
  ambp_m[ambp_m>1] <- 1
  rownames(ambp_m) <- strat_v
  
  # Calculate Ambulances (NON-MONITORED)
  ev_dt[(Event == "Hospital") & !Monitored, "TrigEvents" := rbinom(
    .N, size = 1, hsrr_m["UC", Run]
  )]
  
  # Calculate Ambulances (MONITORED)
  ev_dt[(Event == "Hospital") & Monitored, "TrigEvents" := rbinom(
    .N, size = 1, hsrr_m["ePROM", Run]
  )]
  
  
  ## COSTS & QALYs -------------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Calculating Costs & QALYs"))
  if(pb_shiny) progress$set(value = 0.5, message = "Calculating costs & QALYs...")
  
  # Present Value of Annuity Due (start of period)
  # Link: https://gainbridge.com/post/present-value-of-annuity 
  # PMT x [(1 - {1 + r}-n ) / r] x (1 + r)]
  
  
  # HOSPITAL & AMBULANCE COSTS (discounted)
  ev_dt[, `:=`(Costs = 0, QALYs = 0)]
  ev_dt[Event == "Hospital", Costs := (
    par_m["ed_c", Run] + (par_m["amb_c", Run] * TrigEvents)) / 
      (1 + par_m["discount", Run]) ^ (Time/12)]
  
  # ePROM MONITORING COST - calculated at simulation end
  ev_dt[(Event == "End") & (Strategy == "ePROM"), Costs := fifelse(
    par_m["discount", Run]==0,
    par_m["eprom_m_c", Run] * Time,
    12 * par_m["eprom_m_c", Run] * 
      ((1 - (1 + par_m["discount", Run]) ^ (-Time/12)) / # Present Value
         par_m["discount", Run]) * (1 + par_m["discount", Run]) 
  )]
  
  # BASE QALYs
  ev_dt[Event == "End", QALYs := fifelse(
    par_m["discount", Run] == 0,
    par_m["h_u", Run] * Time/12,
    par_m["h_u", Run] * 
      ((1 - (1 + par_m["discount", Run]) ^ (-Time/12)) / # Present value
         par_m["discount", Run]) * (1 + par_m["discount", Run])
  )]
  
  # ePROM MONITORING QALYs
  
  du_v <- par_m["d_u",] / ρɣ_bc # utility gain adjusted by probability of monitoring
  
  
  ev_dt[ # When a symptom is monitored, apply a utility bonus until next event
    (Event == "Symptom") & (EventGroup == 1) & Monitored,
    QALYs := fifelse(
      par_m["discount", Run] == 0,
      du_v[Run] * ((TN - Time)/12), # if no discounting
      du_v[Run] * 
        # Present value of annuity at the future date
        (((1 - (1 + par_m["discount", Run])^(-((TN - Time)/12)) ) / 
            par_m["discount", Run]) * (1 + par_m["discount", Run])) / 
        # Discounted back to present value
        (1 + par_m["discount", Run]) ^ (Time/12)
    )
  ]     
  
  
  ## CUMULATIVE OUTCOMES -------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Calculating Cumulative Outcomes"))
  if(pb_shiny) progress$set(value = 0.6, message = "Calculating totals...")
  
  out_v <- c("QALYs", "Costs") # Main outcomes
  
  sum_dt <- ev_dt[
    ,lapply(.SD, sum), by = .(Strategy, Run, Patient), 
    .SDcols = out_v][, LYs := tend_v][
      ,lapply(.SD, mean),  by = .(Strategy, Run), 
      .SDcols = out_v
    ] %>% dcast(Run ~ Strategy, value.var = out_v)
  
  sum_dt[, `:=`( # Differences in Costs & QALYs
    Costs = Costs_ePROM - Costs_UC,
    QALYs = QALYs_ePROM - QALYs_UC
  )]
  
  # Rounding
  # ev_dt[, `:=`(Costs = round(Costs), QALYs = round(QALYs, 2)) ]
  # sum_dt[, (names(.SD)) := lapply(.SD, round), .SDcols = patterns('Costs')]
  # sum_dt[, (names(.SD)) := lapply(.SD, round, 2), .SDcols = patterns('LYs')]
  
  
  ## COST-EFFECTIVENESS --------------------------------------------------------
  if(pb_rcon) pb$tick(tokens = list(what = "Calculating CE"))
  if(pb_shiny) progress$set(value = 0.9, message = "Calculating CE...")
  
  # ICER
  sum_dt[, ICER := Costs / QALYs]
  
  # EVENT TABLE (base case)
  
  basev_dt <- rbind(
    ev_dt[Run == "Mu",],
    ev_dt[(Run == "Mu") & (Died ==  T),][, Event := "Death" ],
    ev_dt[(Run == "Mu") & (Event == "Hospital") & (TrigEvents == 1),][
      , Event := "Ambulance"
    ]
  )[Event != "End",] %>% select(! c(Run, Died, TN, TrigEvents, Costs, QALYs))
  setorder(basev_dt, Strategy, Time)
  basev_dt[, Count := 1:.N, by = .(Strategy, Event)]
  
  # OWSA
  
  # Format and reshape summary data to prepare owsa tornado
  owsa_l_dt <- copy(sum_dt)[owsa_v, .SD, .SDcols = c("Run", out_v)][, `:=`(
    INMB = base_wtp * QALYs - Costs,
    Bound = c("Lower", "Upper")[2 - str_detect(Run, "CIl_")],
    Var_l = str_replace_all(Run, c("CIl_" = "", "CIu_" = ""))
    # Variable = lpar_df[str_replace_all(Run, c("CIl_" = "", "CIu_" = "")),]$Text
  )][Var_l %in% names(!excl_owsa_v)[!excl_owsa_v],][
    , `:=`(Costs = NULL, QALYs = NULL, Run = NULL)]
  
  owsa_w_dt <- dcast( # long to wide format
    owsa_l_dt, # OWSA uses base WTP to calculate INMB for each run
    Var_l ~ Bound, value.var = "INMB"
  )
  
  owsa_dt <- cbind(
    data.table("Variable" = with(owsa_w_dt, lpar_df[Var_l, "Text"])),
    owsa_w_dt,
    with(lpar_df[owsa_w_dt$Var_l,], paste.matrix( # Add variable values in each scenario
      Pref, round(Mult * par_m[owsa_w_dt$Var_l, c("CIl", "Mu", "CIu")], Round), 
      Suf, sep = ""
    )) %>% as.data.frame() %>% setDT() %>% setnames(c("CIl", "Mu", "CIu"))
  )[, `:=`(
    Base = with(sum_dt["Mu"], base_wtp * QALYs - Costs),
    Diff = abs(Upper - Lower)
  )] # [!(Variableexcl_owsa_v),]
  
  # CHOOSE 5 top rows for tornado diagram
  owsa_dt <- setorder(owsa_dt, -Diff)[1:5,]
  
  
  # PSA
  
  wtpl_v <-tocurr_f(wtp_v) # formatted labels
  
  inmb_m <- (with( # INMB values at each WTP (PSA)
    sum_dt[psa_v,], (matrix(QALYs, ncol = 1) %*% wtp_v) - Costs
  ))
  colnames(inmb_m) <- wtpl_v
  
  eprom_ce_v <- c(with( # check if base case ePROMs are cost effective under each INMB
    sum_dt["Mu",], (matrix(QALYs, ncol = 1) %*% wtp_v) - Costs) > 0)
  
  # CEAC
  
  ceac_v <- colSums(inmb_m > 0) / psa_n
  names(ceac_v) <- wtpl_v
  
  # VOI
  
  evpi_m <- t(t(inmb_m) * c(1, -1)[eprom_ce_v + 1])
  evpi_m[evpi_m<0] <- 0
  
  evpi_v <- colMeans(evpi_m)
  
  
  # WTP TABLE
  wtp_dt <- data.table(
    "WTP" = wtp_v,
    "CEAC" = ceac_v,
    "EVPI" = evpi_v
  )
  
  # PSA TABLE
  psa_dt <- sum_dt[c(psa_v, "Mu"), .(Run, Costs, QALYs, ICER)][
    , `:=`(Costs_s = tocurr_f(Costs), ICER_s = paste0(tocurr_f(ICER), "/QALY"))
  ][, Run := factor(c(rep("Probabilistic", psa_n), "Base Case"), levels = c(
    "Probabilistic", "Base Case"
  ))]
  
  
  ## MODEL INPUT TABLE ---------------------------------------------------------
  
  
  changev_v <- c("eprom_m_c", "sim_review", "sim_padh", "adh_effect" ) %>% # edited by user
    match(rownames(par_df))
  
  par_dt <- as.data.table(par_df) # data table for easy manipulationn
  par_dt[,Var_l := rownames(par_df) ]
  par_dt[, c("Value", "CIL", "CIU") := par_m[,c("Mu", "CIl", "CIu")] %>% 
           asplit(2)] # import edited values
  if(sc_custom_adh){par_dt[changev_v, "SE"] <- NA} # remove SEs for values edited by user
  
  par_dt[,`:=`( # Format values as strings
    Mu_s = paste(Pref, round(Mult * Value, Round), Suf, sep = "") %>% rmna_f,
    SE_s = paste(Pref, round(Mult * SE, Round), Suf, sep = "") %>% rmna_f,
    CIL_s = paste(Pref, round(Mult * CIL, Round), Suf, sep = "") %>% rmna_f,
    CIU_s = paste(Pref, round(Mult * CIU, Round), Suf, sep = "") %>% rmna_f
  )]
  par_dt[CIL == CIU, SE_s := ""] # remove SE for HR if nt applicable
  par_dt[,`:=`( # Mean ± SE, upper-lower bound formatting
    MuSE = paste(Mu_s, SE_s, sep = " ± ") %>% str_replace_all(
      c(" ± $" = "", " ± 0%$" = "")),
    Bound = paste(CIL_s, "to", CIU_s)
  )] 
  par_dt[CIL == CIU, Bound := "-"]
  
  # LOG ASSUMPTIONS MADE BY USER
  if(!sc_surv){par_dt[Var_l == "death_hr", Source := "Assumption"]}
  if(!sc_lower_ed){par_dt[Var_l == "hosp_hr", Source := "Assumption"]}
  if(!sc_lower_amb){par_dt[Var_l == "amb_dr", Source := "Assumption"]}
  if(!sc_monitor){par_dt[Var_l == "eprom_m_c", Source := "Assumption"]}
  if(sc_custom_adh){par_dt[
    Var_l %in% c("sim_review", "sim_padh"), Source := "Assumption"]}
  
  
  
  ### MAIN TOTALS --------------------------------------------------------------
  
  
  bc_dt <- sum_dt["Mu", ] %>% select(QALYs_UC:QALYs)
  setnames(bc_dt, c("Costs", "QALYs"), c("Costs_Δ", "QALYs_Δ"))
  bc_dt <- melt(
    bc_dt, measure.vars = patterns("^Costs_", "^QALYs_"),
    variable.name = "Strategy",
    value.name = c("Costs", "QALYs"))[, Strategy := c("UC", "ePROM", "Δ")][
      , Costs := tocurr_f(Costs)
    ]
  
  # Check if ePROMS are a winning strategy
  ePROMs_b <- with(sum_dt["Mu",], base_wtp * QALYs - Costs) > 0
  
  ce_dt <- data.table(
    "Outcome" = c("ICER", "INMB", "Decision"), 
    "Result" = c(
      paste0(tocurr_f(with(sum_dt["Mu",], Costs/QALYs)), "/QALY"),
      paste0(tocurr_f(with(
        sum_dt["Mu",], base_wtp * QALYs - Costs)), ", WTP = ", 
        tocurr_f(base_wtp)),
      paste(strat_v[2 - !ePROMs_b], ">", strat_v[2 - ePROMs_b])
    )
    
  )
  
  ## RETURN RESULTS ------------------------------------------------------------
  if(pb_rcon) pb$terminate()
  
  return(list(
    
    # Parameters
    "parameters" = select(par_dt, Text, Dist, MuSE, Bound, Source),
    "patients" = pat_dt,
    
    # Events
    "events" = basev_dt,
    
    # BASE CASE SUMMARY
    "costs_qalys" = bc_dt,
    "decision" = ce_dt,
    
    # Sensitivity
    "psa" = psa_dt,
    "wtp" = wtp_dt,
    "owsa" = owsa_dt,
    
    # Other
    "base_wtp" = base_wtp,
    "wtprange" = wtprange_v

  ))
  
}


# GRAPHING FUNCTIONS -----------------------------------------------------------

## STARTING PATIENT GRID -------------------------------------------------------

plotpat_f <- function(pat_data){
  
  ### FORMATTING ---------------------------------------------------------------
  pat_dt <- copy(pat_data)
  
  # n for Cancer Site
  age_v <- do.call(paste, c(dplyr::count(pat_dt, Site), list("sep" = ", n=")))
  names(age_v) <- dplyr::count(pat_dt, Site)$Site
  pat_dt[, Cancer := factor(age_v[pat_dt$Site])]
  pat_dt[, "Age Band" := factor(bands_s_v[findInterval(Age, dedge_v)])][
    , "Since Diagnosis" := factor(
      paste(Condition, "year(s)") ,
      levels = rev(c("0 year(s)", "1 year(s)", "5 year(s)")))
  ]
  
  # n for age band and years since diagnosis
  pat_dt[, Tip := paste(
    "Count:<b>", add_count(pat_dt, Site, `Age Band`, `Since Diagnosis`)$n, "/",
    add_count(pat_dt, Site, `Age Band`)$n, "</b>patients"
  )]
  

  # PLOT -----------------------------------------------------------------------
  
  pat_p <- ggplot(data = pat_dt, aes(
    y = `Age Band`, fill = `Since Diagnosis`, text = Tip)) +
    geom_bar(position = "stack") + facet_wrap(~ Cancer ) + theme_bw() +
    scale_fill_manual(values = rev(grad_v[1:3])) +
    labs(x = "Counts", title = paste( 
      "Simulated Patients, N=", 
      length(pat_dt$Site), sep =""))
  # pat_p <- ggplotly(pat_p, tooltip = "text")
  
  return(pat_p) # ggplotly(pat_p, tooltip = "text")
}

## COST EFFECTIVENESS PLANE ----------------------------------------------------

cep_f <- function(
    psa_data, wtprange_v #, psa_costs_v, mu_v, wtprange_v
    # vector of QALYs, vector of costs, base case vector (length = 2), c(20K, 30K)
){
  
  psa_dt <- copy(psa_data)
  psa_dt[, ICER_s := paste(
    "Incremental Cost-Effectiveness Ratio\nICER =", ICER_s
  )]
  
  # WTP Polygon & labels
  
  band_dt <- data.table(
    x = c(-1, -1, 1, 1) * 10^5 , y = c(-wtprange_v, rev(wtprange_v)) * 10^5
  )
  wtp_s <- paste(paste(tocurr_f(wtprange_v), collapse = "-"), 
                 "/QALY threshold", sep = "")
  txt_dt <- data.table(
    "x" = c(-0.3, 0.3) * max(abs(psa_dt$QALYs)), 
    "y" = c(0.3, -0.3) * max(abs(psa_dt$Costs))
  )[, Txt := c("not cost-effective", "cost-effective")][
    , Txt2 := c(
      "ePROM monitoring leads to:\n - Higher costs \n - Worse health outcomes",
      "ePROM monitoring leads to:\n - Cost savings \n - Improved health outcomes"
    )]
  
  
  # Plot function
  
  psa_p <- psa_dt %>% ggplot(aes(
    x = QALYs, y = Costs, text = ICER_s, colour = Run, fill = Run,
    shape = Run, size = Run)) + ggtitle("ePROM monitoring is:") + 
    theme_bw() + geom_point() + 
    
    # Colour scales and formatting etc.
    scale_fill_manual(values = c(alpha(pal_m[1,2],0.1), pal_m[1,4])) + 
    scale_colour_manual(values = c(alpha(pal_m[1,2], 0), "black")) +
    scale_shape_manual(values = c(21, 23)) +
    scale_size_manual(values = c(3,4)) +
    
    # Axis scales & limits etc
    coord_cartesian(xlim = c(-1,1) * max(abs(psa_dt$QALYs)), 
                    ylim = c(-1,1) * max(abs(psa_dt$Costs))) + 
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1/5) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1/5) +
    scale_y_continuous(labels = tocurr_f) + 
    labs(x = "ΔQALYs", y = "ΔCosts" ) + 
    
    # WTP range
    geom_polygon(
      data = band_dt, aes(x = x, y = y, text = wtp_s),
      fill = "black", alpha = 0.1, inherit.aes = F) +
    
    # Text & labels
    geom_text(
      data = txt_dt, size = 5, inherit.aes = F, # angle = c(0, 45, 45), 
      color = alpha(c(pal_m[1, c("Red", "Green")]), 1), 
      # bg.color = "white", 
      aes(x = x, y = y, label = Txt, text = Txt2))
  
  # psa_plotly <- ggplotly(psa_p, tooltip = "text")
  
  return(psa_p) # ggplotly(psa_p, tooltip = "text")
  
}

## WTP PLOTS -------------------------------------------------------------------

wtplot_f <- function(wtp_data, wtprange_v){
  
  wtp_dt <- copy(wtp_data) # local copy
  ceac_v <- wtp_dt$CEAC
  
  # Transform so that axes can be plotted together
  evpii_v <- pretty(wtp_dt$EVPI, n = 5) # plotting interval vector
  evpimax_n <- max(evpii_v)
  if(length(evpii_v) > 6) { 
    evpii_v <- 0:10 * evpii_v[2]
    evpimax_n <- max(evpii_v)
  }
  wtp_dt[, CEAC := CEAC * evpimax_n]
  
  # Wide to Long
  wtpl_dt <- melt(
    wtp_dt, measure.vars = c("EVPI", "CEAC"), variable.name = "Curve")
  wtpl_dt[
    ,  Text := fifelse(
      Curve == "EVPI",
      paste0(
        "Expected Value of Perfect Information at WTP=", tocurr_f(WTP), ": ", 
        tocurr_f(value)
      ),
      paste0(
        "Probability that ePROM monitoring is \ncost-effective at WTP=", 
        tocurr_f(WTP), ": ", round(100 * value / evpimax_n), "%"
      )
    )
  ]
  
  # WTP box
  wtpr_dt <- data.table(
    y = c(0, evpimax_n, evpimax_n, 0), 
    x = rep(wtprange_v, each = 2)
  )
  wtp_s <- paste(paste(tocurr_f(wtprange_v), collapse = "-"), 
                 "/QALY \nthreshold range", sep = "")

  # PLOT FUNCTION
  
  wtp_p <- wtpl_dt %>% ggplot(aes(x = WTP)) +
    geom_polygon(
      data = wtpr_dt, aes(x=x, y=y), inherit.aes = F,
      fill = alpha("black", 0.1) #, text = wtp_s
    ) + 
    geom_text(
      label = wtp_s, y = 0.95 * evpimax_n, x = mean(wtprange_v),
      colour = "black", alpha = 0.1 #, alpha = 0.4 #, bg.colour = "white"
    ) +
    geom_point(aes(y = value, text=Text, colour = Curve), size = 2) +
    geom_line(aes(y = value, colour = Curve), size = 1) +
    scale_colour_manual(
      values = unname(pal_m[1, c(2, 4)]),
      labels = c(
        "Expected value of perfect information (EVPI)",
        "Probability that ePROM monitoring is cost-effective"
      )) +
    scale_x_continuous(
      labels = tocurr_f, 
      name = "Willingness-to-Pay (WTP) Threshold",
      breaks = pretty(wtp_dt$WTP)) +
    scale_y_continuous(
      name = paste0(
        "ePROM monitoring cost-effectiveness:\n",
        "← Less likely   |   More likely →"
      ),
      breaks = evpii_v, #[(evpii_v / evpimax_n) <= 1],
      labels = paste0(
        round(evpii_v[(evpii_v / evpimax_n) <= 1] / evpimax_n * 100), "%"), 
      sec.axis = sec_axis( ~.*1, breaks = evpii_v, labels = tocurr_f)
    ) +
    geom_hline(yintercept = 0.5 * evpimax_n, col = pal_m[1,4], linetype = 2) + 
    coord_cartesian(ylim = c(0, max(evpii_v))) +
    theme_bw() + 
    theme(
      
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_blank(),

      # CEAC
      axis.text.y = element_text(colour = pal_m[1,4]),
      axis.ticks.y = element_line(colour = pal_m[1,4], size = 1),
      axis.line.y = element_line(colour = pal_m[1,4], size = 1), 
      # axis.title.y = element_blank(), 
      # EVPI
      axis.text.y.right = element_text(colour = pal_m[1, 2]),
      axis.ticks.y.right = element_line(colour = pal_m[1,2], size = 1),
      axis.line.y.right = element_line(colour = pal_m[1,2], size = 1),
      axis.title.y.left = element_text(colour = pal_m[1, 4])
    )
    
    # WTP Box
  
  return(wtp_p) 
  
}


## OWSA TORNADO ----------------------------------------------------------------

owsa_f <- function(owsaplot_dt, base_wtp){
  # owsaplot_dt = data table, base_wtp = 20K, mean_inmb_n = numeric
  
  base_inmb <- owsaplot_dt$Base[1]
  
  owsal_dt <- copy(owsaplot_dt)[, y := 5:1][, Diff := NULL][, Var_l := NULL][
    , Variable := factor(Variable, levels = rev(Variable))] %>% setnames(
    c("Lower", "Upper", "CIl", "Mu", "CIu", "Base"), c(
      "INMB_Lower", "INMB_Upper", "Input_Lower", 
      "Input_Base", "Input_Upper", "INMB_Base"
      )) %>% melt(
        id.vars = c("Variable", "y"),
        measure.vars = measure(
          value.name, Bound, sep = "_"
        )
        # measure.vars = patterns("^INMB_", "^Input_"),
        # value.name = c("INMB", "Input")
      )
  owsal_dt[, Bound := factor(Bound, levels = c("Lower", "Base", "Upper"))]
  
  # SHADE EFFECT
  owsashade_dt <- copy(owsal_dt)[Bound != "Base",][
    , `:=`(Variable = NULL, Input = NULL, ID = 1:.N)][
      rep(1:.N, each = 1000),][, `:=`(
        Frac =(1:.N)^5/(.N^5), Alpha = pmax(0.005, (1:.N)^3/(.N^3))), by = ID][ # (1:.N)/.N
        , x := INMB - ((INMB - base_inmb) * (Frac))
      ]
  
  
  owsa_p <- owsal_dt %>% ggplot(aes(x = INMB, y = y, colour = Bound)) +
    geom_vline(xintercept = base_inmb, colour = pal_m[1,4], linetype = 2,) +
    geom_vline(xintercept = 0, colour = alpha("black", 0.6)) +
    theme_bw() + geom_point(size = 10) + 
    coord_cartesian(ylim = c(0.5,5.5)) + 
    geom_point(
      data = owsashade_dt, inherit.aes = F, aes(y=y, x=x, colour = Bound),
      alpha = 0.01, size = 5
    ) + geom_point(size = 12) +
    scale_colour_manual(values = unname(pal_m[1, c(2, 4, 3)])) +
    scale_x_continuous(
      labels = tocurr_f,
      sec.axis = sec_axis(
        ~.* 1, breaks = base_inmb, labels = tocurr_f(base_inmb),
        paste(
          "← Less cost-effective",
          "ePROM monitoring is cost-effective if INMB>0", 
           "More cost-effective →", sep = "   |   "
        ))) +
    geom_shadowtext(
      data = owsal_dt[Bound == "Lower",], bg.r = 0.2,
      aes(label=Input), colour = "white", bg.colour = pal_m[1,2], size = 4) +
    geom_shadowtext(
      data = owsal_dt[Bound == "Upper",], bg.r = 0.2,
      aes(label=Input), colour = "white", bg.colour = pal_m[1,3], size = 4) +
    geom_shadowtext(
      data = owsal_dt[Bound == "Base",], bg.r = 0.2,
      aes(label=Input), colour = "white", bg.colour = pal_m[1,4], size = 4) +
    theme(
      axis.text.y = element_blank(), # axis.title.y = element_blank(),
      # axis.title.x.top = element_text(),
      axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(), 
      panel.grid.minor.x = element_line(linetype = 2),
      axis.text.x.top = element_text(colour = pal_m[1,4])
    ) + 
    geom_shadowtext(
      data = owsal_dt[Bound == "Base",], size = 4, bg.r = 0.3,
      aes(label = Variable, y = y - 0.35),
      colour = "black", bg.colour = "white"
    ) +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    labs(
      x = paste("Impact on the Incremental Net Monetary Benefit (INMB) of", 
                "ePROM monitoring at a\n",
                "Willingness-to-Pay (WTP) threshold of", tocurr_f(base_wtp)), 
      y = "Model Parameter")
    # geom_shadowtext(aes(label=Input), colour = "white", size = 5)
  
  
  return(owsa_p)

  
}

## EVENT TIME PLOT -------------------------------------------------------------

eventplot_f <- function(ev_data, linear = T){
  
  ev_dt <- copy(ev_data)
  
  # MONTHS VS YEARS 
  th_n <- max(ev_dt$Time) # modified time horizon (year/months depending on value)
  y_b <- th_n > 24 # convert months to years?
  th_s <- c("Months", "Years")[y_b+1] # THIS IS THE DEFAULT
  th_div_n <- c(1, 12)[y_b+1] # 12 = months, 1 = years: multiplier if results are in months
  mth_n <- th_n/th_div_n # modified time horizon
  ev_dt[, Time := Time/th_div_n]
  
  mag <- mag_f(max(ev_dt$Count)) # Magnitude
  
  # Reconfigure factor order
  ev_dt[,Strategy := factor(Strategy, levels = c("ePROM", "UC"))]
  
  
  # PLOT 
  
  ev_p <- ev_dt %>% ggplot(aes( # linear scale plot
    x = Time, y = Count, linetype = Strategy, colour = Event
  )) + geom_step(linewidth = 0.75)  + theme_bw() +
    scale_colour_manual(values = unname(c(pal_m[1, c(3, 4, 2)], "black"))) +
    theme(text = element_text(size = 14)) +
    scale_y_continuous(labels = comma) +
    xlab(th_s) 
  
  
  if(th_n == 12){ # stops axis labelling every 2.5 points
    ev_p <- ev_p + scale_x_continuous(breaks = function(x) {pretty(x, n = 6)})
  }
  
  log_p <- ev_p + scale_y_continuous( # log plot
    trans="log2", labels = comma, breaks = c(
      10^(0:mag), 10^(0:mag) * 2, 10^(0:mag) * 5), minor_breaks = F
  )
  
  if(linear) return(ev_p) else return(log_p)
  
}

# SHINY UI ---------------------------------------------------------------------
chunk_n <- 1 # global chunk tracker for markdown
ui <- {page_navbar(
  id = "MainPage",
  
  ### THEME & TITLES -----------------------------------------------------------
  
  theme = bs_theme(
    bg = "white", fg = pal_m[1,2], 
    primary = pal_m[1,2], secondary = pal_m[1,4]),
  
  title = "Cost-Effectiveness of Monitoring Electronic Patient-Reported Outcomes",
  inverse = T,
  
  tags$style(HTML( # Tooltip width
    ".wide-tooltip {--bs-tooltip-max-width: 500px !important;}
    .wide-tooltip .tooltip-inner {text-align: left;}")),
  
  
  ### INTRO PAGE ---------------------------------------------------------------
  
  nav_panel(
    "About", 
    navset_card_tab(
      nav_panel("Introduction", pad_f(
        h2("Introduction"), md_f())),
      nav_panel("Explainers", pad_f(h2("Explainers"), md_f())),
      nav_panel("Model Structure", pad_f(
        h2("Model Structure"), md_f())),
      nav_panel("Instructions", pad_f(
        h2("Instructions"), md_f(), actionButton(
          "enter_model", "Enter the model.")
      ))
    )
  ),
  
  ### ENGINE PAGE --------------------------------------------------------------
  
  nav_panel(
    #### MODEL SETTINGS --------------------------------------------------------
    
    "Model Engine", {layout_sidebar(
      sidebar = sidebar(
        h3("Settings"), width = 450, bg = grey_v[1],
        
        # WIDGET MAP 
        do.call(navset_pill, c(
          lapply(section_l[["sidebar"]], function(s){section_f(s)}), 
          list("header" = hr())))
      ),
      
      
      card(
        
        #### BUTTONS -----------------------------------------------------------
        
        card_header(
          layout_columns(
            actionButton("run", "Update Model", class = "btn-secondary"),
            downloadButton("GenReport", "Generate report") # %>%  withSpinner(id = "report_spin", color = pal_m[1, 4])
            # actionButton("debug", "Print parameters")
          )
        ),
        
        #### RESULTS -----------------------------------------------------------
        
        navset_pill(
          nav_panel(
            "Inputs", p(""), md_f(), 
            plotlyOutput("pat_p") %>%  withSpinner(color = pal_m[1, 2]),
            md_f(), card(
              tableOutput("par_t") %>% fullscreen_this() %>% 
                withSpinner(color = pal_m[1, 2]), textOutput("seed"))
          ),
          
          nav_panel(
            "Base Case", p(""), md_f(), 
            card(
              fluidRow(
                column(
                  width = 4,
                  tableOutput("res_t") %>% withSpinner(color = pal_m[1, 2])),
                column(
                  width = 8,
                  tableOutput("res2_t") %>% withSpinner(color = pal_m[1, 2]))
              )
            ),
            # md_f(), # plotlyOutput("KM") %>% withSpinner(color = pal_m[1, 2]),
            
            md_f(), radioButtons(
              "ev_scale", "Y axis scale", 
              choices = c("log (base 2)" = 1, "linear" = 2),
              inline = T
            ), 
            conditionalPanel(
              "input.ev_scale == 1", 
              plotOutput("log_p") %>% fullscreen_this() %>% 
                withSpinner(color = pal_m[1, 2])
            ),
            conditionalPanel(
              "input.ev_scale == 2", 
              plotOutput("ev_p") %>% fullscreen_this() %>% 
                withSpinner(color = pal_m[1, 2])
            )
          ),
          nav_panel(
            "PSA", p(""), md_f(),
            plotlyOutput("CEP") %>% withSpinner(color = pal_m[1, 2]), md_f(),
            plotOutput("WTP") %>% withSpinner(color = pal_m[1, 2])),
          nav_panel(
            "OWSA", p(""), md_f(),
            plotOutput("OWSA")  %>% fullscreen_this() 
            %>% withSpinner(color = pal_m[1, 2])),
          nav_panel("References", p(""), md_f())
          
        )
      )
      
      
    )} # SIDEBAR PANEL WITH MODEL SETTINGS & OUTPUTS
  )
  
)}

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # EXTRACT PARAMETERS ---------------------------------------------------------
  
  get_input_f <- reactive(c(input_f(input)))
  
  # REACTIVES `_r`--------------------------------------------------------------
  
  # MODEL RESULTS
  model_r <- reactiveVal(NULL)
  # running <- reactiveVal(FALSE)
  
  # INDIVIDUAL ITEMS
  pat_r <- reactiveVal(NULL) # Patient plot breakdown
  seed_r <- reactiveVal(NULL)
  par_r <- reactiveVal(NULL) # input table placeholder
  bc1_r <- reactiveVal(NULL) # Costs/QALY table
  bc2_r <- reactiveVal(NULL) # ICER/INMB table
  ev_r <- reactiveVal(NULL) # Event count plot
  cep_r <- reactiveVal(NULL) # CEP
  wtp_r <- reactiveVal(NULL) # WTP
  owsa_r <- reactiveVal(NULL) # OWSA
  
  # NAVIGATION & FUNCTIONALITY -------------------------------------------------
  
  # NAVIGATE TO MODEL INTERFACE
  observeEvent(input$enter_model, nav_select("MainPage", "Model Engine"))
  
  observe({ # ENSURE USER CANNOT SELECT 0 CANCER SITES
    selected_v <- input$sites_v
    if(length(selected_v)==0) {
      showNotification(
        "You must select at least 1 option.", type = "error", duration = 5)
      updateCheckboxGroupInput(session, "sites_v", selected = 1:4)
    }
  })
  
  observe({ # ENSURE USER CANNOT SELECT 0 CANCER SITES
    selected_v <- input$survcond_v
    if(length(selected_v)==0) {
      showNotification(
        "You must select at least 1 option.", type = "error", duration = 5)
      updateCheckboxGroupInput(session, "survcond_v", selected = 1:3)
    }
  })

  # RUN MODEL ------------------------------------------------------------------
  observeEvent(input$run,{
    
    # RETRIEVE USER's INPUT PARAMETERS
    par_l <- get_input_f()
    par_l$pb_shiny <- T
    
    # RUN MODEL 
    results_l <- do.call(sim_f, par_l)
    model_r(results_l) 
    #debug_l <<- model_r() # debug
    #print(model_r()) # debug
    
    # SAVE OUTPUTS IN REACTIVE VARIABLES
    
    # Inputs
    pat_r(with(results_l, plotpat_f(patients))) # patient population plot
    seed_r(paste("Seed ID:", input$seed_n)) # save seed
    
    # Input Parameter table
    par_dt <- model_r()[["parameters"]]
    colnames(par_dt) <- c( # The model function scrambles column names without this
      "Variable", "Distribution", "Mean ± SE", "Bound", "Source"
    )
    par_r(par_dt) 
    
    # Base case
    bc1_r(model_r()[["costs_qalys"]]) # Costs & QALYs (base case)
    bc2_r(model_r()[["decision"]]) # ICER & INMB table
    ev_r(with(model_r(), eventplot_f(events, F))) # event plot (log scale)
    
    # PSA
    wtp_r(with(model_r(), wtplot_f(wtp, wtprange))) # WTP plot
    cep_r(with(model_r(), cep_f(psa, wtprange))) # CEP
    
    # OWSA
    owsa_r(with(model_r(), owsa_f(owsa, base_wtp)))
    

  }, ignoreNULL=F)
  
  # OUTPUTS --------------------------------------------------------------------
  
  # INPUT SUMMARY

  output$pat_p <- renderPlotly({
    req(model_r())
    ggplotly(pat_r(), tooltip = "text") # render Plotly (interactive)
  })
  
  output$par_t <- renderTable({
    req(model_r())
    par_r()
  }, striped = T)
  
  output$seed <- renderText({
    req(model_r())
    seed_r()
  })
  
  # BASE CASE RESULTS
  
  output$res_t <- renderTable({
    req(model_r())
    bc1_r()
  }, striped = T)
  
  output$res2_t <- renderTable({
    req(model_r())
    bc2_r()
  }, striped = T)
  
  # EVENTS 
  
  output$ev_p <- renderPlot({ # One-Way Sensitivity Tornado
    #print(model_r()[c("owsa", "base_wtp")])
    req(model_r())
    with(model_r(), eventplot_f(events, T))
  })
  
  output$log_p <- renderPlot({ # One-Way Sensitivity Tornado
    #print(model_r()[c("owsa", "base_wtp")])
    req(model_r())
    ev_r()
  })
  
  
  # PSA
  
  output$CEP <- renderPlotly({
    req(model_r())
    ggplotly(cep_r(), tooltip = "text")
  })
  
  output$WTP <- renderPlot({ # One-Way Sensitivity Tornado
    req(model_r())
    wtp_r()
  })
  
  # OWSA
  
  output$OWSA <- renderPlot({ # One-Way Sensitivity Tornado
    # print(model_r())
    req(model_r())
    owsa_r()
  })
  
 
  
  # REPORT ---------------------------------------------------------------------

  output$GenReport <- downloadHandler(

    filename = function() {paste0("ePROM-model-", Sys.Date(), ".docx")},

    content = function(file) {

      showPageSpinner(color = pal_m[1,4], background = alpha("white", 0.4))
      showNotification(
        "Generating report...", type = "message", duration = NULL,
        id = "report_gen"
      )

      on.exit(removeNotification("report_gen"),add = TRUE)

      tempReport <- file.path(tempdir(), "ePROM-model-.Rmd")
      file.copy("ePROM-model-.Rmd", tempReport, overwrite = TRUE)

      params <- list(
        "inp" = par_r(),
        "bc1" = bc1_r(),
        "bc2" = bc2_r(),
        "pat" = pat_r(),
        "seed" = paste("Random seed value used to generate results:", seed_r()),
        "evlog" = ev_r(),
        "cep" = cep_r(),
        "wtp" = wtp_r(),
        "owsa" = owsa_r()
      )
      
      print(params)

      rmarkdown::render(
        "UI text/report.Rmd",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )

      showNotification(
        "Report ready for download.", type = "message", duration = 3
      )
      hidePageSpinner()
    }

  )
}

# CREATE APP ===================================================================

# SHINY APP
shinyApp(ui = ui, server = server)


# TEST -------------------------------------------------------------------------

# test_l <- sim_f(pb_rcon = T)


# with(test_l, eventplot_f(events, F))
#
#
# seed_n <-  123 # random seed
# psa_n <-  10 # PSA samples
# th_n <-  3 * 12 # time horizon (months)
# p_n <-  5 # number of patients
# au_n <-  0.2 # Artificial uncertainty
# 
# 
# base_wtp <-  20000
# wtprange_v <-  2:3 * 10^4
# 
# 
# age_v <-  30:64 # min &  max age bands
# survcond_v <-  c(0, 1, 5) # allowed prior survival with cancer (years)
# sites_v <-  c("Breast", "Colorectal", "Lung", "Prostate") # vector of included cancer sites
# 
# 
# sc_surv <-  T # Use HR for survival benefit
# sc_lower_ed <-  T # Do ePROMs lower ED visits
# sc_lower_amb <-  T # Do ePROMs reduce ambulance use
# sc_custom_adh <-  F # input/change adherence
# sc_monitor <-  T # default monitoring cost
# sc_utility <-  T # default: use difference in utility values
# 
# 
# sc_monitor_c <-  0 # Custom per patient-month monitoring cost option
# sc_alert_c <-  0 # Cost per alert
# sc_adh_pat <-  0.6 # patients' adherence rate
# sc_adh_hcp <-  0.7 # HCP engagement rate
# sc_adh_eff <-  0.5 # Missing alerts x adherence
# 
# pb_rcon <-  F # show progress bar in R console
# pb_shiny <-  F # show progress bar in shiny




