# Install and load necessary libraries
if (!require("dplyr", quietly = TRUE)) {install.packages("dplyr")}
if (!require("survey", quietly = TRUE)) {install.packages("survey")}
if (!require("Hmisc", quietly = TRUE)) {install.packages("Hmisc")}
if (!require("purrr", quietly = TRUE)) {install.packages("purr")}
if (!require("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}
if (!require("scales", quietly = TRUE)) {install.packages("scales")}



library(dplyr)
library(survey)
library(Hmisc)
library(purrr)
library(tidyverse)
library(scales)


# Set directory
setwd("C:\\Users\\ecche\\Desktop\\Tesi\\HFCS\\HFCS2014\\dati_wave2_csv")

# Clean
rm(list = ls())


# ==============================================================================
# 1. DATA CLEANING
# ==============================================================================

# Load data
data_der <- read.csv("d1.csv", sep = ",", header = TRUE)
repl_weights <- read.csv("w.csv",sep=",",header = TRUE)
data_core1 <- read.csv("p1.csv",sep = ",", header = TRUE)
data_core2 <- read.csv("h1.csv",sep = ",",header=TRUE)
data_noncore2 <- read.csv("hn1.csv",sep = ",",header=TRUE)
data_noncore1 <- read.csv("pn1.csv",sep = ",", header = TRUE)

# Rename variables 
repl_weights <- repl_weights %>%
  rename(SA0010 = sa0010)

data_der <- data_der %>%
  rename(RA0010=DHIDH1)


# Select and merge column HD1800 (investment attitude) from data_core2 and column PA0100 (marital status) and column PE0300 (Job description) from data_core1
df_merged <- data_der %>%
  left_join(data_core2 %>% select(SA0010, HD1800, HD1300,HD1320a,HD1320b,
                                  HD1320c,HD1320d,HD1320e,HD1320f,HD1320g), by = "SA0010") %>%
  left_join(data_core1 %>% select(SA0010, RA0010, PA0100, PE0300), 
            by = c("SA0010", "RA0010")) 


# Select only relevant variables 
data_final <- df_merged %>% select(SA0010, RA0010, HW0010, DH0001,DHAGEH1,DHGENDERH1,DHEDUH1,
                                DHEMPH1, DA1110 , DA1110i,DA1120, DA1130, DA1131, 
                                DA1140 , DA2101 , DA2102, DA2103 ,DA2100 ,DA2104, DA2105 ,
                                DA2106 , DA2107 , DA2108 , DA2109 , DA2109i, 
                                DA3001, DN3001, DL1000, DI2000, PE0300, PA0100,HD1800,
                                HD1320a,HD1320b,HD1320c,HD1320d,HD1320e,HD1320f,HD1320g)

# Merge relevant variables with replication weights and remove irrelevant columns 
#data <- data_sub %>% left_join(repl_weights, by = "SA0010")
# data_final <- data %>% select(-sa0100,-((ncol(data)-2):ncol(data)) )


# NA MANAGEMENT
counts_na <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}
counts_na(data_final)

# Substitute NA with 0 (no possession) for variables relating to asset value, liabilities, and income 
data_final <- data_final %>%
  mutate(across(c(DA1110,DA1120,DA1130,DA1131,DA1140,DA2101,DA2102,DA2103,DA2104,DA2105,
                  DA2106,DA2107,DA2108,DA2109,DA2100,DL1000,HD1320a,HD1320b,HD1320c,HD1320d,
                  HD1320e,HD1320f,HD1320g), ~replace(., is.na(.), 0)))
# Check
counts_na(data_final)

# Min value
my_min<-function(df) {
  sapply(df, function(x) min(x))
}
my_min(data_final)

# Number households with net_wealth < 0 
sum(data_final$DN3001 < 0, na.rm = TRUE)





# ==============================================================================
# 2. EXPLORATORY ANALYSIS
# ==============================================================================

# Create survey design
design <- svydesign(ids = ~1,  weights = ~HW0010, data = data_final)

## Create quartiles of net wealth and assign a value (1:4) for each quartile
nw_quartile <- svyquantile(~DN3001,design,quantiles =c(0.25,0.50,0.75))
cutoff <- nw_quartile[[1]][,1]
data_final$quartile <- cut(data_final$DN3001, breaks=c(-Inf,cutoff, Inf),labels=1:4,include.lowest=TRUE,right=FALSE)


# Compute top 5% and create dummy 
nw_95 <- svyquantile(~DN3001, design, quantiles = 0.95)
cutoff95 <- nw_95[[1]][[1]]
data_final$top5 <- ifelse(data_final$DN3001 >= cutoff95, 1, 0)


# Compute top 1% and create dummy 
nw_99 <- svyquantile(~DN3001, design, quantiles = 0.99)
cutoff99 <- nw_99[[1]][[1]]
data_final$top1 <- ifelse(data_final$DN3001 >= cutoff99, 1, 0)



# Compute median net wealth
nw_median <- svyquantile(~DN3001, design, quantiles = 0.5)

# Compute mean net wealth
nw_mean <- svymean(~DN3001, design)
  
# Compute first 5% net wealth
nw_p5 <- svyquantile(~DN3001, design, quantiles = 0.05)




# ------------------------------------------------------------------------------
# PTF COMPOSITION total and divided by quartile of net-wealth 
# ------------------------------------------------------------------------------

## Update survey design
design <- svydesign(ids = ~1, weights = ~ HW0010, data = data_final)


## Compute average PTF composition and by quartile of net wealth

### Define function to compute asset share
calc_shares <- function(design_obj, label) {
  tot_HMR      <- sum(svytotal(~DA1110, design_obj, na.rm = TRUE)) # Household main residence 
  tot_ORE      <- sum(svytotal(~DA1120, design_obj, na.rm = TRUE)) # Other real estate
  tot_ORA      <- sum(svytotal(~DA1130 + DA1131, design_obj, na.rm = TRUE)) # Other real assets (vehicles and valuables)
  tot_BUS<- sum(svytotal(~DA1140, design_obj, na.rm = TRUE)) # Self-employment business
  tot_BUS      <- sum(svytotal(~DA1140, design_obj, na.rm = TRUE)) # Self-employment business
  tot_OFA      <- sum(svytotal(~DA2101+DA2103+DA2104+DA2106+DA2107+DA2108+DA2109+
                                 HD1320b+HD1320c+HD1320d+HD1320e+HD1320f+HD1320g,
                               design_obj, na.rm = TRUE)) # Other financial assets (mutual funds (exl. equity), 
                                                    # managed accounts, money owed to households, other assets, 
                                                    # deposits, bonds, non-employment business, voluntary pension - life insurance) 
  tot_ALLSTOCK <- sum(svytotal(~DA2105+HD1320a, design_obj, na.rm = TRUE)) # Stock + mutual funds that invest prevalently in shares
  tot_gross    <- sum(svytotal(~DA3001, design_obj, na.rm = TRUE))  # Total asset value (gross-wealth)
  
  tibble(
    quartile = as.character(label),
    HMR      = tot_HMR / tot_gross,
    ORE      = tot_ORE / tot_gross,
    ORA      = tot_ORA / tot_gross,
    BUS     = tot_BUS / tot_gross,
    OFA    = tot_OFA / tot_gross,
    ALLSTOCK = tot_ALLSTOCK / tot_gross
  )
}

### General
gen_comp <- calc_shares(design, "Overall")

### By quartile
comp_byquart <- map_dfr(1:4, ~calc_shares(subset(design, quartile == .x), paste0(.x, "° quartile")))

### TOP 5%
comp_TOP5 <- calc_shares(subset(design, top5 == 1), "Top 5%")


### Union
ptf_comp <- bind_rows(gen_comp, comp_byquart,comp_TOP5)


# Convert dataset in long format 
ptf_long <- ptf_comp %>%
  pivot_longer(
    cols = HMR:ALLSTOCK,
    names_to = "Asset",
    values_to = "Share"
  )

# Stacked chart 
ggplot(ptf_long, aes(x = factor(quartile, levels = c("Overall", "1° quartile", "2° quartile", "3° quartile", "4° quartile","Top 5%")), y = Share, fill = Asset)) +
  geom_col(width=0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "HMR" = "#FFFDD0",    
    "ORE" = "#FFE4C4",      
    "ORA" = "#D8BFD8",      
    "BUS" = "#D0F0C0",
    "ALLSTOCK" = "#FFB3BA",
    "OFA" = "#E6E6FA"     
    
  ),labels = c(
    "HMR" = "HMR",
    "ORE" = "ORE",
    "ORA" = "ORA",
    "BUS" = "BUS",
    "OFA" = "OFA",
    "ALLSTOCK" = "ALL STOCK"
  ),  ) +
  labs(# title = "Composizione del portafoglio per quartile di ricchezza netta",
    x = "",
    y = "Quota (%)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "bottom",legend.text = element_text(size = 7),
        legend.key.size = unit(0.35, "cm"))+
  guides(fill = guide_legend(nrow = 1))



# ------------------------------------------------------------------------------
# PTF COMPOSITION FINANCIAL ASSETS total and divided by quartile of net-wealth 
# ------------------------------------------------------------------------------

### Define function to compute fin asset share
calc_shares_fin <- function(design_obj, label) {
  tot_MF      <- sum(svytotal(~HD1320b+HD1320c+HD1320d+HD1320e+HD1320f+HD1320g, design_obj, na.rm = TRUE)) # Mutual funds (excl. prev. equity) 
  tot_VOL     <- sum(svytotal(~DA2109, design_obj, na.rm = TRUE)) # Voluntary pensions and whole life insurance policies
  tot_DEP      <- sum(svytotal(~DA2101, design_obj, na.rm = TRUE)) # Deposits
  tot_BONDS <- sum(svytotal(~DA2103, design_obj, na.rm = TRUE)) # Bonds
  tot_OFA      <- sum(svytotal(~DA2104+DA2106+DA2107+DA2108, design_obj, na.rm = TRUE)) # Other financial assets, 
                                                                   # managed accounts, money owed to households, other assets, 
                                                                  # non-employment business,
  tot_ALLSTOCK <- sum(svytotal(~DA2105+HD1320a, design_obj, na.rm = TRUE)) # Stock + mutual funds that invest prevalently in shares
  tot_fin    <- sum(svytotal(~DA2100, design_obj, na.rm = TRUE))  # Total  value financial assets
  
  tibble(
    quartile = as.character(label),
    MF     = tot_MF / tot_fin,
    VOL     = tot_VOL / tot_fin,
    DEP     = tot_DEP / tot_fin,
    BONDS     = tot_BONDS / tot_fin,
    OFA    = tot_OFA / tot_fin,
    ALLSTOCK = tot_ALLSTOCK / tot_fin
  )
}

### General
gen_comp_fin <- calc_shares_fin(design, "Overall")

### By quartile
comp_fin_byquart <- map_dfr(1:4, ~calc_shares_fin(subset(design, quartile == .x), paste0(.x, "° quartile")))

### TOP 5%
comp_fin_TOP5 <- calc_shares_fin(subset(design, top5 == 1), "Top 5%")

### Union
ptf_comp_fin <- bind_rows(gen_comp_fin, comp_fin_byquart,comp_fin_TOP5)


# Convert dataset in long format 
ptf_long_fin <- ptf_comp_fin %>%
  pivot_longer(
    cols = MF:ALLSTOCK,
    names_to = "Asset",
    values_to = "Share"
  )

# Stacked chart 
ggplot(ptf_long_fin, aes(x = factor(quartile, levels = c("Overall", "1° quartile", "2° quartile", "3° quartile", "4° quartile","Top 5%")), y = Share, fill = Asset)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    "MF" = "#FFFDD0",    
    "VOL" = "#FFE4C4",      
    "BONDS" = "#D8BFD8",      
    "DEP" =  "#D0F0C0",
    "ALLSTOCK" = "#FFB3BA",
    "OFA" = "#E6E6FA"),
    labels = c(
    "MF" = "MF",
    "VOL" = "VOL",
    "BONDS" = "BOND",
    "DEP" = "DEP",
    "OFA" = "OFA",
    "ALLSTOCK" = "ALL STOCK"
  ),  ) +
  labs(# title = "Composizione del portafoglio per quartile di ricchezza netta",
    x = "",
    y = "Quota (%)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",legend.text = element_text(size = 7),
        legend.key.size = unit(0.35, "cm"))+
  guides(fill = guide_legend(nrow = 1))



# ---------------------------------------------------------------------------------
# STOCK MARKET PARTICIPATION  
# ---------------------------------------------------------------------------------

# Define the function plot chart 
plot_smp_category <- function(data, x_label = "Categoria", y_label = "SMP (Media)") {
  x_var <- names(data)[1]
  y_var <- names(data)[2] 
  
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), group = 1)) +
    geom_line(color = "#4a4a4a", linewidth = 1, alpha = 0.8) +
    geom_point(size = 3.8, shape = 21,
               color = "#2c3e50", fill = "#8ab6d6", stroke = 1.1, alpha = 0.9) +
    labs(x = x_label,
         y = y_label) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       breaks = scales::pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.02, 0.08))) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5,
                                margin = margin(b = 12), color = "#2c3e50"),
      axis.title.x = element_text(face = "bold", size = 11, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = 11, margin = margin(r = 8)),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 10, color = "#3d3d3d"),
      axis.text.y = element_text(size = 10, color = "#3d3d3d"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "#dfe6e9"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 12, 12, 12)
    )
}


# Create dummies  
data_final$SMP <- ifelse(data_final$DA2105 + data_final$HD1320a>0, 1,0) # SMP

data_final$share<-ifelse(data_final$SMP==1,(data_final$DA2105+data_final$HD1320a)/ (data_final$DA2100),0) # Conditional SMP share 

data_final$gender <- ifelse(data_final$DHGENDERH1==1,1,0) # gender = 1 if male, = otherwise 

data_final$mar_status <- ifelse(data_final$PA0100 %in% c(2, 3) ,1,0) # mar_status = 1 if married or consensual union, 0 otherwise

data_final$HH_size <- ifelse(data_final$DH0001 >3,1,0) # Normal size = 0 if household members are <=3 (normal size), 1 otherwise

data_final$Busjob <- ifelse(data_final$PE0300 %in% c(12, 24), 1, 0)# if job is related to business 1, otherwise 0

data_final$age_brackets <- cut(data_final$DHAGEH1, breaks = c(-Inf, 30, 40, 50, 60, 70, Inf),  
                               labels = c("<31", "31-40", "41-50", "51-60", "61-70", ">70"),
                               right = TRUE) # create brackets for age 

data_final$college <- ifelse(data_final$DHEDUH1>2, 1,0) # college = 1 if at least upper secondary, 0 otherwise

data_final$selfemp <- ifelse(data_final$DHEMPH1==2, 1,0) # selfemp = 1 if self-employed, 0 otherwise
  

fw_quartiles <- svyquantile(~DA2100,design,quantiles =c(0.25,0.50,0.75)) # create quartiles of financial wealth 
cut <- fw_quartiles[[1]][,1]
data_final$fw_quartiles <- cut(data_final$DA2100, breaks=c(-Inf,cut, Inf),labels=c("Q1","Q2","Q3","Q4"),right=TRUE, include.lowest=TRUE)  

inc_quartiles <- svyquantile(~DI2000,design,quantiles =c(0.25,0.50,0.75)) # create quartiles of total gross income
cut_inc <- inc_quartiles[[1]][,1]
data_final$inc_quartiles <- cut(data_final$DI2000, breaks=c(-Inf,cut_inc, Inf),labels=c("Q1","Q2","Q3","Q4"),right=TRUE, include.lowest=TRUE)  


# New design 
new_design<-svydesign(ids=~1,weights= ~HW0010, data = data_final)


# EXTENSIVE MARGIN

## Compute mean SMP
SMP_mean<-svymean(~SMP, new_design) 

## Compute SMP by gender
SMP_by_gender<-svyby(~SMP,~gender,new_design,svymean)

## Compute SMP by age brackets and plot
SMP_by_age_brackets<-svyby(~SMP,~age_brackets,new_design,svymean)
plot_smp_category(SMP_by_age_brackets, x_label = "Classi di età",y_label="SMP (Margine Estensivo)")

## Compute SMP by marital status
SMP_by_mar_status<-svyby(~SMP,~mar_status,new_design,svymean)

## Compute SMP by number of household member
SMP_by_HHsize<-svyby(~SMP,~HH_size,new_design,svymean)

## Compute SMP by college
SMP_by_college<-svyby(~SMP,~college,new_design,svymean)

## Compute SMP by labour status
SMP_by_selfemp<-svyby(~SMP,~selfemp,new_design,svymean)

## Compute SMP by type of job: business related job
SMP_by_bus<-svyby(~SMP,~Busjob,new_design,svymean)
  
## Compute SMP by pension
SMP_by_pens_life<-svyby(~SMP,~DA2109i,new_design,svymean)

## SMP by Risk Aversion
SMP_by_riskav<-svyby(~SMP,~HD1800,new_design,svymean)
plot_smp_category(SMP_by_riskav, x_label = "Risk Aversion",y_label="SMP (Margine Estensivo)")

## SMP by HMR 
SMP_by_HMR<-svyby(~SMP,~DA1110i,new_design,svymean)

## SMP by quartiles of financial wealth
SMP_by_fw<-svyby(~SMP,~fw_quartiles,new_design,svymean)
plot_smp_category(SMP_by_fw, x_label = "Quartili di ricchezza finanziaria",y_label="SMP (Margine Estensivo)")

## SMP by quartiles of income
SMP_by_inc<-svyby(~SMP,~inc_quartiles,new_design,svymean)
plot_smp_category(SMP_by_inc, x_label = "Quartili reddito lordo",y_label="SMP (Margine Estensivo)")




# INTENSIVE MARGIN

# Compute conditional mean SMP share
cond_share_mean<-svymean(~share,subset(new_design, SMP==1))

## Compute conditional mean SMP share by gender
Condshare_by_gender<-svyby(~share,~gender,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by age brackets 
Condshare_by_age_brackets<-svyby(~share,~age_brackets,subset(new_design, SMP==1),svymean)
plot_smp_category(Condshare_by_age_brackets, x_label = "Classi di età",y_label="SMP (Margine Intensivo)")

## Compute conditional mean SMP share by marital status
Condshare_by_mar_status<-svyby(~share,~mar_status,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by number of household member
Condshare_by_HHsize<-svyby(~share,~HH_size,new_design,svymean)

## Compute conditional mean SMP share by Education
Condshare_by_college<-svyby(~share,~college,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by labour status
Condshare_by_selfemp<-svyby(~share,~selfemp,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by type of job: business related job
Condshare_by_bus<-svyby(~share,~Busjob,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by pension
Condshare_by_pens_life<-svyby(~share,~DA2109i,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by Risk Aversion
Condshare_by_riskav<-svyby(~share,~HD1800,subset(new_design, SMP==1),svymean)
plot_smp_category(Condshare_by_riskav, x_label = "Risk Aversion",y_label="SMP (Margine Intensivo)")

## Compute conditional mean SMP share by HMR 
Condshare_by_HMR<-svyby(~share,~DA1110i,subset(new_design, SMP==1),svymean)

## Compute conditional mean SMP share by quartiles of financial wealth
Condshare_by_fw<-svyby(~share,~fw_quartiles,subset(new_design, SMP==1),svymean)
plot_smp_category(Condshare_by_fw, x_label = "Quartili di ricchezza finanziaria",y_label="SMP (Margine Intensivo)")

## Compute conditional mean SMP share by quartiles of income
Condshare_by_inc<-svyby(~share,~inc_quartiles,subset(new_design, SMP==1),svymean)
plot_smp_category(Condshare_by_inc, x_label = "Quartili reddito lordo",y_label="SMP (Margine Intensivo)")





# ==============================================================================
# 3. EMPIRICAL ANALYSIS
# ==============================================================================





