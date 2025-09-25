# Install and load necessary libraries
install.packages("dplyr")


library(dplyr)


# Set directory
setwd("C:\\Users\\ecche\\Desktop\\Tesi\\HFCS\\HFCS2014\\dati_wave2_csv")

# Clean
rm(list = ls())


--------------------------------------------------------------------------------
# DATA MANIPULATION
--------------------------------------------------------------------------------

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
  left_join(data_core2 %>% select(SA0010, HD1800, HD1300, HD1310a,HD1310b,HD1310c,
                                  HD1310d,HD1310e,HD1310f,HD1310g,HD1320a,HD1320b,
                                  HD1320c,HD1320d,HD1320e,HD1320f,HD1320g), by = "SA0010") %>%
  left_join(data_core1 %>% select(SA0010, RA0010, PA0100, PE0300), 
            by = c("SA0010", "RA0010")) 


# Select only relevant variables 
data_sub <- df_merged %>% select(SA0010, RA0010, HW0010, DH0001,DHAGEH1,DHGENDERH1,DHEDUH1,
                                DHEMPH1, DA1110 , DA1110i,DA1120, DA1130, DA1131, 
                                DA1140 , DA2101 , DA2102, DA2103 , DA2104, DA2105 , 
                                DA2105i,  DA2106 , DA2107 , DA2108 , DA2109 , DA2109i, 
                                DA3001, DN3001, DL1000, DI2000, PE0300, PA0100,HD1800, HD1300, HD1310a,HD1310b,HD1310c,
                                HD1310d,HD1310e,HD1310f,HD1310g,HD1320a,HD1320b,
                                HD1320c,HD1320d,HD1320e,HD1320f,HD1320g )

# Merge relevant variables with replication weights and remove irrelevant columns 
data <- data_sub %>% left_join(repl_weights, by = "SA0010")
data_final <- data %>% select(-sa0100,-((ncol(data)-2):ncol(data)) )

ss






