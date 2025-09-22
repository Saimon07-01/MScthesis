# Install and load necessary libraries
install.packages("dplyr")


library(dplyr)


# Set directory
setwd("C:\\Users\\ecche\\Desktop\\Tesi\\HFCS\\HFCS2014\\dati_wave2_csv")

# Load data
data_der <- read.csv("d1.csv", sep = ",", header = TRUE)

repl_weights <- read.csv("w.csv",sep=",",header = TRUE)

data_core1 <- read.csv("p1.csv",sep = ",", header = TRUE)
data_core2 <- read.csv("h1.csv",sep = ",",header=TRUE)

data_noncore2 <- read.csv("hn1.csv",sep = ",",header=TRUE)
data_noncore1 <- read.csv("pn1.csv",sep = ",", header = TRUE)


# Rename variables 
data_der <- data_der %>%
  rename(ws = HW0010)

repl_weights <- repl_weights%>%
  rename(SA0010 = sa0010)

