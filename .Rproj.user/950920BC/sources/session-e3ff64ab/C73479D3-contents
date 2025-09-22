# Install and load necessary libraries
install.packages("dplyr")


library(dplyr)


# Set directory
setwd("C:\\Users\\ecche\\Desktop\\Tesi\\HFCS\\HFCS2014\\dati_wave2_csv")

# Load data
data <- read.csv("d1.csv", sep = ",", header = TRUE)
repl_weights <- read.csv("w.csv",sep=",",header = TRUE) 


# Rename variables 
data <- data %>%
  rename(ws = HW0010)

repl_weights <- repl_weights%>%
  rename(SA0010 = sa0010)

sdde