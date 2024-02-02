#### 1 ####
#1) Use R to figure out how many elements in the vector below are greater than 2.

#Jeg tildeler objektet "rooms" dataene
rooms <- c(1, 5, 2, 1, 3, 1, NA, 3, 1, 3, 2, 1, NA, 1, 8, 3, 1, 4, NA, 1, 3, 1, 2, 1, 7, 1, NA)
rooms
#Jeg laver en liste af rum, som udelader alle ikke-observationer, dem som er NA
rooms_no_na <- na.omit(rooms)
rooms_no_nav
#Jeg bruger funktionen "length", som tæller antallet af oberservationer i et givent objekt, sammen med funktionen af objektet, som er over 2.
#Jeg giver det det tilfældige navn "rooms_over_2"
rooms_over_2 <- length(rooms_no_na[rooms_no_na>2])
rooms_over_2
#Der er 9 rum, som er større end 2 i datasættet


#### 2 ####
##2) Which function tells you the type of data the 'rooms' vector above contains?

#Funktionen "class" fortæller mig hvilken type data det er. Det kan skelne mellem forskellige slags: numerisk, logisk, tekst
class(rooms)
#Vectoren består af numerisk data


#### 3 ####
##3) What is the result of running the median() function on the above 'rooms' vector?

#Jeg bruger median()-funktionen på "rooms"
median(rooms)
#Den giver mig NA for at indikere at den ikke kan finde medianen af en række data, som indeholder ikke-observationer, manglende data
#I stedet burde man først udelade NA, som vi har gjort i objektet "rooms_no_NA"



#### 4 ####
##4) Submit the following image to Github: Inside your R Project (.Rproj), install the 'tidyverse' package and use the download.file() and read_csv() function to read the SAFI_clean.csv dataset into your R project as 'interviews' digital object (see instructions in https://datacarpentry.org/r-socialsci/setup.html and 'Starting with Data' section). Take a screenshot of your RStudio interface showing

#install packages
install.packages("tidyverse")
install.packages("here")
library("tidyverse")
library("here")

#Download filen fra nettet
download.file(
  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",
  "data/SAFI_clean.csv", mode = "wb")

# objektet interviews bliver til alt data fra filen SAFI_clean.csv. "Here"-pakken gør at jeg bare kan fortælle hvilken mappe, og navnet på filen jeg skal bruge
interviews <- read_csv(
  here("data", "SAFI_clean.csv"))
  