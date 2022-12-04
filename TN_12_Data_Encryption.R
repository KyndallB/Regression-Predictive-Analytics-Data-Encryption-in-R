

## Setting Working directory
setwd("/Path/To/STAT4030/Datasets/")

# Install packages for hashing
install.packages("sodium")
library(sodium)


## Understand Seeding in R
sample(1:100, 5)


set.seed(1)
sample(1:100, 5)

set.seed(0517)
sample(1:100, 5)


## Introduction to Hashing
library(sodium)

your_password <- "STAT4030"

## Basic hashing - vulnerable to brute-force
hashed_password <- hash(charToRaw(your_password))
hashed_password


## advanced hashing - unbeatable as long as you don't lose your key
# hash your salt pin will make your data even more secure 
# size of hashed values are fixed for some hashing algorithm
your_key_256 <- hash(charToRaw("0517"), size=32)
your_key_512 <- hash(charToRaw("0517"), size=64)

# now we try to use sha256 and sha512 for hashing
credit_card_number <- "STAT4030"

sha256(charToRaw(credit_card_number), key = your_key_256)
sha512(charToRaw(credit_card_number), key = your_key_512)


# let's apply perform hashing for all customers in your dataset
# we will use funtion and mapply technique for this task
# first building a function performing sha256 hashing
hashing_sha256 <- function (hashing_object, your_key) {
  hashed_key_256 <- hash(charToRaw(your_key), size=32)
  hashing_object_256 <- charToRaw(hashing_object)
  hashed_value <- sha256(hashing_object_256, key = hashed_key_256)
  return (paste(hashed_value, collapse = ""))
}

your_pin = "0517"
credit_card_number = "STAT4030"

hashing_sha256(credit_card_number, your_pin)


# grab one of your customers to see if the function works
restaurant_vip <- read.csv("Restaurant_Vip/2021_05_02.csv")

your_salt_pin = "0916"

hashing_sha256(restaurant_vip$name[56], your_salt_pin)

# now we create a column containing hashed name for all customers
restaurant_vip$hashed_name <- mapply(hashing_sha256, 
                                     restaurant_vip$name, 
                                     your_salt_pin)

# check consistency of your hashing
hashed_martha_moore <- hashing_sha256("Martha Moore", your_salt_pin)
restaurant_vip[restaurant_vip$hashed_name==hashed_martha_moore,]

# delete the column containing sensitive information
restaurant_vip <- restaurant_vip[ , -which(colnames(restaurant_vip)=="name")]

