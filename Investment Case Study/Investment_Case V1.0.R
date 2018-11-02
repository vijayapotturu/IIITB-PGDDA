# Load packages
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

                            ################
                            # Checkpoint 1 #
                            ################
# Load the companies and rounds data into two data frames and name them companies and
# rounds2 respectively.

# Need to set 'fill = TRUE' for companies as some elements in companies.txt file are
# empty.
companies <- read.delim("companies.txt", sep = "\t", fill = TRUE, header = TRUE,
                        stringsAsFactors = FALSE, quote = "")
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE, quote = "")

###################################################################################

# How many unique companies are present in rounds2?
nrow(rounds2)
length(unique(rounds2$company_permalink))

# There are 114949 entries but only 90247 unique entries of company_permalink, which
# is the unique identifier for each company. However there is inconsisency in the use
# of upper and lower case so not all duplicates are correctly identified. Create a
# new variable in each dataframe with permalink converted to lower case only.
rounds2$permalink_lower <- tolower(rounds2$company_permalink)
companies$permalink_lower <- tolower(companies$permalink)

# Use this new variable to check for unique companies.
length(unique(rounds2$permalink_lower))

# There are 66368 unique companies in rounds2.

###################################################################################

# How many unique companies are present in companies?
length(unique(companies$permalink_lower))

# There are 66368 entries and all 66368 are unique companies in companies.txt.

###################################################################################

# In the companies data frame, which column can be used as the unique key for each
# company? Write the name of the column.

# permalink

###################################################################################

# Are there any companies in the rounds2 file which are not present in companies?
# Answer yes or no: Y/N
unique_to_rounds2 <- subset(rounds2, !(permalink_lower %in% companies$permalink_lower))
nrow(unique_to_rounds2)

# There are no companies in the rounds2 file, which are not present in
# companies. Therefore the answer is: no/N.

###################################################################################

# Merge the two data frames so that all variables (columns) in the companies frame are
# added to the rounds2 data frame. Name the merged frame master_frame. How many
# observations are present in master_frame?

# Create a left outer join on rounds2.
master_frame <- merge(x = rounds2, y = companies, by = "permalink_lower",
                      all.x = TRUE)
nrow(master_frame)

# 114949 observations are present in master_frame

###################################################################################

                                  ################
                                  # Checkpoint 2 #
                                  ################

# Average funding amount of funding_round_types

# Data quality: some rounds have NA as raised_amount_usd. Remove these rows
# from the datasetwith raised_amount_usd is NA.
master_frame <- subset(master_frame, !is.na(master_frame$raised_amount_usd))
# This leaves 94959 companies in master_frame.

# Group by funding_round_type and find the mean of each group.
by_funding_round <- group_by(master_frame, funding_round_type)
average_funding_amounts <- summarize(by_funding_round,
                                     mean_raised_amount_usd = mean(raised_amount_usd, na.rm = TRUE))
colnames(average_funding_amounts) <- c("fund_type", "avg_raised_amt")
average_funding_amounts

# Table 2.1 1.Average funding amount of venture type 

filter(group_by(average_funding_amounts, fund_type), fund_type == "venture")

# Average funding amount for venture type:       11,748,949

# Table 2.1 2.Average funding amount of angel type

filter(group_by(average_funding_amounts, fund_type), fund_type == "angel")

# Average funding amount of angel type:             958,694
# Table 2.1 3.Average funding amount of seed type

filter(group_by(average_funding_amounts, fund_type), fund_type == "seed")
# Average funding amount of seed type:              719,818

# Table 2.1 4.Average funding amount of private_equity type

filter(group_by(average_funding_amounts, fund_type), fund_type == "private_equity")

# Average funding amount of private equity type: 73,308,593

###################################################################################

# Considering that Spark Funds wants to invest between 5 to 15 million USD per
# investment round, which investment type is the most suitable for it?

# Table 2.1 5. Filter average_funding_amounts to show only those with a mean within the
# required range.
filter(average_funding_amounts, avg_raised_amt >= 5000000 &
         avg_raised_amt <=15000000)

# Venture is the only suitable option.

###################################################################################

                                ################
                                # Checkpoint 3 #
                                ################

# Identify the top three English-speaking countries in the data frame top9.

# Combine the list of all countries in the dataset with English as an official
# language (from the PDF file provided).
english_speaking <- c("USA", "GBR", "IND", "CAN", "PAK", "AUS", "SGP", "IRL", "NZL",
                      "GHA", "ZAF", "BAH", "TAN", "KEN", "DOM", "MLT", "UGA", "RWA",
                      "SYC", "ZWE", "BRB", "DMA", "JAM", "TTO", "ZMB", "GRD", "BLZ",
                      "BWA", "CMR", "KNA")

# Add this as a new boolean column to master_frame. This can be used when we export
# to Tableau later to highlight English-speaking countries on the charts.
master_frame$english_speaking_country <-
  master_frame$country_code %in% english_speaking

# Our chosen investment type is venture, so create a dataframe to show only these
# observations.
venture_investments <- filter(master_frame, funding_round_type == "venture" &
                                english_speaking_country == TRUE)

# Group by country_code and to find sum of raised_amount_usd for each country.
by_country <- group_by(venture_investments, country_code)
total_country_funding <- summarize(by_country, total_raised = sum(as.numeric(raised_amount_usd), na.rm = TRUE))

# Arrange by descending total_raised and slice the first 9 countries
top9 <- slice(arrange(total_country_funding, desc(total_raised)), 1:9)
top9

# Top English-speaking country: USA
# Second English-speaking country: GBR
# Third English-speaking country: IND

###################################################################################

                                  ################
                                  # Checkpoint 4 #
                                  ################

# Extract the primary sector of each category list from the category_list column

# Create a new variable "primary_sector" to store the primary sector of each company
venture_investments$primary_sector <- sapply(strsplit(venture_investments$category_list, "\\|"),"[", 1)

# Use the mapping file 'mapping.csv' to map each primary sector to one of the eight
# main sectors (Note that ‘Others’ is also considered one of the main sectors)

# Load the mapping file into a dataframe called "mapping"
mapping = read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)

# Data quality: in multiple categories, "na" and "Na" have been replaced by "0". This
# will result in non-matches against the companies file and incorect matches into the
# "Others" category. Therefore replace all instances of "0" with "na" in this
# column only apart from where the "0" is immediately preceded by "." (i.e. for the 
# category_list "Enterprise 2.0").
mapping$category_list <- sapply(mapping$category_list, function(x) gsub("(?<!\\.)0", "na", x, perl = TRUE))

# If "na" are the first characters of the string, the 'N' should be upper case.
mapping$category_list <- sapply(mapping$category_list, function(x) gsub("^na", "Na", x, perl = TRUE))

# mapping is currently in 'wide' format. Convert it to 'narrow' format before merging.
# Use gather function to narrow the columns.
mapping_narrow <- gather(mapping, "main_sector", "n",
                         Automotive...Sports:Social..Finance..Analytics..Advertising)

# Delete observations with '0' for the new 'n' variable.
mapping_narrow <- mapping_narrow[!(mapping_narrow$n == 0),]

# Delete the now-obsolete 'n' variable (column 3).
mapping_narrow <- mapping_narrow[, -3]

# Merge the new narrow mappings into the venture_investments dataframe.
venture_investments <- merge(x = venture_investments, y = mapping_narrow,
                by.x = "primary_sector", by.y = "category_list", all.x = TRUE)

# Data quality check:
unique(venture_investments$main_sector)

# We are instructed that there should be 8 main sectors only, but we have 9 including
# NA. Remove the Blanks observations
venture_investments <- subset(venture_investments, !is.na(venture_investments$main_sector))

###################################################################################

                                  ################
                                  # Checkpoint 5 #
                                  ################

# Create three separate data frames D1, D2 and D3 for each of the three countries
# containing the observations of funding type FT falling within the 5-15 million USD
# range.

# Start with dataframe for United States, filtering the relevant rows
D1 <- filter(venture_investments, country_code == "USA",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)

nrow(D1) # Total number of investments: 12,063
sum(D1$raised_amount_usd) # Total amount of investment: $107,757,097,294

# We want a count of investments for each main sector in a separate column, so group
# observations by main_sector and add the columns.
by_main_sector_D1 <- group_by(D1, main_sector)
main_sector_D1 <- summarize(by_main_sector_D1, sector_investment_count = n(),
                            sector_total_invested = sum(raised_amount_usd))
D1_arrange <- arrange(main_sector_D1, desc(sector_investment_count))
D1_arrange
# Top sectors by investment count:
# Others: 2950
# Social..Finance..Analytics..Advertising: 2714
# Cleantech...Semiconductors: 2350
D1_top_sector <- D1_arrange[1,1]
D1_top_sector
D1_secondtop_sector <- D1_arrange[2,1]
D1_secondtop_sector
D1_thirdtop_sector <- D1_arrange[3,1]
D1_thirdtop_sector

# Merge the sector count and total amount invested columns into dataframe D1,
# which contains all of the columns of the master_frame.
D1_merged <- merge(x = D1, y = main_sector_D1)

# Top sector for USA was "Others". Need the company with the highest investment
# within this sector. Some companies received investment in more than one round, so
# group by permalink_lower.
D1_others <- filter(D1_merged, D1_merged$main_sector == toString(D1_top_sector))
D1_others_by_company <- group_by(D1_others, permalink_lower)
D1_others_by_company <- summarize(D1_others_by_company,
                                  company_total = sum(raised_amount_usd))

# Find the top result
D1_top_result <- head(D1_others_by_company[order(D1_others_by_company$company_total,
                                decreasing = TRUE),],1)
D1_top_result

companies[which(companies$permalink_lower == D1_top_result$permalink_lower),"name"]

# "Virtustream" received the highest total funding: $64,300,000.

# Now repeat for Social..Finance..Analytics..Advertising.
D1_social <-filter(D1_merged,
                      main_sector == toString(D1_secondtop_sector))
D1_social_by_company <- group_by(D1_social, permalink_lower)
D1_social_by_company <- summarize(D1_social_by_company,
                                     company_total = sum(raised_amount_usd))
D1_second_top_result <- head(D1_social_by_company[order(D1_social_by_company$company_total,
                         decreasing = TRUE),],1)
D1_second_top_result

companies[which(companies$permalink_lower == D1_second_top_result$permalink_lower),"name"]

# SST Inc. (Formerly ShotSpotter) received the highest total funding: $67,933,006.

                      ###################################

# Repeat for Great Britain
D2 <- filter(venture_investments, country_code == "GBR",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)

nrow(D2) # # Total number of investments: 621
sum(D2$raised_amount_usd) # Total amount of investment: $5,379,078,691

# We want a count of investments for each main sector in a separate column, so group
# observations by main_sector and add the columns.
by_main_sector_D2 <- group_by(D2, main_sector)
main_sector_D2 <- summarize(by_main_sector_D2, sector_investment_count = n(),
                            sector_total_invested = sum(raised_amount_usd))
D2_arrange <- arrange(main_sector_D2, desc(sector_investment_count))
D2_arrange
# Top sectors by investment count:
# Others: 147
# Social..Finance..Analytics..Advertising: 133
# Cleantech...Semiconductors: 130

D2_top_sector <- D2_arrange[1,1]
D2_top_sector
D2_secondtop_sector <- D2_arrange[2,1]
D2_secondtop_sector
D2_thirdtop_sector <- D2_arrange[3,1]
D2_thirdtop_sector

# Merge the sector count and total amount invested columns into dataframe D2,
# which contains all of the columns of the master_frame.
D2_merged <- merge(x = D2, y = main_sector_D2)

# Top sector for GBR was "Others". Need the company with the highest investment
# within this sector. Some companies received investment in more than one round, so
# group by permalink_lower.
D2_others <-filter(D2_merged, main_sector == toString(D2_top_sector))
D2_others_by_company <- group_by(D2_others, permalink_lower)
D2_others_by_company <- summarize(D2_others_by_company,
                                  company_total = sum(raised_amount_usd))

# Find the top result
D2_top_result <- head(D2_others_by_company[order(D2_others_by_company$company_total,
                                decreasing = TRUE),],1)
D2_top_result

companies[which(companies$permalink_lower == D2_top_result$permalink_lower),"name"]

# "Electric Cloud" received the highest total funding: $37,000,000
# over 4 funding rounds.

# Now repeat for Social..Finance..Analytics..Advertising.
D2_social <-filter(D2_merged,
                      main_sector == toString(D2_secondtop_sector))
D2_social_by_company <- group_by(D2_social, permalink_lower)
D2_social_by_company <- summarize(D2_social_by_company,
                                     company_total = sum(raised_amount_usd))
D2_second_top_result <-head(D2_social_by_company[order(D2_social_by_company$company_total,
                                   decreasing = TRUE),],1)
D2_second_top_result

companies[which(companies$permalink_lower == D2_second_top_result$permalink_lower),"name"]
# Celltick Technologies received the highest total funding: $37,500,000.

                      ###################################

# Repeat for India
D3 <- filter(venture_investments, country_code == "IND",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)

nrow(D3) # # Total number of investments: 328
sum(D3$raised_amount_usd) # Total amount of investment: $2,949,543,602

# We want a count of investments for each main sector in a separate column, so group
# observations by main_sector and add the columns.
by_main_sector_D3 <- group_by(D3, main_sector)
main_sector_D3 <- summarize(by_main_sector_D3, sector_investment_count = n(),
                            sector_total_invested = sum(raised_amount_usd))
D3_arrange <- arrange(main_sector_D3, desc(sector_investment_count))
D3_arrange
# Top sectors by investment count:
# Others: 110
# Social..Finance..Analytics..Advertising: 60
# News..Search.and.Messaging: 52

D3_top_sector <- D3_arrange[1,1]
D3_top_sector
D3_secondtop_sector <- D3_arrange[2,1]
D3_secondtop_sector
D3_thirdtop_sector <- D3_arrange[3,1]
D3_thirdtop_sector


# Merge the sector count and total amount invested columns into dataframe D3,
# which contains all of the columns of the master_frame.
D3_merged <- merge(x = D3, y = main_sector_D3)

# Top sector for IND was "Others". Need the company with the highest investment
# within this sector. Some companies received investment in more than one round, so
# group by permalink_lower.
D3_others <-filter(D3_merged, main_sector == toString(D3_top_sector))
D3_others_by_company <- group_by(D3_others, permalink_lower)
D3_others_by_company <- summarize(D3_others_by_company,
                                  company_total = sum(raised_amount_usd))

# Find the top result
D3_top_result <- head(D3_others_by_company[order(D3_others_by_company$company_total,
                                decreasing = TRUE),],1)
D3_top_result
companies[which(companies$permalink_lower == D3_top_result$permalink_lower),"name"]

# FirstCry.com received the highest total funding: $39,000,000.
# over 7 funding rounds.

# Now repeat for Social..Finance..Analytics..Advertising.
D3_social <-filter(D3_merged,
                      main_sector == toString(D3_secondtop_sector))
D3_social_by_company <- group_by(D3_social, permalink_lower)
D3_social_by_company <- summarize(D3_social_by_company,
                                     company_total = sum(raised_amount_usd))
D3_second_top_result <- head(D3_social_by_company[order(D3_social_by_company$company_total,
                                   decreasing = TRUE),],1)
D3_second_top_result
companies[which(companies$permalink_lower == D3_second_top_result$permalink_lower),"name"]
# Manthan Systems received the highest total funding: $50,700,000.

###################################################################################

# Export dataframes into CSV files for import into Tableau.
write.csv(by_funding_round, file = "./investments_by_funding_round_type.csv", row.names = FALSE, na = "")
write.csv(venture_investments, file = "./venture_investments.csv", row.names = FALSE, na = "")
