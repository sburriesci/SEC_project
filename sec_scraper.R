library(tidyverse)
library(rvest)
library(lubridate)
library(tidytext)

# get old case list

old_cases <- read_csv("SEC_caselist.csv")

# sort by date descending

old_cases <- old_cases[rev(order(old_cases$Date)),]

# set last run date here; scraper will only retrieve cases added since that date

#lastrun <- parse_date_time("January 31, 2021", order = "mdy")
lastrun <- old_cases$Date[1]

# Download the SEC Administrative Actions page html

html <- read_html("https://www.sec.gov/litigation/admin.htm")

# scrape case info from mainlist table into dataframe

tab <- html %>% html_nodes('#mainlist')
case_table <- tab[[1]] %>% html_table()

# rename columns to avoid spaces in column names

colnames(case_table) <- c("RelNum", "Date", "Respondents")

# filter out non-case rows, change Dates to date type, and add file urls to table

case_table <- case_table %>% filter(RelNum != "" & !grepl("Quarter", RelNum))
case_table <- case_table %>% mutate(Link = as.vector(html_nodes(tab, "td:first-child a") %>% html_attr("href")))
case_table$Link <- paste0("https://www.sec.gov", case_table$Link)
case_table$Date <- mdy(case_table$Date)

# filter out cases before a certain date, e.g., before the date when the script was last run

case_table <- case_table %>% filter(Date > lastrun)
case_table <- case_table[order(case_table$RelNum),]

# create unique identifier for each case

id <- rownames(case_table)
case_table <- cbind(id=id, case_table)

# download the pdf files and store in PDFs directory

directory <- file.path(getwd(), "PDFs")

for (link in case_table$Link) {
  download.file(link, destfile = file.path(directory, basename(link)), mode = "wb")
}

# read pdfs

pdfs <- file.path(directory, list.files(directory, pattern = "*.pdf"))
pdf_names <- list.files(directory, pattern = "*.pdf")
pdfs_text <- map(pdfs, pdftools::pdf_text)

# create pdf_data table from pdf contents

pdf_data <- tibble(document = pdf_names, text = pdfs_text)

# search for "cease and desist", set "cease" variable to count of each pdf's "cease-and-desist" strings in pdf_data table

pdf_data$text <- tolower(pdf_data$text)
pdf_data$text <- gsub('[[:punct:] ]+', ' ', pdf_data$text)
pdf_data <- pdf_data %>% mutate(cease = str_count(pdf_data$text, "cease and desist"))
#pdf_data <- pdf_data %>% mutate(cease = if_else(str_detect(pdf_data$text, "cease-and-desist"), 1, 0))
pdf_data <- pdf_data[order(pdf_data$document),]

# create unique identifier for each document

id2 <- rownames(pdf_data)
pdf_data <- cbind(id=id2, pdf_data)

# drop text and RelNum columns from pdf_data

drops <- c("text")
pdf_data <- pdf_data[, !(names(pdf_data) %in% drops)]

# join case_table and pdf_data into caseinfo dataframe, delete id column

caseinfo <- merge(case_table, pdf_data)
caseinfo <- caseinfo[rev(order(caseinfo$Date)),]
caseinfo <- caseinfo[-c(1)]

# add new cases to old_cases

caseinfo <- rbind(caseinfo, old_cases)

# write caseinfo to csv in parent of PDFs folder

write_csv(caseinfo, "SEC_caselist.csv", append = FALSE, col_names = TRUE)

# move PDFs to an archive folder so download directory is empty for next run

move_pdfs <- function(pdf){
  file.rename(from = file.path(directory, pdf),
              to = file.path(directory, "../PDFsOld", pdf))
}

# apply the function to all files
lapply(pdf_names, move_pdfs)