# Load the tidyverse library
library("tidyverse")


# Load the text file "Viede" copied from the OCR-scan of the "Perlestikkerbogen" parish book
data <- readLines("input_viede.txt")

data[1:10]

# Clean the dataset by removing unnecessary lines using subsetting and replacing in the dataset

## Removing lines of footnotes starting with a number followed by a closing parenthesis, e.g., "1)"
data <- gsub("^\\d\\).*", "", data)


## Remove all instances of "VIEDE" and the content of the line since it is the footer for all pages in the book
data <- gsub(".*VIEDE.*", "", data)


## Remove lines with page numbers in the text. The page number is "pag" followed by a period and enclosed in parentheses or square brackets, e.g., "[pag. 789]"
## Use "\\p{P}" as a denominator for punctuation
# data <- data[!grepl("\\n\\p{P}?pag\\p{P} ?[\\d\\w]*\\p{P}? ?", data, perl = TRUE)]
data <- gsub("\\p{P}?pag\\p{P}? ?\\w*\\p{P}* ?", "", data, perl = TRUE)


## Remove lines like "Ende på det år..." and "Begyndelsen på det år...", and the random book designation "perlestikkerbogen"
data <- gsub("Ende.*", "", data)
data <- gsub("Begynde.*", "", data)
data <- gsub("[Pp]erlestikkerbog.*", "", data)

## Remove page numbers by removing all lines of exactly three digits
data <- gsub("^\\d{3}$", "", data)



## Collapse the entire "data" vector into a long vector, combining lines starting with "F.", "F,", "O.", "0.", or similar with those that are clearly a continuation of a previous line. Use unique characters and a space "<< " to separate the lines
data_collapsed <- paste(data, collapse = "<< ")
data_collapsed



## Replace all instances of "<< " not followed by a capital letter or 0 and punctuation with a space
## The function (?!...) is used to capture everything that is not contained in it
data_collapsed <- gsub("<< (?!\\b[A-Z0] ?\\p{P})", " ", data_collapsed, perl = TRUE)



## Reconstruct the lines into a vector. Now, they should all start with "F." for example
data_full_line <- str_split_1(data_collapsed, "<< ")
data_full_line





# Now, I will continue to clean my dataset with the goal of ending up with a CSV file

## Turning all OCR-artifacts representing a "double s" or ss in a name starting with a capital letter, in this case consisting of capital "B", "8" or "13", into ss since an actual "ss" is regarded as a strange unicode when e.g. working with word-boundaries
data_full_line <- gsub("(\\p{Lu}\\p{L}+)(?:13|B|8|J3|3)(\\p{L})*", "\\1ss\\2", data_full_line, perl = TRUE)


## There are still some in-line footnotes and random parentheses and square brackets enclosing letters, etc. - I'll remove them.
data_full_line <- gsub("\\d\\)", "", data_full_line)
data_full_line <- gsub("\\]", "", data_full_line)
data_full_line <- gsub("\\[", "", data_full_line)
data_full_line <- gsub("\\(", "", data_full_line)
data_full_line <- gsub("\\)", "", data_full_line)

## Fixing other square bracket problems where they are OCR-scanned as a J following a capital. I will remove these J's
data_full_line <- gsub("\\b(\\p{Lu})\\]?J?", "\\1", data_full_line, perl = TRUE)
### Removing J's following a capital letter, thereby conjugating the starting capital letter of a word with the rest of the name, and removing the space and J-artifact.
data_full_line <- gsub("(\\p{Lu}) J", "\\1", data_full_line, perl = TRUE)


## Separating two names that are not separated by a space, by finding capital letters inside strings of only letters
data_full_line <- gsub("\\b([\\p{Lu}][\\p{Ll}]+)([\\p{Lu}][\\p{Ll}]+)\\b", "\\1 \\2", data_full_line, perl = TRUE)
data_full_line

# Starting to isolate sections of the data set with semicolon to make a semicolon-separated csv-file
## Starting by removing all already existing semicolons
data_full_line <- gsub(";", "", data_full_line, perl = TRUE)


## Make all initial designations, "F." etc., followed by a semicolon instead of a space or space and hyphen
data_full_line <- gsub("^([A-Z0]) ?(\\p{P})(?: - | )", 
                       "\\1\\2;", 
                       data_full_line, 
                       perl = TRUE)


## Isolating all men, the first name mentioned in the list. Finding three the sermon-type that I found before, then finding one, two or three names before a comma, then adding in the rest.
data_full_line <- gsub("^(.{2};) *?(\\S*?(?: ?\\S*?)*?), ?(.*?)$", 
                       "\\1\\2;\\3", 
                       data_full_line, 
                       perl = TRUE)


## Isolating the women's names behind the next semicolon
### First the women who is mentioned by their first name, then a comma and their fathers' first and second name
data_full_line <- gsub("(^.{2};[^;]*?; *?)(\\S*?), ?(\\S*? \\S*?)( .*)", 
                       "\\1\\2;\\3;\\4;", 
                       data_full_line, 
                       perl = TRUE)



### Then the other women with a full name. The starting parenthesis is just there to exclude the lines with woman and father's names, that I just made. The non-capturing group in the middle is there to give the possibility of two second names.
data_full_line <- gsub("(?!^.{2};[^;]*;[^;]*?;[^;]*?;.*)(^.{2};[^;]*;)((?:[\\p{L}\\.,]* ?){0,4}[\\p{L}\\.,]*(?:s|ss|z|ter|ttr)\\.?\\b|[\\p{L}\\.]* [\\p{L}\\.]*)(.*)", 
                       "\\1\\2;;\\3;", 
                       data_full_line, 
                       perl = TRUE)


# Isolating date of wedding
## First isolating artifacts of dates with a space between first and second digit
data_full_line <- gsub("(^[^;]*;[^;]*;[^;]*;[^;]*;.*) \\p{P}?((?:[\\w\\d] [\\w\\d]\\p{P}?) ?(?i:j ?a ?n|f ?e ?b|m ?a ?r|a ?p ?|m ?a ?i?j|j ?u ?n|j ?u ?(?:l|j)|q ?u ?i ?n ?t|s ?e ?x|a ?u ?g|s ?e ?p|o ?c ?t|n ?o ?v|d ?e ?c) ?\\w*)([^;];)$",
                       "\\1\\3\\2;;",
                       data_full_line,
                       perl = TRUE)


## Isolating other dates with no spaces
data_full_line <- gsub("(^[^;]*;[^;]*;[^;]*;[^;]*;.*) \\p{P}?((?:[\\w\\d]{1,2}\\p{P}?) ?(?i:j ?a ?n|f ?e ?b|m ?a ?r|a ?p ?|m ?a ?i?j|j ?u ?n|j ?u ?(?:l|j)|q ?u ?i ?n ?t|s ?e ?x|a ?u ?g|s ?e ?p|o ?c ?t|n ?o ?v|d ?e ?c) ?\\w*)([^;]*;)$",
                       "\\1\\3\\2;;",
                       data_full_line,
                       perl = TRUE)


## Isolating "eodem die" "tempore eodem" and other types of expressions meaning "same day"
data_full_line <- gsub("(^[^;]*;[^;]*;[^;]*;[^;]*;.*)\\p{P}?([Ee]o\\w+ \\w+[^;\\P{P}]|[Tt]e\\w+ \\w+[^;\\P{P}])(.*;)",
                       "\\1\\3\\2;;",
                       data_full_line,
                       perl = TRUE)


# Isolating description of which day after which sunday
## Starting by isolating strings starting with specific weekdays: wednesdays and sundays.
## Putting it last separated by semicolon
data_full_line <- gsub(
  "(^[^;]*;[^;]*;[^;]*;[^;]*;.*)\\W((?:(?:\\p{L}*[OØø]ns?d?\\w*\\p{P}? ?)(?:p\\p{P}?\\w* ?)?)(?:[Dd]\\p{P}|[Dd]\\w+\\p{P}?)? ?(?:[^;]+(?: [^;]*)?(?: [^;]*)?(?: [^;]*)?(?: [^;]*)?)?)( ?.*?;)(.+;;$)", 
  "\\1\\3\\4\\2;", 
  data_full_line, 
  perl = TRUE)


## Isolating simple day designations of domini, dom., d. or feria or fer. to the end of the line
data_full_line <- gsub(
  "(^[^;]*;[^;]*;[^;]*;[^;]*;[^;]*?\\W)((?:[Dd]\\p{P}|[Dd]o\\w*\\p{P}?|F\\w+) ?(?:\\S+(?: \\S*)?(?: \\S*)?(?: \\S*)?(?: \\S*)?)?)([^;]*;)([^;]*;;)$", 
  "\\1\\3\\4\\2;", 
  data_full_line, 
  perl = TRUE)
writeLines(data_full_line, "test1.txt", sep = "\n")


## Isolating specific liturgical days designated by "dag" f.x. 3rd day of easter or 3rd day of pentecost etc.
data_full_line <- gsub(
  "(^[^;]*;[^;]*;[^;]*;[^;]*;[^;]*?)([\\w.]*(?: ?[\\w.]*)(?: ?[\\w.]*)(?>[\\w.]*dag[\\w.]*))( ?[^;]*;[^;]*;;$)", 
  "\\1\\3\\2;", 
  data_full_line, 
  perl = TRUE)



##Outputting to a CSV-file
writeLines(data_full_line, "output_viede.csv", sep = "\n")



##Reading the list of marriages into a dataframe 
df <- read.csv2("output_viede.csv")

## For easy handling I add a column of serial numbers
df <- df %>% mutate(id = row_number())

##Making a new row in the start that will serve as names
column_names <- c("WeekDay", "MaleName", "FemaleName", "FatherName", "Misc", "Date", "X1", "LitDay", "X2", "ID")


##Giving the columns of the dataframe the appropriate names
colnames(df) <- column_names

# Cleaning names of daler, mark and skilling, the coins so it can be more easily read

## Fixing 1 or 10 showing up as I or IO
df <- df %>% mutate(Misc = str_replace_all(Misc, " I ", " 1 "))
df <- df %>% mutate(Misc = str_replace_all(Misc, " IO ", " 10 "))

## Removing artifacts of spaces between fractions: f.x. 1/ 2
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d) ?(\\/) ?(\\d)", "\\1\\2\\3"))
##Turning 1/2 and 1/4 into 0,5 and 0,25
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d)1\\/2", "\\1.5"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d)1\\/4", "\\1.25"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "1\\/2", "0.5"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "1\\/4", "0.25"))

## Turning unicodes ½ and ¼ into ,5 and ,25
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d) ?½", "\\1.5"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d) ?¼", "\\1.25"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "½", "0.5"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "¼", "0.25"))

## Removing fill words such as "himself, a whole, half" and turning "one" into "1"
df <- df %>% mutate(Misc = str_remove_all(Misc, "sell?f\\w*"))
df <- df %>% mutate(Misc = str_remove_all(Misc, "heel\\w*"))
df <- df %>% mutate(Misc = str_remove_all(Misc, "halff\\w*"))
df <- df %>% mutate(Misc = str_replace_all(Misc, " en ?(?!\\d)", " 1")) ## Using a negative lookahead so it doesnt turn "en" into "1" when used as f.x. "en 1/4", "a fourth"


## Daler, which can be apprehended by "r." or "rix." to indicate rigsdaler
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d{1,2}|\\d? ?1 ?\\/ ?\\d|I) ?\\S*[da][li]l?[er]?\\S*(?: \\S*x)?", "\\1 daler"))
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d{1,2}|\\d? ?1 ?\\/ ?\\d|I) ?dl.", "\\1 daler"))


## Mark, which is completely made out of artifacts due to the use of the sign mark
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d{1,2}|\\d? ?1 ?\\/ ?\\d|I) ?[FLIJflij1,.\\-\\}\\/] ?[FLIJflij1,.\\-\\}\\/]{2,}", "\\1 mark"))

## Skilling
df <- df %>% mutate(Misc = str_replace_all(Misc, "(\\d{1,2}|\\d? ?1 ?\\/ ?\\d|I) ?(?:B|I3)\\S*", "\\1 skilling"))



#Now making a new data-frame that I will calculate and split columns upon
data_calc <- df

# Splitting who paid
## "the other" signifying what the rest of the parish paid in offering
data_calc <- data_calc %>% 
    separate_wider_regex(
      cols = Misc, 
      patterns = c(ofrPair = ".*", 
                  " ",
                  ofrOther = "\\S* and\\S*.*"), 
      too_few = "align_start")

## splitting what the groom paid, the bride paid, still leaving the instances where they paid together
data_calc <- data_calc %>% 
    separate_wider_regex(
      cols = ofrPair,
      patterns = c(ofrGroom = ".*\\W[Hh]an.*",
                   ofrBride = "[Hh][ui].*"),
      too_few = "align_start",
      cols_remove = FALSE) %>% 
    mutate(
      ofrPair = ifelse(is.na(ofrGroom) & is.na(ofrBride), ofrPair, NA_character_)
    )


## Making when a couple pays an amount each into its own column, and emptying the cells they come from
data_calc <- data_calc %>%
  separate_wider_regex(
    cols = ofrPair,
    patterns = c(ofrEach = ".*huer.*"),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(across(ofrPair, ~ifelse(grepl(".*huer.*", .), NA, .)))

## Copying from ofrEach column into both ofrGroom and ofrBride
data_calc <- data_calc %>%
  mutate(
    ofrGroom = ifelse(!is.na(ofrEach), ifelse(!is.na(ofrGroom), paste(ofrGroom, ofrEach, sep = " "), ofrEach), ofrGroom),
    ofrBride = ifelse(!is.na(ofrEach), ifelse(!is.na(ofrBride), paste(ofrBride, ofrEach, sep = " "), ofrEach), ofrBride)
  ) %>%
  select(-ofrEach)



## Making a regex for finding the exact currencies in each column and isolating them
currencies_pattern <- "(?:(?:\\w+\\.)?\\d{1,2})? ?(?:\\d\\/\\d)? ?(?:daler|mark|skilling|crone)"


## Turning all offering columns into just the coins they donated
data_calc <- data_calc %>% 
  mutate(ofrGroom = paste0(str_extract_all(ofrGroom, currencies_pattern))) %>% 
  mutate(ofrGroom = str_remove_all(ofrGroom, "character\\(0\\)")) %>% 
  mutate(ofrGroom = str_remove_all(ofrGroom, "c\\(")) %>% 
  mutate(ofrGroom = str_remove_all(ofrGroom, "\"")) %>% 
  mutate(ofrGroom = str_remove_all(ofrGroom, "\\)")) %>% 
  unnest(ofrGroom, keep_empty = TRUE)

data_calc <- data_calc %>% 
  mutate(ofrBride = paste0(str_extract_all(ofrBride, currencies_pattern))) %>% 
  mutate(ofrBride = str_remove_all(ofrBride, "character\\(0\\)")) %>% 
  mutate(ofrBride = str_remove_all(ofrBride, "c\\(")) %>% 
  mutate(ofrBride = str_remove_all(ofrBride, "\"")) %>% 
  mutate(ofrBride = str_remove_all(ofrBride, "\\)")) %>% 
  unnest(ofrBride, keep_empty = TRUE)

data_calc <- data_calc %>% 
  mutate(ofrPair = paste0(str_extract_all(ofrPair, currencies_pattern))) %>% 
  mutate(ofrPair = str_remove_all(ofrPair, "character\\(0\\)")) %>% 
  mutate(ofrPair = str_remove_all(ofrPair, "c\\(")) %>% 
  mutate(ofrPair = str_remove_all(ofrPair, "\"")) %>% 
  mutate(ofrPair = str_remove_all(ofrPair, "\\)")) %>% 
  unnest(ofrPair, keep_empty = TRUE)

data_calc <- data_calc %>% 
  mutate(ofrOther = paste0(str_extract_all(ofrOther, currencies_pattern))) %>% 
  mutate(ofrOther = str_remove_all(ofrOther, "character\\(0\\)")) %>% 
  mutate(ofrOther = str_remove_all(ofrOther, "c\\(")) %>% 
  mutate(ofrOther = str_remove_all(ofrOther, "\"")) %>% 
  mutate(ofrOther = str_remove_all(ofrOther, "\\)")) %>% 
  unnest(ofrOther, keep_empty = TRUE)


## What I will do now is a little finnicky. Since I have several currencies in textual form following an amount, and sometimes several currencies in one cell.
## I will proceed to widen the dataframe, then multiply by the exchange rates into skilling and then concatenate it all together for each column: bride, groom, pair, other
## Literrally just brute-forced it here
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrGroom, patterns = c(ofrGroomDaler = "(?:\\w+\\.)?\\d+ (?=daler)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrGroom, patterns = c(ofrGroomMark = "(?:\\w+\\.)?\\d+ (?=mark)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrGroom, patterns = c(ofrGroomSkilling = "(?:\\w+\\.)?\\d+ (?=skilling)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrGroom, patterns = c(ofrGroomCrone = "(?:\\w+\\.)?\\d+ (?=crone)"), too_few = "align_start", cols_remove = TRUE)

data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrBride, patterns = c(ofrBrideDaler = "(?:\\w+\\.)?\\d+ (?=daler)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrBride, patterns = c(ofrBrideMark = "(?:\\w+\\.)?\\d+ (?=mark)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrBride, patterns = c(ofrBrideSkilling = "(?:\\w+\\.)?\\d+ (?=skilling)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrBride, patterns = c(ofrBrideCrone = "(?:\\w+\\.)?\\d+ (?=crone)"), too_few = "align_start", cols_remove = TRUE)

data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrPair, patterns = c(ofrPairDaler = "(?:\\w+\\.)?\\d+ (?=daler)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrPair, patterns = c(ofrPairMark = "(?:\\w+\\.)?\\d+ (?=mark)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrPair, patterns = c(ofrPairSkilling = "(?:\\w+\\.)?\\d+ (?=skilling)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrPair, patterns = c(ofrPairCrone = "(?:\\w+\\.)?\\d+ (?=crone)"), too_few = "align_start", cols_remove = TRUE)

data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrOther, patterns = c(ofrOtherDaler = "(?:\\w+\\.)?\\d+ (?=daler)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrOther, patterns = c(ofrOtherMark = "(?:\\w+\\.)?\\d+ (?=mark)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrOther, patterns = c(ofrOtherSkilling = "(?:\\w+\\.)?\\d+ (?=skilling)"), too_few = "align_start", cols_remove = FALSE)
data_calc <- data_calc %>% 
  separate_wider_regex(cols = ofrOther, patterns = c(ofrOtherCrone = "(?:\\w+\\.)?\\d+ (?=crone)"), too_few = "align_start", cols_remove = TRUE)

### Making the new columns numeric in the data understanding
data_calc <- data_calc %>% 
  mutate(across(starts_with("ofr"), as.numeric))


## Dictionary of conversion rates
Daler = 96
Mark = 6
Skilling = 1
Crone = 144 #Exchange rates to skilling


## 
data_calc <- data_calc %>% 
  mutate(ofrGroomDaler = ofrGroomDaler * Daler) %>% 
  mutate(ofrBrideDaler = ofrBrideDaler * Daler) %>% 
  mutate(ofrPairDaler = ofrPairDaler * Daler) %>% 
  mutate(ofrOtherDaler = ofrOtherDaler * Daler) %>%
  mutate(ofrGroomMark = ofrGroomMark * Mark) %>% 
  mutate(ofrBrideMark = ofrBrideMark * Mark) %>% 
  mutate(ofrPairMark = ofrPairMark * Mark) %>% 
  mutate(ofrOtherMark = ofrOtherMark * Mark) %>%
  mutate(ofrGroomSkilling = ofrGroomSkilling * Skilling) %>% 
  mutate(ofrBrideSkilling = ofrBrideSkilling * Skilling) %>% 
  mutate(ofrPairSkilling = ofrPairSkilling * Skilling) %>% 
  mutate(ofrOtherSkilling = ofrOtherSkilling * Skilling) %>%
  mutate(ofrGroomCrone = ofrGroomCrone * Crone) %>% 
  mutate(ofrBrideCrone = ofrBrideCrone * Crone) %>% 
  mutate(ofrPairCrone = ofrPairCrone * Crone) %>% 
  mutate(ofrOtherCrone = ofrOtherCrone * Crone)

## Making new columns of total value in skilling
data_calc <- data_calc %>%
  mutate(
    ofrGroom_total = rowSums(select(., starts_with("ofrGroom")), na.rm = TRUE),
    ofrBride_total = rowSums(select(., starts_with("ofrBride")), na.rm = TRUE),
    ofrPair_total = rowSums(select(., starts_with("ofrPair")), na.rm = TRUE),
    ofrOther_total = rowSums(select(., starts_with("ofrOther")), na.rm = TRUE)
  )

## Turning values of 0 into NA in all offer-columns
# <- data_calc %>%
 # mutate(across(starts_with("ofr"), ~ifelse(.x == 0, NA, .x)))

# Replace missing values with 0
data_calc$ofrGroom_total[is.na(data_calc$ofrGroom_total)] <- 0
data_calc$ofrBride_total[is.na(data_calc$ofrBride_total)] <- 0
data_calc$ofrPair_total[is.na(data_calc$ofrPair_total)] <- 0
data_calc$ofrOther_total[is.na(data_calc$ofrOther_total)] <- 0


# Create bar chart
TotalPlot <- ggplot(data_calc, aes(x = ID)) +
  geom_col(aes(y = ofrGroom_total, fill = "ofrGroom_total"), width = 0.8) +
  geom_col(aes(y = ofrBride_total, fill = "ofrBride_total"), width = 0.8) +
  geom_col(aes(y = ofrPair_total, fill = "ofrPair_total"), width = 0.8) +
  geom_col(aes(y = ofrOther_total, fill = "ofrOther_total"), width = 0.8) +
  labs(title = "Total money paid in wedding offering per pair. Nakskov 1618-1627.", x = "Pair nr.", y = "Total money, in skilling") + 
  scale_fill_brewer(name="Payers",
                    labels=c("Bride", "Groom", "Other", "Pair, together"), 
                    palette = "Set1")
TotalPlot

ggplot(data_calc, aes(x = ID)) +
  geom_col(aes(y = ofrBride_total, fill = "ofrBride_total"), width = 1) +
  labs(title = "Total money paid in wedding offering per pair. Nakskov 1618-1627.", x = "Pair nr.", y = "Total money, in skilling")  






LogPlot <- ggplot(data_calc, aes(x = ID, y = total)) +
  geom_bar(stat = "identity", aes(y = ofrGroom_total, fill = "ofrGroom_total"), width = 0.8) +
  geom_bar(stat = "identity", aes(y = ofrBride_total, fill = "ofrBride_total"), width = 0.8) +
  geom_bar(stat = "identity", aes(y = ofrPair_total, fill = "ofrPair_total"), width = 0.8) +
  geom_bar(stat = "identity", aes(y = ofrOther_total, fill = "ofrOther_total"), width = 0.8) +
  scale_y_log10(limits = c(1, 2000)) +
  labs(title = "Total money paid in wedding offering per pair. Nakskov 1618-1627.", x = "Pair nr.", y = "Total money, in skilling") + 
  scale_fill_brewer(name="Payers",
                    labels=c("Bride", "Groom", "Other", "Pair, together"), 
                    palette = "Set1")
LogPlot
