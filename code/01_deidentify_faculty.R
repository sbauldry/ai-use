# Create the shareable (committed) version of the faculty survey data.
# Drops Qualtrics "Survey Preview" test rows and respondent-level metadata that could
# identify people (IP address, geolocation, timestamps, response ID, empty recipient fields).
# Keeps Qualtrics' header rows (variable names, question text, ImportId).
# Manual redactions of open-ended text come from data/raw/redactions_faculty.csv (local
# only, since it quotes the original text); `response` is the row among the real responses.

library(readr)
library(dplyr)

raw <- read_csv("data/raw/faculty_ai_use_qualtrics_export.csv",
                col_types = cols(.default = col_character()))

keep <- c(TRUE, TRUE, raw$Status[-c(1, 2)] == "IP Address")   # drop "Survey Preview" rows
message("Dropped ", sum(!keep), " preview/test row(s)")
raw <- raw[keep, ]

drop <- c("Status", "StartDate", "EndDate", "IPAddress", "RecordedDate", "ResponseId",
          "RecipientLastName", "RecipientFirstName", "RecipientEmail",
          "ExternalReference", "LocationLatitude", "LocationLongitude",
          "Last Seen Flow Element ID", "Last Seen Question IDs")
out <- select(raw, -any_of(drop))

if (file.exists("data/raw/redactions_faculty.csv")) {
  rules <- read_csv("data/raw/redactions_faculty.csv", col_types = cols(.default = col_character()),
                    na = character(), trim_ws = FALSE)   # an empty `replace` means "delete", not NA
  for (i in seq_len(nrow(rules))) {
    r <- as.integer(rules$response[i]) + 2
    old <- out[[rules$column[i]]][r]
    stopifnot(!is.na(old))
    if (rules$find[i] == "<<ALL>>") {            # blank the whole field
      out[[rules$column[i]]][r] <- NA_character_
    } else {
      stopifnot(grepl(rules$find[i], old, fixed = TRUE))
      out[[rules$column[i]]][r] <- sub(rules$find[i], rules$replace[i], old, fixed = TRUE)
    }
  }
  message("Applied ", nrow(rules), " redactions")
} else warning("data/raw/redactions_faculty.csv not found: open-ended text NOT redacted")

write_csv(out, "data/deidentified/faculty_ai_use.csv", na = "")
message("Wrote data/deidentified/faculty_ai_use.csv: ", nrow(out) - 2, " responses, ", ncol(out), " columns")
