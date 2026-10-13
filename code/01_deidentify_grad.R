# Create the shareable (committed) version of the graduate student survey data.
# Drops respondent-level metadata that could identify people (IP address,
# geolocation, timestamps, Qualtrics response ID), the empty Qualtrics
# "recipient" fields, and the unused duplicate question block (Q7-Q12).
# Keeps Qualtrics' three header rows: variable names, question text, ImportId.

library(readr)
library(dplyr)

raw <- read_csv("data/raw/grad_ai_use_qualtrics_export.csv",
                col_types = cols(.default = col_character()))

drop <- c("StartDate", "EndDate", "IPAddress", "RecordedDate", "ResponseId",
          "RecipientLastName", "RecipientFirstName", "RecipientEmail",
          "ExternalReference", "LocationLatitude", "LocationLongitude",
          "Last Seen Flow Element ID", "Last Seen Question IDs")

out <- raw |>
  select(-any_of(drop), -matches("^Q(7|8|9|10|11|12)(_|$)"))

# Manual redactions of identifying details in open-ended text. The rules live in
# data/raw/redactions_grad.csv (local only, since they quote the original text).
# `response` is the respondent's row among the 19 responses (1-19), in export order.
if (file.exists("data/raw/redactions_grad.csv")) {
  rules <- read_csv("data/raw/redactions_grad.csv", col_types = cols(.default = col_character()))
  for (i in seq_len(nrow(rules))) {
    r <- as.integer(rules$response[i]) + 2   # skip the two header rows
    old <- out[[rules$column[i]]][r]
    stopifnot(!is.na(old), grepl(rules$find[i], old, fixed = TRUE))
    out[[rules$column[i]]][r] <- sub(rules$find[i], rules$replace[i], old, fixed = TRUE)
  }
  message("Applied ", nrow(rules), " redactions")
} else warning("data/raw/redactions_grad.csv not found: open-ended text NOT redacted")

write_csv(out, "data/deidentified/grad_ai_use.csv", na = "")
message("Wrote data/deidentified/grad_ai_use.csv: ", nrow(out) - 2, " responses, ",
        ncol(out), " columns")
