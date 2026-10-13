# Descriptive tables and figures for the graduate student survey.
# Reads data/deidentified/grad_ai_use.csv; writes to output/tables and output/figures.

suppressMessages({library(readr); library(dplyr); library(tidyr); library(stringr); library(ggplot2)})

raw  <- read_csv("data/deidentified/grad_ai_use.csv", col_types = cols(.default = col_character()))
qtxt <- raw[1, ]                    # row 2 of the Qualtrics export: question text
d    <- raw[-c(1, 2), ]             # drop question-text and ImportId rows
stopifnot(nrow(d) == 19)

# --- Item labels come from the question text, not the column suffix -------------
item_labels <- function(prefix) {
  cols <- grep(paste0("^", prefix, "_\\d+$"), names(d), value = TRUE)
  tibble(var = cols, item = str_replace(unlist(qtxt[cols]), "^.* - ", ""))
}

long <- function(prefix, levels) {
  d |>
    select(all_of(item_labels(prefix)$var)) |>
    pivot_longer(everything(), names_to = "var", values_to = "resp") |>
    filter(!is.na(resp), resp != "") |>
    left_join(item_labels(prefix), by = "var") |>
    mutate(resp = factor(resp, levels = levels))
}

summarise_items <- function(x, levels) {
  x |>
    count(item, resp, .drop = FALSE) |>
    group_by(item) |>
    mutate(n_answered = sum(n), pct = 100 * n / n_answered) |>
    ungroup()
}

# --- Completion / response overview ---------------------------------------------
n_total <- nrow(d)
resp_overview <- tibble(
  measure = c("Invited", "Responses recorded", "Response rate (%)",
              "Answered >=1 coursework item (Q2)", "Answered >=1 AP/dissertation item (Q5)",
              "Wrote any open-ended text (Q3/Q4/Q6)", "Median duration (seconds)"),
  value = c(31, n_total, round(100 * n_total / 31, 1),
            sum(rowSums(!is.na(d[item_labels("Q2")$var]) & d[item_labels("Q2")$var] != "") > 0),
            sum(rowSums(!is.na(d[item_labels("Q5")$var]) & d[item_labels("Q5")$var] != "") > 0),
            sum(rowSums(!is.na(d[c("Q3", "Q4", "Q6")]) & d[c("Q3", "Q4", "Q6")] != "") > 0),
            median(as.numeric(d[["Duration (in seconds)"]]))))
write_csv(resp_overview, "output/tables/grad_response_overview.csv")

# --- Coursework (Q2) -------------------------------------------------------------
lv2 <- c("Never", "Sometimes", "Regularly", "N/A")
t2  <- summarise_items(long("Q2", lv2), lv2)
w2  <- t2 |>
  select(item, resp, n, n_answered, pct) |>
  pivot_wider(names_from = resp, values_from = c(n, pct)) |>
  mutate(any_use_n = n_Sometimes + n_Regularly,
         any_use_pct_of_users_eligible = 100 * any_use_n / (n_answered - `n_N/A`)) |>
  arrange(desc(any_use_n))
write_csv(w2, "output/tables/grad_coursework_use_by_task.csv")

# --- AP / dissertation (Q5) ------------------------------------------------------
lv5 <- c("No", "Yes", "N/A")
t5  <- summarise_items(long("Q5", lv5), lv5)
w5  <- t5 |>
  select(item, resp, n, n_answered, pct) |>
  pivot_wider(names_from = resp, values_from = c(n, pct)) |>
  mutate(yes_pct_of_applicable = 100 * n_Yes / (n_answered - `n_N/A`)) |>
  arrange(desc(n_Yes))
write_csv(w5, "output/tables/grad_ap_dissertation_use_by_task.csv")

# --- Per-respondent breadth of use (number of tasks with any use) ---------------
breadth <- d |>
  mutate(id = row_number()) |>
  select(id, all_of(item_labels("Q2")$var)) |>
  filter(if_any(-id, ~ !is.na(.x) & .x != "")) |>
  mutate(tasks_any_use = rowSums(across(-id, ~ .x %in% c("Sometimes", "Regularly"))),
         tasks_regular = rowSums(across(-id, ~ .x == "Regularly"))) |>
  select(id, tasks_any_use, tasks_regular)
write_csv(count(breadth, tasks_any_use, name = "respondents"),
          "output/tables/grad_coursework_breadth_distribution.csv")

# --- Figures ----------------------------------------------------------------------
pal2 <- c(Never = "#E8E8E8", Sometimes = "#9ECAE1", Regularly = "#08519C", `N/A` = "#B8B8B8")
pal5 <- c(No = "#E8E8E8", Yes = "#08519C", `N/A` = "#B8B8B8")

fig <- function(t, pal, ord, title, subtitle, file, h, w = 9, base = 11, caption = TRUE, lab = 3) {
  t |>
    mutate(item = factor(item, levels = rev(ord)),
           resp = factor(resp, levels = rev(levels(resp))),
           label = ifelse(n > 0, n, "")) |>
    ggplot(aes(pct, item, fill = resp)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.3) +
    geom_text(aes(label = label, group = resp, color = resp %in% c("Regularly", "Yes")),
              position = position_stack(vjust = 0.5), size = lab) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
    scale_fill_manual(values = pal, breaks = names(pal), name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
         caption = if (caption) "Numbers in bars are respondent counts. Purdue Sociology graduate student survey, fall 2026." else NULL) +
    theme_minimal(base_size = base) +
    theme(panel.grid = element_blank(), legend.position = "top",
          legend.justification = "left", plot.title.position = "plot",
          plot.title = element_text(face = "bold"))
  ggsave(file, width = w, height = h, dpi = 300, bg = "white")
}

rng <- function(x) if (min(x) == max(x)) as.character(min(x)) else paste0(min(x), "-", max(x))
n2 <- rng(t2$n_answered); n5 <- rng(t5$n_answered)
fig(t2, pal2, w2$item, "AI use in coursework, past year",
    sprintf("Share of respondents by task (n = %s per item), ordered by number reporting any use", n2),
    "output/figures/grad_coursework_use_by_task.png", 6)
fig(t5, pal5, w5$item, "AI use planned or used for AP / dissertation",
    sprintf("Share of respondents by task (n = %s per item), ordered by number answering yes", n5),
    "output/figures/grad_ap_dissertation_use_by_task.png", 5.5)

# Compact versions for the two-page report (code/03_build_report_grad.R)
fig(t2, pal2, w2$item, "Figure 1. AI use in coursework, past year",
    sprintf("n = %s respondents per item; tasks ordered by number reporting any use", n2),
    "output/figures/grad_coursework_use_by_task_report.png", 3.3, w = 7.6, base = 8.5, caption = FALSE, lab = 2.5)
fig(t5, pal5, w5$item, "Figure 2. AI use planned or used for AP / dissertation",
    sprintf("n = %s respondents per item; tasks ordered by number answering yes", n5),
    "output/figures/grad_ap_dissertation_use_by_task_report.png", 3.0, w = 7.6, base = 8.5, caption = FALSE, lab = 2.5)

print(w2 |> select(item, n_answered, n_Never, n_Sometimes, n_Regularly, `n_N/A`), n = Inf, width = Inf)
print(w5 |> select(item, n_answered, n_No, n_Yes, `n_N/A`), n = Inf, width = Inf)
print(resp_overview); print(count(breadth, tasks_any_use))
print(summary(breadth$tasks_any_use))
