# Descriptive tables and figures for the faculty survey.
# Reads data/deidentified/faculty_ai_use.csv; writes to output/tables and output/figures.
# Skip logic: respondents who said "No" to AI use in a domain (research Q1, teaching Q4,
# service Q7) never saw that domain's task items, so task tables are among users only.

suppressMessages({library(readr); library(dplyr); library(tidyr); library(stringr); library(ggplot2)})

raw  <- read_csv("data/deidentified/faculty_ai_use.csv", col_types = cols(.default = col_character()))
qtxt <- raw[1, ]
d    <- raw[-c(1, 2), ]
N_INVITED <- 31
stopifnot(nrow(d) == 26)

item_labels <- function(prefix) {
  cols <- grep(paste0("^", prefix, "_\\d+$"), names(d), value = TRUE)
  tibble(var = cols, item = str_replace(unlist(qtxt[cols]), "^.* - ", ""))
}

# --- Gateway questions: LLM / AI agent use by domain -----------------------------
type_of <- function(x) case_when(
  is.na(x) | x == "" ~ NA_character_,
  x == "No" ~ "No",
  str_detect(x, "LLM") & str_detect(x, "agent") ~ "LLM and AI agent",
  str_detect(x, "agent") ~ "AI agent only",
  str_detect(x, "LLM") ~ "LLM only")
lv_type <- c("No", "LLM only", "AI agent only", "LLM and AI agent")

domains <- c(Research = "Q1", Teaching = "Q4", Service = "Q7")
gate <- bind_rows(lapply(names(domains), function(nm)
  tibble(domain = nm, type = factor(type_of(d[[domains[[nm]]]]), levels = lv_type)))) |>
  filter(!is.na(type)) |>
  count(domain, type, .drop = FALSE) |>
  group_by(domain) |> mutate(n_answered = sum(n), pct = 100 * n / n_answered) |> ungroup()
write_csv(gate, "output/tables/faculty_domain_use.csv")

any_yes <- sapply(domains, function(q) !is.na(d[[q]]) & d[[q]] != "" & d[[q]] != "No")
n_domains <- rowSums(any_yes)
write_csv(count(tibble(domains_with_ai_use = n_domains), domains_with_ai_use, name = "respondents"),
          "output/tables/faculty_n_domains_distribution.csv")

# --- Task items among users -------------------------------------------------------
lv <- c("Never", "Sometimes", "Regularly", "N/A")
task_tab <- function(prefix, domain_q, name) {
  users <- which(!is.na(d[[domain_q]]) & d[[domain_q]] != "" & d[[domain_q]] != "No")
  il <- item_labels(prefix)
  long <- d[users, ] |>
    select(all_of(il$var)) |>
    pivot_longer(everything(), names_to = "var", values_to = "resp") |>
    filter(!is.na(resp), resp != "") |>
    left_join(il, by = "var") |>
    mutate(resp = factor(resp, levels = lv))
  t <- long |> count(item, resp, .drop = FALSE) |>
    group_by(item) |> mutate(n_answered = sum(n), pct = 100 * n / n_answered) |> ungroup()
  w <- t |> select(item, resp, n, n_answered, pct) |>
    pivot_wider(names_from = resp, values_from = c(n, pct)) |>
    mutate(any_use_n = n_Sometimes + n_Regularly) |>
    arrange(desc(any_use_n), desc(n_Regularly))
  write_csv(w, sprintf("output/tables/faculty_%s_use_by_task.csv", name))
  list(long_t = t, wide = w, n_users = length(users))
}
res <- task_tab("Q2", "Q1", "research")
tea <- task_tab("Q5", "Q4", "teaching")
ser <- task_tab("Q8", "Q7", "service")

# --- Overview ------------------------------------------------------------------------
open_cols <- c("Q3", "Q6", "Q9", "Q10")
ov <- tibble(
  measure = c("Invited", "Responses recorded (test rows excluded)", "Response rate (%)",
              "Used AI in research (n)", "Used AI in teaching (n)", "Used AI in service (n)",
              "Used AI in none of the three domains (n)", "Used AI in all three domains (n)",
              "Wrote any open-ended text (n)", "Median duration (seconds)"),
  value = c(N_INVITED, nrow(d), round(100 * nrow(d) / N_INVITED, 1),
            res$n_users, tea$n_users, ser$n_users, sum(n_domains == 0), sum(n_domains == 3),
            sum(rowSums(!is.na(d[open_cols]) & d[open_cols] != "") > 0),
            median(as.numeric(d[["Duration (in seconds)"]]))))
write_csv(ov, "output/tables/faculty_response_overview.csv")

# --- Figures -------------------------------------------------------------------------
pal_task <- c(Never = "#E8E8E8", Sometimes = "#9ECAE1", Regularly = "#08519C", `N/A` = "#B8B8B8")
pal_gate <- c(No = "#E8E8E8", `LLM only` = "#9ECAE1", `AI agent only` = "#E69F00", `LLM and AI agent` = "#08519C")

fig <- function(t, ycol, fillcol, pal, ord, title, subtitle, file, h, w = 9, base = 11,
                caption = TRUE, lab = 3, dark = c("Regularly", "LLM and AI agent")) {
  p <- t |>
    mutate(yy = factor(.data[[ycol]], levels = rev(ord)),
           ff = factor(.data[[fillcol]], levels = rev(names(pal))),
           label = ifelse(n > 0, n, "")) |>
    ggplot(aes(pct, yy, fill = ff)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.3) +
    geom_text(aes(label = label, group = ff, color = ff %in% dark),
              position = position_stack(vjust = 0.5), size = lab) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
    scale_fill_manual(values = pal, breaks = names(pal), name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
         caption = if (caption) "Numbers in bars are respondent counts. Purdue Sociology faculty survey, fall 2026." else NULL) +
    theme_minimal(base_size = base) +
    theme(panel.grid = element_blank(), legend.position = "top", legend.justification = "left",
          plot.title.position = "plot", plot.title = element_text(face = "bold"))
  ggsave(file, p, width = w, height = h, dpi = 300, bg = "white")
}

rng <- function(x) if (min(x) == max(x)) as.character(min(x)) else paste0(min(x), "-", max(x))

task_fig <- function(r, domain, noun, file, h, ...) {
  fig(r$long_t, "item", "resp", pal_task, r$wide$item,
      sprintf("AI use in %s, past year", noun),
      sprintf("Among the %d respondents who used AI in %s (n = %s per item); ordered by number reporting any use",
              r$n_users, noun, rng(r$long_t$n_answered)),
      file, h, ...)
}

fig(gate, "domain", "type", pal_gate, c("Research", "Teaching", "Service"),
    "Faculty use of AI by area of work, past year",
    sprintf("All %d respondents; check-all-that-apply, so 'LLM and AI agent' means both", nrow(d)),
    "output/figures/faculty_domain_use.png", 3.2)
task_fig(res, "Research", "research", "output/figures/faculty_research_use_by_task.png", 6.5)
task_fig(tea, "Teaching", "teaching", "output/figures/faculty_teaching_use_by_task.png", 4.2)
task_fig(ser, "Service",  "service-related work", "output/figures/faculty_service_use_by_task.png", 3.4)

# Compact versions for the two-page report (code/03_build_report_faculty.R)
fig(gate, "domain", "type", pal_gate, c("Research", "Teaching", "Service"),
    "Figure 1. Faculty use of AI by area of work, past year",
    sprintf("All %d respondents; check-all-that-apply, so 'LLM and AI agent' means both", nrow(d)),
    "output/figures/faculty_domain_use_report.png", 1.55, w = 7.6, base = 8.5, caption = FALSE, lab = 2.5)
fig(res$long_t, "item", "resp", pal_task, res$wide$item,
    "Figure 2. AI use in research, past year",
    sprintf("Among the %d who used AI in research (n = %s per item); ordered by number reporting any use", res$n_users, rng(res$long_t$n_answered)),
    "output/figures/faculty_research_use_by_task_report.png", 3.1, w = 7.6, base = 8.5, caption = FALSE, lab = 2.3)
fig(tea$long_t, "item", "resp", pal_task, tea$wide$item,
    "Figure 3. AI use in teaching, past year",
    sprintf("Among the %d who used AI in teaching (n = %s per item); ordered by number reporting any use", tea$n_users, rng(tea$long_t$n_answered)),
    "output/figures/faculty_teaching_use_by_task_report.png", 2.15, w = 7.6, base = 8.5, caption = FALSE, lab = 2.3)

print(gate, n = Inf); print(ov, n = Inf); print(count(tibble(n_domains), n_domains))
for (r in list(res, tea, ser))
  print(r$wide |> select(item, n_answered, n_Never, n_Sometimes, n_Regularly, `n_N/A`), n = Inf, width = Inf)
