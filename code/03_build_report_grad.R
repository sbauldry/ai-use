# Build the two-page graduate student summary (HTML -> PDF via headless Chrome).
# Numbers come from output/tables; quotes are pulled verbatim from the de-identified
# data and each must match exactly (the script stops if one does not).

suppressMessages({library(readr); library(dplyr); library(base64enc)})

t2 <- read_csv("output/tables/grad_coursework_use_by_task.csv", show_col_types = FALSE)
t5 <- read_csv("output/tables/grad_ap_dissertation_use_by_task.csv", show_col_types = FALSE)
ov <- read_csv("output/tables/grad_response_overview.csv", show_col_types = FALSE)
br <- read_csv("output/tables/grad_coursework_breadth_distribution.csv", show_col_types = FALSE)
get <- function(t, item) t[t$item == item, ]
v   <- function(x) ov$value[ov$measure == x]

d <- read_csv("data/deidentified/grad_ai_use.csv", col_types = cols(.default = col_character()))[-c(1, 2), ]
all_text <- paste(unlist(d[c("Q3", "Q4", "Q6")]), collapse = "\n")
quote <- function(x) {                        # x: fragments joined by " ... "
  parts <- strsplit(x, " ... ", fixed = TRUE)[[1]]
  for (p in parts) stopifnot(grepl(p, all_text, fixed = TRUE))
  sprintf("<blockquote>&ldquo;%s&rdquo;</blockquote>", gsub(" ... ", " &hellip; ", x, fixed = TRUE))
}

code <- get(t2, "Write or debug code"); edit <- get(t2, "Edit writing for grammar, clarity, or style")
expl <- get(t2, "Explain concepts, theories, or methods"); read <- get(t2, "Help understand difficult readings")
lit  <- get(t2, "Search for relevant literature sources"); draft <- get(t2, "Generate a first draft of a paper")
qual <- get(t2, "Analyze qualitative data"); stat2 <- get(t2, "Interpret statistical results")
e5 <- get(t5, "Edit writing for grammar, clarity, or style"); c5 <- get(t5, "Write or debug code")
l5 <- get(t5, "Search for relevant literature"); d5 <- get(t5, "Generate a first draft")
s5 <- get(t5, "Interpret statistical results")
n_any <- sum(br$respondents[br$tasks_any_use > 0]); n_resp <- sum(br$respondents)
med <- median(rep(br$tasks_any_use, br$respondents))

img <- function(f) paste0("data:image/png;base64,", base64encode(f))

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><title>AI Use Among Sociology Graduate Students</title>
<style>
@page { size: Letter; margin: 0.45in 0.55in; }
body { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 9.2pt; line-height: 1.32; color: #1a1a1a; margin: 0; }
h1 { font-size: 16pt; margin: 0 0 1pt; } .sub { color: #555; margin: 0 0 5pt; }
h2 { font-size: 10.5pt; margin: 7pt 0 2pt; color: #08519C; }
.box { background: #f3f6fa; border-left: 3px solid #08519C; padding: 4pt 8pt; margin: 4pt 0; }
ul { margin: 2pt 0 0 14pt; padding: 0; } li { margin-bottom: 1.5pt; }
img { width: 100%%; display: block; margin: 3pt 0 0; }
.pb { page-break-before: always; }
blockquote { margin: 3pt 0 4pt 8pt; padding-left: 8pt; border-left: 2px solid #9ECAE1; font-style: italic; }
.small { font-size: 8pt; color: #555; }
</style></head><body>
<h1>AI Use Among Sociology Graduate Students</h1>
<p class="sub">Graduate Program in Sociology, Purdue University &middot; Summary of the fall 2026 graduate student survey<br>Data analysis and report generated with Claude Code (Anthropic), under the direction and review of the department</p>
<div class="box"><b>About the survey.</b> An anonymous Qualtrics survey asked graduate students how often they used AI tools (e.g., ChatGPT, Claude, Gemini)
for specific tasks in <b>coursework</b> over the past year and whether they used or planned to use AI for their <b>AP or dissertation</b>,
plus open-ended questions about how they decide when to use AI. <b>%d of 31 students responded (%.0f%%)</b>; %d answered the coursework
items and %d the AP/dissertation items, and %d wrote open-ended comments. Because the program is small, each respondent moves a bar by 5&ndash;8 percentage points;
read the counts, not just the shares. Responses reflect who chose to answer and may differ from non-respondents.</div>
<img src="%s"><img src="%s">
<h2>Key observations</h2><ul>
<li><b>Use is nearly universal but selective.</b> %d of %d respondents used AI for at least one coursework task; the typical respondent used it for about %d of 15 tasks (range 0&ndash;15).</li>
<li><b>Editing and coding are the mainstream uses.</b> %d of %d used AI for code (%d regularly) and %d of %d for editing writing (%d regularly). Explaining concepts (%d), understanding readings (%d) and finding literature (%d) follow.</li>
<li><b>Generating original content is rare.</b> Only %d of %d used AI to draft a paper in coursework, and %d of %d plan to for their AP/dissertation; %d of %d used it for qualitative analysis.</li>
<li><b>Dissertation plans mirror coursework.</b> %d of %d plan to use AI for editing, %d of %d for code, and %d of %d for literature searching. Interpreting statistical results is used by %d of %d in coursework but planned by only %d of %d for the AP/dissertation.</li>
</ul>
<div class="pb"></div>
<h1>What students say about how they decide</h1>
<p class="sub">Selected verbatim comments from the open-ended questions (identifying details removed). Themes are the analyst&rsquo;s reading of %d respondents&rsquo; comments.</p>
<h2>1. AI as a support tool, with ideas and writing kept their own</h2>
%s%s%s
<h2>2. Checking AI&rsquo;s output and doubts about its quality</h2>
%s%s%s
<h2>3. Privacy, language support, and the need for clear norms</h2>
%s%s%s%s
<p class="small">Quotes are verbatim from the de-identified data (spelling and punctuation unchanged). Some are excerpts that begin or end mid-sentence.</p>
</body></html>',
 v("Responses recorded"), v("Response rate (%)"), v("Answered >=1 coursework item (Q2)"), v("Answered >=1 AP/dissertation item (Q5)"), v("Wrote any open-ended text (Q3/Q4/Q6)"),
 img("output/figures/grad_coursework_use_by_task_report.png"), img("output/figures/grad_ap_dissertation_use_by_task_report.png"),
 n_any, n_resp, round(med),
 code$any_use_n, code$n_answered, code$n_Regularly, edit$any_use_n, edit$n_answered, edit$n_Regularly,
 expl$any_use_n, read$any_use_n, lit$any_use_n,
 draft$any_use_n, draft$n_answered, d5$n_Yes, d5$n_answered + d5$`n_N/A`, qual$any_use_n, qual$n_answered,
 e5$n_Yes, e5$n_answered, c5$n_Yes, c5$n_answered, l5$n_Yes, l5$n_answered,
 stat2$any_use_n, stat2$n_answered, s5$n_Yes, s5$n_answered,
 v("Wrote any open-ended text (Q3/Q4/Q6)"),
 quote("The line for me is basically I want my writing and ideas to be original to me, but I'm fine using AI to do things that people (myself or others) would take much more time to do (editing for grammar; collecting literature, etc.) so that I can concentrate my time and energy on the stuff that is original to me and my work."),
 quote("I don't use AI to generate writing, but I will use it to polish the work."),
 quote("I tend to think of AI like a \"better\" search engine -- I use it to find information that I would otherwise spend time searching the internet for, but I don't outsource other tasks to it."),
 quote("I also try to verify citations, statistical results, and factual claims independently rather than assuming that AI is correct."),
 quote("I don't think it produces good enough content to use effectively yet. I can't say I wouldn't be tempted to use it more if it produced better work."),
 quote("Sometimes I use it to get feedback on my working drafts, but it tends to give affirmative comments more than giving sharp critiques."),
 quote("AI can be very useful for graduate work, especially for non-native English speakers. Writing and polishing academic English can take a substantial amount of time even when the underlying ideas are already clear"),
 quote("always be aware of and show respect to the moral, legal, and ethical boundaries when using AI. For instance, not to upload interview transcripts or quantitative survey data to AI platform for analysis."),
 quote("Clearer norms about appropriate AI use in research, writing, coding, and collaboration would also be helpful."),
 quote("allowing students to use it at their own discretion, works for now."))

writeLines(html, "reports/grad/grad_ai_use_summary.html")
chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
status <- system2(chrome, c("--headless=new", "--disable-gpu", "--no-pdf-header-footer",
  "--print-to-pdf=reports/grad/grad_ai_use_summary.pdf", "reports/grad/grad_ai_use_summary.html"),
  stdout = FALSE, stderr = FALSE)
pages <- length(gregexpr("/Type /Page[^s]", paste(readLines("reports/grad/grad_ai_use_summary.pdf", warn = FALSE, encoding = "latin1"), collapse = "\n"))[[1]])
message("Wrote reports/grad/grad_ai_use_summary.pdf (", pages, " pages)")
