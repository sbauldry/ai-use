# Build the two-page faculty summary (HTML -> PDF via headless Chrome).
# Numbers come from output/tables; quotes are pulled verbatim from the de-identified
# data and each must match exactly (the script stops if one does not).
# Quote rules (see CLAUDE.md): F7 short excerpts only; never quote F22's sabbatical/return
# details or anything from F25's research description.

suppressMessages({library(readr); library(dplyr); library(base64enc)})

gate <- read_csv("output/tables/faculty_domain_use.csv", show_col_types = FALSE)
res  <- read_csv("output/tables/faculty_research_use_by_task.csv", show_col_types = FALSE)
tea  <- read_csv("output/tables/faculty_teaching_use_by_task.csv", show_col_types = FALSE)
ser  <- read_csv("output/tables/faculty_service_use_by_task.csv", show_col_types = FALSE)
ov   <- read_csv("output/tables/faculty_response_overview.csv", show_col_types = FALSE)
nd   <- read_csv("output/tables/faculty_n_domains_distribution.csv", show_col_types = FALSE)
v    <- function(x) ov$value[startsWith(ov$measure, x)]
get  <- function(t, item) t[t$item == item, ]
users <- function(dom) sum(gate$n[gate$domain == dom & gate$type != "No"])
agent <- function(dom) sum(gate$n[gate$domain == dom & gate$type %in% c("AI agent only", "LLM and AI agent")])

d <- read_csv("data/deidentified/faculty_ai_use.csv", col_types = cols(.default = col_character()))[-c(1, 2), ]
all_text <- paste(unlist(d[c("Q3", "Q6", "Q9", "Q10")]), collapse = "\n")
quote <- function(x) {
  parts <- strsplit(x, " ... ", fixed = TRUE)[[1]]
  for (p in parts) stopifnot(grepl(p, all_text, fixed = TRUE))
  sprintf("<blockquote>&ldquo;%s&rdquo;</blockquote>", gsub(" ... ", " &hellip; ", x, fixed = TRUE))
}

lit <- get(res, "Search for relevant literature"); edit <- get(res, "Edit writing for grammar, clarity, or style")
code <- get(res, "Write or debug code"); fb <- get(res, "Get feedback on a draft"); syn <- get(res, "Synthesize a body of literature")
draft <- get(res, "Generate a first draft of text"); quant <- get(res, "Analyze quantitative data"); qual <- get(res, "Analyze qualitative data")
lec <- get(tea, "Develop lecture content, slides, or class activities"); rdg <- get(tea, "Find or summarize readings and other course materials")
grade <- get(tea, "Grade student work"); feed <- get(tea, "Give feedback on student work")
rec <- get(ser, "Write letters of recommendation")
img <- function(f) paste0("data:image/png;base64,", base64encode(f))
n_none <- nd$respondents[nd$domains_with_ai_use == 0]; n_all <- nd$respondents[nd$domains_with_ai_use == 3]

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><title>AI Use Among Sociology Faculty</title>
<style>
@page { size: Letter; margin: 0.4in 0.55in; }
body { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; font-size: 8.8pt; line-height: 1.28; color: #1a1a1a; margin: 0; }
h1 { font-size: 16pt; margin: 0 0 1pt; } .sub { color: #555; margin: 0 0 4pt; }
h2 { font-size: 10.5pt; margin: 6pt 0 2pt; color: #08519C; }
.box { background: #f3f6fa; border-left: 3px solid #08519C; padding: 3pt 8pt; margin: 3pt 0; }
ul { margin: 2pt 0 0 14pt; padding: 0; } li { margin-bottom: 1.5pt; }
img { width: 100%%; display: block; margin: 2pt 0 0; }
.pb { page-break-before: always; }
blockquote { margin: 3pt 0 4pt 8pt; padding-left: 8pt; border-left: 2px solid #9ECAE1; font-style: italic; }
.small { font-size: 8pt; color: #555; }
</style></head><body>
<h1>AI Use Among Sociology Faculty</h1>
<p class="sub">Graduate Program in Sociology, Purdue University &middot; Summary of the fall 2026 faculty survey<br>Data analysis and report generated with Claude Code (Anthropic), under the direction and review of the department</p>
<div class="box"><b>About the survey.</b> An anonymous Qualtrics survey asked faculty whether they used AI tools (LLMs such as ChatGPT, Claude or Gemini, or AI agents such as Claude Code, Codex or Cursor)
in their <b>research</b>, <b>teaching</b> and <b>service</b> over the past year, and how often for specific tasks. <b>%d of 31 faculty responded (%.0f%%)</b>; %d wrote open-ended comments.
Task questions were shown only to those who used AI in that area, so task results describe <i>users</i>, not all faculty. Read the counts, not just the shares.</div>
<img src="%s"><img src="%s"><img src="%s">
<h2>Key observations</h2><ul>
<li><b>Use varies by area.</b> %d of %d respondents used AI in research, %d in teaching and %d in service. %d used it in none of the three areas and %d in all three. AI agents were used by %d in research, %d in teaching and %d in service.</li>
<li><b>Research use centers on finding, editing and coding.</b> All %d research users used AI for literature searching (%d regularly); %d of %d for editing (%d regularly), %d for feedback on a draft, %d for synthesizing literature and %d for code (%d regularly).</li>
<li><b>Drafting and data analysis are uncommon.</b> %d of %d used AI to generate a first draft of text; %d analyzed quantitative data and %d qualitative data.</li>
<li><b>Teaching use is mostly for preparation.</b> %d of %d teaching users used AI for lecture content, slides or activities and %d for finding or summarizing readings; %d used it for grading and %d for feedback on student work.</li>
<li><b>Service use is limited to %d people.</b> All %d used AI for letters of recommendation (%d regularly); other service tasks were used by 3 or fewer.</li>
</ul>
<div class="pb"></div>
<h1>What faculty say about how they decide</h1>
<p class="sub">Selected verbatim comments from the open-ended questions (identifying details removed). Themes are the analyst&rsquo;s reading of the %d respondents&rsquo; comments.</p>
<h2>1. AI as a supervised assistant, with judgment kept in human hands</h2>
%s%s%s%s
<h2>2. Where some draw the line: teaching judgment, confidentiality and ethics</h2>
%s%s%s
<h2>3. Learning needs and uncertainty about how to use AI well</h2>
%s%s%s
<p class="small">Quotes are verbatim from the de-identified data (spelling and punctuation unchanged). Some are excerpts that begin or end mid-sentence.</p>
</body></html>',
 v("Responses recorded"), v("Response rate"), v("Wrote any"),
 img("output/figures/faculty_domain_use_report.png"), img("output/figures/faculty_research_use_by_task_report.png"), img("output/figures/faculty_teaching_use_by_task_report.png"),
 users("Research"), 26, users("Teaching"), users("Service"), n_none, n_all, agent("Research"), agent("Teaching"), agent("Service"),
 lit$n_answered, lit$n_Regularly, edit$any_use_n, edit$n_answered, edit$n_Regularly, fb$any_use_n, syn$any_use_n, code$any_use_n, code$n_Regularly,
 draft$any_use_n, draft$n_answered, quant$any_use_n, qual$any_use_n,
 lec$any_use_n, lec$n_answered, rdg$any_use_n, grade$any_use_n, feed$any_use_n,
 users("Service"), rec$any_use_n, rec$n_Regularly,
 v("Wrote any"),
 quote("I direct it and check its work, but I give it specific tasks that I know it is good at and/or that would be tedious and time consuming for me to do. I do not yet feel comfortable -- and may never -- having it produce the ideas or do the writing."),
 quote("I use AI fairly heavily, but never as a fire-and-forget solution. Instead, I generally engage in a dialog with AI, much as I might engage in a dialog with a grad RA."),
 quote("In any case, I check out all responses with follow up questions, including doubts and contradictions."),
 quote("If AI can do it easily or well, I change the assignment if possible."),
 quote("I never use it for grading, feedback, or for writing emails, all of which I feel should come from me directly."),
 quote("it may be unethical to put primary data into AI models (i.e., for reasons of confidentiality)."),
 quote("I have questions about the ethical use of AI in qualitative research where assurances of confidentiality is part of the process of receiving IRB approval."),
 quote("I would be interested in a training/workshop specific to understanding how students are using it (as opposed to a workshop how to incorporate it into my teaching)."),
 quote("I experimented with a few things but have not found AI effective in doing the tasks I wanted to be done. I need to learn more about various AI tools."),
 quote("I hope to use AI for more labor saving well defined concrete tasks over time ... because I haven't yet learned efficient ways to make it work for me."))

writeLines(html, "reports/faculty/faculty_ai_use_summary.html")
chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
system2(chrome, c("--headless=new", "--disable-gpu", "--no-pdf-header-footer",
  "--print-to-pdf=reports/faculty/faculty_ai_use_summary.pdf", "reports/faculty/faculty_ai_use_summary.html"),
  stdout = FALSE, stderr = FALSE)
pages <- length(gregexpr("/Type /Page[^s]", paste(readLines("reports/faculty/faculty_ai_use_summary.pdf", warn = FALSE, encoding = "latin1"), collapse = "\n"))[[1]])
message("Wrote reports/faculty/faculty_ai_use_summary.pdf (", pages, " pages)")
