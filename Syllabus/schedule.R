# %%
library(tidyverse)
library(here)
library(tinytable)
library(glue)

# %%
# ## get dates
# library(tidyverse)
# first_day <- ymd("2026-08-16")
# last_day <- ymd("2026-12-03")
# labor_day <- ymd("2026-09-07")
# fall_break <- c(ymd("2026-10-19"), ymd("2026-10-20"))
#
# days <- seq(first_day, last_day, by = "day") |>
#   enframe(name = NULL, value = "day") |>
#   mutate(
#     day_of_week = wday(day, label = TRUE, abbr = FALSE)
#   ) |>
#   filter(day_of_week %in% c("Monday", "Wednesday"))
#
# week_to_dates <- days |>
#   mutate(Week = 1 + floor(as.numeric(day - first_day) / 7)) |>
#   # filter(day != labor_day) |>
#   mutate(value = "") |>
#   mutate(
#     .by = Week,
#     Dates = paste0(
#       sprintf("%02d/%02d", month(day), day(day)),
#       collapse = " - "
#     )
#   ) |>
#   select(Week, Dates, name = day_of_week)
#
# # Update schedule.csv dates using week_to_dates
# updated_schedule <- here("Syllabus/schedule.csv") |>
#   read_csv(show_col_types = FALSE) |>
#   mutate(Week = as.character(Week)) |>
#   left_join(
#     week_to_dates |>
#       distinct(Week, Dates) |>
#       mutate(Week = as.character(Week)) |>
#       rename(Dates_new = Dates),
#     by = "Week"
#   ) |>
#   mutate(Dates = coalesce(Dates_new, Dates)) |>
#   select(Week, Dates, name, value)
#
# write_csv(updated_schedule, here("Syllabus/schedule.csv"))

# %%
cal <- here("Syllabus/schedule.csv") |>
  read_csv(show_col_types = FALSE) |>
  pivot_wider(names_from = "name", values_from = "value") |>
  mutate(across(c(Monday, Wednesday, Assignments), \(x) replace_na(x, "")))


# %%
midterm_idx_monday <- which(str_detect(cal$Monday, "Midterm"))
midterm_idx_wednesday <- which(str_detect(cal$Wednesday, "Midterm"))
noclass_idx_monday <- which(str_detect(cal$Monday, "No Class"))
noclass_idx_wednesday <- which(str_detect(cal$Wednesday, "No Class"))

tab <- cal |>
  mutate(across(
    c(Monday, Wednesday, Assignments),
    \(x) str_replace(x, "<br\\s?/>", " \\\\newline ")
  )) |>
  tt(
    width = c(0.1, 0.2, 0.3, 0.3, 0.3)
  ) |>
  style_tt(i = nrow(cal), j = "Assignments", color = "#9a2515") |>
  style_tt(i = midterm_idx_wednesday, j = "Wednesday", color = "#9a2515") |>
  style_tt(i = noclass_idx_monday, j = "Monday", color = "#f26d21") |>
  style_tt(i = noclass_idx_wednesday, j = "Wednesday", color = "#f26d21")

print(tab, "html")

# %%
save_tt(tab, here("Syllabus/schedule.tex"), overwrite = TRUE)

# %%
# Write to README.md
readme <- xfun::read_utf8(here("README.md"))
insert_idx <- which(str_detect(readme, "<!-- Schedule -->"))

tab_md_string <- tab |>
  tinytable:::build_tt("gfm") |>
  (\(x) x@table_string)() |>
  str_split_1("\n")

# Hacky way to change table text
if (insert_idx[2] > insert_idx[1] + 1) {
  rows_to_keep <- setdiff(
    seq_len(length(readme)),
    (insert_idx[1] + 1):(insert_idx[2] - 1)
  )
  readme <- readme[rows_to_keep]
}
readme <- append(readme, tab_md_string, after = insert_idx[1])

xfun::write_utf8(readme, here("README.md"))

# %%
# tribble(
#   ~Assignments, ~`Percent of grade`,
#   "Homework", "35\\%",
#   "Midterm", "20\\%",
#   "Midterm", "20\\%",
#   "Final", "25\\%",
# ) |>
#   tt() |>
#   print("gfm")
