# Summarising Data with summarise()
# ============================================================
# Learn to compute summary statistics with summarise(), from
# simple global summaries through grouped summaries using .by,
# across the msleep, diamonds, and midwest datasets.
# ============================================================

library(ggplot2)
library(dplyr)


# Task 1: summarise() — global summary statistics
# ------------------------------------------------
# summarise() collapses a data frame down to a single row of
# summary statistics.
#
# Using the msleep dataset, calculate five summary statistics
# for sleep_total:
#   mean_sleep   — mean
#   median_sleep — median
#   sd_sleep     — standard deviation
#   min_sleep    — minimum
#   max_sleep    — maximum
#
# Note that some animals have missing sleep data — use
# na.rm = TRUE inside each function to ignore NA values.
#
# Save the result as sleep_summary and print it.

sleep_summary <- summarise(msleep,
  mean_sleep   = mean(sleep_total,   na.rm = TRUE),
  median_sleep = median(sleep_total, na.rm = TRUE),
  sd_sleep     = ________,
  min_sleep    = ________,
  max_sleep    = ________
)

sleep_summary


# Task 2: summarise() with .by — grouped means
# ---------------------------------------------
# The .by argument inside summarise() splits the data into
# groups before summarising — one row of results per group.
#
# Using msleep, calculate the mean sleep time and count of
# animals for each diet type (vore). Name the new columns
# mean_sleep and n.
#
# Use n() (no arguments) to count rows within each group.
#
# Save as sleep_by_diet and print it.
#
# Note: some animals have no recorded diet — they will appear
# as their own group.

sleep_by_diet <- summarise(msleep,
  mean_sleep = mean(sleep_total, na.rm = TRUE),
  n          = n(),
  .by        = ________
)

sleep_by_diet


# Task 3: summarise() with .by — multiple statistics
# ---------------------------------------------------
# You can compute as many summary columns as you need in a
# single summarise() call.
#
# Using diamonds, calculate three statistics for each cut grade:
#   mean_price   — mean price
#   median_price — median price
#   n            — number of diamonds
#
# Save as price_by_cut and print it.
#
# The starter code is more sparse this time — you write the
# summary statistics.

price_by_cut <- summarise(diamonds,

  .by = cut
)

price_by_cut


# Task 4: summarise() with .by — state-level poverty summary
# -----------------------------------------------------------
# Using the midwest dataset, summarise county-level data up
# to the state level.
#
# For each state, calculate:
#   mean_poverty — mean of percbelowpoverty
#   mean_college — mean of percollege
#   sd_poverty   — standard deviation of percbelowpoverty
#   n_counties   — number of counties
#
# Save as poverty_by_state and print it.
#
# The starter code gives you only the shell.

poverty_by_state <- summarise(midwest,

)

poverty_by_state


# Task 5: summarise() with .by — grouping by two variables
# ---------------------------------------------------------
# .by can take a vector of column names to group by multiple
# variables simultaneously — c(var1, var2).
#
# Using diamonds, calculate the mean price and count for each
# combination of cut and color.
#
# Name the columns mean_price and n. Save as price_by_cut_color.
#
# How many rows do you expect in the result?

price_by_cut_color <- summarise(diamonds,

)

price_by_cut_color


# Task 6: summarise() then arrange() — a complete summary pipeline
# ----------------------------------------------------------------
# Combine everything: group by two variables, compute multiple
# statistics, then sort the result.
#
# Using midwest, summarise counties by both state and inmetro
# (1 = metro area, 0 = non-metro):
#   mean_poverty — mean of percbelowpoverty
#   sd_poverty   — standard deviation of percbelowpoverty
#   mean_college — mean of percollege
#   n_counties   — count
#
# Save as state_metro_summary.
#
# Then use arrange() to sort the result by state (ascending)
# and mean_poverty (descending within each state). Save the
# sorted result back to state_metro_summary.
#
# Hint for arrange(): arrange(data, var1, desc(var2))

state_metro_summary <- summarise(midwest,

)

state_metro_summary <- arrange(state_metro_summary, ________)

state_metro_summary
