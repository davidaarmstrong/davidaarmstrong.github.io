# Recoding Variables with mutate()
# ============================================================
# Learn to recode and transform variables using mutate() with
# case_when() for range-based conditions and recode_values()
# for value replacement, across three real ggplot2 datasets.
# ============================================================

library(ggplot2)
library(dplyr)


# Task 1: case_when() — categorising sleep duration
# --------------------------------------------------
# case_when() lets you create a new column based on a series
# of conditions — think of it as a vectorised if / else if / else.
#
# The msleep dataset (built into ggplot2) records sleep times
# for 83 mammals.
#
# Using mutate() and case_when(), add a column called sleep_cat
# to msleep:
#   "short"   — sleep_total less than 8 hours
#   "average" — sleep_total between 8 and 12 hours (inclusive)
#   "long"    — sleep_total more than 12 hours
#
# Save the result as msleep2 and print a count of each category.

msleep2 <- mutate(msleep,
  sleep_cat = case_when(
    sleep_total < 8   ~ "short",
    sleep_total <= 12 ~ ________,
    sleep_total > 12  ~ ________
  )
)

count(msleep2, sleep_cat)


# Task 2: recode_values() — replacing values one-to-one
# ------------------------------------------------------
# recode_values() is the tidyverse tool for straightforward
# value substitution — replacing specific values with new ones
# without needing to write conditions.
#
# The midwest dataset records demographic data for counties
# across five US states. The state column contains abbreviations:
# "IL", "IN", "MI", "OH", "WI".
#
# Using mutate() and recode_values(), add a column state_name
# that replaces each abbreviation with the full state name.
# Save as midwest2.
#
# Syntax: recode_values(column, "old" ~ "new", ...)

midwest2 <- mutate(midwest,
  state_name = recode_values(state,
    "IL" ~ "Illinois",
    "IN" ~ ________,
    "MI" ~ ________,
    "OH" ~ ________,
    "WI" ~ ________
  )
)

count(midwest2, state, state_name)


# Task 3: case_when() — price tiers in diamonds
# ----------------------------------------------
# The diamonds dataset contains prices and characteristics of
# ~54,000 diamonds.
#
# Using mutate() and case_when(), create a price_tier column:
#   "budget"    — price under $1,000
#   "mid-range" — price $1,000 to $4,999
#   "premium"   — price $5,000 or more
#
# Save as diamonds2. Then use count() to see how many diamonds
# fall in each tier.
#
# This time the starter code is more sparse — fill in the
# conditions yourself.

diamonds2 <- mutate(diamonds,
  price_tier = case_when(

  )
)

count(diamonds2, price_tier)


# Task 4: recode_values() — many-to-one colour grouping
# ------------------------------------------------------
# Diamonds are graded for colour on a scale from D (colourless)
# to J (faint yellow). The color column uses these GIA grades.
#
# Use recode_values() to collapse the seven grades into three
# broader groups in a new column color_group:
#   "colorless"      — grades D, E, F
#   "near-colorless" — grades G, H
#   "faint"          — grades I, J
#
# With recode_values() you can group multiple input values on
# the left side of ~ using c():
#   recode_values(col, c("a", "b") ~ "group1", ...)
#
# Save as diamonds3. The starter code shows the first line —
# complete the rest.

diamonds3 <- mutate(diamonds,
  color_group = recode_values(as.character(color),
    c("D", "E", "F") ~ "colorless",
    ________         ~ ________,
    ________         ~ ________
  )
)

count(diamonds3, color, color_group)


# Task 5: case_when() — combining two conditions
# -----------------------------------------------
# case_when() really shines when you need to cross multiple
# variables to create a category.
#
# Using the midwest dataset, create a county_type column that
# combines urban status (inmetro: 1 = metro, 0 = non-metro)
# with poverty level (percbelowpoverty):
#
#   inmetro == 1 & percbelowpoverty <= 15  ->  "urban-low-poverty"
#   inmetro == 1 & percbelowpoverty >  15  ->  "urban-high-poverty"
#   inmetro == 0 & percbelowpoverty <= 15  ->  "rural-low-poverty"
#   inmetro == 0 & percbelowpoverty >  15  ->  "rural-high-poverty"
#
# Save as midwest3. The starter code gives you the shell only —
# the mutate() call is yours to write.

midwest3 <- mutate(midwest,

)

count(midwest3, county_type)


# Task 6: Combining recode_values() and case_when() in one mutate()
# ------------------------------------------------------------------
# You can create multiple columns in a single mutate() call —
# and a later column can use an earlier one computed in the same call.
#
# Using diamonds, create two new columns inside one mutate():
#
# Step 1 — cut_score (use recode_values()):
#   Convert cut to a numeric quality score:
#   "Fair" -> 1, "Good" -> 2, "Very Good" -> 3,
#   "Premium" -> 4, "Ideal" -> 5
#
# Step 2 — value_assessment (use case_when() on cut_score and price):
#   cut_score >= 4 & price < 2000   ->  "excellent value"
#   cut_score >= 4 & price >= 2000  ->  "quality piece"
#   cut_score <= 2 & price >= 3000  ->  "overpriced"
#   Everything else                 ->  "standard"
#
# Save as diamonds4 and count the value assessments.

diamonds4 <- mutate(diamonds,

)

count(diamonds4, value_assessment)
