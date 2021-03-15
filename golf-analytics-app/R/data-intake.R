# Read in excel data
df_courses <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "courses")

df_rounds <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "rounds")

df_holes <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "holes")

df_clubs <- openxlsx::read.xlsx("data/golf-analytics.xlsx", sheet = "clubs")

