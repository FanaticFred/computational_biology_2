# Packages needed in this section
req_pkgs <- c(
  "dplyr",      # data wrangling
  "ggplot2",    # plotting
  "tidyr",      # tidying
  "readr",      # read/write csv
  "tibble",     # tibbles/printing
  "gridExtra",  # simple plot grids
  "emmeans",    # adjusted means / contrasts
  "effsize",     # effect sizes (Cohen's d, Cliff's delta)
  "DataExplorer" ,#for missingness
  "glmnet",
  "lmtest" ,# for checking the 
  "pROC"
)

# Install any that are missing
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}
# Load all (silently)
invisible(lapply(req_pkgs, require, character.only = TRUE))


#read in data set
trial_ct <- read.csv("ds05_cell_FULL.csv")
str(trial_ct[, 1:7])   # peek

#check if data set is full
type_map <- tibble::tibble(
  variable    = names(trial_ct),
  class       = sapply(trial_ct, \(x) paste(class(x), collapse = "/")),
  n_missing   = sapply(trial_ct, \(x) sum(is.na(x))),
  pct_missing = round(100 * sapply(trial_ct, \(x) mean(is.na(x))), 2),
  n_unique    = sapply(trial_ct, \(x) dplyr::n_distinct(x)),
  example     = sapply(trial_ct, \(x) paste(utils::head(unique(x), 3), collapse = ", "))
)
type_map %>% arrange(desc(class))

#exploratory analysis graphs
p1 <- ggplot(trial_ct, aes(temp_dev)) + geom_histogram(bins = 30) +
  labs(x = "temp_dev", y = "Count")
p2 <- ggplot(trial_ct, aes(ph_dev)) + geom_histogram(bins = 30) +
  labs(x = "ph_dev", y = "Count")
p3 <- ggplot(trial_ct, aes(viability)) + geom_histogram(bins = 30) +
  labs(x = "viability", y = "Count")
p4 <- ggplot(trial_ct, aes(yield_millions)) + geom_histogram(bins = 30) +
  labs(x = "yield_millions", y = "Count")
p5 <- ggplot(trial_ct, aes(potency_score)) + geom_histogram(bins = 30) +
  labs(x = "potency_score", y = "Count")
p6 <- ggplot(trial_ct, aes(culture_days)) + geom_histogram(bins = 30) +
  labs(x = "culture_days", y = "Count")
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

#compare potency score with each input
feature_cols <- c(
  "culture_days",
  "temp_dev",
  "ph_dev",
  "viability",
  "yield_millions"
)

cors <- sapply(feature_cols, function(f) {
  suppressWarnings(
    cor(trial_ct[[f]], trial_ct$potency_score, use = "pairwise.complete.obs")
  )
})

feature_df <- tibble::tibble(
  feature = names(cors),
  cor     = unname(cors)
) %>%
  dplyr::mutate(feature = reorder(feature, abs(cor)))

ggplot(feature_df, aes(x = feature, y = cor, fill = cor > 0)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(
    x = "Feature",
    y = "Correlation with potency_score",
    title = "Feature correlations with potency_score (signed)"
  )

#