# =============================================================
# MDROs and Interfacility Transfer - Arizona Hospital Discharge Data
# Purpose: Demonstrate a basic R workflow (load, clean, summarize,
#          model, visualize) examining the relationship between
#          interfacility transfer and multidrug-resistant organism
#          (MDRO) status using hospital discharge-style data.
#          Intended as a starting template for a git/GitHub tutorial.
# =============================================================

# ---- 1. Setup ----
# Install packages once if needed (uncomment the line below):
# install.packages(c("tidyverse", "broom"))

library(tidyverse)
library(broom)

# ---- 2. Create / Load Data ----
# For demo purposes we generate a synthetic Arizona hospital
# discharge dataset. In a real tutorial, replace this block with:
#   data <- read_csv("data/az_discharges.csv")
# (e.g., an extract from the Arizona Hospital Discharge Database)

set.seed(13)

az_counties <- c("Maricopa", "Pima", "Pinal", "Yavapai", "Yuma",
                  "Coconino", "Mohave", "Cochise")

organisms <- c("MRSA", "VRE", "CRE", "C. difficile", "MDR-Pseudomonas")

n_discharges <- 100

discharge_data <- tibble(
  discharge_id = 1:n_discharges,
  county = sample(az_counties, n_discharges, replace = TRUE,
                   prob = c(0.45, 0.2, 0.07, 0.07, 0.06, 0.05, 0.06, 0.04)),
  admit_date = as.Date("2025-01-01") + sample(0:364, n_discharges, replace = TRUE),
  age = pmax(0, round(rnorm(n_discharges, mean = 60, sd = 20))),
  interfacility_transfer = rbinom(n_discharges, 1, prob = 0.25),
  length_of_stay = NA_real_,
  mdro_status = NA_real_
)

# Build in a realistic relationship: transferred patients tend to have
# longer stays and a higher probability of MDRO, reflecting the idea
# that interfacility transfer is a known MDRO risk factor.
discharge_data <- discharge_data %>%
  mutate(
    length_of_stay = round(pmax(1, rnorm(
      n_discharges,
      mean = 5 + interfacility_transfer * 4,
      sd = 2
    ))),
    mdro_prob = plogis(
      -2.5 +
        1.1 * interfacility_transfer +
        0.02 * age +
        0.03 * length_of_stay
    ),
    mdro_status = rbinom(n_discharges, 1, mdro_prob),
    organism = if_else(
      mdro_status == 1,
      sample(organisms, n_discharges, replace = TRUE,
             prob = c(0.35, 0.2, 0.15, 0.2, 0.1)),
      NA_character_
    )
  ) %>%
  select(-mdro_prob)

# ---- 3. Clean Data ----
discharge_data <- discharge_data %>%
  filter(age > 0, length_of_stay > 0) %>%
  mutate(
    county = factor(county, levels = az_counties),
    interfacility_transfer = factor(interfacility_transfer,
                                     levels = c(0, 1),
                                     labels = c("No", "Yes")),
    mdro_status = factor(mdro_status,
                          levels = c(0, 1),
                          labels = c("No", "Yes")),
    organism = factor(organism, levels = organisms)
  )

# ---- 4. Summary Statistics ----

# Contingency table: interfacility transfer vs. MDRO status
transfer_mdro_table <- table(
  Transfer = discharge_data$interfacility_transfer,
  MDRO = discharge_data$mdro_status
)
print(transfer_mdro_table)

# MDRO rate by transfer status
mdro_rate_by_transfer <- discharge_data %>%
  group_by(interfacility_transfer) %>%
  summarise(
    n = n(),
    n_mdro = sum(mdro_status == "Yes"),
    pct_mdro = mean(mdro_status == "Yes") * 100,
    mean_los = mean(length_of_stay),
    .groups = "drop"
  )
print(mdro_rate_by_transfer)

# MDRO rate by county
mdro_rate_by_county <- discharge_data %>%
  group_by(county) %>%
  summarise(
    n = n(),
    pct_mdro = mean(mdro_status == "Yes") * 100,
    pct_transferred = mean(interfacility_transfer == "Yes") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_mdro))
print(mdro_rate_by_county)

# ---- 5. Statistical Tests ----

# Chi-square test of independence: transfer vs. MDRO status
chisq_result <- chisq.test(transfer_mdro_table)
print(chisq_result)

# Simple odds ratio (2x2 table assumption)
or_table <- transfer_mdro_table
odds_ratio <- (or_table["Yes", "Yes"] * or_table["No", "No"]) /
  (or_table["Yes", "No"] * or_table["No", "Yes"])
cat("Crude odds ratio (transfer vs. MDRO):", round(odds_ratio, 2), "\n")

# Logistic regression adjusting for age and length of stay
mdro_model <- glm(
  mdro_status ~ interfacility_transfer + age + length_of_stay,
  data = discharge_data,
  family = binomial
)

model_summary <- tidy(mdro_model, exponentiate = TRUE, conf.int = TRUE)
print(model_summary)

# ---- 6. Visualization ----

# MDRO rate by interfacility transfer status
transfer_plot <- mdro_rate_by_transfer %>%
  ggplot(aes(x = interfacility_transfer, y = pct_mdro, fill = interfacility_transfer)) +
  geom_col() +
  labs(
    title = "MDRO Rate by Interfacility Transfer Status",
    x = "Interfacility Transfer",
    y = "MDRO Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(transfer_plot)

# MDRO rate by Arizona county
county_plot <- mdro_rate_by_county %>%
  ggplot(aes(x = fct_reorder(county, pct_mdro), y = pct_mdro)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "MDRO Rate by Arizona County",
    x = "County",
    y = "MDRO Rate (%)"
  ) +
  theme_minimal()

print(county_plot)

# Forest-style plot of adjusted odds ratios from the logistic model
or_plot <- model_summary %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Adjusted Odds Ratios for MDRO Status",
    x = NULL,
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal()

print(or_plot)

# ---- 7. Save Outputs ----
# Uncomment to save results when running outside of the tutorial:
# write_csv(mdro_rate_by_transfer, "output/mdro_rate_by_transfer.csv")
# write_csv(mdro_rate_by_county, "output/mdro_rate_by_county.csv")
# write_csv(model_summary, "output/mdro_model_summary.csv")
# ggsave("output/transfer_plot.png", transfer_plot, width = 6, height = 4)
# ggsave("output/county_plot.png", county_plot, width = 6, height = 5)
# ggsave("output/or_plot.png", or_plot, width = 6, height = 4)

# =============================================================
# End of script
# =============================================================
