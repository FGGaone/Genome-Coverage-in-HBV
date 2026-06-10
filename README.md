# Genome-Coverage-in-HBV
An R-script that makes coding easy, genome coverage map for HBV near full length genome.
# ── 1. Load libraries ────────────────────────────────────────────────
library(readxl)

# ── 2. Read the workbook ─────────────────────────────────────────────
df <- read_excel("final_work_book.xlsx", sheet = "Sheet1")

# ── 3. Prepare data --------------------------------------------------
# Keep only anti-HBc positive (1) and negative (2); drop NAs in hbv_vl
df_clean <- df[df$anti_hbc_status %in% c(1, 2) & !is.na(df$hbv_vl), ]

# Split into two groups
vl_positive <- df_clean$hbv_vl[df_clean$anti_hbc_status == 1]  # anti-HBc positive
vl_negative <- df_clean$hbv_vl[df_clean$anti_hbc_status == 2]  # anti-HBc negative

# ── 4. Descriptive statistics ────────────────────────────────────────
cat("=== Anti-HBc POSITIVE (1) ===\n")
cat("n =", length(vl_positive), "\n")
cat("Median VL:", median(vl_positive), "\n")
cat("IQR:", IQR(vl_positive), "\n\n")

cat("=== Anti-HBc NEGATIVE (2) ===\n")
cat("n =", length(vl_negative), "\n")
cat("Median VL:", median(vl_negative), "\n")
cat("IQR:", IQR(vl_negative), "\n\n")


#proportions of anti-HBc status by viral load
prop.table(data$anti_hbc_status)

# ── 5. Wilcoxon Rank-Sum Test (Mann-Whitney U) -----------------------
# Used because HBV viral load data is typically non-normally distributed
# alternative = "greater" tests if positive > negative
result <- wilcox.test(
  vl_positive,
  vl_negative,
  alternative = "two.sided",   # two-sided: detects difference in either direction
  conf.int    = TRUE
)

cat("=== Wilcoxon Rank-Sum Test ===\n")
print(result)

cat("\nP-value:", result$p.value, "\n")
cat("Interpretation:", ifelse(result$p.value < 0.05,
    "Significant difference (p < 0.05)",
    "No significant difference (p ≥ 0.05)"), "\n")

# ── 6. Optional: Visualise with a boxplot ───────────────────────────
df_clean$group <- ifelse(df_clean$anti_hbc_status == 1, "Positive", "Negative")

boxplot(
  hbv_vl ~ group,
  data    = df_clean,
  main    = "HBV Viral Load by Anti-HBc Status",
  xlab    = "Anti-HBc Status",
  ylab    = "HBV Viral Load (IU/mL)",
  col     = c("tomato", "steelblue"),
  outline = TRUE
)
```

**A few notes:**

- **Wilcoxon rank-sum** is used instead of a t-test because viral load data is rarely normally distributed — it's typically right-skewed and often log-transformed in clinical studies.
- The test is set to **two-sided** by default, which tells you *whether* the groups differ. If you have a directional hypothesis (e.g., "positive patients have higher VL"), change `alternative = "two.sided"` to `alternative = "greater"`.
- Your data has some `NA` values in `hbv_vl` — the code handles those automatically.
- If you'd like a **log₁₀-transformed** analysis (common in virology), just add `df_clean$log_hbv_vl <- log10(df_clean$hbv_vl)` and run the test on that column instead.

Let me know if you'd like the log-transformed version or a ggplot2 visualisation!


