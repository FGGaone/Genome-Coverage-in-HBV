# ============================================================
# HBV COVERAGE MAP FROM ALIGNED FASTA
# ============================================================

# Install packages if needed
# install.packages("ggplot2")
# install.packages("dplyr")
# BiocManager::install("Biostrings")

library(Biostrings)
library(ggplot2)
library(dplyr)

# ============================================================
# READ ALIGNED FASTA
# ============================================================

seqs <- readDNAStringSet("genotype_E.fasta")

# Number of sequences
nseq <- length(seqs)
cat("Number of sequences:", nseq, "\n")

# Convert alignment to matrix
mat <- do.call(
  rbind,
  strsplit(as.character(seqs), "")
)

# ============================================================
# CALCULATE COVERAGE
# ============================================================

coverage <- colSums(
  mat != "-" &
    mat != "N"
)

coverage_df <- data.frame(
  Position = 1:length(coverage),
  Coverage = coverage
)

# ============================================================
# HBV ORF COORDINATES
# (Reference genome numbering)
# ============================================================

orf_df <- data.frame(
  ORF = c("Surface", "X", "Core"),
  Start = c(155, 1374, 1901),
  End   = c(835, 1838, 2452)
)

# ============================================================
# PLOT COVERAGE MAP
# ============================================================

p <- ggplot(coverage_df,
            aes(x = Position,
                y = Coverage)) +
  
  geom_line(linewidth = 0.8) +
  
  geom_rect(
    data = orf_df,
    inherit.aes = FALSE,
    aes(
      xmin = Start,
      xmax = End,
      ymin = -0.12*nseq,
      ymax = -0.02*nseq
    ),
    alpha = 0.5
  ) +
  
  geom_text(
    data = orf_df,
    inherit.aes = FALSE,
    aes(
      x = (Start + End)/2,
      y = -0.07*nseq,
      label = ORF
    ),
    size = 4
  ) +
  
  labs(
    title = "HBV Genome Coverage Map",
    x = "Genome Position",
    y = "Number of Sequences"
  ) +
  
  ylim(-0.15*nseq, max(coverage) * 1.05) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(p)

# ============================================================
# SAVE FIGURE
# ============================================================

ggsave(
  "HBV_coverage_map.png",
  p,
  width = 12,
  height = 5,
  dpi = 300
)

# ============================================================
# COVERAGE SUMMARY BY ORF
# ============================================================

for(i in 1:nrow(orf_df)){
  
  region_cov <- coverage_df$Coverage[
    coverage_df$Position >= orf_df$Start[i] &
      coverage_df$Position <= orf_df$End[i]
  ]
  
  cat(
    "\n",
    orf_df$ORF[i],
    "\nMean coverage:",
    round(mean(region_cov),1),
    "\nMedian coverage:",
    median(region_cov),
    "\n"
  )
}
