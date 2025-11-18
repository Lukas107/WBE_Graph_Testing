library(readxl)
library(data.tree)
library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(ggplot2)
library(patchwork)
library(rlang)
library(xtable)

setwd("/Users/lukas/Documents/Unizeug/Bachelorarbeit")
source("Code/bachelor.R")

# Define a reusable theme with larger fonts ####
custom_theme <- theme_minimal(base_size = 14) +  # sets base font size
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    plot.title   = element_text(size = 18, hjust = 0.5)
  )

# Boxplot Flows ####
# Prepare data in long format for the four components
abfluss_long <- data.frame(
  Komponente = rep(c("Häuslich", "Gewerblich",
                     "Fremdwasser", "Trockenwetter"),
                   each = nrow(Grundlagendaten)),
  Wert = c(
    Grundlagendaten$qh_L_s,
    Grundlagendaten$qg_L_s,
    Grundlagendaten$qf_L_s,
    Grundlagendaten$qt_L_s
  )
)

abfluss_long$Komponente <- factor(
  abfluss_long$Komponente,
  levels = c("Häuslich", "Gewerblich", "Fremdwasser", "Trockenwetter")
)

# Boxplot for all four components
boxplot_abflusskomponenten <- ggplot(abfluss_long, aes(x = Komponente, y = Wert)) +
  geom_boxplot(fill = "grey", color = "black", width = 0.5) +
  labs(
    title = "Abflusskomponenten der Einzugsgebiete",
    x = NULL,
    y = "Abfluss (L/s)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Save figure
ggsave("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/boxplot_flows.pdf",
       plot = boxplot_abflusskomponenten, width = 7, height = 5, dpi = 150)



# Scatters pop/flow ####
# Determine common y-axis limits
y_min <- min(
  Grundlagendaten$qh_L_s,
  Grundlagendaten$qg_L_s,
  Grundlagendaten$qf_L_s,
  na.rm = TRUE
)

y_max <- max(
  Grundlagendaten$qh_L_s,
  Grundlagendaten$qg_L_s,
  Grundlagendaten$qf_L_s,
  na.rm = TRUE
)

# Create the three scatterplots with identical y-axes and font settings
p_qh_pop <- ggplot(Grundlagendaten, aes(x = pop / 1000, y = qh_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(
    title = "Häusliches Schmutzwasser",
    x = "Einwohner (in Tausend)",
    y = "Wasserabfluss (L/s)"
  ) +
  custom_theme +
  ylim(y_min, y_max)

p_qg_pop <- ggplot(Grundlagendaten, aes(x = pop / 1000, y = qg_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(
    title = "Gewerbliches Schmutzwasser",
    x = "Einwohner (in Tausend)",
    y = "Wasserabfluss (L/s)"
  ) +
  custom_theme +
  ylim(y_min, y_max)

p_qf_pop <- ggplot(Grundlagendaten, aes(x = pop / 1000, y = qf_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(
    title = "Fremdwasser",
    x = "Einwohner (in Tausend)",
    y = "Wasserabfluss (L/s)"
  ) +
  custom_theme +
  ylim(y_min, y_max)

# Combine plots in one row
scatterplots_pop_flow <- (p_qh_pop | p_qg_pop | p_qf_pop)

# Save figure
ggsave(
  "/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/scatter_pop_flow.pdf",
  plot = scatterplots_pop_flow,
  width = 16,
  height = 8,
  dpi = 150
)

# Scatters area/flow ####

# Find common y-axis range
y_max <- max(
  Grundlagendaten$qh_L_s,
  Grundlagendaten$qg_L_s,
  Grundlagendaten$qf_L_s,
  na.rm = TRUE
)

y_min <- min(
  Grundlagendaten$qh_L_s,
  Grundlagendaten$qg_L_s,
  Grundlagendaten$qf_L_s,
  na.rm = TRUE
)

# Plots with fixed y-axis limits
p_qh_area <- ggplot(Grundlagendaten, aes(x = area_ha, y = qh_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(title = "Häusliches Schmutzwasser", x = "Fläche (ha)",
       y = "Wasserabfluss (L/s)") +
  theme_minimal() +
  theme(panel.border = element_rect()) +
  ylim(y_min, y_max) +
  custom_theme

p_qg_area <- ggplot(Grundlagendaten, aes(x = area_ha, y = qg_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(title = "Gewerbliches Schmutzwasser", x = "Fläche (ha)",
       y = "Wasserabfluss (L/s)") +
  theme_minimal() +
  theme(panel.border = element_rect()) +
  ylim(y_min, y_max) +
  custom_theme

p_qf_area <- ggplot(Grundlagendaten, aes(x = area_ha, y = qf_L_s)) +
  geom_point(alpha = 0.3, size = 3) +
  labs(title = "Fremdwasser", x = "Fläche (ha)",
       y = "Wasserabfluss (L/s)") +
  theme_minimal() +
  theme(panel.border = element_rect()) +
  ylim(y_min, y_max) +
  custom_theme

# Combine plots with patchwork
scatterplots_area_flow <- (p_qh_area | p_qg_area | p_qf_area)

# Save figure
ggsave(
  "/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/scatter_area_flow.pdf",
  plot = scatterplots_area_flow, width = 16, height = 8, dpi = 150
)



# Boxplots Flow/Beckenart ####
# Extract acronym before the underscore in EZG
Grundlagendaten$acronym <- sub("_.*", "", Grundlagendaten$EZG)

# Order boxplots by acronym alphabetically (or by median, see note below)
Grundlagendaten$acronym <- factor(
  Grundlagendaten$acronym,
  levels = sort(unique(Grundlagendaten$acronym))
)

# Calculate sample size per acronym
n_labels <- Grundlagendaten %>%
  group_by(acronym) %>%
  summarise(n = n()) %>%
  mutate(label = paste0("n = ", n))

# Create boxplot with sample sizes below each box
boxplot_qt <- ggplot(Grundlagendaten, aes(x = acronym, y = qt_L_s)) +
  geom_boxplot(fill = "grey", color = "black") +
  # Add sample size labels below x-axis
  geom_text(
    data = n_labels,
    aes(x = acronym, y = min(Grundlagendaten$qt_L_s, na.rm = TRUE) - 0.05 * diff(range(Grundlagendaten$qt_L_s, na.rm = TRUE)),
        label = label),
    size = 3
  ) +
  labs(
    title = "Trockenwetterabfluss nach Art des Einzugsgebiet",
    x = "EZG Art",
    y = "Trockenwetterabfluss (L/s)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(),
    plot.margin = margin(t = 10, r = 10, b = 25, l = 10) # extra space for labels
  )

# Save plot
ggsave(
  "/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/boxplots_flow_nodetype.pdf",
  plot = boxplot_qt,
  width = 10, height = 6.5, dpi = 150
)


# Barplot rpop ####
# Extract number after underscore in EZG
Grundlagendaten$number <- sub(".*_", "", Grundlagendaten$EZG)
Grundlagendaten$number <- as.numeric(Grundlagendaten$number)

# Compute tick positions (every 5 units, rounded nicely)
min_num <- min(Grundlagendaten$number, na.rm = TRUE)
max_num <- max(Grundlagendaten$number, na.rm = TRUE)
breaks_seq <- c(2, seq(5, max_num, by = 5))

# Bar plot: numbers (EZG suffix) vs rpop
barplot_rpop <- ggplot(Grundlagendaten, aes(x = factor(number), y = rpop)) +
  geom_col(fill = "grey", color = "black") +
  scale_x_discrete(breaks = as.character(breaks_seq)) +
  labs(
    title = "Bevölkerungsanteile der Einzugsgebiete",
    x = "Einzugsgebiet Nummer",
    y = "Bevölkerungsanteil"
  ) +
  theme_minimal() +
  theme(panel.border = element_rect()) +
  ylim(0, 0.148)

# Save plot
ggsave("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/barplot_rpop.pdf",
       plot = barplot_rpop, width = 10, height = 6, dpi = 150)

