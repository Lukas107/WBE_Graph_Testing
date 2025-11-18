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

setwd("/Users/lukas/Documents/Unizeug/Bachelor_Git")

# Load functions
source("Code/functions.R")

# Define abbreviations for nodes ####
rename_df <- data.frame(
  old = c(
    "Direktabfluss zur KLA", "KAS DO-Rheinische Straße", "RRB Opel Werke",
    "RÜ 1.01 Kuhstraße", "RÜ 2.01 Landweg", "RÜ 4.01 Goethestraße",
    "RÜ 6.14 Hellerstraße", "RÜ 6.15 Kobbendelle", "RÜ 6.17 Kirchhörder Berg",
    "RÜ 6.19 Rahmkebach", "RÜ 6.26 Weingartenstraße", "RÜ DO-Aldinghoferstraße",
    "RÜ DO-Am Nocken", "RÜ DO-Dellwiger Straße", "RÜ DO-In der Meile",
    "RÜ Emscherpark", "RÜ Entenpoth II", "RÜ Himpendahlweg",
    "RÜ Lohbach, Ruhfusstr.", "RÜ Selzerstr II Heimatbach",
    "RÜ Selzerstraße, Lohbach", "RÜ Seydlitzstr/Emscherpromenade",
    "RÜ Spissenagelstr", "RÜ Teich Pferdebach", "RÜ Zillestraße",
    "RÜB 1.02 Holzwickeder Straße", "RÜB 2.02 Hauptstraße",
    "RÜB 6.15 Witten-Rüdinghausen", "RÜB DO-Galoppstraße",
    "RÜB DO-Huckarder Straße", "RÜB DO-Lindberghstraße",
    "RÜB DO-Westfaliastraße", "RÜB HOW-Gartenstraße", "RÜB Süggelrandweg",
    "SK Baroper Straße", "SKO DO-Haferkampstraße", "SKO DO-Sydowstraße",
    "SKO Gottlieb Daimlerstr 1400", "SKO Gottlieb Daimlerstr 2000",
    "SKO WIT-Mühlenstraße", "SKU Brennaborstraße", "SKU DO-Altenrathstraße",
    "SKU DO-Am Lohbach", "SKU DO-Am Mühlenberg", "SKU DO-Am Talenberg",
    "SKU DO-Blümingstraße", "SKU DO-Froschlake", "SKU DO-Großholthauser Straße",
    "SKU DO-Heiduferweg", "SKU DO-Höfkerstraße", "SKU DO-Kruckeler Straße",
    "SKU DO-Lindstraße", "SKU DO-Martener Straße",
    "SKU DO-Menglinghauser Straße", "SKU DO-Ostenbergstraße",
    "SKU DO-Röhrenstraße", "SKU DO-Seekante", "SKU DO-Somborner Straße",
    "SKU DO-Steinklippenweg", "SKU DO-Strickerstraße", "SKU DO-Sudkamp",
    "SKU DO-Uferstraße", "SKU DO-Vieselerhofstraße", "SKU DO-Wörthstraße",
    "SKU WIT-Siemensstraße", "SKU WIT-Weizenkamp"
  ),
  new = c(
    "KLA_1", "KAS_2", "RRB_3", "RÜ_4", "RÜ_5", "RÜ_6", "RÜ_7", "RÜ_8", "RÜ_9",
    "RÜ_10", "RÜ_11", "RÜ_12", "RÜ_13", "RÜ_14", "RÜ_15", "RÜ_16", "RÜ_17",
    "RÜ_18", "RÜ_19", "RÜ_20", "RÜ_21", "RÜ_22", "RÜ_23", "RÜ_24", "RÜ_25",
    "RÜB_26", "RÜB_27", "RÜB_28", "RÜB_29", "RÜB_30", "RÜB_31", "RÜB_32",
    "RÜB_33", "RÜB_34", "SK_35", "SKO_36", "SKO_37", "SKO_38", "SKO_39",
    "SKO_40", "SKU_41", "SKU_42", "SKU_43", "SKU_44", "SKU_45", "SKU_46",
    "SKU_47", "SKU_48", "SKU_49", "SKU_50", "SKU_51", "SKU_52", "SKU_53",
    "SKU_54", "SKU_55", "SKU_56", "SKU_57", "SKU_58", "SKU_59", "SKU_60",
    "SKU_61", "SKU_62", "SKU_63", "SKU_64", "SKU_65", "SKU_66"
  ),
  stringsAsFactors = FALSE
)

rename_map <- setNames(rename_df$new, rename_df$old)
renamer <- function(x) {
  x <- trimws(as.character(x))
  mapped <- rename_map[x]
  ifelse(is.na(mapped), x, mapped)
}

xtable(rename_df, label = "Kürzel_Table",
       caption = "EZG Namen mit den in diesem Bericht verwendeten Kürzeln")

# Load and prepare data ####

# Read + clean Grundlagendaten
Grundlagendaten <- read_excel("Code/KA-DO-Deusen_Grundlagendaten.xlsx") %>%
  as.data.frame()

Grundlagendaten$EZG <- renamer(Grundlagendaten$EZG)

Grundlagendaten$pop  <- as.numeric(Grundlagendaten$pop)
Grundlagendaten$rpop <- Grundlagendaten$pop / sum(Grundlagendaten$pop, na.rm = TRUE)

# Will be used later in create_tree()
rpop_map <- setNames(Grundlagendaten$rpop, Grundlagendaten$EZG)

# Load network data
netzwerk_raw <- read_xlsx("Code/KA-DO-Deusen_Netzwerk.xlsx", col_names = FALSE)

# Extract and abbreviate labels
col_labels <- unlist(netzwerk_raw[1, -1]) %>% 
  as.character() %>% 
  renamer()
row_labels <- unlist(netzwerk_raw[-1, 1]) %>% 
  as.character() %>% 
  renamer()

# Build adjacency matrix with abbreviated dimnames
adj_matrix <- as.matrix(netzwerk_raw[-1, -1])
mode(adj_matrix) <- "numeric"
adj_matrix <- t(adj_matrix)
dimnames(adj_matrix) <- list(col_labels, row_labels)

# Row/col names should match
if (!identical(sort(unique(col_labels)), sort(unique(row_labels)))) {
  warning("Row/column label sets differ after renaming.")
}


# Create tree plots ####

tree = set_up_tree()
tree_cum_pop = set_up_tree()

png("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/sewage_plot.png",
    width = 2000, height = 2000, res = 300)

# style edges on the tree (data.tree styling still works)
SetEdgeStyle(tree, arrowhead = "vee", dir = "back")
SetEdgeStyle(tree_cum_pop, arrowhead = "vee", dir = "back")

# Where the plots are saved
out_path_tree = paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/",
                       "Plots/tree_plots/tree_plot.png")
out_path_tree_cum_pop = paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/",
                               "Plots/tree_plots/tree_plot_cum_pop.png")

# convert the tree to a DiagrammeR graph and export
# Add rpop to node labels
SetNodeStyle(tree_cum_pop, label = function(node) {
  if (!is.null(node$rpop)) {
    paste0(node$name, "\n", round(node$cum_pop, 3))
  } else {
    node$name
  }
})


# Then convert to DiagrammeR graph and export
tree_diagrammergraph = ToDiagrammeRGraph(tree)
tree_rpop_diagrammergraph = ToDiagrammeRGraph(tree_cum_pop)

DiagrammeR::export_graph(
  graph = tree_diagrammergraph,
  file_name = out_path_tree,
  file_type = "png",
  width = 3000,
  height = 2000
)

DiagrammeR::export_graph(
  graph = tree_rpop_diagrammergraph,
  file_name = out_path_tree_cum_pop,
  file_type = "png",
  width = 3000,
  height = 2000
)

# Population + area boxplots ####
# Precompute means
mean_pop_k   <- mean(Grundlagendaten$pop / 1000, na.rm = TRUE)
mean_area_ha <- mean(Grundlagendaten$area_ha,     na.rm = TRUE)

# Boxplot: population (in thousands)
boxplot_pop <- ggplot(Grundlagendaten, aes(x = factor(""), y = pop / 1000)) +
  geom_boxplot(fill = "grey", color = "black", width = 0.5) +
  geom_hline(yintercept = mean_pop_k, color = "red", linewidth = 1) +
  labs(
    title = "Einwohner der Einzugsgebiete",
    x = NULL, y = "Einwohner (in Tausend)"
  ) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Boxplot: area (ha)
boxplot_area <- ggplot(Grundlagendaten, aes(x = factor(""), y = area_ha)) +
  geom_boxplot(fill = "grey", color = "black", width = 0.5) +
  geom_hline(yintercept = mean_area_ha, color = "red", linewidth = 1) +
  labs(
    title = "Größe der Einzugsgebiete",
    x = NULL, y = "Fläche (ha)"
  ) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Arrange side-by-side with patchwork
boxplots_pop_area <- boxplot_pop | boxplot_area

# Save combined figure
ggsave(
  "/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/boxplots_pop_area.pdf",
  plot = boxplots_pop_area, width = 12, height = 5, dpi = 150
)


# Scatter: Area/Pop####
scatter_area_pop <- ggplot(Grundlagendaten, aes(x = area_ha, y = pop / 1000)) +
  geom_point(alpha=0.3, size = 2.5) +
  labs(title = "Verhältnis Einwohnerzahl und Fläche",
       x = "Fläche der Einzugsgebiete (ha)", y = "Einwohner (in Tausend)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA,
                                    linewidth = 0.8))

# Save scatterplot
ggsave("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/scatter_area_pop.pdf",
       plot = scatter_area_pop, width = 7, height = 5, dpi = 150)

# Calculate strat diagnostics ####

# Compute PMFs for each tester count 1:18
nary_split_pmfs <- lapply(1:18, function(n) {
  get_strat_pmf(nary_split, number_testers = n)
})
max_cum_pop_pmfs <- lapply(1:18, function(n) {
  get_strat_pmf(max_cum_pop, number_testers = n)
})
max_rpop_pmfs <- lapply(1:18, function(n) {
  get_strat_pmf(max_rpop, number_testers = n)
})
skipping_cum_pop_pmfs <- lapply(1:18, function(n) {
  get_strat_pmf(skipping_cum_pop, number_testers = n)
})

# Compute corresponding CDFs
nary_split_cdfs <- lapply(nary_split_pmfs, cumsum)
max_cum_pop_cdfs <- lapply(max_cum_pop_pmfs, cumsum)
max_rpop_cdfs <- lapply(max_rpop_pmfs, cumsum)
skipping_cum_pop_cdfs <- lapply(skipping_cum_pop_pmfs, cumsum)

# Compute average total tests for each tester count 1:18
nary_split_total_tests <- sapply(1:18, function(n) {
  average_total_tests(nary_split, number_testers=n)
})
max_cum_pop_total_tests <- sapply(1:18, function(n) {
  average_total_tests(max_cum_pop, number_testers=n)
})
max_rpop_total_tests <- sapply(1:18, function(n) {
  average_total_tests(max_rpop, number_testers=n)
})
skipping_cum_pop_total_tests <- sapply(1:18, function(n) {
  average_total_tests(skipping_cum_pop, number_testers=n)
})




# Make barplots for strat pdfs ####

# Convert vectors to data frames with x = 1:19
pmf_barplot <- function(pmf, ymax) {
  df <- data.frame(x = seq_along(pmf), value = as.numeric(pmf))
  
  ggplot(df, aes(x = factor(x), y = value)) +
    geom_col(fill = "grey", color = "black") +
    labs(
      title = "Strategie",
      x = "Benötigte Tests",
      y = "Wahrscheinlichkeit"
    ) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA)) +
    coord_cartesian(ylim = c(0, ymax))
}


# This is used to set the upper limit for all y-axes
ymax_pmf <- max(
  c(nary_split_pmfs[[1]], max_cum_pop_pmfs[[1]], max_rpop_pmfs[[1]])
)

# Barplot 1: binary split
plot_binary <- pmf_barplot(nary_split_pmfs[[1]], ymax_pmf)

# Barplot 2: kumulierte Bevölkerung
plot_cum_pop <-pmf_barplot(max_cum_pop_pmfs[[1]], ymax_pmf)

# Barplot 3: rpop
plot_rpop <- pmf_barplot(max_rpop_pmfs[[1]], ymax_pmf)

# Arrange the three barplots vertically
strat_barplots <- plot_binary / plot_cum_pop / plot_rpop

# Save combined figure
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
    "strategy_distributions/one_tester/strat_distributions_barplots.pdf"),
  plot = strat_barplots,
  width = 8, height = 10, dpi = 150
)


# Make CDF plots for strat distributions ####

# Helper for consistent styling
cdf_plot <- function(cdf, title_txt) {
  df = data.frame(x = 1:19, value = cdf)
  
  ggplot(df, aes(x = x, y = value)) +
    geom_step(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = 1:19) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = title_txt,
      x = "Benötigte Tests",
      y = "Kumulative Wahrscheinlichkeit"
    ) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA))
}

# CDF plots 1 tester
plot_binary_cdf <- cdf_plot(nary_split_cdfs[[1]], "Binary Split – CDF")
plot_cum_pop_cdf <- cdf_plot(max_cum_pop_cdfs[[1]], "cum_pop – CDF")
plot_rpop_cdf   <- cdf_plot(max_rpop_cdfs[[1]],   "Rpop – CDF")

# Arrange the three CDF plots vertically
strat_cdf_plots <- plot_binary_cdf / plot_cum_pop_cdf / plot_rpop_cdf

# Save combined figure
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/one_tester/strat_distributions_cdf.pdf"),
       plot = strat_cdf_plots, width = 8, height = 10, dpi = 150)

# Compare Strat CDFs 1 Tester ####

cdf_plot_compare <- function(cdfs, labels, title_txt) {
  long_df <- do.call(rbind, Map(function(cdf, label) {
    data.frame(
      x = seq_along(cdf),
      value = as.numeric(cdf),
      Strategy = label
    )
  }, cdfs, labels))
  
  x_max <- max(long_df$x)
  cols  <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
  
  ggplot(long_df, aes(x = x, y = value, color = Strategy)) +
    geom_step(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq_len(x_max)) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(values = cols[seq_along(unique(long_df$Strategy))]) +
    labs(
      title = title_txt,
      x = "Benötigte Testdurchläufe",
      y = "Kumulative Wahrscheinlichkeit",
      color = "Strategie"
    ) +
    theme_minimal() +
    theme(
      panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


# Nary Split vs Max_cum_pop
plot_cdfs <- cdf_plot_compare(
  cdfs   = list(nary_split_cdfs[[1]], max_cum_pop_cdfs[[1]]),
  labels = c("Nary Split", "Max_cum_pop"),
  title_txt = "Verteilungsfunktion bei einem Probennehmer"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/",
              "Plots/strategy_distributions/one_tester/nary_cum_pop_cdf.pdf"),
       plot = plot_cdfs, width = 8, height = 6, dpi = 150)

# Nary Split vs Rpop
plot_cdf_bin_rpop <- cdf_plot_compare(
  cdfs   = list(nary_split_cdfs[[1]], max_rpop_cdfs[[1]]),
  labels = c("Nary Split", "Rpop"),
  title_txt = "Verteilungsfunktion bei einem Probennehmer"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/one_tester/nary_rpop_cdf.pdf"),
  plot = plot_cdf_bin_rpop, width = 8, height = 6, dpi = 150)

# Max_um_pop vs Rpop
plot_cdf_cum_pop_rpop <- cdf_plot_compare(
  cdfs   = list(max_cum_pop_cdfs[[1]], max_rpop_cdfs[[1]]),
  labels = c("Max_cum_pop", "Rpop"),
  title_txt = "Verteilungsfunktion bei einem Probennehmer"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/",
              "Plots/strategy_distributions/one_tester/cum_pop_rpop_cdf.pdf"),
  plot = plot_cdf_cum_pop_rpop, width = 8, height = 6, dpi = 150)

# Compare Strat CDFs 3 Testers ####

# Nary Split vs Max_cum_pop
plot_nary_max_cum <- cdf_plot_compare(
  cdfs   = list(nary_split_cdfs[[3]], max_cum_pop_cdfs[[3]]),
  labels = c("Nary Split", "Max_cum_pop"),
  title_txt = "Verteilungsfunktion bei drei Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/three_testers/nary_max_cum.png"),
       plot = plot_nary_max_cum, width = 8, height = 6, dpi = 150)

# Nary Split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs   = list(nary_split_cdfs[[3]], max_rpop_cdfs[[3]]),
  labels = c("Nary Split", "Rpop"),
  title_txt = "Verteilungsfunktion bei drei Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/three_testers/nary_rpop.png"),
       plot = plot_nary_rpop, width = 8, height = 6, dpi = 150)

# Nary Split vs Skipping_cum_pop
plot_nary_skipping <- cdf_plot_compare(
  cdfs   = list(nary_split_cdfs[[3]], skipping_cum_pop_cdfs[[3]]),
  labels = c("Nary", "Skipping_cum_pop"),
  title_txt = "Verteilungsfunktion bei drei Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
        "strategy_distributions/three_testers/nary_skipping.png"),
       plot = plot_nary_skipping, width = 8, height = 6, dpi = 150)

# Compare Strat CDFs 5 Testers ####

# Nary Split vs Max_cum_pop
plot_nary_max_cum <- cdf_plot_compare(
  cdfs   = list(max_rpop_cdfs[[5]], nary_split_cdfs[[5]]),
  labels = c("Rpop", "Nary Split"),
  title_txt = "Verteilungsfunktion bei fünf Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/five_testers/rpop_nary.png"),
       plot = plot_nary_max_cum, width = 8, height = 6, dpi = 150)

# Nary Split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs   = list(max_rpop_cdfs[[5]], max_cum_pop_cdfs[[5]]),
  labels = c("Rpop", "Max_cum_pop"),
  title_txt = "Verteilungsfunktion bei fünf Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/five_testers/rpop_max_cum.png"),
       plot = plot_nary_rpop, width = 8, height = 6, dpi = 150)

# Nary Split vs Skipping_cum_pop
plot_nary_skipping <- cdf_plot_compare(
  cdfs   = list(max_rpop_cdfs[[5]], skipping_cum_pop_cdfs[[5]]),
  labels = c("Rpop", "Skipping_cum_pop"),
  title_txt = "Verteilungsfunktion bei fünf Probennehmern"
)
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/five_testers/rpop_skipping.png"),
       plot = plot_nary_skipping, width = 8, height = 6, dpi = 150)

# Average test iterations by number_testers ####

# Compute mean tests for each strategy and number of testers (1–10)
means_df <- data.frame(
  number_testers = rep(1:10, 4),
  strategy = rep(c("nary_split", "max_cum_pop", "max_rpop", "skipping_cum_pop"),
                 each = 10),
  mean_tests = c(
    sapply(nary_split_pmfs[1:10],       mean_tests),
    sapply(max_cum_pop_pmfs[1:10],      mean_tests),
    sapply(max_rpop_pmfs[1:10],         mean_tests),
    sapply(skipping_cum_pop_pmfs[1:10], mean_tests)
  )
)

# Set a factor to create desired order
means_df$strategy <- factor(
  means_df$strategy,
  levels = c("nary_split", "max_rpop", "skipping_cum_pop", "max_cum_pop")
)

# Shared y-axis limit
ymax_common <- ceiling(max(means_df$mean_tests))

# Combined barplot with HCL spectrum colours
p_combined <- ggplot(means_df, aes(x = factor(number_testers), y = mean_tests,
                                   fill = strategy)) +
  geom_col(position = position_dodge(width=0.8), color = "black",
           width = 0.7) +
  scale_fill_manual(values = hcl.colors(4, palette = "Set 2")) +
  labs(title = paste0("Mittlere Anzahl Testdurchläufe nach Strategie ",
                      "und Anzahl Probennehmer"),
       x = "Anzahl Probennehmer",
       y = "Mittlere Testdurchläufe",
       fill = "Strategie") +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.border = element_rect(color="black", fill=NA, linewidth=0.8),
    legend.position = "top"
  )

# Save the combined figure
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/barplot_mean_test_iterations.pdf"),
       plot = p_combined, width = 10, height = 6, dpi = 150)

# Average total tests by number testers ####

# Compute mean tests for each strategy and number of testers (1–10)
total_tests_df <- data.frame(
  number_testers = rep(1:10, 4),
  strategy = rep(c("nary_split", "max_cum_pop", "max_rpop", "skipping_cum_pop"),
                 each = 10),
  mean_tests = c(nary_split_total_tests[1:10],
                 max_cum_pop_total_tests[1:10],
                 max_rpop_total_tests[1:10],
                 skipping_cum_pop_total_tests[1:10]))

# Set a factor to create desired order
total_tests_df$strategy <- factor(
  means_df$strategy,
  levels = c("nary_split", "max_rpop", "skipping_cum_pop", "max_cum_pop")
)

# Shared y-axis limit
ymax_common <- ceiling(max(total_tests_df$mean_tests))

# Combined barplot with HCL spectrum colours
p_combined <- ggplot(total_tests_df, aes(x = factor(number_testers), y = mean_tests,
                                 fill = strategy)) +
  geom_col(position = position_dodge(width=0.8), color = "black",
           width = 0.7) +
  scale_fill_manual(values = hcl.colors(4, palette = "Set 2")) +
  labs(title = paste0("Mittlere Anzahl Tests nach Strategie",
                      "und Anzahl Probennehmer"),
       x = "Anzahl Probennehmer",
       y = "Mittlere Anzahl Tests",
       fill = "Strategie") +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.border = element_rect(color="black", fill=NA, linewidth=0.8),
        legend.position = "top"
  )

# Save the combined figure
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/barplot_mean_total_tests.pdf"),
       plot = p_combined, width = 10, height = 6, dpi = 150)

# Efficiency ratio for nary_split ####

# Prepare data
df_ns <- data.frame(
  number_testers = 1:10,
  mean_iterations = sapply(nary_split_pmfs[1:10], mean_tests),
  mean_total_tests = nary_split_total_tests[1:10]
)

# Compute deltas between consecutive numbers of testers
df_ns$delta_iterations <- c(NA, diff(df_ns$mean_iterations))
df_ns$delta_total_tests <- c(NA, diff(df_ns$mean_total_tests))

# Compute ratio: reduction in iterations per added total test
df_ns$efficiency_ratio <- -df_ns$delta_iterations / df_ns$delta_total_tests

# Remove the first row (no prior tester to compare)
df_eff <- df_ns[-1, ]

# Use absolute values so bars go upward even if ratio is negative
df_eff$efficiency_ratio_abs <- abs(df_eff$efficiency_ratio)

# Shared y-axis limit (optional)
ymax_common <- ceiling(max(df_eff$efficiency_ratio_abs, na.rm = TRUE))

# Barplot
p_eff <- ggplot(df_eff, aes(x = factor(number_testers), y = efficiency_ratio_abs)) +
  geom_col(color = "black", width = 0.7) +
  labs(
    title = "Reduktion Testdurchläufe pro zusätzlichem Test",
    x = "Anzahl Probennehmer",
    y = "-Diff Testdurchläufe / Diff Tests"
  ) +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Save the plot
ggsave(paste0("/Users/lukas/Documents/Unizeug/Bachelorarbeit/Plots/",
              "strategy_distributions/nary_split_efficiency_ratio.pdf"),
       plot = p_eff, width = 9, height = 5.5, dpi = 150)
