library(readxl)
library(data.tree)
library(dplyr)
library(DiagrammeR)
library(ggplot2)
library(patchwork)
library(xtable)

setwd("/Users/lukas/Documents/Unizeug/Bachelor_Git")
colour_palette = hcl.colors(4, palette = "Set 2")

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
tree_cum_rpop = set_up_tree()
tree_simplified = set_up_tree()

`%||%` <- function(a, b) if (is.null(a)) b else a

all_nodes <- Traverse(tree_cum_rpop, traversal = "pre-order")
cum_lookup <- setNames(
  vapply(all_nodes, function(n) n$cum_rpop %||% NA_real_, numeric(1)),
  vapply(all_nodes, function(n) n$name, character(1))
)

edits <- list(
  list(
    parent = "KLA_1",
    remove = c("RÜB_30", "RÜB_31", "RÜB_32",
               "SKO_37", "SKO_40",
               "SKU_46", "SKU_47", "SKU_50", "SKU_66"),
    add = "other9"
  ),
  list(
    parent = "SKU_59",
    remove = c("RÜ_12", "RÜ_17", "RÜ_18",
               "RÜ_22", "RÜ_24", "RÜ_25",
               "RÜB_29", "SKU_49"),
    add = "other8"
  )
)

for (e in edits) {
  parent <- FindNode(tree_simplified, e$parent)
  if (is.null(parent)) next
  
  sum_removed <- sum(cum_lookup[e$remove], na.rm = TRUE)
  
  # Remove selected children
  for (child in e$remove) {
    if (child %in% names(parent$children)) {
      parent$RemoveChild(child)
    }
  }
  
  # Add aggregated node
  other_node <- parent$AddChild(e$add)
  other_node$rpop <- sum_removed
}

RecalcCumRpop <- function(node) {
  if (!node$isLeaf) {
    for (ch in node$children) RecalcCumRpop(ch)
  }
  own  <- if (is.null(node$rpop) || is.na(node$rpop)) 0 else node$rpop
  kids <- if (node$isLeaf) 0 else sum(vapply(node$children, function(ch) ch$cum_rpop %||% 0, numeric(1)))
  node$cum_rpop <- own + kids
}
RecalcCumRpop(tree_simplified)


# style edges on the tree (data.tree styling still works)
SetEdgeStyle(tree, arrowhead = "vee", dir = "back")
SetEdgeStyle(tree_cum_rpop, arrowhead = "vee", dir = "back")
SetEdgeStyle(tree_simplified, arrowhead = "vee", dir = "back")

# convert the tree to a DiagrammeR graph and export
# Add rpop to node labels
SetNodeStyle(tree_cum_rpop, label = function(node) {
  if (!is.null(node$rpop)) {
    paste0(node$name, "\n", round(node$cum_rpop, 3))
  } else {
    node$name
  }
})

# Highlight the "otherx" nodes
other_nodes <- c("other8", "other9")

SetNodeStyle(
  tree_simplified,
  fontcolor = function(node) {
    if (node$name %in% other_nodes) "red" else "black"
  }
)

# Then convert to DiagrammeR graph and export
tree_diagrammergraph = ToDiagrammeRGraph(tree)
tree_rpop_diagrammergraph = ToDiagrammeRGraph(tree_cum_rpop)
tree_simplified_graph <- ToDiagrammeRGraph(tree_simplified)

tree_diagrammergraph <- add_global_graph_attrs(
  tree_diagrammergraph,
  attr = c("label", "labelloc", "labeljust", "fontsize"),
  value = c("Abwassernetz Graph", "t", "c", "60"),
  attr_type = rep("graph", 4)
)

tree_rpop_diagrammergraph <- add_global_graph_attrs(
  graph = tree_rpop_diagrammergraph,
  attr  = c("label", "labelloc", "labeljust", "fontsize"),
  value = c("Abwassernetz Graph", "t", "c", "60"),
  attr_type = rep("graph", 4)
)

tree_simplified_graph <- add_global_graph_attrs(
  graph = tree_simplified_graph,
  attr  = c("label", "labelloc", "labeljust", "fontsize"),
  value = c("Abwassernetz Graph", "t", "c", "60"),
  attr_type = rep("graph", 4)
)

DiagrammeR::export_graph(
  graph = tree_diagrammergraph,
  file_name = "Plots/tree_plots/tree_plot.png",
  file_type = "png",
  width = 6000,
  height = 4000
)

DiagrammeR::export_graph(
  graph = tree_rpop_diagrammergraph,
  file_name = "Plots/tree_plots/tree_plot_cum_rpop.png",
  file_type = "png",
  width = 6000,
  height = 6000
)
DiagrammeR::export_graph(
  graph = tree_simplified_graph,
  file_name = "Plots/tree_plots/tree_plot_simplified.png",
  file_type = "png",
  width = 3000,
  height = 3000
)


SetNodeStyle(
  tree_simplified,
  label = function(node) paste0(
    node$name, "\n",
    ifelse(is.na(node$cum_rpop), "NA", sprintf("%.3f", node$cum_rpop))
  ),
  fontcolor = function(node) {
    if (node$name %in% other_nodes) "red" else "black"
  }
)

tree_simplified_cum_graph <- ToDiagrammeRGraph(tree_simplified)

tree_simplified_cum_graph <- add_global_graph_attrs(
  graph = tree_simplified_cum_graph,
  attr  = c("label", "labelloc", "labeljust", "fontsize"),
  value = c("Abwassernetz Graph", "t", "c", "60"),
  attr_type = rep("graph", 4)
)

DiagrammeR::export_graph(
  graph = tree_simplified_cum_graph,
  file_name = "Plots/tree_plots/tree_plot_simplified_cum_rpop.png",
  file_type = "png",
  width = 3000,
  height = 3000
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
  "Plots/boxplots_pop_area.pdf",
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
ggsave("Plots/scatter_area_pop.pdf",
       plot = scatter_area_pop, width = 5, height = 3, dpi = 150)

# Calculate strat diagnostics ####

# This will typically take a long time

# Compute PMFs for each tester count 1:10
nary_testcycle_pmfs <- lapply(1:10, function(n) {
  strat_testcycle_pmf(nary_split, number_testers = n)
})
cum_testcycle_pmfs <- lapply(1:10, function(n) {
  strat_testcycle_pmf(max_cum_rpop, number_testers = n)
})
rpop_testcycle_pmfs <- lapply(1:10, function(n) {
  strat_testcycle_pmf(max_rpop, number_testers = n)
})
skipping_testcycle_pmfs <- lapply(1:10, function(n) {
  strat_testcycle_pmf(skipping_cum_rpop, number_testers = n)
})

# Compute corresponding CDFs
nary_testcycle_cdfs <- lapply(nary_testcycle_pmfs, cumsum)
cum_testcycle_cdfs <- lapply(cum_testcycle_pmfs, cumsum)
rpop_testcycle_cdfs <- lapply(rpop_testcycle_pmfs, cumsum)
skipping_testcycle_cdfs <- lapply(skipping_testcycle_pmfs, cumsum)

nary_mean_testcycles <- sapply(1:10, function(n) {
  pmf_to_mean(nary_testcycle_pmfs[[n]])
})
rpop_mean_testcycles <- sapply(1:10, function(n) {
  pmf_to_mean(rpop_testcycle_pmfs[[n]])
})
skipping_mean_testcycles <- sapply(1:10, function(n) {
  pmf_to_mean(skipping_testcycle_pmfs[[n]])
})
cum_mean_testcycles <- sapply(1:10, function(n) {
  pmf_to_mean(cum_testcycle_pmfs[[n]])
})

# Compute average total tests for each tester count 1:10
nary_test_pmfs <- lapply(1:10, function(n) {
  strat_total_tests_pmf(nary_split, number_testers = n)
})
rpop_test_pmfs <- lapply(1:10, function(n) {
  strat_total_tests_pmf(max_rpop, number_testers = n)
})
skipping_test_pmfs <- lapply(1:10, function(n) {
  strat_total_tests_pmf(skipping_cum_rpop, number_testers = n)
})
cum_test_pmfs <- lapply(1:10, function(n) {
  strat_total_tests_pmf(max_cum_rpop, number_testers = n)
})

# Compute corresponding CDFs for total tests
nary_test_cdfs     <- lapply(nary_test_pmfs, cumsum)
rpop_test_cdfs     <- lapply(rpop_test_pmfs, cumsum)
skipping_test_cdfs <- lapply(skipping_test_pmfs, cumsum)
cum_test_cdfs      <- lapply(cum_test_pmfs, cumsum)

nary_mean_tests <- sapply(1:10, function(n) {
  pmf_to_mean(nary_test_pmfs[[n]])
})
rpop_mean_tests <- sapply(1:10, function(n) {
  pmf_to_mean(rpop_test_pmfs[[n]])
})
skipping_mean_tests <- sapply(1:10, function(n) {
  pmf_to_mean(skipping_test_pmfs[[n]])
})
cum_mean_tests <- sapply(1:10, function(n) {
  pmf_to_mean(cum_test_pmfs[[n]])
})




# Make barplots for strat pdfs ####

# Convert vectors to data frames with x = 1:19
pmf_barplot <- function(pmf, ymax, title) {
  df <- data.frame(x = seq_along(pmf), value = as.numeric(pmf))
  
  ggplot(df, aes(x = factor(x), y = value)) +
    geom_col(fill = "grey", color = "black") +
    labs(
      title = title,
      x = "Benötigte Tests",
      y = "Wahrscheinlichkeit"
    ) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA)) +
    coord_cartesian(ylim = c(0, ymax))
}


# This is used to set the upper limit for all y-axes
ymax_pmf <- max(
  c(nary_testcycle_pmfs[[1]], cum_testcycle_pmfs[[1]], rpop_testcycle_pmfs[[1]])
)

# Barplot 1: nary split
plot_binary <- pmf_barplot(nary_testcycle_pmfs[[1]], ymax_pmf,
                           title="Nary-split Strategie")

# Barplot 2: kumulierte Bevölkerung
plot_cum_rpop <-pmf_barplot(cum_testcycle_pmfs[[1]], ymax_pmf,
                            title="Max-cum-rpop Strategie")

# Barplot 3: rpop
plot_rpop <- pmf_barplot(rpop_testcycle_pmfs[[1]], ymax_pmf,
                         title="Max-rpop Strategie")

# Arrange the three barplots vertically
strat_barplots <- plot_binary / plot_cum_rpop / plot_rpop

# Save combined figure
ggsave(
  "Plots/strategy_distributions/one_tester/strat_distributions_barplots.pdf",
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
    labs(title=title_txt, x="Benötigte Tests",
         y="Kumulative Wahrscheinlichkeit") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA))
}

# CDF plots 1 tester
plot_binary_cdf <- cdf_plot(nary_testcycle_cdfs[[1]], "Nary-split Strategie")
plot_cum_rpop_cdf <- cdf_plot(cum_testcycle_cdfs[[1]], "Max-cum-rpop Strategie")
plot_rpop_cdf   <- cdf_plot(rpop_testcycle_cdfs[[1]],   "Max-rpop Strategie")

# Arrange the three CDF plots vertically
strat_cdf_plots <- plot_binary_cdf / plot_cum_rpop_cdf / plot_rpop_cdf

# Save combined figure
ggsave("Plots/strategy_distributions/one_tester/strat_distributions_cdf.pdf",
       plot = strat_cdf_plots, width = 8, height = 10, dpi = 150)

# Compare strat testcycle CDFs 1 tester ####

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
    scale_color_manual(
      values = cols[seq_along(unique(long_df$Strategy))],
      breaks = labels
    ) +
    labs(
      title = title_txt,
      x = "Benötigte Testzyklen",
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

# Nary Split vs Rpop
plot_cdf_bin_rpop <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[1]], rpop_testcycle_cdfs[[1]]),
  labels = c("Nary-split", "Max-rpop"),
  title_txt = "Verteilungsfunktionen bei einem Probennehmer"
)

# Nary Split vs Max_cum_rpop
plot_cdf_bin_cum_rpop <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[1]], cum_testcycle_cdfs[[1]]),
  labels = c("Nary-split", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei einem Probennehmer"
)


# Save plots
ggsave("Plots/strategy_distributions/one_tester/nary_rpop_one.pdf",
       plot = plot_cdf_bin_rpop, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/one_tester/nary_cum_one.pdf",
       plot = plot_cdf_bin_cum_rpop, width = 8, height = 4, dpi = 150)

# Compare strat testcycle CDFs 3 testers ####

# Nary Split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[3]][1:9], rpop_testcycle_cdfs[[3]][1:9]),
  labels = c("Nary-split", "Max-rpop"),
  title_txt = "Verteilungsfunktionen bei drei Probennehmern"
)

# Nary Split vs Skipping_cum_rpop
plot_nary_skipping <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[3]][1:9], skipping_testcycle_cdfs[[3]][1:9]),
  labels = c("Nary-split", "Skipping-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei drei Probennehmern"
)

# Nary Split vs Max_cum_rpop
plot_nary_cum <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[3]][1:9], cum_testcycle_cdfs[[3]][1:9]),
  labels = c("Nary-split", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei drei Probennehmern"
)

# Skipping-cum-rpop vs Max_cum_rpop
plot_skipping_cum <- cdf_plot_compare(
  cdfs   = list(skipping_testcycle_cdfs[[3]][1:9], cum_testcycle_cdfs[[3]][1:9]),
  labels = c("Skipping-cum-rpop", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei drei Probennehmern"
)

# Save plots
ggsave("Plots/strategy_distributions/three_testers/nary_rpop_three.pdf",
       plot = plot_nary_rpop, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/three_testers/nary_skipping_three.pdf",
       plot = plot_nary_skipping, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/three_testers/nary_cum_three.pdf",
       plot = plot_nary_cum, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/three_testers/skipping_cum_three.pdf",
       plot = plot_skipping_cum, width = 8, height = 4, dpi = 150)

# Compare strat testcycle CDFs 7 testers ####
# Nary-split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[7]][1:4], rpop_testcycle_cdfs[[7]][1:4]),
  labels = c("Nary-split", "Max_rpop"),
  title_txt = "Verteilungsfunktionen bei sieben Probennehmern"
)
# Nary-split vs Skipping-cum-rpop
plot_nary_skipping <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[7]][1:4], skipping_testcycle_cdfs[[7]][1:4]),
  labels = c("Nary-split", "Skipping-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei sieben Probennehmern"
)
# Nary Split vs Max-cum-rpop
plot_nary_cum <- cdf_plot_compare(
  cdfs   = list(nary_testcycle_cdfs[[7]][1:4], cum_testcycle_cdfs[[7]][1:4]),
  labels = c("Nary-split", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen bei sieben Probennehmern"
)

# Save plots
ggsave("Plots/strategy_distributions/seven_testers/nary_rpop_seven.pdf",
       plot = plot_nary_rpop, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/seven_testers/nary_skipping_seven.pdf",
       plot = plot_nary_skipping, width = 8, height = 4, dpi = 150)
ggsave("Plots/strategy_distributions/seven_testers/nary_cum_seven.pdf",
       plot = plot_nary_cum, width = 8, height = 4, dpi = 150)

# Compare strat total tests CDFs 3 testers ####

# Nary Split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[3]][1:25], rpop_test_cdfs[[3]][1:25]),
  labels    = c("Nary-split", "Max-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei drei Probennehmern"
)

# Nary Split vs Skipping-cum-rpop
plot_nary_skipping <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[3]][1:25], skipping_test_cdfs[[3]][1:25]),
  labels    = c("Nary-split", "Skipping-cum-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei drei Probennehmern"
)

# Nary Split vs Max-cum-rpop
plot_nary_cum <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[3]][1:25], cum_test_cdfs[[3]][1:25]),
  labels    = c("Nary-split", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei drei Probennehmern"
)

# Save plots
ggsave("Plots/strategy_distributions/three_testers/nary_rpop_three_total.pdf",
       plot = plot_nary_rpop, width = 8, height = 4)
ggsave("Plots/strategy_distributions/three_testers/nary_skipping_three_total.pdf",
       plot = plot_nary_skipping, width = 8, height = 4)
ggsave("Plots/strategy_distributions/three_testers/nary_cum_three_total.pdf",
       plot = plot_nary_cum, width = 8, height = 4)

# Compare strat total tests CDFs 7 testers ####

# Nary Split vs Rpop
plot_nary_rpop <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[7]][7:28], rpop_test_cdfs[[7]][7:28]),
  labels    = c("Nary-split", "Max-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei sieben Probennehmern"
)

# Nary Split vs Skipping-cum-rpop
plot_nary_skipping <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[7]][7:28], skipping_test_cdfs[[7]][7:28]),
  labels    = c("Nary-split", "Skipping-cum-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei sieben Probennehmern"
)

# Nary Split vs Max-cum-rpop
plot_nary_cum <- cdf_plot_compare(
  cdfs      = list(nary_test_cdfs[[7]][7:28], cum_test_cdfs[[7]][7:28]),
  labels    = c("Nary-split", "Max-cum-rpop"),
  title_txt = "Verteilungsfunktionen (Gesamtzahl Tests) bei sieben Probennehmern"
)

# Save plots
ggsave("Plots/strategy_distributions/seven_testers/nary_rpop_seven_total.pdf",
       plot = plot_nary_rpop, width = 8, height = 4)
ggsave("Plots/strategy_distributions/seven_testers/nary_skipping_seven_total.pdf",
       plot = plot_nary_skipping, width = 8, height = 4)
ggsave("Plots/strategy_distributions/seven_testers/nary_cum_seven_total.pdf",
       plot = plot_nary_cum, width = 8, height = 4)

# Average test iterations by number_testers ####

# Compute mean tests for each strategy and number of testers (1–10)
means_df <- data.frame(
  number_testers = rep(1:10, 4),
  strategy = rep(c("nary-split", "max-cum-rpop", "max-rpop", "skipping-cum-rpop"),
                 each = 10),
  mean_test_col = c(nary_mean_testcycles[1:10], rpop_mean_testcycles[1:10],
                    skipping_mean_testcycles[1:10], cum_mean_testcycles[1:10])
)

# Set a factor to create desired order
means_df$strategy <- factor(
  means_df$strategy,
  levels = c("nary-split", "max-rpop", "skipping-cum-rpop", "max-cum-rpop")
)

# Shared y-axis limit
ymax_common <- ceiling(max(means_df$mean_test_col))

# Combined barplot with HCL spectrum colours
p_combined <- ggplot(means_df, aes(x = factor(number_testers), y = mean_test_col,
                                   fill = strategy)) +
  geom_col(position = position_dodge(width=0.8), color = "black",
           width = 0.7) +
  scale_fill_manual(values = colour_palette) +
  labs(title = "Mittlere Anzahl Testdurchläufe",
       x = "Anzahl Probennehmer",
       y = "Mittlere Testdurchläufe",
       fill = "Strategie") +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 19),
    
    # Tick labels
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
    
    # Axis titles
    axis.title = element_text(size = 17),
    
    # Legend text + title
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "top"
  )

# Save the combined figure
ggsave("Plots/strategy_distributions/barplot_mean_test_iterations.pdf",
       plot = p_combined, width = 10, height = 6, dpi = 150)

# Average total tests by number testers ####

# Compute mean tests for each strategy and number of testers (1–10)
total_tests_df <- data.frame(
  number_testers = rep(1:10, 4),
  strategy = rep(c("Nary-split", "Max-cum-rpop", "Max-rpop", "Skipping-cum-rpop"),
                 each = 10),
  mean_test_col = c(nary_mean_tests[1:10],
                 cum_mean_tests[1:10],
                 rpop_mean_tests[1:10],
                 skipping_mean_tests[1:10]))

# Set a factor to create desired order
total_tests_df$strategy <- factor(
  total_tests_df$strategy,
  levels = c("Nary-split", "Max-rpop", "Skipping-cum-rpop", "Max-cum-rpop")
)

# Shared y-axis limit
ymax_common <- ceiling(max(total_tests_df$mean_test_col))

# Combined barplot with HCL spectrum colours
p_combined <- ggplot(total_tests_df, aes(x = factor(number_testers), y = mean_test_col,
                                 fill = strategy)) +
  geom_col(position = position_dodge(width=0.8), color = "black",
           width = 0.7) +
  scale_fill_manual(values = hcl.colors(4, palette = "Set 2")) +
  labs(title = paste0("Mittlere Anzahl Tests"),
       x = "Anzahl Probennehmer",
       y = "Mittlere Anzahl Tests",
       fill = "Strategie") +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=19),
        # Tick labels
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
        
        # Axis titles
        axis.title = element_text(size = 17),
        
        # Legend text + title
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        panel.border = element_rect(color="black", fill=NA, linewidth=0.8),
        legend.position = "top"
  )

# Save the combined figure
ggsave("Plots/strategy_distributions/barplot_mean_total_tests.pdf",
       plot = p_combined, width = 10, height = 6, dpi = 150)

# Efficiency ratio for Nary-split ####

# Prepare data
df_ns <- data.frame(
  number_testers = 1:10,
  mean_iterations = sapply(nary_testcycle_pmfs[1:10], pmf_to_mean),
  mean_total_tests = nary_mean_tests[1:10]
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
ggsave("Plots/strategy_distributions/nary_split_efficiency_ratio.pdf",
       plot = p_eff, width = 9, height = 5.5, dpi = 150)

# Efficiency ratio vs. 1 tester Nary-split ####

# Prepare data
efficiency_df<- data.frame(
  number_testers   = 1:10,
  mean_iterations  = sapply(nary_testcycle_pmfs[1:10], pmf_to_mean),
  mean_total_tests = nary_mean_tests[1:10]
)

# Baseline for 1 tester
baseline_iter  <- efficiency_df$mean_iterations[efficiency_df
                                              $number_testers == 1]
baseline_tests <- efficiency_df$mean_total_tests[efficiency_df
                                                $number_testers == 1]

# Deltas vs. 1 tester
efficiency_df$delta_iterations_vs1  <- efficiency_df$mean_iterations  - baseline_iter
efficiency_df$delta_total_tests_vs1 <- efficiency_df$mean_total_tests - baseline_tests

# Ratio: reduction in iterations per *additional* total test vs. 1 tester
efficiency_df$efficiency_ratio_vs1 <- -efficiency_df$delta_iterations_vs1 / efficiency_df$delta_total_tests_vs1

# Drop the 1-tester row (0/0) and any non-finite ratios
df_eff_vs1 <- subset(efficiency_df
                    , number_testers != 1 & is.finite(efficiency_ratio_vs1))

# Use absolute values so bars go upward even if ratio is negative
df_eff_vs1$efficiency_ratio_vs1_abs <- abs(df_eff_vs1$efficiency_ratio_vs1)

# Shared y-axis limit (either recompute, or reuse your old ymax_common)
ymax_common_vs1 <- ceiling(max(df_eff_vs1$efficiency_ratio_vs1_abs, na.rm = TRUE))

# Barplot
p_eff_vs1 <- ggplot(df_eff_vs1,
                    aes(x = factor(number_testers), y = efficiency_ratio_vs1_abs)) +
  geom_col(color = "black", width = 0.7) +
  labs(
    title = "Reduktion Testdurchläufe pro zusätzlichem Test beim Nary-split_1",
    x = "Anzahl Probennehmer",
    y = expression("Effizienz verglichen mit " ~
        Nary - split[1])
  ) +
  ylim(0, ymax_common_vs1) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Save the plot
ggsave("Plots/strategy_distributions/nary_split_efficiency_ratio_vs1.pdf",
       plot = p_eff_vs1, width = 9, height = 4.5, dpi = 150)

# Efficiency ratio vs. Nary-split with 1 tester (for Nary-split AND Max-rpop) ####
# Efficiency ratio vs. Nary-split with 1 tester (Nary-split & Max-rpop) ####

# Predefined palette
colour_palette <- hcl.colors(4, palette = "Set 2")

# Baseline (Nary-split with 1 tester)
baseline_iter  <- nary_mean_testcycles[1]
baseline_tests <- nary_mean_tests[1]

# Strategy summaries
df_nary <- data.frame(
  strategy        = "Nary-split",
  number_testers  = 1:10,
  mean_iterations = nary_mean_testcycles,
  mean_total_tests= nary_mean_tests
)

df_rpop <- data.frame(
  strategy        = "Max-rpop",
  number_testers  = 1:10,
  mean_iterations = rpop_mean_testcycles,
  mean_total_tests= rpop_mean_tests
)

df_eff_both <- rbind(df_nary, df_rpop)

# Deltas vs baseline
df_eff_both$delta_iterations_vs_baseline  <-
  df_eff_both$mean_iterations - baseline_iter

df_eff_both$delta_total_tests_vs_baseline <-
  df_eff_both$mean_total_tests - baseline_tests

# Efficiency ratio
df_eff_both$efficiency_ratio_vs_baseline <-
  -df_eff_both$delta_iterations_vs_baseline /
  df_eff_both$delta_total_tests_vs_baseline

# Remove 1 tester and non-finite values
df_eff_both <- subset(
  df_eff_both,
  number_testers != 1 & is.finite(efficiency_ratio_vs_baseline)
)

# Absolute value for upward bars
df_eff_both$eff_ratio_abs <- abs(df_eff_both$efficiency_ratio_vs_baseline)

# Enforce strategy order (plot + legend)
df_eff_both$strategy <- factor(
  df_eff_both$strategy,
  levels = c("Nary-split", "Max-rpop")
)

# Shared y-limit
ymax_common <- ceiling(max(df_eff_both$eff_ratio_abs, na.rm = TRUE))

# Plot
p_eff_vs_nary1_both <- ggplot(
  df_eff_both,
  aes(x = factor(number_testers), y = eff_ratio_abs, fill = strategy)) +
  geom_col(color = "black", width = 0.7,
           position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Nary-split" = colour_palette[1],
                               "Max-rpop"   = colour_palette[2])) +
  labs(title = "Reduktion Testdurchläufe pro zusätzlichem Test",
       x = "Anzahl Probennehmer",
       y = expression("Effizienz verglichen mit " ~ Nary - split[1]),
       fill = "Strategie") +
  ylim(0, ymax_common) +
  theme_minimal(base_size = 13) +
  theme(plot.title   = element_text(face = "bold", hjust = 0.5),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave(
  "Plots/strategy_distributions/efficiency_ratio_vs_nary_split_1_both.pdf",
  plot = p_eff_vs_nary1_both,
  width = 9,
  height = 4.5,
  dpi = 150
)

# Create test cycle and total test data ####

# mean test cycles
testcycle_df_wide <- data.frame(
  number_testers = 1:10,
  nary_split       = nary_mean_testcycles,
  max_rpop         = rpop_mean_testcycles,
  skipping_cum_rpop = skipping_mean_testcycles,
  max_cum_rpop      = cum_mean_testcycles
)
# mean total tests
test_df_wide <- data.frame(
  number_testers   = 1:10,
  nary_split       = nary_mean_tests[1:10],
  max_rpop         = rpop_mean_tests[1:10],
  skipping_cum_rpop = skipping_mean_tests[1:10],
  max_cum_rpop      = cum_mean_tests[1:10]
)

# Diffs
test_cycle_diff_df <- data.frame(
  number_testers = 2:10,
  nary_split       = diff(test_cycle_df_wide$nary_split),
  max_cum_rpop      = diff(test_cycle_df_wide$max_cum_rpop),
  max_rpop         = diff(test_cycle_df_wide$max_rpop),
  skipping_cum_rpop = diff(test_cycle_df_wide$skipping_cum_rpop)
)

total_tests_diff_df <- data.frame(
  number_testers = 2:10,
  nary_split       = diff(total_tests_wide$nary_split),
  max_cum_rpop      = diff(total_tests_wide$max_cum_rpop),
  max_rpop         = diff(total_tests_wide$max_rpop),
  skipping_cum_rpop = diff(total_tests_wide$skipping_cum_rpop)
)
