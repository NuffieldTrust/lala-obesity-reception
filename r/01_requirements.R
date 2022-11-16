########################################################################################################################

############# Basic requirements for all code in project #############

# Packages
# Folder structure
# Functions
# NT chart style

########################################################################################################################

############# Install and load packages #############

#install.packages(c("pacman", "fs", "plyr", "RColorBrewer", "lm.beta", "lmtest", "ciTools", "MASS", "mctest", "relaimpo"))

pacman::p_load("tidyverse", "tidylog", "janitor", "broom", "psych", "tidytext", "ggforce", "rgdal", "sf", "cowplot", "writexl", "ggpubr")

########################################################################################################################

############# Output folder structure #############

fs::dir_create("data")
fs::dir_create("output")
fs::dir_create("output/descriptives_dependent")
fs::dir_create("output/descriptives_dependent/plots")
fs::dir_create("output/descriptives_independent")
fs::dir_create("output/descriptives_independent/plots")
fs::dir_create("output/correlation_dependent_independent")
fs::dir_create("output/correlation_dependent_independent/plots")
fs::dir_create("output/simple_linear_regression")
fs::dir_create("output/simple_linear_regression/plots")
fs::dir_create("output/multiple_linear_regression")
fs::dir_create("output/multiple_linear_regression/plots")

########################################################################################################################

############# Functions #############

`%notin%` <- Negate(`%in%`)

########################################################################################################################

############# NT chart style #############

# Nuffield Trust colour list

NT_colours <- c(
  `NT ink` = "#271544",
  `white` = "#FFFFFF",
  `NT iris` = "#AC8ACF",
  `cool black` = "#0E1B26",
  `cool dark grey` = "#556370",
  `cool mid grey` = "#9AA0AA",
  `cool light grey` = "#F4F4F4",
  `bright purple` = "#9F67FF",
  `light purple 1` = "#D3C4FC",
  `light purple 2` = "#B39DFF",
  `dark purple 1` = "#7140EA",
  `dark purple 2` = "#49148C",
  `bright blue` = "#0066F4",
  `light blue 1` = "#99DBFF",
  `light blue 2` = "#63B2FF",
  `dark blue 1` = "#005AC7",
  `dark blue 2` = "#192889",
  `bright red` = "#FF6B57",
  `light red 1` = "#FFCFC9",
  `light red 2` = "#FF997F",
  `dark red 1` = "#B71C1C",
  `dark red 2` = "#700C28",
  `bright yellow` = "#EABE17",
  `light yellow 1` = "#FDEA9D",
  `light yellow 2` = "#F4D05A",
  `dark yellow 1` = "#DD931C",
  `dark yellow 2` = "#B26605",
  `bright green` = "#00C27A",
  `light green 1` = "#8BF8BD",
  `light green 2` = "#39DA91",
  `dark green 1` = "#00823F",
  `dark green 2` = "#195442",
  `bright cyan` = "#4DCFF5",
  `light cyan 1` = "#9EF7FF",
  `light cyan 2` = "#6AE8F9",
  `dark cyan 1` = "#008CB3",
  `dark cyan 2` = "#004C70"
)

NT_colour <- function(index = NULL, named = FALSE){
  
  if(is.null(index)){
    index <- names(NT_colours)
  }
  
  return_value <- NT_colours[index]
  if (!named) {
    names(return_value) <- NULL
  }
  
  return(return_value)
  
}

##########################################################

# NT colour palette

NT_palette <- function(NT_theme = NULL, reverse = FALSE, ...) {
  
  function(n) {
    
    stopifnot(n <= 5 | (n <= 12 & (is.null(NT_theme) | NT_theme == "bright")))
    
    colour_indices <-
      if (n == 1 & is.null(NT_theme)) { "bright purple" }
    else if (n == 2 & is.null(NT_theme)) { c("bright purple", "bright green") }
    else if (n == 3 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue") }
    else if (n == 4 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow") }
    else if (n == 5 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red") }
    else if (n == 6 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan") }
    else if (n == 7 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1") }
    else if (n == 8 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1") }
    else if (n == 9 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1") }
    else if (n == 10 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1") }
    else if (n == 11 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1") }
    else if (n == 12 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1", "light cyan 1") }
    else if (n == 1 & NT_theme == "bright") { "bright purple" }
    else if (n == 2 & NT_theme == "bright") { c("bright purple", "bright green") }
    else if (n == 3 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue") }
    else if (n == 4 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow") }
    else if (n == 5 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red") }
    else if (n == 6 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan") }
    else if (n == 7 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1") }
    else if (n == 8 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1") }
    else if (n == 9 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1") }
    else if (n == 10 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1") }
    else if (n == 11 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1") }
    else if (n == 12 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1", "light cyan 1") }
    else if (n == 1 & NT_theme == "purple") { "bright purple" }
    else if (n == 2 & NT_theme == "purple") { c("dark purple 2", "bright purple") }
    else if (n == 3 & NT_theme == "purple") { c("dark purple 2", "bright purple", "light purple 2") }
    else if (n == 4 & NT_theme == "purple") { c("dark purple 2", "dark purple 1", "bright purple", "light purple 2") }
    else if (n == 5 & NT_theme == "purple") { c("dark purple 2", "dark purple 1", "bright purple", "light purple 2", "light purple 1") }
    else if (n == 1 & NT_theme == "blue") { "bright blue" }
    else if (n == 2 & NT_theme == "blue") { c("dark blue 2", "bright blue") }
    else if (n == 3 & NT_theme == "blue") { c("dark blue 2", "bright blue", "light blue 2") }
    else if (n == 4 & NT_theme == "blue") { c("dark blue 2", "dark blue 1", "bright blue", "light blue 2") }
    else if (n == 5 & NT_theme == "blue") { c("dark blue 2", "dark blue 1", "bright blue", "light blue 2", "light blue 1") }
    else if (n == 1 & NT_theme == "red") { "bright red" }
    else if (n == 2 & NT_theme == "red") { c("dark red 2", "bright red") }
    else if (n == 3 & NT_theme == "red") { c("dark red 2", "bright red", "light red 2") }
    else if (n == 4 & NT_theme == "red") { c("dark red 2", "dark red 1", "bright red", "light red 2") }
    else if (n == 5 & NT_theme == "red") { c("dark red 2", "dark red 1", "bright red", "light red 2", "light red 1") }
    else if (n == 1 & NT_theme == "yellow") { "bright yellow" }
    else if (n == 2 & NT_theme == "yellow") { c("dark yellow 2", "bright yellow") }
    else if (n == 3 & NT_theme == "yellow") { c("dark yellow 2", "bright yellow", "light yellow 2") }
    else if (n == 4 & NT_theme == "yellow") { c("dark yellow 2", "dark yellow 1", "bright yellow", "light yellow 2") }
    else if (n == 5 & NT_theme == "yellow") { c("dark yellow 2", "dark yellow 1", "bright yellow", "light yellow 2", "light yellow 1") }
    else if (n == 1 & NT_theme == "green") { "bright green" }
    else if (n == 2 & NT_theme == "green") { c("dark green 2", "bright green") }
    else if (n == 3 & NT_theme == "green") { c("dark green 2", "bright green", "light green 2") }
    else if (n == 4 & NT_theme == "green") { c("dark green 2", "dark green 1", "bright green", "light green 2") }
    else if (n == 5 & NT_theme == "green") { c("dark green 2", "dark green 1", "bright green", "light green 2", "light green 1") }
    else if (n == 1 & NT_theme == "cyan") { "bright cyan" }
    else if (n == 2 & NT_theme == "cyan") { c("dark cyan 2", "bright cyan") }
    else if (n == 3 & NT_theme == "cyan") { c("dark cyan 2", "bright cyan", "light cyan 2") }
    else if (n == 4 & NT_theme == "cyan") { c("dark cyan 2", "dark cyan 1", "bright cyan", "light cyan 2") }
    else if (n == 5 & NT_theme == "cyan") { c("dark cyan 2", "dark cyan 1", "bright cyan", "light cyan 2", "light cyan 1") }
    
    return_colours <- NT_colour(colour_indices)
    
    if (reverse) {
      
      return_colours <- rev(NT_colour(colour_indices))
      
    }
    
    return(return_colours)
    
  }
}

##########################################################

# NT colour scale

scale_colour_NT <- function(palette = NT_palette(NT_theme = NULL, reverse = FALSE, ...), ...) {
  
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "NT1",
    palette = palette,
    ...
  )
  
}

##########################################################

# NT fill scale

scale_fill_NT <- function(palette = NT_palette(NT_theme = NULL, reverse = FALSE, ...), ...) {
  
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "NT2",
    palette = palette,
    ...
  )
  
}

##########################################################

# Nuffield Trust ggplot theme

# Manual changes: legend justification, colouring of chart, hollow symbols with fill

NT_style <- function(){
  
  font <- "TT Arial"
  family <- "sans"
  
  theme_minimal() %+replace%
    
    theme(
      
      # Background elements
      
      panel.background = element_rect(fill = NT_colour("cool light grey"), colour = NT_colour("cool light grey")),
      panel.border = element_blank(),
      plot.background = element_rect(fill = NT_colour("cool light grey"), colour = NT_colour("cool light grey")),
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit ="cm"),
      
      # Grid elements
      
      axis.ticks = element_blank(),      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = NT_colour("cool mid grey"), size = 0.3),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "cm"),
      
      # Text elements
      
      axis.text.x = element_text(colour = NT_colour("cool mid grey"), size = 8, family = "sans", vjust = 0),
      axis.text.y = element_text(colour = NT_colour("cool mid grey"), size = 8, family = "sans"),
      axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit ="cm"), colour = NT_colour("NT ink"), size = 8, face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit ="cm"), colour = NT_colour("NT ink"), size = 8, face = "bold", angle = 90),
      legend.text = element_text(colour = NT_colour("NT ink"), size = 8, family = "sans"),
      legend.title = element_blank(),
      plot.caption = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit ="cm"), colour = NT_colour("NT ink"), size = 8, hjust = 1, vjust = 1),
      plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit ="cm"), colour = NT_colour("NT ink"), size = 10, face = "bold", hjust = 0),
      #plot.title.position = "plot",
      strip.text = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit ="cm"), colour = NT_colour("NT ink"), size = 8, face = "bold"),
      
      # Legend elements
      
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm"),
      legend.key = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm"),
      legend.position = "bottom",
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm")
      
    )
  
}

###########################################################################################################################
