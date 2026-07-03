# =============================================================================
# Short description of script's purpose
# =============================================================================

library(tidyverse)
library(stargazer)
library(ggplot2)
library(lmtest)
library(sandwich)

# Paths
input_dir <- "../../1_data/output"
output_dir <- "../output"

# =============================================================================

main <- function() {
  load("../input/mpg.Rdata")
  regression_table(mpg_clean)
  regression_table_cluster_year(mpg_clean)
  city_figure(mpg_clean)
  hwy_figure(mpg_clean)
}

regression_table <- function(data) {
  reg_cty <- lm(displ ~ cty, data = data)
  summary(reg_cty)

  reg_hwy <- lm(displ ~ hwy, data = data)
  summary(reg_hwy)

  reg_hwy_cty <- lm(displ ~ hwy + cty, data = data)
  summary(reg_hwy_cty)

  stargazer(reg_cty, reg_hwy, reg_hwy_cty, title = "Results", align = TRUE,
            dep.var.labels = c("Engine displacement (L)"),
            covariate.labels = c("City fuel economy (mpg)",
                                 "Highway fuel economy (mpg)"),
            float = FALSE,
            out = "../output/table_reg.tex")
}

regression_table_cluster_year <- function(data) {

  reg_cty <- lm(displ ~ cty, data = data)
  reg_hwy <- lm(displ ~ hwy, data = data)
  reg_hwy_cty <- lm(displ ~ hwy + cty, data = data)

  vcov_cty <- vcovCL(reg_cty, cluster = ~ year, data = data)
  vcov_hwy <- vcovCL(reg_hwy, cluster = ~ year, data = data)
  vcov_hwy_cty <- vcovCL(reg_hwy_cty, cluster = ~ year, data = data)

  se_cty <- sqrt(diag(vcov_cty))
  se_hwy <- sqrt(diag(vcov_hwy))
  se_hwy_cty <- sqrt(diag(vcov_hwy_cty))

  p_cty <- coeftest(reg_cty, vcov. = vcov_cty)[, 4]
  p_hwy <- coeftest(reg_hwy, vcov. = vcov_hwy)[, 4]
  p_hwy_cty <- coeftest(reg_hwy_cty, vcov. = vcov_hwy_cty)[, 4]

  stargazer(
    reg_cty,
    reg_hwy,
    reg_hwy_cty,
    align = TRUE,
    dep.var.labels = c("Engine displacement (L)"),
    covariate.labels = c(
      "City fuel economy (mpg)",
      "Highway fuel economy (mpg)"
    ),
    se = list(se_cty, se_hwy, se_hwy_cty),
    p = list(p_cty, p_hwy, p_hwy_cty),
    float = FALSE,
    out = "../output/table_reg_cluster_year.tex"
  )

}

city_figure <- function(data) {
  p <- ggplot(data, aes(x = displ, y = log(cty), color = year)) +
    geom_point() +
    xlab("Engine displacement (L)") +
    ylab("City fuel economy (log(mpg)")
  ggsave("../output/figure_city.jpg", plot = p)
}

hwy_figure <- function(data) {
  p <- ggplot(data, aes(x = displ, y = hwy, color = year)) +
    geom_point() +
    xlab("Engine displacement (L)") +
    ylab("Highway fuel economy (mpg)")
  ggsave("../output/figure_hwy.jpg", plot = p)
}

# Execute
main()