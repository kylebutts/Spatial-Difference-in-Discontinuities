## figure-diff_in_disc.R -------------------------------------------------------
## Kyle Butts, CU Boulder Economics 
## 
## 

library(tidyverse)
library(kfbmisc)
library(latex2exp)
library(patchwork)

# ---- Pre Plot ----------------------------------------------------------------

te_0 <- 0.2
x_grid <- tibble(x = seq(-1, 1))

# Y_0(0)
y_pre_0 <- function(x) { 0.05 + .02 * x + 0.08 * x^2 + 0.1 * x^3 + te_0 * (x > 0)}


(plot_y_pre <- ggplot(x_grid, aes(x = x)) +
    # Cutoff
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey20", size = 1) +
    # Y_0(0)
    geom_function(
        fun = y_pre_0, colour = "#107895", xlim = c(-1, 0), linetype = "solid",
        size = 1.1
    ) + 
    geom_function(
        fun = y_pre_0, colour = "#107895", xlim = c(0.0001, 1), linetype = "solid",
        size = 1.1
    ) + 
    annotate(
        "text", x = 1.02, y = y_pre_0(1), colour = "#107895", 
        label =  TeX(r'($Y_{t=0}(0)$)'),
        hjust = 0, size = 3, parse = TRUE
    ) +
    # RD_0
    annotate(
        "segment", x = 0, xend = 0, y = y_pre_0(0), yend = y_pre_0(0) + te_0, 
        size = 1, colour = "grey60"
    ) +
    annotate(
        "text", x = 0.05, y = y_pre_0(0)/2 + y_pre_1(0)/2, 
        label = TeX(r'($\tau_{t=0}$)'), colour = "grey60",
        hjust = 0, size = 4, parse = TRUE
    ) +
    # Label
    ggtext::geom_textbox(
        data = tibble(x = -0.95, y = 0.92, label = "Pre-treatment RD informs us of effects of other policy changes and non-treatment based-sorting"),
        aes(x, y, label = label), 
        hjust = 0, size = 3.5, width = unit(0.9, "npc"), 
        fill = "white", box.color = NA,
        box.padding = unit(c(4,4,4,4), "pt"), family = "fira_sans"
    ) +
    kfbmisc::theme_kyle(base_size = 14) +
    # Remove x- and y-axes
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
    ) +
    labs(x = NULL, y = NULL) +
    ylim(0,1) + 
    scale_x_continuous(breaks = c(-1,0,1), labels = c(" ", "Border Cutoff", " "), limits = c(-1, 1.2)) +
    theme(
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    ))


# ---- Post Plot ---------------------------------------------------------------

# Y_0(0)
y_post_0 <- function(x) { 0.05 + .02 * x + 0.08 * x^2 + 0.1 * x^3 + te_0 * (x > 0)}
# Y_0(1)
y_post_1 <- function(x) { 0.55 + .05 * x + 0.05 * x^2 + 0.1 * x^3 }


x_grid <- tibble(x = seq(-1, 1))

(plot_y_post <- ggplot(x_grid, aes(x = x)) +
    # Cutoff
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey20", size = 1) +
    # Y_1(0)
    geom_function(
        fun = y_post_0, colour = "#107895", xlim = c(-1, 0), linetype = "solid",
        size = 1.1
    ) + 
    geom_function(
        fun = y_post_0, colour = "#107895", xlim = c(0.001, 1), linetype = "dashed",
        size = 1.1
    ) + 
    annotate(
        "text", x = 1.02, y = y_post_0(1), colour = "#107895", 
        label =  TeX(r'($Y_{t=1}(0)$)'),
        hjust = 0, size = 3, parse = TRUE
    ) +
    # Y_1(1)
    geom_function(
        fun = y_post_1, colour = "#9A2515", xlim = c(-1, 0), linetype = "dashed",
        size = 1.1
    ) + 
    geom_function(
        fun = y_post_1, colour = "#9A2515", xlim = c(0, 1), linetype = "solid",
        size = 1.1
    ) +
    annotate(
        "text", x = 1.02, y = y_post_1(1), colour = "#9A2515", 
        label =  TeX(r'($Y_{t=1}(1)$)'),
        hjust = 0, size = 3, parse = TRUE
    ) +
    # RD_0
    annotate(
        "segment", x = 0, xend = 0, y = y_post_0(0), yend = y_post_0(0) + te_0, 
        size = 1, color = "grey60"
    ) +
    annotate(
        "text", x = 0.05, y = y_post_0(0) + te_0/2, 
        label = TeX(r'($\tau_{t=0}$)'), color = "grey60",
        hjust = 0, size = 4, parse = TRUE
    ) +
    # RD_1 - RD_0
    annotate(
        "segment", x = 0, xend = 0, y = y_post_0(0) + te_0, yend = y_post_1(0), 
        size = 1
    ) +
    annotate(
            "text", x = 0.05, y = y_post_0(0) + te_0 +  (y_post_1(0) - y_post_0(0) - te_0)*3/5, 
            label = TeX(r'($LATE = \tau_{t=1} - \tau_{t=0}$)'),
            hjust = 0, size = 4, parse = TRUE
    ) +
    # Label
    ggtext::geom_textbox(
        data = tibble(x = -0.95, y = 0.92, label = "Difference of post RD and pre RD is the treatment effect of interest"),
        aes(x, y, label = label),
        hjust = 0, size = 3.5, width = unit(0.9, "npc"), 
        fill = "white", box.color = NA,
        box.padding = unit(c(4,4,4,4), "pt"), family = "fira_sans"
    ) +
    kfbmisc::theme_kyle(base_size = 14) +
    # Remove x- and y-axes
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
    ) +
    labs(x = NULL, y = NULL) +
    ylim(0,1) + 
    scale_x_continuous(breaks = c(-1,0,1), labels = c(" ", "Border Cutoff", " "), limits = c(-1, 1.2)) +
    theme(
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    ))


combined <- (plot_y_pre + plot_y_post) & theme(
    rect = element_rect(fill = "transparent") # all rectangles
)



kfbmisc::ggpreview(
    combined, device = "pdf", width = 8, height = 3, dpi = 300
)

ggsave(
    "figures/diff_in_disc.pdf", combined, width = 8, height = 3
)

# For web
ggsave(
    "figures/diff_in_disc.svg", combined, width = 8, height = 3, bg = "transparent"
)






