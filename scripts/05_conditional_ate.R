library(dplyr)
library(broom)
library(ggplot2)
library(here)

source(file.path("R", "helpers.R"))
source(file.path("R", "theme.R"))

theme_set(theme_nice())

# ---- Load data --------------------------------------------------------------

ds_long <- file.path("data", "processed", "fairness_survey_long.rds") |>
    read_rds()

# ---- Variables --------------------------------------------------------------

breaks <- c(0, 3, 6, 10)

ds_long <- ds_long %>%
    mutate(
        polright = cut(
            political_right,
            breaks = breaks,
            include.lowest = TRUE,
            labels = c("Left", "Neutral", "Right")
        ),
        trust_eu_cut = cut(
            trust_eu,
            breaks = breaks,
            include.lowest = TRUE,
            labels = c("Low", "Medium", "High")
        ),
        alt_label = dplyr::replace_values(
            alt,
            "no_relocation" ~ "No relocation",
            "relocation_GDP" ~ "Relocation by GDP",
            "relocation_population" ~ "Relocation by Population"
        )
    )

# ---- Helpers ----------------------------------------------------------------
run_models <- function(data, groups, formula) {
    data |>
        dplyr::reframe(
            lm(formula, data = pick(everything())) |>
                broom::tidy(conf.int = TRUE),
            .by = dplyr::all_of(groups)
        )
}

plot_coefficients <- function(data, facet = NULL) {
    p <- ggplot(
        data,
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            label = sprintf("%.2f", estimate),
            color = abs(estimate) / std.error > 1.96
        )
    ) +
        geom_vline(
            xintercept = 0,
            linewidth = .5,
            color = "salmon"
        ) +
        geom_pointrange(
            position = position_dodge(.5)
        ) +
        geom_label(
            aes(x = Inf),
            border.colour = NA,
            hjust = 1,
            size = 3.5
        ) +
        scale_color_brewer(
            palette = "Paired"
        ) +
        theme(
            legend.position = "none"
        )

    if (!is.null(facet)) {
        p <- p + facet
    }

    p
}

plot_cate <- function(
    data, variable, xlab,
    method = "loess",
    breaks = c(0, 5, 10),
    limits = c(0, 10)) {
    data[[variable]] <- as.numeric(data[[variable]])

    p <- data %>%
        filter(treatment != "Relative") %>%
        ggplot(
            aes(
                x = .data[[variable]],
                y = as.numeric(rank),
                color = treatment,
                linetype = treatment,
            )
        ) +
        facet_grid(
            country_type ~ alt_label,
            labeller = label_wrap_gen(width = 15)
        ) +
        geom_smooth(
            method = method,
            se = TRUE,
            linewidth = 0.9
        ) +
        scale_color_brewer(
            palette = "Set1"
        ) +
        scale_x_continuous(
            breaks = breaks,
            limits = limits
        ) +
        scale_y_reverse(
            breaks = scales::pretty_breaks()
        ) +
        labs(
            x = xlab,
            y = "Fairness ranking\n(higher values = less fair)",
            color = "Treatment",
            linetype = "Treatment"
        ) +
        theme(
            legend.key.width = unit(1.5, "cm")
        )

    outpath <- ggsave(
        filename = file.path("output", "figures", paste0(variable, ".pdf")),
        width = 9, height = 5,
        plot = p,
    )
    if (interactive()) {
        system(paste("open", outpath))
    }

    invisible(outpath)
}


# ---- Trust ----------------------------------------------------------------

p_trust <- ds_long %>%
    plot_cate("trust", xlab = "Trust in others (0 = Low, 10 = High)")

p_trust_gov <- ds_long %>%
    plot_cate("trust_gov", xlab = "Trust in the national gov. (0 = Low, 10 = High)")

p_trust_eu <- ds_long %>%
    plot_cate("trust_eu", xlab = "Trust in the EU (0 = Low, 10 = High)")

# ---- Age ----------------------------------------------------------------

p_age <- ds_long %>%
    plot_cate(
        "age",
        limits = c(18, 80),
        breaks = seq(20, 80, by = 10),
        xlab = "Age (years)"
    )

p_fair_share <- ds_long %>%
    mutate(
        fair_share = factor(
            fair_share,
            c("No, the share is too low", "Yes", "No, the share is too high")
        ) %>%
            as.numeric()
    ) %>%
    plot_cate(
        "fair_share",
        xlab = "Fair share (1 = Too low, 2 = Fair, 3 = Too high)",
        breaks = c(1, 2, 3),
        limits = c(1, 3)
    ) 

p_political_right <- ds_long %>% 
    plot_cate(
        "political_right",
        xlab = "Political left-right scale (0 = Left, 10 = Right)"
    )

p_asylum_knowledge  <- ds_long %>% 
    mutate(
        asylum_knowledge = factor(
            asylum_knowledge,
            c(
                "I have no knowledge", 
                "I have basic knowledge",
                "I have a good understanding",
                "I have great expertise"
            ),
            ordered = TRUE
        )
    ) %>%
    plot_cate(
        "asylum_knowledge",
        xlab = "Knowledge of EU asylum system (1 = No knowledge, 4 = Expert)",
        breaks = c(1,2,3,4),
        limits = c(1, 4)
    )



# ---- Models  --------------------------------------------------

models <- list(
    m0 = as.numeric(rank) ~ alt * treatment  * trust_eu,
    m1 = as.numeric(rank) ~ alt * treatment  * polright,
    m2 = as.numeric(rank) ~ alt  + treatment + scale(age) + alt:treatment + treatment:scale(age) + alt:scale(age)
)
fits <- lapply(models, lm, data = ds_long)
tests <- lapply(fits, anova)


m0 <- lm(
    formula = ,
    data = ds_long
)
anova(m0) # significant at 95%


m0 <- lm(
    formula = as.numeric(rank) ~ alt * (treatment  * trust_eu),
    data = ds_long
)
anova(m0) # significant at 95%

coeffs <- ds_long %>% 
    rename(treatment_ = treatment, trustEU_ = trust_eu_cut) %>% 
    run_models(
        c("alt", "country_type"),
        as.numeric(rank) ~  treatment_ * trustEU_
    )

coeffs %>%
    filter(term != "(Intercept)") %>% 
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            color = abs(estimate) / std.error < 1.96
        )
    ) + 
    geom_vline(xintercept = 0) + 
    facet_grid(country_type ~ alt) + 
    geom_pointrange() +
    scale_y_discrete(
        labels = function(x) {
            case_when(
                TRUE ~ x
            )
        }
    ) +
    scale_color_brewer(palette = "Accent")

coeffs %>% 
    filter(
        alt == "no_relocation",
        country_type == "Net sender"
    ) %>% 
    select(term, estimate, statistic)


coeffs <- run_models(
    ds_long,
    c("trust_eu_cut", "country_type"),
    as.numeric(rank) ~ alt * treatment
)


coeffs |>
    filter(grepl("treat", term)) |>
    mutate(
        term = dplyr::replace_values(
            term,
            "altrelocation_GDP:treatmentControl" ~ "GDP x Control",
            "altrelocation_GDP:treatmentRelative" ~ "GDP x Relative",
            "altrelocation_population:treatmentControl" ~ "Pop. x Control",
            "altrelocation_population:treatmentRelative" ~ "Pop. x Relative",
            "treatmentControl" ~ "Control",
            "treatmentRelative" ~ "Relative"
        ),
        group = case_when(
            grepl("Control", term) ~ "Control",
            TRUE ~ "Relative"
        )
    ) %>%
    plot_coefficients(
        facet_grid(term ~ country_type, scales = "free", switch = "y")
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_discrete() +
    aes(y = trust_eu_cut) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(size = 8)
    ) +
    labs(
        y = NULL,
        x = "Effect of switching to Absolute on fairness ranking\n(higher = less fair)"
    )

file.path("output", "figures", "coeffs_trust_eu.png") |>
    save_plot()


# ---- Trust EU orientation --------------------------------------------------

coeffs <- run_models(
    ds_long,
    c("fair_share", "country_type"),
    as.numeric(rank) ~ alt * treatment
)

coeffs |>
    filter(grepl("treat", term)) |>
    mutate(
        term = dplyr::replace_values(
            term,
            "altrelocation_GDP:treatmentControl" ~ "GDP x Control",
            "altrelocation_GDP:treatmentRelative" ~ "GDP x Relative",
            "altrelocation_population:treatmentControl" ~ "Pop. x Control",
            "altrelocation_population:treatmentRelative" ~ "Pop. x Relative",
            "treatmentControl" ~ "Control",
            "treatmentRelative" ~ "Relative"
        ),
        group = case_when(
            grepl("Control", term) ~ "Control",
            TRUE ~ "Relative"
        )
    ) %>%
    plot_coefficients(
        facet_grid(term ~ country_type, scales = "free", switch = "y")
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_discrete() +
    aes(y = fair_share) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(size = 8)
    ) +
    labs(
        y = NULL,
        x = "Effect of switching to Absolute on fairness ranking\n(higher = less fair)"
    )

file.path("output", "figures", "coeffs_fair_share.png") |>
    save_plot()


# ---- Asylum knowledge --------------------------------------------------

coeff_knowledge <- run_models(
    ds_long,
    c("asylum_knowledge", "country_type"),
    as.numeric(rank) ~ alt * treatment
)

coeff_knowledge |>
    filter(grepl("treat", term)) |>
    mutate(
        term = dplyr::replace_values(
            term,
            "altrelocation_GDP:treatmentControl" ~ "GDP x Control",
            "altrelocation_GDP:treatmentRelative" ~ "GDP x Relative",
            "altrelocation_population:treatmentControl" ~ "Pop. x Control",
            "altrelocation_population:treatmentRelative" ~ "Pop. x Relative",
            "treatmentControl" ~ "Control",
            "treatmentRelative" ~ "Relative"
        ),
        group = case_when(
            grepl("Control", term) ~ "Control",
            TRUE ~ "Relative"
        )
    ) %>%
    plot_coefficients(
        facet_grid(term ~ country_type, scales = "free", switch = "y")
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_discrete() +
    aes(y = asylum_knowledge) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(size = 8)
    ) +
    labs(
        y = NULL,
        x = "Effect of switching to Absolute on fairness ranking\n(higher = less fair)"
    )

file.path("output", "figures", "coeffs_knowledge.png") |>
    save_plot()


# ---- Political orientation --------------------------------------------------

coeff_polright <- run_models(
    ds_long,
    c("polright", "country_type"),
    as.numeric(rank) ~ alt * treatment
)

coeff_polright |>
    filter(grepl("treat", term), !is.na(polright)) |>
    mutate(
        term = dplyr::replace_values(
            term,
            "altrelocation_GDP:treatmentControl" ~ "GDP x Control",
            "altrelocation_GDP:treatmentRelative" ~ "GDP x Relative",
            "altrelocation_population:treatmentControl" ~ "Pop. x Control",
            "altrelocation_population:treatmentRelative" ~ "Pop. x Relative",
            "treatmentControl" ~ "Control",
            "treatmentRelative" ~ "Relative"
        ),
        group = case_when(
            grepl("Control", term) ~ "Control",
            TRUE ~ "Relative"
        )
    ) %>%
    plot_coefficients(
        facet_grid(term ~ country_type, scales = "free", switch = "y")
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_discrete() +
    aes(y = polright) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(size = 8)
    ) +
    labs(
        y = NULL,
        x = "Effect of switching to Absolute on fairness ranking\n(higher = less fair)"
    )

file.path("output", "figures", "coeffs_polright.png") |>
    save_plot()


# ---- Age --------------------------------------------------

coeff_age <- run_models(
    ds_long,
    c("age_cat", "country_type"),
    as.numeric(rank) ~ alt * treatment
)

coeff_age |>
    filter(grepl("treat", term)) |>
    mutate(
        term = dplyr::replace_values(
            term,
            "altrelocation_GDP:treatmentControl" ~ "GDP x Control",
            "altrelocation_GDP:treatmentRelative" ~ "GDP x Relative",
            "altrelocation_population:treatmentControl" ~ "Pop. x Control",
            "altrelocation_population:treatmentRelative" ~ "Pop. x Relative",
            "treatmentControl" ~ "Control",
            "treatmentRelative" ~ "Relative"
        ),
        group = case_when(
            grepl("Control", term) ~ "Control",
            TRUE ~ "Relative"
        )
    ) %>%
    plot_coefficients(
        facet_grid(term ~ country_type, scales = "free", switch = "y")
    ) +
    scale_x_continuous(limits = c(-1, 1) * 2) +
    scale_y_discrete() +
    aes(y = age_cat) +
    theme(
        strip.placement = "outside",
        strip.text.y = element_text(size = 8)
    ) +
    labs(
        y = NULL,
        x = "Effect of switching to Absolute on fairness ranking\n(higher = less fair)"
    )

file.path("output", "figures", "coeffs_age_cat.png") |>
    save_plot()




# # ---- regression
# m0 <- as.numeric(rank) ~ alt * treatment * country
# fit0 <- lm(m0, ds_long)


# coeffs <- ds_long %>%
#     reframe(
#         lm(as.numeric(rank) ~ alt * treatment) %>%
#             tidy(conf.int = TRUE),
#         .by = c(polright, country_type)
#     )

# coeffs %>%
#     filter(!is.na(polright), grepl("treat", term)) %>%
#     ggplot(
#         aes(
#             x = estimate,
#             y = interaction(polright, country_type, sep = " x "),
#             xmin = conf.low,
#             xmax = conf.high,
#             color = polright,
#             label = sprintf(
#                 "%2.2f",
#                 estimate
#             )
#         )
#     ) +
#     facet_wrap(~term) +
#     geom_vline(xintercept = 0, linewidth = 0.5, color = "salmon") +
#     geom_pointrange(
#         position = position_dodge(.5),
#     ) +
#     geom_text(
#         hjust = 1,
#         vjust = 0.5,
#         aes(x = Inf),
#         size = 3.5,
#         color = "darkgray"
#     ) +
#     scale_color_brewer(
#         palette = "Set2"
#     ) +
#     scale_x_continuous(
#         limits = c(-1, 1)
#     ) +
#     theme(legend.position = "none")

# ### ----- Age ------
# coeffs <- ds_long %>%
#     reframe(
#         lm(as.numeric(rank) ~ alt * treatment) %>%
#             tidy(conf.int = TRUE),
#         .by = c(country_type, age_cat)
#     )


# coeffs %>%
#     filter(grepl("treat", term)) %>%
#     ggplot(
#         aes(
#             y = age_cat,
#             x = estimate, # interaction(term, country_type, sep = " x "),
#             xmin = conf.low,
#             xmax = conf.high,
#             color = abs(estimate) / std.error > 1.96,
#             label = sprintf(
#                 "%2.2f",
#                 estimate
#             )
#         )
#     ) +
#     facet_grid(term ~ country_type, scales = "free") +
#     geom_vline(xintercept = 0, linewidth = 0.5, color = "salmon") +
#     geom_pointrange(
#         position = position_dodge(.5),
#     ) +
#     geom_label(
#         border.colour = NA,
#         hjust = 1,
#         vjust = 0.5,
#         aes(x = Inf),
#         size = 3.5
#     ) +
#     scale_color_brewer(
#         palette = "Paired"
#     ) +
#     # scale_x_continuous(
#     #     limits = c(-1, 1)
#     # ) +
#     theme(legend.position = "none")


# ### Trust in the EU

# coeffs <- ds_long %>%
#     reframe(
#         lm(as.numeric(rank) ~ alt * treatment * trust_eu_cut) %>%
#             tidy(conf.int = TRUE),
#         .by = c(country_type)
#     )


# coeffs %>%
#     filter(grepl("trust", term)) %>%
#     mutate(
#         group = case_when(
#             grepl("population", term) ~ "by population",
#             grepl("GDP", term) ~ "by GDP",
#             TRUE ~ "No relocation"
#         )
#     ) %>%
#     ggplot(
#         aes(
#             x = estimate,
#             y = term, # interaction(term, country_type, sep = " x "),
#             xmin = conf.low,
#             xmax = conf.high,
#             color = abs(estimate) / std.error > 1.96,
#             label = sprintf(
#                 "%2.2f",
#                 estimate
#             )
#         )
#     ) +
#     facet_grid(group ~ country_type, scales = "free") +
#     geom_vline(xintercept = 0, linewidth = 0.5, color = "salmon") +
#     geom_pointrange(
#         position = position_dodge(.5),
#     ) +
#     geom_label(
#         border.colour = NA,
#         hjust = 1,
#         vjust = 0.5,
#         aes(x = Inf),
#         size = 3.5
#     ) +
#     scale_color_brewer(
#         palette = "Paired"
#     ) +
#     scale_x_continuous(
#         limits = c(-1, 1)
#     ) +
#     scale_y_discrete(
#         labels = function(x) {
#             gsub("trust_eu_cut|treatment", " ", x)
#             gsub(":", " ", x)
#         }
#     ) +
#     theme(legend.position = "none")


# ### Fair share




# ### Konowledge of the Asylum System
