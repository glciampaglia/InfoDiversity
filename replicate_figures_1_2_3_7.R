## Replication code for figures 1, 2, and 3 for 
## "Political audience and algorithmic bias in news reccomendation"

# Set up ----------------------------
library(tidyverse)
library(egg)
library(cowplot)
library(ggpubr)
library(modelsummary)
library(kableExtra)
library(broom)

pulse <- read_csv("pulse_newsguard_merged.csv")
ng <- read_csv("newsguard_anonymized.csv")


# Functions --------------------------
standardize <- function(vec) {
  avg <- mean(vec, na.rm = T)
  std_dev <- sd(vec, na.rm = T)
  vec_new <- (vec - avg) / std_dev
  return(vec_new)
}

get_partial_cor <- function(df, var1, var2, control) {
  var1_vec <- pull(df, !!var1)
  var2_vec <- pull(df, !!var2)
  control_vec <- pull(df, !!control)
  reg1 <- lm(var1_vec ~ control_vec)
  res1 <- reg1$residuals
  reg2 <- lm(var2_vec ~ control_vec)
  res2 <- reg2$residuals
  test <- cor.test(res1, res2, use = "complete.obs")
  results <- tibble(
    correlation = test$estimate,
    t_stat = test$statistic,
    p_val = test$p.value,
    lower = test$conf.int[1],
    upper = test$conf.int[2]
  )
  return(results)
}

corr_equiv_test <- function(vec1, vec2, equ_bound, iter = 1000) {
  df <- na.omit(cbind(vec1, vec2))
  estimates <- rep(NA, iter)
  
  # bootstrap 
  for(i in 1:iter) {
    df_boot <- df[sample(c(1:nrow(df)), nrow(df), replace = T),]
    estimates[i] <- cor(df_boot[,1], df_boot[,2])
  }
  
  # calculate bootstrap one-sided tests
  c(sum(estimates < (-equ_bound)) / iter, 
    sum(equ_bound < estimates) / iter)
}

corr_equiv_interval <- function(vec1, vec2, upper = 1) {
  # set up 
  bounds <- seq(0, upper, by = 0.001)
  i <- 0
  equi_bounds <- c(NA_real_, NA_real_)
  is_not_equiv <- TRUE
  
  # loop through to find bounds
  while(is_not_equiv) {
    i <- i + 1
    p_vals <- corr_equiv_test(vec1, vec2, bounds[i])
    
    if(sum(is.na(equi_bounds)) == 2 & any(p_vals <= 0.05)) {
      equi_bounds[which(p_vals <= 0.05)] <- bounds[i]
    }
    is_not_equiv <- any(p_vals > 0.05)
  }
  
  equi_bounds[is.na(equi_bounds)] <- bounds[i]
  equi_bounds[1] <- -equi_bounds[1]
  equi_bounds
}

# Calculate quantities --------------
# Correlations quality vs popularity
# Number of unique vistors
cor.test(pulse$Score, pulse$log_n_visitor, use = "complete.obs")
cor.test(pulse[pulse$is_conservative_visitor == 1,]$Score, 
         pulse[pulse$is_conservative_visitor == 1,]$log_n_visitor, use = "complete.obs")
cor.test(pulse[pulse$is_conservative_visitor == 0,]$Score, 
         pulse[pulse$is_conservative_visitor == 0,]$log_n_visitor, use = "complete.obs")

# Pageviews
cor.test(pulse$Score, pulse$log_n_pageview, use = "complete.obs")
cor.test(pulse[pulse$is_conservative_pageview == 1,]$Score, 
         pulse[pulse$is_conservative_pageview == 1,]$log_n_pageview, use = "complete.obs")
cor.test(pulse[pulse$is_conservative_pageview == 0,]$Score, 
         pulse[pulse$is_conservative_pageview == 0,]$log_n_pageview, use = "complete.obs")


# Equivalence confidence interval
set.seed(123)
corr_equiv_interval(pulse$Score, pulse$log_n_pageview)
corr_equiv_interval(pulse[pulse$is_conservative_visitor == 1,]$Score, 
                    pulse[pulse$is_conservative_visitor == 1,]$log_n_visitor)

corr_equiv_interval(pulse$Score, pulse$log_n_pageview)
corr_equiv_interval(pulse[pulse$is_conservative_pageview == 1,]$Score, 
                    pulse[pulse$is_conservative_pageview == 1,]$log_n_pageview)
corr_equiv_interval(pulse[pulse$is_conservative_pageview == 0,]$Score, 
                    pulse[pulse$is_conservative_pageview == 0,]$log_n_pageview)

# Partial correlations quality vs variance controlling for partisanship 
# User level 
pulse_with_ng <- filter(pulse, !is.na(Score))
get_partial_cor(pulse_with_ng, "Score", "visitor_var", "visitor_avg")

# Pageview level
get_partial_cor(pulse_with_ng, "Score", "pageview_var", "pageview_avg")

# Partial correlations quality vs variance controlling for extremity
# User level
pulse_with_ng <- 
  pulse_with_ng %>% 
  mutate(
    visitor_extremity = abs(visitor_avg - 4),
    page_extremity = abs(pageview_avg - 4)
  )
get_partial_cor(pulse_with_ng, "Score", "visitor_var", "visitor_extremity")

# Pageview
get_partial_cor(pulse_with_ng, "Score", "pageview_var", "page_extremity")

# Partial correlations quality vs variance controlling for popularity
# User level
get_partial_cor(pulse_with_ng, "Score", "visitor_var", "log_n_visitor")

# Pageview 
get_partial_cor(pulse_with_ng, "Score", "pageview_var", "log_n_visitor")

# Regress NewsGuard scores on popularity 
# User level
popularity_user <- 
  lm(Score ~ log_n_visitor,
     data = pulse)
popularity_user_interact <- 
  lm(Score ~ log_n_visitor * is_conservative_visitor, 
     data = pulse)

# Pageview level 
popularity_pview <- 
  lm(Score ~ log_n_pageview,
     data = pulse)
popularity_pview_interact <- 
  lm(Score ~ log_n_pageview * is_conservative_pageview, 
     data = pulse)

# Regression tables
pop_mods <- 
  list(
    `Model 1` = popularity_user,
    `Model 2` = popularity_user_interact,
    `Model 3` = popularity_pview,
    `Model 4` = popularity_pview_interact
  )
cm_pop <- 
  c('log_n_visitor' = 'Logged N visitors',
    'log_n_pageview' = 'Logged N pageviews',
    'is_conservative_visitor' = 'Conservative indicator',
    'is_conservative_pageview' = 'Conservative indicator',
    'log_n_visitor:is_conservative_visitor' = 'N visitor x Conservative',
    'log_n_pageview:is_conservative_pageview' = 'N pageview x Conservative',
    '(Intercept)' = 'Constant')
popularity_reg <- 
  modelsummary(
    pop_mods,
    caption = "\\label{tab:pop_reg}Relationship betwen NewsGuard scores and popularity",
    coef_map = cm_pop,
    stars_note = FALSE,
    stars = FALSE,
    statistic = c("conf.int","p = {p.value}"),
    gof_omit = "AIC|BIC|Log.Lik",
    output = "latex"
  ) %>% 
  kable_styling(
    font_size = 8,
    latex_options = "HOLD_position"
  ) %>%
  add_header_above(
    c(" ", "Dependent variable: NewsGuard score" = 4),
    italic = T
  ) %>%
  footnote(
    threeparttable = T,
    general_title = "",
    general = "* p < 0.1, ** p < 0.05, *** p < 0.01. 95\\\\% confidence intervals are reported in square brackets and exact p values below.",
    escape = F
  )
cat(popularity_reg, file = "popularity_regression.tex")

# Regress NewsGuard scores on partisanship variance 
# User level
diversity_user <- 
  lm(Score ~ standardize(visitor_var), 
     data = pulse)
diversity_user_interact <- 
  lm(Score ~ standardize(visitor_var) * is_conservative_visitor, 
     data = pulse)


# Pageview level
diversity_pview <-
  lm(Score ~ standardize(pageview_var), 
     data = pulse)
diversity_pview_interact <- 
  lm(Score ~ standardize(pageview_var) * is_conservative_pageview, 
     data = pulse)

div_mods <- 
  list(
    `Model 1` = diversity_user,
    `Model 2` = diversity_user_interact,
    `Model 3` = diversity_pview,
    `Model 4` = diversity_pview_interact
  )
cm_div <- 
  c('standardize(visitor_var)' = 'User-level variance',
    'standardize(pageview_var)' = 'Pageview-level variance',
    'is_conservative_visitor' = 'Conservative indicator',
    'is_conservative_pageview' = 'Conservative indicator',
    'standardize(visitor_var):is_conservative_visitor' = 'User variance x Conservative',
    'standardize(pageview_var):is_conservative_pageview' = 'Pageview variance x Conservative',
    '(Intercept)' = 'Constant')
diversity_reg <- 
  modelsummary(
    div_mods,
    caption = "\\label{tab:var_reg}Relationship betwen NewsGuard scores and diversity",
    coef_map = cm_div,
    stars_note = FALSE,
    stars = FALSE,
    statistic = c("conf.int","p = {p.value}"),
    gof_omit = "AIC|BIC|Log.Lik",
    output = "latex"
  ) %>% 
  kable_styling(
    font_size = 8,
    latex_options = "HOLD_position"
  ) %>%
  add_header_above(
    c(" ", "Dependent variable: NewsGuard score" = 4),
    italic = T
  ) %>%
  footnote(
    threeparttable = T,
    general_title = "",
    general = 
      "* p < 0.1, ** p < 0.05, *** p < 0.01. 95\\\\% confidence intervals are reported in square brackets and exact p values below.
    User-level and pageview-level variance measures are standardized to have mean zero, variance 1. ",
    escape = F
  )
cat(diversity_reg, file = "diversity_regression.tex")


# Graphs ----------------------------
# Figure 1 
pop_visitor <- 
  pulse %>% 
  ggplot(aes(x = log_n_visitor, y = Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x) + 
  theme_bw() + 
  ggtitle("User level") +
  xlab("Log(number of visitors)") + ylab("NewsGuard reliability scores") 
pop_pageview <- pulse %>% 
  ggplot(aes(x = log_n_pageview, y = Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x) + 
  theme_bw() + 
  ggtitle("Pageview level") +
  xlab("Log(number of visits)") + ylab("NewsGuard reliability scores") 
pop_pane <- egg::ggarrange(pop_visitor, pop_pageview, nrow = 1)
ggsave("fig1_traffic_vs_quality.pdf", pop_pane, 
       width = 180, height = 90, units = "mm")

  # Figure 2 
domain_ng <- 
  pulse %>% 
  ggplot(aes(x = visitor_avg, y = visitor_var)) + 
  geom_point(data = filter(pulse, is.na(Score)),
             aes(x = visitor_avg, y = visitor_var), 
             color = "grey", alpha = 0.6) +
  geom_point(data = filter(pulse, !is.na(Score)),
             aes(x = visitor_avg, y = visitor_var, 
                 color = Score), alpha = 0.8) +
  scale_color_gradient() +
  theme_bw() + 
  scale_x_continuous(
    "Mean partisanship",
    limits = c(1, 7),
    breaks = seq(1,7, by = 1),
    labels = c("Strong\nDemocrat", "", "", "Independent",
               "", "", "Strong\nRepublican")
  ) + 
  scale_y_continuous(
    "Partisanship variance",
    limits = c(0, 9),
    breaks = seq(2, 8, by = 2)
  ) +
  ggtitle("User level") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 5)
  ) +
  labs(color = "NewsGuard Rating")

domain_ng_w <- 
  pulse %>% 
  ggplot(aes(x = pageview_avg, y = pageview_var)) + 
  geom_point(data = filter(pulse, is.na(Score)),
             aes(x = pageview_avg, y = pageview_var), 
             color = "grey", alpha = 0.6) +
  geom_point(data = filter(pulse, !is.na(Score)),
             aes(x = pageview_avg, y = pageview_var, 
                 color = Score), alpha = 0.8) +
  scale_color_gradient() +
  theme_bw() +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(
    "Mean partisanship",
    limits = c(1, 7),
    breaks = seq(1,7, by = 1),
    labels = c("Strong\nDemocrat", "", "", "Independent",
               "", "", "Strong\nRepublican")
  ) + 
  scale_y_continuous(
    "Partisanship variance",
    limits = c(0, 9),
    breaks = seq(2, 8, by = 2)
  ) +  ggtitle("Pageview level") +
  labs(color = "NewsGuard\nscore")

legend <- get_legend(domain_ng_w)
domain_ng_w <- domain_ng_w + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 5)
  )
plot_grid(
  ggarrange(domain_ng, domain_ng_w, ncol = 2, widths = c(1,1)),
  legend, rel_widths = c(10,1)
)

ggsave("fig2_slant_vs_variance_pane_ng_unweighted.pdf", 
       width = 190, height = 90, units = "mm")


 # Figure 3 
variance_ng <- 
  pulse %>%
  ggplot(aes(x = visitor_var, y = Score, 
             color = factor(is_conservative_visitor))) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(
    "Partisanship variance",
    limits = c(0, 8),
    breaks = seq(2, 8, by = 2)
  ) +
  scale_y_continuous(
    "NewsGuard Score",
    breaks = seq(0,100, by = 10),
    limits = c(0,100)
  ) + 
  theme_bw() +
  ggtitle("User level") +
  scale_color_manual(values = c("#113285", "#E83015")) +
  theme(legend.position = "none") 

w_variance_ng <- 
  pulse %>% 
  ggplot(aes(x = pageview_var, y = Score, 
             color = factor(is_conservative_pageview))) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(
    "Partisanship variance",
    limits = c(0, 8.5),
    breaks = seq(2, 8, by = 2)
  ) +
  scale_y_continuous(
    "NewsGuard Score",
    breaks = seq(0,100, by = 10),
    limits = c(0,100)
  ) + 
  ggtitle("Pageview level") +
  scale_color_manual(
    values = c("#113285", "#E83015"),
    labels = c("Democrat", "Republican")
  ) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6)
  ) 

legend <- get_legend(w_variance_ng)
w_variance_ng <- w_variance_ng + theme(legend.position = "none")
plot_grid(
  ggarrange(variance_ng, w_variance_ng, ncol = 2, widths = c(1,1)),
  legend, rel_widths = c(10,1)
)

ggsave("fig3_variance_quality_slant.pdf", 
       width = 220, height = 90, units = "mm")

 # Figure 7 
ng %>% 
  filter(!is.na(Score)) %>%
  mutate(is_good = ifelse(Score >= 60, "Trustworthy", "Untrustworthy") %>% 
           factor(levels = c("Trustworthy", "Untrustworthy"))) %>% 
  ggplot(aes(x = Score, color = is_good, fill = is_good)) +
  geom_histogram(binwidth = 5, alpha = 0.5) + 
  theme_bw() +
  xlab("NewsGuard reliability scores") + 
  ylab("Count") +
  scale_color_manual(
    values = c("#1b813e", "#1c1c1c"),
  ) +
  scale_fill_manual(
    values = c("#1b813e", "white"),
  ) +
  scale_x_continuous(breaks = seq(0,100, by = 10)) + 
  geom_vline(xintercept = mean(ng$Score, na.rm = T), 
             linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, color = "black", size = 0.4) + 
  theme(legend.title = element_blank())
ggsave("fig8_ng_distribution.pdf", 
       width = 150, height = 90, units = "mm")
 