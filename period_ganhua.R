# Clear everything
rm(list=ls())
# Read library if you have tidyverse
library(tidyverse)

###### base r
period_lineplot <- function(d_now = 32, d_start = 0, d_end = 40, period_length = 28, on_time = 150){
  
  d <- d_now
  t <- period_length
  a <- on_time
  b <- a/t
  f <- function(x){dgamma(x, shape = a, rate = b)}
  S <- function(x){1 - pgamma(x, shape = a, rate = b)}
  
  f_cond <- function(x){ifelse(x > d, f(x) / S(d), 0)}
  
  curve(f(x), from = d_start, to = d_end, n = 1001, col = "gray40", lwd = 3, ylim = c(0, 1), xlab = "Day", ylab = "Probability")
  curve(f_cond(x), from = d_start, to = d_end, n = 1001, col = "#FF8090", add = TRUE, lwd = 3)
  title(main = "When will your period start?")
  legend("topleft",
         legend = c("Initial prediction", "Current prediction"),
         col = c("gray40", "#FF8090"),
         lwd = 3,
         bty = "n")
}
######

###### ggplot/tidyverse
period_lineplot <- function(d_now = 32, d_start = 0, d_end = 40, period_length = 28, on_time = 150){
  
  d <- d_now
  t <- period_length
  a <- on_time
  b <- a/t
  f <- function(x){dgamma(x, shape = a, rate = b)}
  S <- function(x){1 - pgamma(x, shape = a, rate = b)}
  
  f_cond <- function(x){ifelse(x > d, f(x) / S(d), 0)}
  
  # Build data
  df <- data.frame(x = seq(d_start, d_end, length.out = 1001))
  df %>%
    mutate(f = f(x), f_cond = f_cond(x)) %>%
    pivot_longer(cols = c(f, f_cond), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = x, y = value, color = variable)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("grey40", "#FF8090"),
                       labels = c("Initial prediction", "Current prediction")) +
    ylim(0, 1) +
    labs(
      title = "When will your period start?",
      x = "Day",
      y = "Probability"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5))
}
######

###### Display results
period_lineplot(d_now = 32, d_start = 0, d_end = 40, period_length = 28, on_time = 200)

###### base r
period_barplot <- function(d_start = 0, d_end = 40, period_length = 28, on_time = 150){
  
  t <- period_length
  a <- on_time
  b <- a/t
  f <- function(x) dgamma(x, shape = a, rate = b)
  S <- function(x) 1 - pgamma(x, shape = a, rate = b)
  
  df <- data.frame(day = c(rep(0, d_end)), prob_0 = c(rep(0, d_end)), prob_1 = c(rep(0, d_end)))
  for (i in 1:d_end) {
    df[i, 1] <- i
    df[i, 2] <- pgamma(i, shape = a, rate = b) - pgamma(i - 1, shape = a, rate = b)
    df[i, 3] <- df[i, 2] / S(i - 1)
  }
  
  df <- df[df$day >= d_start & df$day <= d_end,]
  bar_df <- rbind(df$prob_0, df$prob_1)
  colnames(bar_df) <- df$day
  
  barplot(
    bar_df,
    beside = TRUE,
    col = c("gray55", "#FF8090"),
    border = NA,
    ylim = c(0, 1),
    names.arg = df$day,
    xlab = "Day",
    ylab = "Probability",
    main = "What day will your period start?"
  )
  
  legend(
    "topleft",
    legend = c("Initial prediction", "Current day prediction"),
    fill = c("gray55", "#FF8090"),
    border = NA,
    bty = "n"
  )
}
######

###### ggplot/tidyverse
period_barplot <- function(d_start = 0, d_end = 40, period_length = 28, on_time = 150){
  
  t <- period_length
  a <- on_time
  b <- a/t
  f <- function(x) dgamma(x, shape = a, rate = b)
  S <- function(x) 1 - pgamma(x, shape = a, rate = b)

  df <- data.frame(day = c(rep(0, d_end)), prob_0 = c(rep(0, d_end)), prob_1 = c(rep(0, d_end)))
  for (i in 1:d_end) {
    df[i, 1] <- i
    df[i, 2] <- pgamma(i, shape = a, rate = b) - pgamma(i - 1, shape = a, rate = b)
    df[i, 3] <- df[i, 2] / S(i - 1)
  }
  
  df %>%
    filter(day %in% d_start:d_end) %>%
    pivot_longer(cols = c(prob_0, prob_1), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = day, y = value, fill = variable)) +
    scale_fill_manual(values = c("grey55", "#FF8090"),
                      labels = c("Initial prediction", "Current day prediction")) +
    ylim(0, 1) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Day", y = "Probability", title = "What day will your period start?") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5))
}
######

###### Display results
period_barplot(d_start = 0, d_end = 48, period_length = 33, on_time = 300)
