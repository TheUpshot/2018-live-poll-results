library(tidyverse)
library(ggmap)
library(cowplot)

fnames <- list.files(path = "data")
tmp <- lapply(str_split(fnames, "elections-poll-"), function(x) x[[2]])
states <- lapply(tmp, function(x) substr(x, 1, 2))

dat <- lapply(list.files(path = "data", full.names = TRUE), read_csv)
for (i in 1:length(states)) {
  dat[[i]]$state <- states[[i]]
}

dat <- bind_rows(dat)

plot_demo <- function(col) {
  df <- dat
  df$col <- df[[col]]
  df %>%
    filter(!(col %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>%
    group_by(col, response) %>%
    tally() %>%
    filter(response %in% c("Dem", "Rep")) %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    spread(response, p) %>%
    mutate(diff = Dem - Rep) %>%
    ggplot(aes(fct_rev(col), diff, fill = diff)) +
    geom_col() + 
    coord_flip() + 
    scale_fill_gradient2() + 
    theme_minimal() + 
    labs(title = paste("Early voting by", col), x = "", y = "", fill = "")
}

demos <- c("ager", "educ", "file_race", "gender", "race_eth", "state")

plots <- c()
for (col in demos) {
  plots[[col]] <- plot_demo(col)
}

plot_grid(plotlist = plots, ncol = 2)
