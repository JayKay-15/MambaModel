if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, shiny,
               nbastatR, ggimage, ggrepel, data.table, gt, janitor)
# devtools::install_github("beanumber/teamcolors", force=TRUE)
# library(teamcolors)
rm(list=ls())


#### Twitter Tracking
web_picks <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Website_Picks.xlsx")
web_picks$Date <- as_date(web_picks$Date)

web_picks %>%
    filter(!is.na(Result)) %>%
    mutate(POTD = replace_na(POTD,0)) %>%
    group_by(Date) %>%
    summarise(day_total = sum(Units), 
              day_total_potd = sum(POTD)) %>%
    mutate(cume = cumsum(day_total), 
           cume_potd = cumsum(day_total_potd)) %>%
    ggplot(aes(Date)) +
    geom_line(aes(y=cume, color="All Plays"), size=1) +
    geom_line(aes(y=cume_potd, color="POTD"), size=1) +
    scale_color_manual(name = "", values = c("All Plays" = "#9590FF", "POTD" = "darkblue")) +
    labs(x = "Date", y = "Units", title = "2022 Performance", subtitle = "Since 3/7/2022") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme_bw()






results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
results_book$Date <- as_date(results_book$Date)

#### Current Keys by Bet Type ####
fil <- 30

adriana_spread_line <- results_book %>%
    # filter(Adriana_Spread_Edge >= 1.332 & Date >= max(Date) - fil) %>%
    filter(Adriana_Spread_Edge >= 1.332) %>%
    select(2,111) %>%
    mutate(model = "Spread - Adriana") %>%
    rename(value = Adriana_Spread_Result)

gisele_spread2_line <- results_book %>%
    # filter(Gisele_Spread2_Edge >= 1.747 & Date >= max(Date) - fil) %>%
    filter(Gisele_Spread2_Edge >= 1.747) %>%
    select(2,92) %>%
    mutate(model = "Spread2 - Gisele") %>%
    rename(value = Gisele_Spread2_Result)

kendall_ml_line <- results_book %>%
    # filter(Kendall_ML_Edge >= .076 & Date >= max(Date) - fil) %>%
    filter(ML > 0) %>%
    filter(Kendall_ML_Edge >= .076) %>%
    select(2,83) %>%
    mutate(model = "ML - Kendall") %>%
    rename(value = Kendall_ML_Result)

gisele_over_line <- results_book %>%
    # filter(Gisele_Over_Edge >= 8.719 & Date >= max(Date) - fil) %>%
    filter(Gisele_Over_Edge >= 8.719) %>%
    select(2,94) %>%
    mutate(model = "Over - Gisele") %>%
    rename(value = Gisele_Over_Result)

current_keys_line <- bind_rows(adriana_spread_line, gisele_spread2_line, kendall_ml_line, gisele_over_line)

current_keys_line$model <- factor(current_keys_line$model, 
                                  levels=c("Spread - Adriana", "Spread2 - Gisele", "ML - Kendall", "Over - Gisele"))

current_keys_line %>%
    filter(Date >= max(Date) - fil) %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "Current Keys", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    scale_color_discrete(name="Model",
                         labels=c("Spread - Combo (Adriana)", "Spread - KNN (Gisele)", 
                                  "ML - Simple (Kendall)", "Over - KNN (Gisele")) +
    theme_bw()



##### Rating Quadrants ####
logos <- teamcolors %>%
    filter(league == "nba") %>%
    select(1,3,11) %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

bound.label <- 115
df.text <- data.frame(lab.text = c("+Off, +Def", "+Off, -Def", "-Off, -Def", "-Off, +Def"),
                      x = c(bound.label, bound.label, bound.label-15, bound.label-15),
                      y = c(bound.label-15, bound.label, bound.label, bound.label-15))

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618

season_viz %>%
    ggplot(aes(ORtg, DRtg)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$DRtg), linetype = "dashed", alpha=.5) +
    geom_vline(xintercept=median(season_final_wt$ORtg), linetype = "dashed", alpha=.5) +
    labs(x = "Offensive Rating (points scored per 100 possessions)",
         y = "Defensive Rating (points allowed per 100 possessions)",
         title = "Offensive vs Defensive Rating",
         subtitle = "Updated 3/19/2022") +
    # geom_text_repel(aes(label=name),
    #                 force=1, point.padding=0,
    #                 segment.size=0.1) +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=6, fontface=2) +
    theme_bw()






