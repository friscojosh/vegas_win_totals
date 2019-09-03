######################################################################
### Analysis of betting market NFL win totals. We'll use them to
### attempt to predict team wins.
######################################################################

library(tidyverse)
library(ggridges)
library(theme538)
library(ggbeeswarm)
library(ggjoy)
library(rvest)
library(httr)

### First we need to grab current NFL win totals for 2019
### let's hack some fucking html

### Data from BetOnline
url <- "https://mobile.betonline.ag/sports/futures-and-props/nfl-team-wins"
betonline <- GET(url)
new <- html_nodes(content(betonline), xpath = '//*[@id="moneyline-contest-content"]/ul/li/div/ul/li/div') %>%
   html_text() %>%
   ### let's just create a CSV
   str_remove_all("\n") %>%
   str_replace_all("Regular Season Wins ", '\n') %>%
   str_replace_all("2019 Regular Season ", " ") %>%
   str_replace_all("More InfoCloseXMore Info", ",") %>%
   str_replace_all("\\)", "),") %>%
   str_replace_all("½", '.5')
## read in the CSV we created with the parser
betonline_odds <- read_csv(new, col_names = FALSE) %>%
   select(-'X4')
### beter column names
colnames(betonline_odds) <- c("Team", "odds1", "odds2")
### extract the over and under totals and the moneylines
betonline_odds <- betonline_odds %>%
   ### extract the win totals
   mutate(total = str_extract(odds1, "(?<= )\\d+\\.*\\d*"),
          ### extract the moneline odds
          under_odds = as.numeric(ifelse(str_detect(odds1, "Under"), str_extract(odds1, "(?<=\\().+?(?=\\))"),
                                  ifelse(str_detect(odds2, "Under"), str_extract(odds2, "(?<=\\().+?(?=\\))"), 0))),
          over_odds = as.numeric(ifelse(str_detect(odds1, "Over"), str_extract(odds1, "(?<=\\().+?(?=\\))"),
                                 ifelse(str_detect(odds2, "Over"), str_extract(odds2, "(?<=\\().+?(?=\\))"), 0)))) %>%
   ### clean up the columns
   select(-odds1, -odds2)

### load win totals from 4 other books that I hand collected by scraping reeks of effort
misc_odds <- read_csv("data/win_totals_2019.csv")

win_totals_19 <- betonline_odds %>%
   select(team = Team, total) %>%
   mutate(book = "betonline",
          total = as.numeric(total)) %>%
   bind_rows(misc_odds) %>%
   na.omit() %>%
   group_by(team) %>%
   ### take the average but round to half wins
   summarise(total = as.integer(mean(total) * 2) / 2 )

### data from sportshistoryodds.com. Easiest way to view the data is on PFR here:
### https://www.pro-football-reference.com/years/2018/preseason_odds.htm
soh_totals <- read_csv("data/sportsoddshistory.csv") %>%
   ### extract the actual wins from the result column
   mutate(actual_wins = as.numeric(str_extract(result, '^[[:digit:]]+')),
          over_under = ifelse(actual_wins > total, "over",
                              ifelse(actual_wins < total, "under", "push")))

soh_totals %>%
   group_by(total) %>%
   summarise(actual_wins = mean(actual_wins),
             count = n())

### check Vegas calibration
soh_totals %>%
   group_by(total) %>%
   summarise(actual_wins = mean(actual_wins),
             count = n()) %>%
   ggplot(aes(x = total, y = actual_wins, size = count)) +
      geom_point() + theme_538 +
   geom_smooth(method = "lm", formula = y ~ x + 0, linetype = 5, color = "black", alpha = .5, se = FALSE) +
   scale_x_continuous(limits = c(0,16), breaks = 1:16) +
   scale_y_continuous(limits = c(0,16), breaks = 1:16) +
   coord_fixed(ratio = 1) +
   labs(title = "Vegas underestimates low win teams", subtitle = "Win total data from 1989 - 2018", caption = "Data courtesy of sportsoddshistory and Greg Guglielmo", x = "Vegas win total", y = "Actual wins") +
   theme(legend.position = "none")

### this is interesting, and it implies that simply using Vegas win totals will
### underestimate low win total teams, and - generally - teams projected with a
### half point win total below 8.5.

### generate some expected win values based on Vegas totals
soh_totals %>%
   group_by(total) %>%
   summarise(actual_wins = mean(actual_wins),
             count = n())

### we'll use these as our actual predicted wins.
vegas_adjusted_wins <- soh_totals %>%
   group_by(total) %>%
   summarise(actual_wins = mean(actual_wins),
             count = n()) %>%
   group_by(total) %>%
   ### take the average but round to half wins
   summarise(actual_wins = as.integer(mean(actual_wins) * 2) / 2 )

### let's plot all the points without aggregating to see how that looks...
soh_totals %>%
   ggplot(aes(x = total, y = actual_wins)) +
   geom_point() + theme_538 +
   scale_x_continuous(limits = c(0,16), breaks = 1:16) +
   scale_y_continuous(limits = c(0,16), breaks = 1:16)

### does the super bowl moneyline tell us anything?
soh_totals %>%
   ggplot(aes(x = actual_wins, y = sb_ml)) +
   geom_point() + theme_538
### not really.

### out of curioisty, do the predicted win total and SB odds together help?
model <- lm(data = soh_totals, actual_wins ~ total + sb_ml)
summary(model)
### nope

soh_totals %>%
   filter(total >= 4.5,
          total <= 12,
          over_under != "push") %>%
   ggplot(aes(x = actual_wins, y = as.factor(total), color = over_under, fill = over_under)) +
   stat_binline(stat = "binline", binwidth = 1,
                        draw_baseline = FALSE, alpha = .5, color = '#A9A9A9', panel_scaling = TRUE, pad = TRUE,
                vline_size = 10, vline_color = "black", vline_alpha = 1) +
   theme_538 +
   scale_fill_manual(values = c("#F83572", "#36B5BD"), labels = c("over", "under/push")) +
   scale_colour_manual(values = alpha(c("#F83572", "#36B5BD"), .3)) +
   scale_x_continuous(limits = c(-1,17), breaks = 0:16) +
   labs(title = "NFL wins by Vegas win total", x = "Actual wins", y = "Vegas win total") +
   theme(legend.position = "none")

### how often do overs and unders hit by win total
soh_totals %>%
   group_by(over_under, total) %>%
   summarize(count = n()) %>%
   group_by(total) %>%
   mutate(pct = count / sum(count))

### let's see how often the over hits for each half win
soh_totals %>%
   mutate(half_win = total %% 1) %>%
   filter(half_win == .5) %>%
   group_by(over_under, total) %>%
   summarize(count = n()) %>%
   group_by(total) %>%
   mutate(pct = count / sum(count))

### make a line chart
half_wins %>%
   filter(total > 3.5) %>%
   ggplot(aes(x = total, y = pct, color = over_under, size = count)) +
   geom_line(size = 1) +
   geom_point() +
   geom_hline(yintercept = 0.5, linetype = 5) +
   theme_538 +
   scale_x_continuous(limits = c(3.5,12.5), breaks = 3.5:12.5)


soh_totals %>%
   mutate(half_win = total %% 1) %>%
   filter(half_win == .5,
          total > 3.5,
          total <= 8.5) %>%
   group_by(over_under) %>%
   summarize(won = n()) %>%
   mutate(lost = abs(won - sum(won)))

###
prop.test(x = c(175, 150), n = c(375, 375), alternative = "greater", correct = TRUE)
qchisq(0.950, 1)

### Ok so there is a systematic undervaluing of a subset of NFL teams.
### To find out if this is exploitable though, we need to know the
### cost to bet, or vigorish. Let's load some historical moneyline
### data.

vigs <- read_csv("data/ELDO_VegasWinTotals_2002-2018.csv") %>%
   na.omit() %>%
   mutate(implied_probability = round(ifelse(over_odds < 0, (-1 * (over_odds)) / (-1 * (over_odds) + 100),
                                       100 / (over_odds + 100)),2 ),
          over_binary = ifelse(over_under == "Over", 1,
                               ifelse(over_under == "Under", 0, -1)))


### let's see if there is anything to exploit
all_over_favored <- vigs %>%
   na.omit() %>%
   filter(over_odds < 0,
          under_odds > 0) %>%
   mutate(favored = "over",
          odds = over_odds)

all_under_favored <- vigs %>%
   na.omit() %>%
   filter(over_odds > 0,
          under_odds < 0) %>%
   mutate(favored = "under",
          odds = under_odds)

all_joined_favored <- all_over_favored %>%
   bind_rows(all_under_favored)

all_joined_favored %>%
   filter(over_binary != -1) %>%
   group_by(favored, win_total) %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n()) %>%
   mutate(moneyline = ifelse(implied_probability > 0.5, ((-1 * implied_probability * 100) / (100 - implied_probability * 100)) * 100,
                                  ifelse(implied_probability < 0.5, ((100 - (implied_probability * 100)) / (implied_probability * 100)) * 100,
                                         100)))

### filter to just half win totals between 5.5 and 8.5
all_joined_favored %>%
   mutate(half_win = win_total %% 1) %>%
   filter(over_binary != -1,
          half_win == .5,
          win_total < 9,
          win_total > 5) %>%
   group_by(favored, win_total) %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n()) %>%
   mutate(moneyline = ifelse(implied_probability > 0.5, ((-1 * implied_probability * 100) / (100 - implied_probability * 100)) * 100,
                                  ifelse(implied_probability < 0.5, ((100 - (implied_probability * 100)) / (implied_probability * 100)) * 100,
                                         100)))

### if we blindedly bet on the over for all bets, would this be +EV?
all_joined_favored %>%
   mutate(half_win = win_total %% 1) %>%
   filter(over_binary != -1,
          half_win == .5,
          win_total < 9,
          win_total > 5) %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n())
### no, it would be slight negative EV


### if we remove the 7.5 win totals, are these bets positive EV?
all_joined_favored %>%
   mutate(half_win = win_total %% 1) %>%
   filter(over_binary != -1,
          half_win == .5,
          win_total < 9,
          win_total > 5,
          win_total != 7.5) %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n())
### yes

### what about when the moneyline is positive and Vegas thinks the under is favored?
### Are those bets +EV?
all_joined_favored %>%
   mutate(half_win = win_total %% 1) %>%
   filter(over_binary != -1,
          half_win == .5,
          favored == "under") %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n())
### Yes
### So if you had bet $100 on each of these totals where the under was favored
### by the moneyline you would have made money.

### what if we blindly bet the over when Vegas thinks the over is favored?
### Is that +EV?
all_joined_favored %>%
   mutate(half_win = win_total %% 1) %>%
   filter(over_binary != -1,
          half_win == .5,
          favored == "over") %>%
   summarize(implied_probability = mean(implied_probability),
             over_pct = mean(over_binary),
             count = n())
### No, this is a losing strategy

### Ok that was fun, let's do what we came here to do...

win_totals_19 <- win_totals_19 %>%
   left_join(vegas_adjusted_wins, by = c("total"))
