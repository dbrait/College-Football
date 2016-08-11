library(rvest) #web scraping
library(stringr) #string processing

base_url <- "http://www.247sports.com/Season/%i-Football/CompositeTeamRankings"

year_list <- seq(from = 2005, to = 2016, by = 1)
conf_list <- c("ACC", "Big-12", "AAC", "Big-Ten", "C-USA", "IND", "MAC", "M-West", "Pac-12", "SEC", "SBC")

#initialize matrix to append teams to
recruit_matrix <- matrix("", nrow=1, ncol=6)
for(year in year_list) {
  year_url <- sprintf(base_url, year)
  year_url <- str_c(year_url, "?Conference=%s")
  for(conf in conf_list){
    conf_url <- sprintf(year_url, conf)
    conf_values <- read_html(conf_url) %>%
      html_nodes(".team_itm span, .playerinfo_blk a") %>% #from inspector gadget tool
      html_text %>%
      str_trim %>%
      matrix(ncol = 4, byrow=T) %>%
      cbind(conf, year)
    recruit_matrix <- rbind(recruit_matrix, conf_values)
    Sys.sleep(1) #slight wait to not crush 247 servers
  }
}


#remove first empty row
recruit_matrix <- recruit_matrix[-1,]
recruit_df <- data.frame(Team = recruit_matrix[,1],
                         Recruits = as.numeric(str_extract_all(recruit_matrix[,2], "[0-9]+", simplify=T)[,1]),
                         Class_Points = as.numeric(recruit_matrix[,4]),
                         Conference = recruit_matrix[,5],
                         Year = recruit_matrix[,6],
                         stringsAsFactors=F)

write.csv(recruit_df, "247_recruit_rankings_05_16.csv", row.names=F)


#Data validation
library(ggplot2)
library(dplyr)
recruit_df <- read.csv("247_recruit_rankings_05_16.csv", as.is=T)
#spread of recruits in year
ggplot(aes(x=Recruits), data=recruit_df) +
  geom_histogram(binwidth=5, boundary = 0, color="black") +
  theme_bw()

head(arrange(recruit_df, desc(Recruits)))

head(arrange(recruit_df, Recruits))


# how many teams per year
teams_per_year <- recruit_df %>%
  group_by(Year) %>%
  summarize(Teams = n_distinct(Team), Teams_with_recruits = sum(Class_Points > 0))
head(teams_per_year)
