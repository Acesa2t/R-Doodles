#Using a simplified version of the TBI prevalence files (NOT for manuscript use) to practice using interactive data visualization tools

rm(list = ls(all.names = TRUE))

library(tidyverse)
library(plotly)
library(gganimate)
library(png)
library(gifski)

tbi_2001 <- readRDS("estimates_2001_v4.rds")
tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)

grouping_function <- function(df){
  
  tbi_data <- merge(df, tb_inc, by = "ISO3")
  # Less bands
  tbi_data_bands <- tbi_data%>%
    mutate(inc_band_per100k = ifelse(
      e_inc_100k < 50, "0-49 per 100,000 persons", ifelse(
        e_inc_100k >= 50 & e_inc_100k < 100, "50-99 per 100,000 persons", ifelse(
          e_inc_100k >= 100 & e_inc_100k < 200, "100-199 per 100,000 persons",ifelse(
            e_inc_100k >= 200 & e_inc_100k <= 1000, "200+ per 100,000 persons", "No value")))))
  
  tbi_data_bands$inc_band_per100k <- factor(tbi_data_bands$inc_band_per100k,
                                            levels = c("0-49 per 100,000 persons",
                                                       "50-99 per 100,000 persons",
                                                       "100-199 per 100,000 persons",
                                                       "200+ per 100,000 persons"))
  
  tbi_age_groups <- tbi_data_bands%>%
    mutate(AGE_BANDS = ifelse(
      AGEP < 15, "0-14", ifelse(
        AGEP >= 15 & AGEP < 35, "15-34", ifelse(
          AGEP >= 35 & AGEP < 55, "35-54",ifelse(
            AGEP >= 55 & AGEP < 75, "55-74",ifelse(
              AGEP >= 75 & AGEP <= 200, "75+", "No value"))))),
      AARP = YARP-YOBP)
  
  
  
  tbi_graph_aarp <- tbi_age_groups%>%
    mutate(AARP_BANDS = ifelse(
      AARP < 15, "AARP 0-14", ifelse(
        AARP >= 15 & AARP < 35, "AARP 15-34", ifelse(
          AARP >= 35 & AARP < 55, "AARP 35-54",ifelse(
            AARP >= 55 & AARP <= 75, "AARP 55-74", ifelse(
              AARP >= 75 & AARP <= 200, "AARP 75+", "No value"))))))%>%
    mutate(BPLP = recode(BPLP, `China, People's Republic of`="China"))%>%
    mutate(BPLP = ifelse(str_detect(BPLP, "Hong Kong"), "Hong Kong", 
                         ifelse(str_detect(BPLP, "United States"), "United States of America", BPLP)))
  
  tbi_graph_aarp
}

tbi_2001_grouped <- grouping_function(tbi_2001)

filter_vec <- tbi_2001_grouped%>%
  group_by(BPLP)%>%
  summarize(NUMP = sum(NUMP))%>%
  arrange(desc(NUMP))%>%
  slice_head(n=10)


tbi_top10 <- tbi_2001_grouped%>%filter(BPLP%in%c(filter_vec$BPLP))

tbi_top10_grouped <- tbi_top10%>%group_by(BPLP,YARP)%>%summarize(NUMP = sum(NUMP))
tbi_top10_grouped[tbi_top10_grouped=="United States of America"] <- "United States"
tbi_top10_grouped$YARP <- as.Date(paste(tbi_top10_grouped$YARP, 6, 1, sep = "-"))

  
  
# Each individual country of origin appears over time
gganimation_plot <- ggplot(tbi_top10_grouped, aes(x = YARP, y = NUMP, color = BPLP))+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 10), se = FALSE)+
  labs(x = "Year of Immigration to Canada",
       y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
       color = "Country of Origin",
       title = str_c("Number from Top 10 Most Common Countries of Origin \nLiving in Canada", " in 2001"))+
  scale_y_continuous(limits = c(0,40000), breaks = round(seq(0, 40000, by = 5000),0))+
  theme_bw()+
  theme(text = element_text(size = 13, face = "bold"))+
  theme(axis.text.x = element_text(angle = 70))+
  theme(axis.text = element_text(size = 12))+
  theme(plot.title = element_text(size=13))+
  theme(legend.position="none")+
  facet_wrap(.~BPLP)


animation2 <- gganimation_plot +
  transition_states(BPLP,
                    transition_length = 2,
                    state_length = 1)+
  enter_appear()+
  exit_fade()

anim_save(animation = animation2, "immigration_coo_appears_2001.gif")


# Population appears over time
gganimation_plot2 <- ggplot(tbi_top10_grouped, aes(x = YARP, y = NUMP, color = BPLP))+
  geom_line()+
  labs(x = "Year of Immigration to Canada",
       y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
       color = "Country of Origin",
       title = str_c("Number from Top 10 Most Common Countries of Origin \nLiving in Canada", " in 2001"))+
  scale_y_continuous(limits = c(0,40000), breaks = round(seq(0, 40000, by = 5000),0))+
  theme_bw()+
  theme(text = element_text(size = 13, face = "bold"))+
  theme(axis.text.x = element_text(angle = 70))+
  theme(axis.text = element_text(size = 12))+
  theme(plot.title = element_text(size=13))+
  theme(legend.position="none")+
  facet_wrap(.~BPLP)

animation3 <- gganimation_plot2+
  transition_reveal(YARP)

anim_save(animation = animation3, "immigration_pop_appears_2001.gif")



