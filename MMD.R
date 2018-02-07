library(tidyverse)
library(lubridate)

#  https://americanlegislatures.com/data/
#  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7ELHW
#  Codebook at https://dataverse.harvard.edu/file.xhtml;jsessionid=1c69c8124a4cfab1d433500079ac?fileId=2690452&version=RELEASED&version=.0


source_data <- "shor mccarty 1993-2014 state individual legislator scores public June 2015.tab" ##

#the import step
npat_june_2015 <- source_data %>% 
        read_delim(delim="\t", escape_double=FALSE) %>% 
        mutate(member_id = paste0(st, sprintf("%04d", st_id)))

if (nrow(npat_june_2015) != length(unique(npat_june_2015$member_id))) message("Error:  There is a duplicate member_id somewhere in npat_june_2015")

npat_master <- npat_june_2015 %>% 
        select(name, party, st, member_id, np_score)

#https://ballotpedia.org/State_legislative_chambers_that_use_multi-member_districts

leg_counts <- read_csv("district numbers.csv")

state_legislatures <- leg_counts %>% 
  filter(double == TRUE) %>%  
  select(stcd) 

state_legislatures <- map_chr(state_legislatures[[1]], as.character) 

fields <- names(npat_june_2015[6:93])
years <- c(1993:2014)
string_sections <- c("senate", "house", "sdistrict", "hdistrict")

make_field_name <- function(year, field_names, string_section) {
        
        s <- paste0(string_section, as.character(year))
        x <- str_detect(s, field_names)
        which(x)
}

make_f1 <- map_dbl(years, make_field_name, field_names = fields, string_section = "sdistrict")
make_f2 <- map_dbl(years, make_field_name, field_names = fields, string_section = "hdistrict")
make_f3 <- map_dbl(years, make_field_name, field_names = fields, string_section = "senate")
make_f4 <- map_dbl(years, make_field_name, field_names = fields, string_section = "house")

field_names <- bind_cols(year = years, senate_year = fields[make_f1], house_year = fields[make_f2], senate_districts = fields[make_f3], house_districts = fields[make_f4])

generate_chamber_table <- function(year, chamber="lower") {
  
  if (chamber == "lower") {
       district_field <- paste0("hdistrict", year)
       district_flag <- paste0("house", year)
  } else if (chamber == "upper") {
      district_field <- paste0("sdistrict", year)
      district_flag <- paste0("senate", year)
  } else {
      stop("what is the chamber?")
  }
        
  chamber <- npat_june_2015 %>% 
        drop_na(!!district_flag) %>% 
        rename(district = !!district_field) %>% 
        mutate(district = paste0(st,"_",district)) %>% 
        select(member_id, party, st, district, np_score) %>% 
        mutate(year = year)
                                                             
}

lower <- map_dfr(years, generate_chamber_table, chamber = "lower")
upper <- map_dfr(years, generate_chamber_table, chamber = "upper")

overall_mean_scores <- lower %>% 
        group_by(st, year) %>% 
        summarise(mean = mean(np_score)) %>% 
        spread(year, mean)

# relabel Washington State and Idaho districts from WA_xxx-0[1 or 2] to WA_xxx
lower <- lower %>% 
        mutate(district = if_else(st == "WA" | st == "ID", substr(district, 1, 6), district))

# include only states appearing in the state_legislatures vector.
lower <- lower %>% filter(st %in% state_legislatures)
upper <- upper %>% filter(st %in% state_legislatures)

double_dists <- lower %>% 
        group_by(district, year) %>% 
        summarise(freq = n()) %>% 
        filter(freq > 1)

lower <- inner_join(lower, double_dists)

#split districts are those with at least one D and one R in the same term
split_districts <- lower %>% group_by(district, year) %>% 
  count(party) %>% 
  spread(party, n) %>% 
  mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
  mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
  mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
  mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE)) %>%
  mutate(split_label = if_else(split==TRUE, "2 Party Dists", "1 Party Dists")) %>%
  mutate(state = substr(district,1,2))

#compute means by year, state, and split status
state_group <- group_by(lower, district, year)

state_means_and_range <- state_group %>% 
        summarise(mean=mean(np_score), max=max(np_score), min=min(np_score))

state_means_and_range <- inner_join(split_districts, state_means_and_range, by=c("district","year"))

state_means_and_range <- inner_join(leg_counts, state_means_and_range, by=c("stcd"="state"))

state_means_and_range<- state_means_and_range %>% 
        mutate(range = max - min) %>% 
        select(-one_of(c("upper", "upper_term", "lower_term"))) 

anal <- state_means_and_range %>% 
        group_by(stcd, year) %>% 
        mutate(denom = n()) %>% 
        
        group_by(split, split_label, add=TRUE) %>% 
        mutate(range = mean(range), count = n()) %>%
        mutate(spct = count / denom) %>% 
        mutate(ss = if_else(split==TRUE, 
                            paste0(stcd,"-","2 Party Dists"), 
                            paste0(stcd,"-","1 Party Dists")))

#group_by(df, group) %>% mutate(percent = value/sum(value))

(mmd_plot <- ggplot(data=anal, aes(x=year, y=range, color=ss)) +
        ylab("Difference between Max and Min Average NPAT Score") +
        geom_line(aes(linetype=split_label), show.legend=FALSE) + 
        facet_wrap(~stcd)+
        theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))
        )

save.image(file = "State Means and Ranges.png")

(mmd_plot <- ggplot(data=anal, aes(x=year, y=spct, color=ss)) +
        ylab("Percentage of All Districts") +
        geom_line(aes(linetype=split_label), show.legend=FALSE) + 
        facet_wrap(~stcd) +
        theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))
        )

save.image(file = "Proportion of Districts with One Party v. Two Party")

