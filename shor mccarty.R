library(tidyverse)
library(lubridate)


#the import step
npat_june_2015 <- read_delim("shor mccarty 1993-2014 state individual legislator scores public June 2015.tab", delim="\t", escape_double=FALSE) ##

npat_june_2015 <- npat_june_2015 %>% mutate(member_id = paste0(st, sprintf("%04d", st_id)))

if (nrow(npat_june_2015) != length(unique(npat_june_2015$member_id))) message("Error:  Duplicate member_id")

npat_master <- npat_june_2015 %>% select(name, party, st, member_id, np_score)

#https://ballotpedia.org/State_legislative_chambers_that_use_multi-member_districts
leg_counts <- read_csv("district numbers.csv")

st_list <-  leg_counts %>% 
  filter(double == TRUE) %>%  
  select(stcd) 

st_list <- map_chr(st_list[[1]], as.character) 

fields <- names(npat_june_2015[6:93])
years <- c(1993:2014)
field_names <- as.tibble(cbind(years, senate_year = fields[1:22], house_year = fields[23:44], senate_districts = fields[45:66], house_districts = fields[67:88]))

gen_chamber <- function(year, chamber="lower") {
  
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
                                                              
  return(chamber)

}

lower <- map_dfr(years, gen_chamber, chamber = "lower")
upper <- map_dfr(years, gen_chamber, chamber = "upper")
overall_mean_scores <- lower %>% group_by(st, year) %>% summarise(mean = mean(np_score)) %>% spread(year, mean)

#error check:  validate a single record for each row in h and s equals 1's in source
source_1_values <- rowSums(npat_june_2015[,6:49], na.rm=TRUE)
if (nrow(lower) + nrow(upper) != sum(source_1_values)) message("Error: Sum of source records does not equal lengths of lower + upper")

# relabel Washington State districts from WA_xxx-0[1 or 2] to WA_xxx
lower <- lower %>% mutate(district = if_else(st == "WA", substr(district, 1, 6), district))

# drop any states that do not appear in st_list, i.e., states with multi-member districts.
lower <- lower %>% filter(st %in% st_list)
upper <- upper %>% filter(st %in% st_list)

double_dists <- lower %>% group_by(district, year) %>% summarise(freq = n()) %>% filter(freq > 1)
lower <- inner_join(lower, double_dists)
upper <- inner_join(upper, double_dists) #presumes district numbers of upper house and lower house are identical

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
state_means_and_range <- state_group %>% summarise(mean=mean(np_score), max=max(np_score), min=min(np_score))
state_means_and_range <- inner_join(split_districts, state_means_and_range, by=c("district","year"))
state_means_and_range<- state_means_and_range %>% mutate(range = max - min)

anal <- state_means_and_range %>% group_by(state, year, split, split_label) %>% summarise(range = mean(range), count = n()) %>% mutate(ss = if_else(split==TRUE, paste0(state,"-","2 Party Dists"), paste0(state,"-","1 Party Dists")))

mmd_plot <- ggplot(data=anal, aes(x=year, y=range, color=ss)) +
  geom_line(aes(linetype=split_label), show.legend=TRUE) + facet_wrap(~state)
mmd_plot
save.image(file = "State Means and Ranges.png")

#use r for tables http://blogs.reed.edu/ed-tech/2015/10/creating-nice-tables-using-r-markdown/
