
library(tidyverse)

#the import step
npat_june_2015 <- read_delim("shor mccarty 1993-2014 state individual legislator scores public June 2015.tab", delim="\t", escape_double=FALSE) ##

npat_june_2015 <- npat_june_2015 %>% mutate(member_id = paste0(st, sprintf("%04d", st_id)))

if (nrow(npat_june_2015) != length(unique(npat_june_2015$member_id))) message("Error:  Duplicate member_id")

npat_master <- npat_june_2015 %>% select(name, party, st, member_id, np_score)

#https://ballotpedia.org/State_legislative_chambers_that_use_multi-member_districts
leg_counts <- read_csv("district numbers.csv")

st_list <-  leg_counts %>% 
  filter(double == TRUE) %>%  ####
        
  select(stcd) 

st_list <- map_chr(st_list[[1]], as.character) #

fields <- names(npat_june_2015[6:93])
years <- c(1993:2014)
field_names <- cbind(years, senate_year = fields[1:22], house_year = fields[23:44], senate_districts = fields[45:66], house_districts = fields[67:88])

genh <- function(y, chamber="lower") {
  
  if (chamber == "lower") {
       district_field <- paste0("hdistrict", y)
       district_flag <- paste0("house", as.character(y))
  } else if (chamber == "upper") {
      district_field <- paste0("sdistrict", y)
      district_flag <- paste0("senate", y)
  } else {
      stop("what is the chamber?")
  }

  
  h <- npat_june_2015 %>% 
    drop_na(!!district_flag) %>% 
    rename(district = !!district_field) %>% 
    mutate(district = paste0(st,"_",district)) %>% 
    select(member_id, party, st, district, np_score) %>% 
    mutate(year = y)
                                                              
  return(h)

}

h <- map_dfr(years, genh, chamber = "lower")
s <- map_dfr(years, genh, chamber = "upper")
m <- h %>% group_by(st, year) %>% summarise(mean = mean(np_score))
m <- m %>% spread(year, mean)
#validate a single record for each row in h and s equals 1's in source
source_1_values <- rowSums(npat_june_2015[,6:49], na.rm=TRUE)
if (nrow(h) + nrow(s) != sum(source_1_values)) message("Error: Sum of source records does not equal lengths of h + s")


# relabel Washington State districts from WA_xxx-0[1 or 2] to WA_xxx

h <- h %>% mutate(district = if_else(st == "WA", substr(district, 1, 6), district))

h <- h %>% filter(st %in% st_list)
s <- s %>% filter(st %in% st_list)

double_dists <- h %>% group_by(district, year) %>% summarise(freq = n()) %>% filter(freq > 1)

h <- inner_join(h, double_dists)
s <- inner_join(s, double_dists)

splits <- h %>% group_by(district, year) %>% 
  count(party) %>% 
  spread(party, n) %>% 
  mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
  mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
  mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
  mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE))

#1  Increase or decrease in split voting over the years
#2  NPAT scores

splits <- mutate(splits, state = substr(district,1,2))

g <- group_by(h,district,year)
gg <- g %>% summarise(mean=mean(np_score), max=max(np_score), min=min(np_score))
i <- inner_join(splits, gg, by=c("district","year"))
i <- i %>% mutate(range = max - min)

anal <- group_by(i, state, year, split)
anal <- summarise(anal, range = mean(range))
anal <- anal %>% mutate(ss = paste0(state,"-",split))

#anal <- anal %>% spread(year,range)

ggplot(data=anal, aes(x=year, y=range, color=ss)) +
  geom_line(aes(linetype=split)) + facet_wrap(~state)
