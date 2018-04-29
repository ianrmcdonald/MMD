
library(tidyverse)
library(lubridate)
library(purrr)
library(stringr)
library(readr)

#  https://americanlegislatures.com/data/
#  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7ELHW
#  Codebook at https://dataverse.harvard.edu/file.xhtml;jsessionid=1c69c8124a4cfab1d433500079ac?fileId=2690452&version=RELEASED&version=.0

COMP_THRESHOLD <- .9

npat_source_data <- "Source Data/shor mccarty 1993-2014 state individual legislator scores public June 2015.tab" 

npat_june_2015 <- npat_source_data %>% 
        read_delim(delim="\t", escape_double=FALSE) %>% 
        mutate(member_id = paste0(st, sprintf("%04d", st_id)))

if (nrow(npat_june_2015) != length(unique(npat_june_2015$member_id))) message("Error:  There is a duplicate member_id somewhere in npat_june_2015")

npat_master <- npat_june_2015 %>% 
        select(name, party, st, member_id, np_score)

#https://ballotpedia.org/State_legislative_chambers_that_use_multi-member_districts

leg_counts <- read_csv("Source Data/district numbers.csv")

state_legislatures <- leg_counts %>% 
        filter(double == TRUE) %>%  
        select(stcd) 

state_legislatures <- map_chr(state_legislatures[[1]], as.character) 

#this segment makes it possible to select years from npat_june_2015 (1993 - 2014)
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

field_names <- bind_cols(year = years, 
                         senate_year = fields[make_f1], 
                         house_year = fields[make_f2], 
                         senate_districts = fields[make_f3], 
                         house_districts = fields[make_f4]
                )

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

npat_lower <- map_dfr(years, generate_chamber_table, chamber = "lower")
npat_upper <- map_dfr(years, generate_chamber_table, chamber = "upper")

overall_mean_scores <- npat_lower %>% 
        group_by(st, year) %>% 
        summarise(mean = mean(np_score)) %>% 
        spread(year, mean)

# relabel Washington State and Idaho districts from WA_xxx-0[1 or 2] to WA_xxx
npat_lower <- npat_lower %>% 
        mutate(district = if_else(st == "WA" | st == "ID", substr(district, 1, 6), district))

# include only states appearing in the state_legislatures vector.
npat_lower <- npat_lower %>% filter(st %in% state_legislatures | st == "OR")
npat_upper <- npat_upper %>% filter(st %in% state_legislatures | st == "OR")

double_dists <- npat_lower %>% 
        group_by(district, year) %>% 
        summarise(freq = n())# %>% 
        #filter(freq > 1)

npat_lower <- inner_join(npat_lower, double_dists)

#split districts are those with at least one D and one R in the same term
split_districts <- npat_lower %>% group_by(district, year) %>% 
        count(party) %>% 
        spread(party, n) %>% 
        mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
        mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
        mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
        mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE)) %>%
        mutate(split_label = if_else(split==TRUE, "2 Party Dists", "1 Party Dists")) %>%
        mutate(state = substr(district,1,2))

#compute means by year, state, and split status
state_group <- npat_lower %>% group_by(district, year)

state_means_and_range <- state_group %>% 
        summarise(mean=mean(np_score), max=max(np_score), min=min(np_score))

state_means_and_range <- inner_join(split_districts, state_means_and_range, 
                                    by=c("district", "year"))

state_means_and_range <- inner_join(leg_counts, state_means_and_range, 
                                    by=c("stcd" = "state"))

state_means_and_range <- state_means_and_range %>% 
        mutate(range = max - min) %>% 
        select(-one_of(c("upper", "upper_term", "lower_term"))) 

state_means_anal <- state_means_and_range %>% 
        group_by(stcd, year) %>% 
        mutate(denom = n()) %>% 
        group_by(split, split_label, add=TRUE) %>% 
        mutate(range = mean(range), count = n()) %>%
        mutate(spct = count / denom) %>% 
        mutate(ss = if_else(split==TRUE, 
                            paste0(stcd,"-","2 Party Dists"), 
                            paste0(stcd,"-","1 Party Dists")))

#group_by(df, group) %>% mutate(percent = value/sum(value))

(mmd_plot <- ggplot(data=state_means_anal, aes(x=year, y=range, color=ss)) +
                ylab("Difference between Max and Min Average NPAT Score") +
                geom_line(aes(linetype=split_label), show.legend=FALSE) + 
                facet_wrap(~stcd)+
                theme(axis.text.x=element_text(color = "black", 
                                               size=11, angle=30, vjust=.8, hjust=0.8))
)

save.image(file = "State Means and Ranges.png")

(mmd_plot <- ggplot(data=state_means_anal, aes(x=year, y=spct, color=ss)) +
                ylab("Percentage of All Districts") +
                geom_line(aes(linetype=split_label), show.legend=FALSE) + 
                facet_wrap(~stcd) +
                theme(axis.text.x=element_text(color = "black", 
                                               size=11, angle=30, vjust=.8, hjust=0.8))
)

save.image(file = "Proportion of Districts with One Party v. Two Party")


#  Create TW data of district ideology for 2002 and 2012 districts

tw_csv_file_2012_lower <- "Source Data/shd_2012_TW_ideology_estimates.csv"

tw_lower_2012 <- read_csv(tw_csv_file_2012_lower, col_types = 
        cols(
                shd_fips = col_character(),
                district = col_character()
        )
)

tw_csv_file_2002_lower <- "Source Data/shd_2002_TW_ideology_estimates_v2.csv"

tw_lower_2002 <- read_csv(tw_csv_file_2002_lower, col_types = 
        cols(
                shd_fips = col_character()
                                                   )
)

tw_csv_file_2012_upper <- "Source Data/ssd_2012_TW_ideology_estimates.csv"

tw_upper_2012 <- read_csv(tw_csv_file_2012_upper, col_types = 
                                  cols(
                                          ssd_fips = col_character(),
                                          district = col_character()
                                  )
)

tw_csv_file_2002_upper <- "Source Data/ssd_2002_TW_ideology_estimates_v2.csv"

tw_upper_2002 <- read_csv(tw_csv_file_2002_upper, col_types = 
                                  cols(
                                          ssd_fips = col_character()
                                  )
)

#state election data
load("Source Data/SLERs1967to2010_2012_05_26.RData")
election_data <- as_tibble(x)
sler_data <- "Source Data/SLERs2011to2012_only_2013_05_14.RData"
load(sler_data)
election_data_2011 <- as_tibble(x)
rm(x)

election_data_col_names <- as_tibble(read_csv("Source Data/dv_names.txt"))
election_data_col_names_2011 <- as_tibble(read_csv("Source Data/dv_names_2011.txt"))
names(election_data) <- election_data_col_names$name
names(election_data_2011) <- election_data_col_names_2011$name

princeton <- read.csv("Source Data/state_legislative_election_results_1971_2016.csv")


# merge them:  Just WA, OR, ID, and AZ for now

# Create a standaridized year and district field in all the tables in this format [ST_NNN]

#npat_lower and #npat_upper are fine:  field = district
npat_lower_WA_OR <- npat_lower %>% 
        filter(st == "WA" | st == "OR" | st == "ID" | st == "AZ") %>% 
        mutate(chamber = "lower")

npat_upper_WA_OR <- npat_upper %>% 
        filter(st == "WA" | st == "OR" | st == "ID" | st == "AZ") %>% 
        mutate(chamber = "upper")

npat_WA_OR <- bind_rows(npat_lower_WA_OR, npat_upper_WA_OR)

#election data:
election_data_WA_OR <- election_data %>% 
        filter(state_cd == "WA" | state_cd == "OR" | state_cd == "ID" | state_cd == "AZ") %>%
        mutate(district = paste0(state_cd,"_",sprintf("%03d", dist_number)))

election_data_WA_OR_2011 <- election_data_2011 %>% 
        filter(state_cd == "WA" | state_cd == "OR" | state_cd == "ID" | state_cd == "AZ") %>%
        mutate(district = paste0(state_cd,"_",sprintf("%03d", dist_number)))

princeton_WA_OR <- princeton %>% filter(State == "WA" | State == "OR" | State == "ID" | State == "AZ") %>%
        filter(Year > 2010) %>%
        mutate(District = if_else(State == "ID" & 
                District == "District 1", "District 1A", as.character(District))) %>% 
                #data error in raw table
        mutate(district = paste0(State,"_",sprintf("%03s", District))) %>%
        mutate(district = if_else(State == "ID" | State == "WA", 
                str_sub(district, 1, str_length(district)-1), district))
#Create a split district table from election data

q1 <- election_data_WA_OR %>% filter(election_winner == TRUE & chamber == 9) %>% 
        mutate(party = if_else(party_code_simplified == 100, "D", if_else(party_code_simplified == 200, "R", "I")))

q1a <- princeton_WA_OR

q2 <- q1 %>% group_by(state_cd, year, district, party)
q2a <- q1a %>% group_by(state_cd = State, year = Year, district, party = Party)

q3 <- q2 %>%count(party) %>% spread(party,n) %>% 
        mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
        mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
        mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
        mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE)) %>%
        mutate(split_label = if_else(split==TRUE, "2 Party Dists", "1 Party Dists")) %>%
        filter(split == TRUE & year >= 1980)

q3a <- q2a %>%count(party) %>% spread(party,n) %>% 
        mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
        #mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
        mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
        mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE)) %>%
        mutate(split_label = if_else(split==TRUE, "2 Party Dists", "1 Party Dists")) #%>%
        #filter(split == TRUE)


#tw data
#with fix for missing WA rows; just going to use the senate rows which should be identical

tw_lower_2002_WA_OR_1 <- tw_lower_2002 %>% 
        filter(abb == "OR" | abb == "ID" | abb == "AZ") %>%
        mutate(district = paste0(abb,"_",sprintf("%03d", shd_fips_num %% 100)))

tw_lower_2002_WA_OR_2 <- tw_upper_2002 %>%  #we use upper here
        filter(abb == "WA") %>%
        mutate(district = paste0(abb,"_",sprintf("%03d", ssd_fips_num %% 100))) %>% 
        rename(shd_fips = ssd_fips, shd_fips_num = ssd_fips_num)

tw_lower_2002_WA_OR <- bind_rows(tw_lower_2002_WA_OR_1, tw_lower_2002_WA_OR_2)
rm(tw_lower_2002_WA_OR_1, tw_lower_2002_WA_OR_2)


tw_lower_2012_WA_OR <- tw_lower_2012 %>% 
        filter(abb == "WA" | abb == "OR" | abb == "ID" | abb == "AZ") %>%
        mutate(district = paste0(abb,"_",sprintf("%03d", as.integer(district))))

tw_upper_2002_WA_OR <- tw_upper_2002 %>% 
        filter(abb == "WA" | abb == "OR" | abb == "ID" | abb == "AZ") %>%
        mutate(district = paste0(abb,"_",sprintf("%03d", ssd_fips_num %% 100)))

tw_upper_2012_WA_OR <- tw_upper_2012 %>%
        filter(abb == "WA" | abb == "OR" | abb == "ID" | abb == "AZ") %>%
        mutate(district = paste0(abb,"_",sprintf("%03d", as.integer(district))))

by_state_2002 <- tw_lower_2002_WA_OR %>% 
        group_by(abb)

by_state_2002 %>% summarise(mean(mrp_mean))


by_state_2012 <- tw_lower_2012_WA_OR %>% 
        group_by(abb)

by_state_2012 %>% summarise(mean(mrp_mean))

# Shor & McCarty Leg. Ideal Point =φ0 + φ1[Tausanovitch & Warshaw Dist. Ideal Point] + φ2[RepublicanP artyDummy] + ε
# (1) EstimatedDistrictIdealPoint =φ0 + φ1[Tausanovitch & Warshaw Dist. Ideal Point] + λφ2 (2) Ideological Distance = |Shor & McCarty Leg. Ideal Point − EstimatedDistrictIdealPoint| (3)


#merge tw and npat data sets

npat_lower_WA_OR_year <- npat_lower_WA_OR %>% 
        filter(year == 2012)

#npat_lower_WA_OR_2005 <- 
        
d_year <- left_join(x=npat_lower_WA_OR_year, y=tw_lower_2012_WA_OR)

d_year <- d_year %>% mutate(pdummy = if_else(party == "R", 1, 0))
d_year_st <- d_year %>% group_by(st)

d_year_lm <- lm(d_year$np_score ~ d_year$mrp_mean + d_year$pdummy)
x <- predict.lm(d_year_lm)
edip <- coef(d_year_lm)[1] + coef(d_year_lm)[2] * d_year$mrp_mean + .5 * coef(d_year_lm)[3]
id <- d_year$np_score - edip
qplot(d_year_st$mrp_mean, id, colour = d_year_st$party)
qplot(data=d_year, mrp_mean, np_score, colour = party) + facet_wrap(~st)

#GUP
suppressWarnings(
district_heterogeneity <- read_table2("Source Data/GUP_merged public and legislators.tab", col_names = TRUE, col_types = cols( st = col_character(), year = col_double(),sld = col_character(), party = col_character(), pred.np = col_double(),het = col_double()))
) 

## TABLE 1 Post position and Senate elections in WA and ID

tbl1 <- election_data_WA_OR %>% 
        #filter(state_cd == "WA") %>% 
        select(district, year, votes_cast_dem, votes_cast_rep, votes_cast_other, 
               votes_cast_total, incumbency_dummy, party_code_simplified, chamber, post_position_a, post_position_b) %>%
        mutate(post_position = if_else(is.na(post_position_a), post_position_b, post_position_a)) %>%
        mutate(inc_democrat = if_else(party_code_simplified == 100 & incumbency_dummy, 1, 0)) %>%
        mutate(inc_republican = if_else(party_code_simplified == 200 & incumbency_dummy, 1, 0)) %>%
        mutate(inc_other = if_else(party_code_simplified == 400 & incumbency_dummy, 1, 0))

tbl1 <- unique(tbl1)

tbl1_group <- tbl1 %>% group_by(district, year, post_position, chamber)

tbl1_incumbency <- tbl1_group %>% summarise(inc_d = sum(inc_democrat), inc_r = sum(inc_republican), inc_o = sum(inc_other)) 
tbl1a <- left_join(tbl1, tbl1_incumbency, by=c("district", "year", "post_position", "chamber"))
tbl1b <- tbl1a %>% select(-c(inc_democrat, inc_republican, inc_other, incumbency_dummy, party_code_simplified, post_position_a, post_position_b))
tbl1c <- unique(tbl1b)
tbl1c <- tbl1c %>% mutate(winner = if_else(votes_cast_other > votes_cast_dem & votes_cast_other > votes_cast_rep, "other",""))
tbl1c <- tbl1c %>% mutate(winner = if_else(votes_cast_dem > votes_cast_rep & winner != "other", "dem", winner))
tbl1c <- tbl1c %>% mutate(winner = if_else(votes_cast_rep >= votes_cast_dem & winner != "other", "rep", winner))
tbl1c <- tbl1c %>% mutate(vote_pct_dem = votes_cast_dem / votes_cast_total, votes_pct_rep = votes_cast_rep / votes_cast_total, votes_pct_other = votes_cast_other / votes_cast_total) %>% mutate(compete = if_else(votes_pct_rep <= COMP_THRESHOLD & votes_pct_rep <= COMP_THRESHOLD,1,0))

tbl1_OR <- election_data_WA_OR %>% 
        filter(state_cd == "OR" & year >= 2002) %>% 
        select(district, year, votes_cast_dem, votes_cast_rep, votes_cast_other, 
               votes_cast_total, incumbency_dummy, party_code_simplified, chamber, post_position_a, post_position_b) %>%
        mutate(post_position = if_else(is.na(post_position_a), post_position_b, post_position_a)) %>%
        mutate(inc_democrat = if_else(party_code_simplified == 100 & incumbency_dummy, 1, 0)) %>%
        mutate(inc_republican = if_else(party_code_simplified == 200 & incumbency_dummy, 1, 0)) %>%
        mutate(inc_other = if_else(party_code_simplified == 400 & incumbency_dummy, 1, 0))

tbl1_OR <- unique(tbl1_OR)

tbl1_OR_group <- tbl1_OR %>% group_by(district, year, chamber)

tbl1_OR_incumbency <- tbl1_OR_group %>% summarise(inc_d = sum(inc_democrat), inc_r = sum(inc_republican), inc_o = sum(inc_other)) 
tbl1_ORa <- left_join(tbl1_OR, tbl1_OR_incumbency, by=c("district", "year", "chamber"))
tbl1_ORb <- tbl1_ORa %>% select(-c(inc_democrat, inc_republican, inc_other, incumbency_dummy, party_code_simplified, post_position_a, post_position_b))
tbl1_ORc <- unique(tbl1_ORb)
tbl1_ORc <- tbl1_ORc %>% mutate(winner = if_else(votes_cast_other > votes_cast_dem & votes_cast_other > votes_cast_rep, "other",""))
tbl1_ORc <- tbl1_ORc %>% mutate(winner = if_else(votes_cast_dem > votes_cast_rep & winner != "other", "dem", winner))
tbl1_ORc <- tbl1_ORc %>% mutate(winner = if_else(votes_cast_rep >= votes_cast_dem & winner != "other", "rep", winner))
tbl1_ORc <- tbl1_ORc %>% mutate(vote_pct_dem = votes_cast_dem / votes_cast_total, votes_pct_rep = votes_cast_rep / votes_cast_total, votes_pct_other = votes_cast_other / votes_cast_total) %>% mutate(compete = if_else(votes_pct_rep <= COMP_THRESHOLD & votes_pct_rep <= COMP_THRESHOLD,1,0))

tbl1_ORc <- tbl1_ORc %>% mutate(dnumber = as.integer(str_sub(district,4,6)))
tbl1_ORc <- tbl1_ORc %>% mutate(senate_district = if_else(chamber == 8, district, paste0("OR_", sprintf("%03d", floor(dnumber/2) + dnumber %% 2))))
tbl1_ORc <- tbl1_ORc %>% select(-dnumber)
tbl1c <- tbl1c %>% mutate(senate_district = district) %>% mutate(year = as.integer(year))
tbl1d <- bind_rows(tbl1c, tbl1_ORc)

#merge with npag
tbl1d <- tbl1d %>% mutate(chamber = if_else(chamber == 8, "upper", "lower"))
tbl1e <- inner_join(tbl1d, npat_WA_OR, by=c("year", "district", "chamber"))

#merge with tw
tw_lower_2002_WA_OR <- tw_lower_2002_WA_OR %>% mutate(chamber = "lower")
tw_upper_2002_WA_OR <- tw_upper_2002_WA_OR %>% mutate(chamber = "upper")

tw_2002 <- bind_rows(tw_lower_2002_WA_OR, tw_upper_2002_WA_OR)

tw_lower_2012_WA_OR <- tw_lower_2012_WA_OR %>% mutate(chamber = "lower")
tw_upper_2012_WA_OR <- tw_upper_2012_WA_OR %>% mutate(chamber = "upper")

tw_2012 <- bind_rows(tw_lower_2012_WA_OR, tw_upper_2012_WA_OR)

tbl1e <- tbl1e %>%  filter(year >= 2002 & year <= 2010) %>%  mutate(stcd = substr(district,1,2))

tbl1f <- inner_join(tbl1e, tw_2002, by=c("district", "chamber"))
tbl1f <- tbl1f %>% mutate(winner = as.factor(winner))
ggplot(tbl1f, aes(x=pres_2008, y=np_score, col = winner )) + geom_point(alpha = 0.4) + facet_wrap(~stcd)
ggplot(tbl1f, aes(x=mrp_mean, y=np_score, col = winner )) + geom_jitter(alpha = 0.4) + facet_wrap(~stcd) + theme_light()
