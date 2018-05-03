#state election data
# merge them:  Just WA, OR, ID, and AZ for now

# Create a standaridized year and district field in all the tables in this format [ST_NNN]


#election data:
election_data_1968_92 <- election_data %>% 
        filter(state_cd %in% state_legislatures) %>%
        mutate(district = paste0(state_cd,"_",sprintf("%03d", dist_number)))


#Create a split district table from election data

q1 <- election_data_1968_92 %>% filter(election_winner == TRUE & chamber == 9) %>% 
        mutate(party = if_else(party_code_simplified == 100, "D", if_else(party_code_simplified == 200, "R", "I"))) %>% filter(year <= 2010 & year != 1981) %>% 
        select(party, year, district, state_cd)

q2 <- q1 %>% group_by(state_cd, year, district, party)
q3 <- q2 %>% count(party) %>% spread(party,n) %>% 
        mutate(D = if_else(is.na(D), 0, as.double(D))) %>% 
        mutate(I = if_else(is.na(I), 0, as.double(I))) %>% 
        mutate(R = if_else(is.na(R), 0, as.double(R))) %>% 
        mutate(split = if_else(D >= 1 & R >= 1, TRUE, FALSE)) %>%
        mutate(split_label = if_else(split==TRUE, "2 Party Dists", "1 Party Dists")) %>%
        mutate(stcd = state_cd) %>% 
        filter(year <= 2010)

q4 <- q3 %>% 
        group_by(stcd, year) %>% 
        mutate(denom = n()) %>% 
        group_by(split, split_label, add=TRUE) %>% 
        mutate(count = n()) %>%
        mutate(spct = count / denom)

q5 <- bind_rows(filter(state_means_anal, year > 2000), q4)

(mmd_plot <- ggplot(data=q4, aes(x=year, y=spct, color=split_label)) +
                ylab("Percentage of All Districts") +
                geom_line(aes(linetype=split_label), show.legend=TRUE) + 
                facet_wrap(~stcd) +
                theme(axis.text.x=element_text(color = "black", 
                                               size=11, angle=30, vjust=.8, hjust=0.8))
)
