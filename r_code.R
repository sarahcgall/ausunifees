#=========================LIBRARY============================#
library(tidyverse)
library(pdftools)
library(rvest)
Sys.setenv('JAVA_HOME'='C:/Program Files/Java/jre1.8.0_251')
library(tabulizer)
library(extrafont)
library(scales)
library(networkD3)
#============================================================#



#=========================SCRAPE=============================#
#Prepare for scraping each years' funding cluster page from the Australian Federal Department of Education website
pages <- c("2020_allocation_of_units_of_study.pdf/","2019_allocation_of_units_of_study.pdf","2018_allocation_of_units_of_study_2018_12_18.pdf","2017_allocation_of_units_of_study_v2.pdf","2016_allocation_of_units_of_study_revised_without_ed.pdf", "2015_allocation_of_units_of_study_revised_without_ed.pdf","2014_allocation_of_units_of_study_no_ed_0.pdf","allocation_units_study2013.pdf","allocation_units_study2012.pdf","allocationunitsstudy2011.pdf","allocation_units_study.pdf","codes2009.pdf")
url <- list()
for (i in 1:length(pages)){
  urls <- paste0("https://docs.education.gov.au/system/files/doc/other/",pages[i])
  url[[i]] <- urls
}

#Scrape PDFs for years 2009 - 2020
raw_data <- lapply(url, function(i){
  webpages <- extract_tables(i,
                             method = "stream",
                             output = "csv",
                             outdir = "C:/Users/Sarah/RProjects/ausunifees/data")
})

#============================================================#



#=========================DATA===============================#
#For ease, cleaning data was done in a csv and each year and page within extracted PDFs were combined.
#The clinical psychology field of education code was the only change to make them consistent throughout the years.
#All units were matched to the current 2020 funding cluster and part clusters for ease of analysis.
#Projected 2021 clusters added from: https://www.dese.gov.au/system/files/doc/other/job-ready_graduates_discussion_paper.pdf
cleaned_data <- read_csv("C:/Users/Sarah/RProjects/ausunifees/cleaned/Funding.csv", col_names = TRUE)

#Data were then further cleaned, removing $ and , symbols and creating two new columns - total contribution and funding ratio
funding_data <- cleaned_data %>%
  mutate(`Student enrolments: commencing students` = as.numeric(str_remove_all(`Student enrolments: commencing students`, ",")),
         `Student enrolments: not commencing students` = as.numeric(str_remove_all(`Student enrolments: not commencing students`, ",")),
         `Award course completions` = as.numeric(str_remove_all(`Award course completions`, ",")),
         `Maximum student contribution amounts` = str_remove_all(`Maximum student contribution amounts`, ","),
         `Maximum student contribution amounts` = as.numeric(str_remove_all(`Maximum student contribution amounts`, "\\$")),
         `Commonwealth contribution amounts` = str_remove_all(`Commonwealth contribution amounts`, ","),
         `Commonwealth contribution amounts` = as.numeric(str_remove_all(`Commonwealth contribution amounts`, "\\$"))) %>% 
  mutate(`Total contribution` = `Maximum student contribution amounts`+`Commonwealth contribution amounts`, 
         `Funding ratio` = `Maximum student contribution amounts`/`Total contribution`)

#============================================================#



#======================ANALYSIS 1============================#
#      Sankey Diagram: Funding change 2020 - 2021
#============================================================#
#Clean data for plot: Funding change 2020 - 2021
#NB. excuse the messy code - haven't learnt how to do matrices in r yet...
sankey <- funding_data %>%
  filter(Year == "2020" | Year == "2021" & Type == "New") %>%
  select(Year, `Part funding cluster`, `Total contribution`) %>%
  group_by(`Part funding cluster`, Year) %>%
  summarise(total_mean = mean(`Total contribution`)) %>%
  pivot_wider(names_from = Year, values_from = total_mean) %>%
  mutate(change = `2021` - `2020`)

neg <- sankey %>% filter(change <= 0)
pos <- sankey %>% filter(change >= 0)

#create matrix with split remaining savings going to courses with increases
neg <- neg %>%
  mutate(amount = (sum(pos$change)/sum(neg$change))*change) %>%
  mutate(`1` = (pos[[4]][1]/sum(pos$change))*amount,
         `2` = (pos[[4]][2]/sum(pos$change))*amount, 
         `3` = (pos[[4]][3]/sum(pos$change))*amount, 
         `4` = (pos[[4]][4]/sum(pos$change))*amount, 
         `5` = (pos[[4]][5]/sum(pos$change))*amount, 
         `6` = (pos[[4]][6]/sum(pos$change))*amount,
         `7` = (pos[[4]][7]/sum(pos$change))*amount,
         `8` = NA,`9` = NA,`10` = NA,`11` = NA, `12` = NA,`13` = NA,`14` = NA, `15` = NA, `16` = NA, `17` = NA)

pos <- pos %>%
  mutate(`1` = NA,`2` = NA,`3` = NA,`4` = NA,`5` = NA,`6` = NA,`7` = NA,`8` = NA,`9` = NA,`10` = NA,`11` = NA, `12` = NA,`13` = NA,`14` = NA, `15` = NA, `16` = NA, `17` = NA)

#rename columns
names(neg)[names(neg) == "1"] <- pos[[1]][1]
names(neg)[names(neg) == "2"] <- pos[[1]][2]
names(neg)[names(neg) == "3"] <- pos[[1]][3]
names(neg)[names(neg) == "4"] <- pos[[1]][4]
names(neg)[names(neg) == "5"] <- pos[[1]][5]
names(neg)[names(neg) == "6"] <- pos[[1]][6]
names(neg)[names(neg) == "7"] <- pos[[1]][7]
names(neg)[names(neg) == "8"] <- neg[[1]][1]
names(neg)[names(neg) == "9"] <- neg[[1]][2]
names(neg)[names(neg) == "10"] <- neg[[1]][3]
names(neg)[names(neg) == "11"] <- neg[[1]][4]
names(neg)[names(neg) == "12"] <- neg[[1]][5]
names(neg)[names(neg) == "13"] <- neg[[1]][6]
names(neg)[names(neg) == "14"] <- neg[[1]][7]
names(neg)[names(neg) == "15"] <- neg[[1]][8]
names(neg)[names(neg) == "16"] <- neg[[1]][9]
names(neg)[names(neg) == "17"] <- neg[[1]][10]

names(pos)[names(pos) == "1"] <- pos[[1]][1]
names(pos)[names(pos) == "2"] <- pos[[1]][2]
names(pos)[names(pos) == "3"] <- pos[[1]][3]
names(pos)[names(pos) == "4"] <- pos[[1]][4]
names(pos)[names(pos) == "5"] <- pos[[1]][5]
names(pos)[names(pos) == "6"] <- pos[[1]][6]
names(pos)[names(pos) == "7"] <- pos[[1]][7]
names(pos)[names(pos) == "8"] <- neg[[1]][1]
names(pos)[names(pos) == "9"] <- neg[[1]][2]
names(pos)[names(pos) == "10"] <- neg[[1]][3]
names(pos)[names(pos) == "11"] <- neg[[1]][4]
names(pos)[names(pos) == "12"] <- neg[[1]][5]
names(pos)[names(pos) == "13"] <- neg[[1]][6]
names(pos)[names(pos) == "14"] <- neg[[1]][7]
names(pos)[names(pos) == "15"] <- neg[[1]][8]
names(pos)[names(pos) == "16"] <- neg[[1]][9]
names(pos)[names(pos) == "17"] <- neg[[1]][10]


#Append amounts to complete matrix
pos <- pos %>%
  mutate(`Computing, built environment or other health` = ifelse(`Part funding cluster` == pos[[1]][1], `2020`, 0),
         `Creative arts` = ifelse(`Part funding cluster` == pos[[1]][2], `2020`, 0),
         `Dentistry, medicine or veterinary science` = ifelse(`Part funding cluster` == pos[[1]][3], `2020`, 0),
         `English` = ifelse(`Part funding cluster` == pos[[1]][4], `2020`, 0),
         `Humanities` = ifelse(`Part funding cluster` == pos[[1]][5], `2020`, 0),
         `Languages` = ifelse(`Part funding cluster` == pos[[1]][6], `2020`, 0),
         `Law, accounting, administration, economics, commerce` = ifelse(`Part funding cluster` == pos[[1]][7], `2020`, 0))

neg <- neg %>%
  rbind(pos) %>%
  mutate(`Agriculture` = ifelse(`Part funding cluster` == neg[[1]][1], `2021`, 0),
         `Allied health` = ifelse(`Part funding cluster` == neg[[1]][2], `2021`, 0),
         `Behavioural science or social studies` = ifelse(`Part funding cluster` == neg[[1]][3], `2021`, 0),
         `Clinical psychology` = ifelse(`Part funding cluster` == neg[[1]][4], `2021`, 0),
         `Communications` = ifelse(`Part funding cluster` == neg[[1]][5], `2021`, 0),
         `Education` = ifelse(`Part funding cluster` == neg[[1]][6], `2021`, 0),
         `Environmental Studies` = ifelse(`Part funding cluster` == neg[[1]][7], `2021`, 0),
         `Mathematics or statistics` = ifelse(`Part funding cluster` == neg[[1]][8], `2021`, 0),
         `Nursing` = ifelse(`Part funding cluster` == neg[[1]][9], `2021`, 0),
         `Science, Engineering or surveying` = ifelse(`Part funding cluster` == neg[[1]][10], `2021`, 0))

rownames(neg) <- neg[[1]]
neg <- neg %>% 
  select(`Agriculture`, `Allied health`, `Behavioural science or social studies`, `Clinical psychology`, `Communications`, `Computing, built environment or other health`, `Creative arts`, `Dentistry, medicine or veterinary science`, `Education`, `English`, `Environmental Studies`, `Humanities`, `Languages`, `Law, accounting, administration, economics, commerce`, `Mathematics or statistics`, `Nursing`, `Science, Engineering or surveying`)

matrix <- as.matrix(neg)
matrix <- matrix[, colnames(matrix) != "Part funding cluster"]

#Transform matrix into "source", "target" and "value"
links <- matrix %>%
  as.data.frame() %>%
  rownames_to_column(var="source") %>%
  gather(key = "target", value = "value", -1) %>%
  mutate(value = round(as.numeric(str_trim(value, side="left")))) %>%
  filter(value != 0) %>%
  mutate(source = paste0("2020 ", source), target = paste0("2021 ", target))

nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", 
              Target = "IDtarget",
              Value = "value", 
              NodeID = "name",
              sinksRight=FALSE)

#============================================================#



#======================ANALYSIS 2============================#
#          Student Funding per year per cluster
#============================================================#
#Summary statistics
`label` <- funding_data %>%
  filter(Year == "2020" | Year == "2021" & Type == "New") %>%
  select(Year, `Part funding cluster`, `Maximum student contribution amounts`) %>%
  group_by(`Part funding cluster`, Year) %>%
  summarise(total_mean = mean(`Maximum student contribution amounts`)) %>%
  pivot_wider(names_from = Year, values_from = total_mean) %>%
  mutate(change = `2021` - `2020`) %>%
  mutate(change = ifelse(change <= 0, paste0("-$", abs(change)), paste0("+$", change))) %>%
  arrange(desc(change))

funding_data %>%  
  filter(Year == "2021" & `Part funding cluster`== "Creative arts") %>%
  group_by(`Part funding cluster`, Type) %>% 
  summarise(mean = mean(`Total contribution`))

#Create plot: Student Funding per year per cluster
ggplot(funding_data %>%  
         filter(Type == "Old") %>%
         group_by(`Part funding cluster`, Year) %>% 
         summarise(mean = mean(`Total contribution`)),
       aes(Year, `mean`, linetype = "blank")) +
  geom_bar(stat = "identity", fill = "grey", alpha= 0.7, show.legend = TRUE) +
  geom_bar(data=funding_data %>%  
             filter(Type == "Old") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Total contribution`)), 
           aes(Year, `mean`, linetype = "dotted"), fill = "brown2", stat = "identity", alpha = 0, show.legend = TRUE) +
  geom_bar(data=funding_data %>% 
             filter(Type == "New") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Total contribution`)),
           aes(Year, `mean`, colour = "grey45"), stat = "identity", fill = alpha(NA, 0.7), linetype = "dashed", show.legend = TRUE) +
  geom_bar(data=funding_data %>% 
             filter(Year <= "2020" & Type == "Old") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Maximum student contribution amounts`)),
           aes(Year, `mean`, fill = `Part funding cluster`), stat = "identity", show.legend = FALSE) +
  geom_bar(data=funding_data %>% 
             filter(Year == "2021" & Type == "Old") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Maximum student contribution amounts`)),
           aes(Year, `mean`, fill = `Part funding cluster`), stat = "identity", alpha = 0.3, show.legend = FALSE) +
  geom_bar(data=funding_data %>%
             filter(Type == "New") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Maximum student contribution amounts`)),
           aes(Year, `mean`, colour = "black"), stat = "identity", fill = alpha(NA, 0.7), linetype = "dashed", show.legend = TRUE) +
  geom_text(data=`label`, aes(2021, `2021`+3000, label = change), size = 3, family = "Calibri Light") +
  scale_y_continuous(breaks = c(5000, 15000, 25000, 35000), 
                     labels = dollar, limits = c(0,40000), expand = c(0,0)) +
  scale_x_continuous(breaks = c(2009, 2012, 2015, 2018, 2021)) +
  scale_colour_manual(name = "New funding arrangement", 
                      values = c("grey45"="grey45", "black"="black"), 
                      labels = c("Proposed Commonwealth contribution", "Proposed student contribution"), 
                      drop = FALSE) +
  scale_linetype_manual(name = "Current funding arrangement",
                        values = c("blank", "dotted"), 
                        label = c("Commonwealth contribution", "Student contribution")) +
  guides(colour = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                               override.aes = list(colour = c("grey60"="grey60", "black"="black"), fill = "white")),
         linetype = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                                 override.aes = list(alpha = 0.6, fill = c("grey", "coral"))),
         fill = FALSE) +
  facet_wrap(~`Part funding cluster`) +
  labs(x="Funding year", 
       y="Funding contribution ($)", 
       title = "University course fees: student and Commonwealth contribution amounts by funding cluster and year") +
  theme(
    plot.margin = margin(1,1,1,1, "cm"),
    plot.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(face = "plain", size = 14, family = "Calibri Light", hjust = 0.5, vjust = 3),
    axis.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    axis.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    strip.text.x = element_text(face = "plain", size = 8.6, family = "Calibri Light"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
#============================================================#



#======================ANALYSIS NA===========================#
#         Student funding ratio per year per cluster
#============================================================#
#Clean data for plot: Student funding ratio per year per cluster
x <- funding_data %>%
  filter(Type == "New") %>%
  group_by(`Part funding cluster`, Year) %>% 
  summarise(mean = mean(`Funding ratio`)) %>%
  mutate(Type = "New") %>%
  select(`Part funding cluster`, Type, `mean`)


y <- funding_data %>% 
  filter(Year == "2021" & Type == "Old") %>%
  group_by(`Part funding cluster`, Year) %>% 
  summarise(mean = mean(`Funding ratio`)) %>%
  mutate(Type = "Old") %>%
  select(`Part funding cluster`, Type, `mean`)

z <- left_join(x, y, by = "Part funding cluster") %>%
  mutate(Change = round((mean.x - mean.y)*100)) %>%
  mutate(Change = paste0(ifelse(Change >= 0, "+", ""), Change)) %>%
  mutate(Change = paste0(Change, str_detect(Change, "^"), "%", ""), Change, Change = str_replace_all(Change, "TRUE", ""))


#Create plot: Student funding ratio per year per cluster
ggplot(data=funding_data %>%  
         filter(Type == "Old") %>%
         group_by(`Part funding cluster`, Year) %>% 
         summarise(mean = mean(`Funding ratio`)), 
       aes(Year, `mean`, linetype = "dotted")) +
  geom_bar(fill = "brown2", stat = "identity", alpha = 0, show.legend = TRUE) +
  geom_bar(data=funding_data %>% 
             filter(Year <= "2020" & Type == "Old") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Funding ratio`)),
           aes(Year, `mean`, fill = `Part funding cluster`), stat = "identity", show.legend = FALSE) +
  geom_bar(data=funding_data %>% 
             filter(Year == "2021" & Type == "Old") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Funding ratio`)),
           aes(Year, `mean`, fill = `Part funding cluster`), stat = "identity", alpha = 0.3, show.legend = FALSE) +
  geom_bar(data=funding_data %>%
             filter(Type == "New") %>%
             group_by(`Part funding cluster`, Year) %>% 
             summarise(mean = mean(`Funding ratio`)),
           aes(Year, `mean`, colour = "black"), stat = "identity", fill = alpha(NA, 0.7), linetype = "dashed", show.legend = TRUE) +
  geom_text(data=z, aes(2021, `mean.x`+0.05), label = z$Change, size = 3, family = "Calibri Light") +
  scale_y_continuous(breaks = c(.25, .50, .75, 1), labels = percent, limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(breaks = c(2009, 2012, 2015, 2018, 2021)) +
  scale_colour_manual(name = "New funding arrangement", 
                      values = c("black"="black"), 
                      labels = c("Proposed student contribution"), 
                      drop = FALSE) +
  scale_linetype_manual(name = "Current funding arrangement",
                        values = c("dotted"), 
                        label = c("Student contribution")) +
  guides(colour = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                               override.aes = list(colour = c("black"="black"), fill = "white")),
         linetype = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                                 override.aes = list(alpha = 0.6, fill = c("coral"))),
         fill = FALSE) +
  facet_wrap(~`Part funding cluster`) +
  labs(x="Funding year", 
       y="Funding contribution (%)", 
       title = "University course fees: proportion of maximum student contribution by funding cluster and year") +
  theme(
    plot.margin = margin(1,1,1,1, "cm"),
    plot.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(face = "plain", size = 14, family = "Calibri Light", hjust = 0.5, vjust = 3),
    axis.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    axis.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_rect(colour = "white"),
    legend.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    strip.text.x = element_text(face = "plain", size = 8.6, family = "Calibri Light"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
  )

#============================================================#



#======================ANALYSIS 3============================#
#      Change in student contribution per cluster ($)
#============================================================#
#Clean data for plot: Change in student contribution per cluster ($)
student_new <-funding_data %>%
  filter(Type == "New") %>%
  select(Year, `Part funding cluster`, `Maximum student contribution amounts`) %>%
  group_by(Year, `Part funding cluster`) %>%
  summarise(student_mean = mean(`Maximum student contribution amounts`)) %>%
  pivot_wider(names_from = Year, values_from = student_mean)

student_funding1 <- funding_data %>%
  filter(Type == "Old") %>%
  select(Year, `Part funding cluster`, `Maximum student contribution amounts`) %>%
  group_by(Year, `Part funding cluster`) %>%
  summarise(student_mean = mean(`Maximum student contribution amounts`)) %>%
  pivot_wider(names_from = Year, values_from = student_mean) %>%
  inner_join(student_new, student_old, by = "Part funding cluster") %>%
  mutate(`2010$` = `2010`-`2009`) %>%
  mutate(`2011$` = `2011`-`2010`) %>%
  mutate(`2012$` = `2012`-`2011`) %>%
  mutate(`2013$` = `2013`-`2012`) %>%
  mutate(`2014$` = `2014`-`2013`) %>%
  mutate(`2015$` = `2015`-`2014`) %>%
  mutate(`2016$` = `2016`-`2015`) %>%
  mutate(`2017$` = `2017`-`2016`) %>%
  mutate(`2018$` = `2018`-`2017`) %>%
  mutate(`2019$` = `2019`-`2018`) %>%
  mutate(`2020$` = `2020`-`2019`) %>%
  mutate(`2021_old` = `2021.x`-`2020`) %>%
  mutate(`2021_new` = `2021.y`-`2020`) %>%
  select(`Part funding cluster`, `2010$`:`2021_new`) %>%
  gather("Year", "change$", `2010$`:`2021_new`) %>%
  mutate(Type = ifelse(str_detect(Year, "new") == TRUE, "New", "Old")) %>%
  mutate(Year = str_replace_all(Year, "\\$", ""), Year = str_replace_all(Year, "_new", ""), Year = str_replace_all(Year, "_old", "")) %>%
  mutate(`change$` = round(`change$`)) %>%
  mutate(`label$` = ifelse(`change$` <= 0, paste0("-$", abs(`change$`)), paste0("$", `change$`)))



#Create plot: Change in student contribution per cluster ($)
ggplot(data=student_funding1 %>%
         filter(Type == "Old"), 
       aes(`Part funding cluster`, `change$`, linetype = "dotted")) +
  geom_bar(fill = "brown2", stat = "identity", alpha = 0, show.legend = TRUE) +
  geom_bar(data=student_funding1 %>%
             filter(Type == "Old" & Year <= "2020"),
       aes(`Part funding cluster`, `change$`, fill = `Part funding cluster`), stat = "identity", show.legend = FALSE) +
  geom_bar(data=student_funding1 %>%
             filter(Type == "Old" & Year == "2021"),
           aes(`Part funding cluster`, `change$`, fill = `Part funding cluster`), stat = "identity", alpha = 0.7, show.legend = FALSE) +
  geom_bar(data=student_funding1 %>%
             filter(Type == "New"),
           aes(`Part funding cluster`, `change$`, colour = "black"), stat = "identity", fill = alpha(NA, 0.7), linetype = "dashed", show.legend = TRUE) +
  geom_hline(yintercept = 0) +
  geom_text(data=student_funding1 %>%
              filter(Type == "Old" & Year == "2010" & `Part funding cluster` == "Nursing"),
            aes(`Part funding cluster`, `change$`+1000, label = `label$`), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding1 %>%
              filter(Type == "Old" & Year == "2010" & `Part funding cluster` == "Education"),
            aes(`Part funding cluster`, `change$`+1000, label = `label$`), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding1 %>%
              filter(Type == "Old" & Year == "2013" & `Part funding cluster` == "Mathematics or statistics"),
            aes(`Part funding cluster`, `change$`+1000, label = `label$`), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding1 %>%
              filter(Type == "Old" & Year == "2013" & `Part funding cluster` == "Science, Engineering or surveying"),
            aes(`Part funding cluster`, `change$`+1000, label = `label$`), size = 3, family = "Calibri Light") +
  scale_colour_manual(name = "New funding arrangement", 
                      values = c("black"="black"), 
                      labels = c("Proposed student contribution"), 
                      drop = FALSE) +
  scale_linetype_manual(name = "Current funding arrangement",
                        values = c("dotted"), 
                        label = c("Student contribution")) +
  guides(colour = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                               override.aes = list(colour = c("black"="black"), fill = "white")),
         linetype = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                                 override.aes = list(alpha = 0.6, fill = c("coral"))),
         fill = FALSE) +
  coord_flip() +
  facet_wrap(~Year) +
  labs(y="Change in funding contribution ($)", 
       title = "University course fees: change in maximum student contribution by funding cluster and year") +
  theme(
    plot.margin = margin(1,1,1,1, "cm"),
    plot.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(face = "plain", size = 14, family = "Calibri Light", hjust = 0.5, vjust = 3),
    axis.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    axis.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(colour = "white"),
    legend.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    strip.text.x = element_text(face = "plain", size = 8.6, family = "Calibri Light"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#============================================================#



#======================ANALYSIS 4============================#
#      Change in student contribution per cluster (%)
#============================================================#
#Clean data for plot: Change in student contribution per cluster (%)
student_funding2 <- funding_data %>%
  filter(Type == "Old") %>%
  select(Year, `Part funding cluster`, `Maximum student contribution amounts`) %>%
  group_by(Year, `Part funding cluster`) %>%
  summarise(student_mean = mean(`Maximum student contribution amounts`)) %>%
  pivot_wider(names_from = Year, values_from = student_mean) %>%
  inner_join(student_new, student_old, by = "Part funding cluster") %>%
  mutate(`2010%` = (`2010`-`2009`)/`2009`) %>%
  mutate(`2013%` = (`2013`-`2012`)/`2012`) %>%
  mutate(`2021_old` = (`2021.x`-`2020`)/`2020`) %>%
  mutate(`2021_new` = (`2021.y`-`2020`)/`2020`) %>%
  select(`Part funding cluster`, `2010%`:`2021_new`) %>%
  gather("Year", "change%", `2010%`:`2021_new`) %>%
  mutate(Type = ifelse(str_detect(Year, "new") == TRUE, "New", "Old")) %>%
  mutate(Year = str_replace_all(Year, "%", ""), Year = str_replace_all(Year, "_new", ""), Year = str_replace_all(Year, "_old", "")) %>%
  mutate(changelabel = round(`change%`*100), 
         changelabel = ifelse(`changelabel` >= 0, paste0("+", `changelabel`, "%"), paste0(`changelabel`, "%")))


#Create plot: Change in student contribution per cluster (%)
ggplot(data=student_funding2 %>%
         filter(Type == "Old"), 
       aes(`Part funding cluster`, `change%`, linetype = "dotted")) +
  geom_bar(fill = "brown2", stat = "identity", alpha = 0, show.legend = TRUE) +
  geom_bar(data=student_funding2 %>%
             filter(Type == "Old" & Year <= "2020"),
           aes(`Part funding cluster`, `change%`, fill = `Part funding cluster`), stat = "identity", show.legend = FALSE) +
  geom_bar(data=student_funding2 %>%
             filter(Type == "Old" & Year == "2021"),
           aes(`Part funding cluster`, `change%`, fill = `Part funding cluster`), stat = "identity", alpha = 0.7, show.legend = FALSE) +
  geom_bar(data=student_funding2 %>%
             filter(Type == "New"),
           aes(`Part funding cluster`, `change%`, colour = "black"), stat = "identity", fill = alpha(NA, 0.7), linetype = "dashed", show.legend = TRUE) +
  geom_text(data=student_funding2 %>%
              filter(Type == "Old" & Year == "2010" & `Part funding cluster` == "Nursing"),
            aes(`Part funding cluster`, `change%`+0.12, label = changelabel), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding2 %>%
              filter(Type == "Old" & Year == "2010" & `Part funding cluster` == "Education"),
            aes(`Part funding cluster`, `change%`+0.12, label = changelabel), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding2 %>%
              filter(Type == "Old" & Year == "2013" & `Part funding cluster` == "Mathematics or statistics"),
            aes(`Part funding cluster`, `change%`+0.12, label = changelabel), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding2 %>%
              filter(Type == "Old" & Year == "2013" & `Part funding cluster` == "Science, Engineering or surveying"),
            aes(`Part funding cluster`, `change%`+0.12, label = changelabel), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding2 %>%
              filter(Type == "New" & Year == "2021" & `change%` >= 0),
            aes(`Part funding cluster`, `change%`+0.12, label = changelabel), size = 3, family = "Calibri Light") +
  geom_text(data=student_funding2 %>%
              filter(Type == "New" & Year == "2021" & `change%` <= 0),
            aes(`Part funding cluster`, `change%`-0.1, label = changelabel), size = 3, family = "Calibri Light") +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), 
                     labels = percent) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(name = "New funding arrangement", 
                      values = c("black"="black"), 
                      labels = c("Proposed student contribution"), 
                      drop = FALSE) +
  scale_linetype_manual(name = "Current funding arrangement",
                        values = c("dotted"), 
                        label = c("Student contribution")) +
  guides(colour = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                               override.aes = list(colour = c("black"="black"), fill = "white")),
         linetype = guide_legend(label.position = "right", title.position = "top", direction = "vertical",
                                 override.aes = list(alpha = 0.6, fill = c("coral"))),
         fill = FALSE) +
  coord_flip() +
  facet_wrap(~Year) +
  labs(y="Change in funding contribution (%)", 
       title = "University course fees: change in maximum student contribution by funding cluster, 2010, 2013 & 2021") +
  theme(
    plot.margin = margin(1,1,1,1, "cm"),
    plot.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(face = "plain", size = 14, family = "Calibri Light", hjust = 0.5, vjust = 3),
    axis.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    axis.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(colour = "white"),
    legend.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    legend.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    strip.text.x = element_text(face = "plain", size = 8.6, family = "Calibri Light"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#============================================================#



#======================ANALYSIS 5============================#
#             Enrolments for Maths and Science
#============================================================#
#Clean data for plot:
enrolments <- funding_data %>%
  filter(Year <= "2018" & !`Broad field of education` %in% c("Food, Hospitality and Personal Services","Mixed Field Programmes")) %>%
  select(`Year`, `Student enrolments: commencing students`, `Student enrolments: not commencing students`, `Award course completions`, `Broad field of education`, `Maximum student contribution amounts`, `Commonwealth contribution amounts`, `Total contribution`) %>%
  group_by(`Broad field of education`, `Year`) %>%
  summarise(`Student enrolments: commencing students` = mean(`Student enrolments: commencing students`),
            `Student enrolments: not commencing students` = mean(`Student enrolments: not commencing students`),
            `Award course completions` = mean(`Award course completions`),
            `Maximum student contribution amounts` = mean(`Maximum student contribution amounts`),
            `Commonwealth contribution amounts` = mean(`Commonwealth contribution amounts`),
            `Total contribution` = mean(`Total contribution`))

#Create plot: Enrolments for Maths and Science
enrolments %>%
  filter(`Broad field of education` == "Natural and Physical Sciences") %>%
  ggplot(aes(Year, `Award course completions`, fill = "red")) +
  geom_bar(stat = "identity") +
  labs(y="Change in funding contribution (%)", 
       title = "") +
  theme(
    plot.margin = margin(1,1,1,1, "cm"),
    plot.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(face = "plain", size = 12, family = "Calibri Light", hjust = 0, vjust = 3),
    axis.title = element_text(face = "plain", size = 10, family = "Calibri Light"),
    axis.text = element_text(face = "plain", size = 9, family = "Calibri Light"),
    axis.title.y = element_blank(),
    legend.position = element_blank()
  )

enrolments %>%
  filter(`Broad field of education` == "Natural and Physical Sciences") %>%
  ggplot(aes(Year, `Student enrolments: not commencing students`, fill = "blue")) +
  geom_bar(stat = "identity")

enrolments %>%
  filter(`Broad field of education` == "Natural and Physical Sciences") %>%
  ggplot(aes(Year, `Student enrolments: commencing students`, fill = "blue")) +
  geom_bar(stat = "identity")









