### plotting

### Status of LLR
llr_status = llr_joined$llr %>% group_by(registration_status) %>% summarise(n=n()) %>% rbind(data.frame(registration_status = "other", n = (llr_joined$llr %>% group_by(registration_status) %>% summarise(n=n()) %>% filter(n<1000) %>% summarize(n= sum(n)) %>% .$n) )) %>% filter(n>=538)
png("../charts/llr_status.png", width=1200, height=600)
ggplot(llr_status , aes(x=reorder(registration_status, n), y=n, fill=n, label=n)) + 
  geom_bar(stat='identity') + 
  scale_fill_continuous(high="#D55E00") +
  coord_flip() +
  geom_text(size = 7, hjust=0, nudge_y = 500, color = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_line(color="#dddddd"), 
        panel.grid.minor.x = element_line(color="#dddddd"), panel.background = element_blank(), 
        axis.line=element_line(colour="black", size=.25), legend.position='none',
        axis.title = element_blank(), axis.text = element_text(size=17),
        panel.spacing = unit(0.5, "cm")) +
  scale_y_continuous(expand=c(.005,2), limits = c(0, max(llr_joined$llr %>% group_by(registration_status) %>% summarise(n=n()) %>% select(n))+3000))
dev.off()

### LLR final numbers
png("../charts/llr_totals.png", width=1200, height=600)
ggplot((llr_totals_table %>% select(dataset, total_records, uprns_held) %>% melt(id.vars="dataset")),
       aes(dataset, value, label=value, fill=variable)) +
  geom_bar(stat="identity", 
          width = 0.7, 
          position = position_dodge(width=.8)) +
  geom_text(size=5, color="black", aes(label=value),position=position_dodge(width=0.8), vjust=2) +
  annotate("text", x=c("CT", "HB","LLR"), y=c(llr_totals_table[2,"total_records"]+10000, llr_totals_table[1,"total_records"]+10000, llr_totals_table[3,"total_records"]+10000), 
           label=paste(llr_totals_table$coverage*100, "%", " Coverage",sep=""),
           size=5) +
  theme(panel.grid.major = element_blank(), panel.grid.major.y = element_line(color="#dddddd"), 
        panel.grid.minor.y = element_line(color="#dddddd"), panel.background = element_blank(), 
        axis.line=element_line(colour="black", size=.25),
        axis.title = element_blank(), axis.text = element_text(size=17),
        panel.spacing = unit(0.5, "cm"),
        legend.position=c(.8, .8))+
  labs(fill = "") 
dev.off()
      
### LLR to chase
png("../charts/llr_analysis.png", width=1200, height=600)
ggplot(llr_analysis_table, aes(x=reorder(description, figures), y=figures, fill=figures, label=figures)) + 
  geom_bar(stat="identity")+
  coord_flip() +
  geom_text(size = 5, hjust=1, color = "white") + 
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_line(color="#dddddd"), 
        panel.grid.minor.x = element_line(color="#dddddd"), panel.background = element_blank(), 
        axis.line=element_line(colour="black", size=.25), legend.position='none',
        axis.title = element_blank(), axis.text = element_text(size=17),
        panel.spacing = unit(1, "cm")) +
  scale_y_continuous(expand=c(.005,2), limits = c(0, max(llr_analysis_table$figures)))
dev.off()

### Overall computed tenure
png("../charts/city_tenure.png", width=1200, height=600)
ggplot(tenure_analysis, aes(x=reorder(tenure, total), y=total, fill=total, label=total)) + 
  geom_bar(stat="identity")+
  coord_flip() +
  geom_text(size = 5, hjust=0, color = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_line(color="#dddddd"), 
        panel.grid.minor.x = element_line(color="#dddddd"), panel.background = element_blank(), 
        axis.line=element_line(colour="black", size=.25), legend.position='none',
        axis.title = element_blank(), axis.text = element_text(size=17),
        panel.spacing = unit(1, "cm")) +
  scale_y_continuous(expand=c(.005,3), limits = c(0, max(tenure_analysis$total+10000)))
dev.off()


### Existing LLR status
