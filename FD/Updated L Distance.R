source('fd_cleaner.R')
# split up the list 

#get unique list of tns
unique_tns_full <- unique(fd_clean[trackable_type == 'Condition',
                                   .(trackable_name, count)][order(-count)]$trackable_name)

unique_tns <-
  unique(fd_clean[trackable_type == 'Condition' &
                    nchar(trackable_name) > 4,
                  .(trackable_name, count)][order(-count)]$trackable_name)

unique_tns_4char <-
  unique(fd_clean[trackable_type == 'Condition' &
                    nchar(trackable_name) <= 4,
                  .(trackable_name, count)][order(-count)]$trackable_name)

# create blank field for the common name
fd_clean[, common_trackable_name := '']
tns_for_audit <-
  data.table(common_trackable_name = '',
             trackable_names = '')

# for each one of the unique_tns
for (i in 1:length(unique_tns)) {
  if (nrow(fd_clean[trackable_name == unique_tns[i] &
                    common_trackable_name != '']) > 0)
    next
  
  tns <- unique(fd_clean[trackable_type == "Condition" &
                           common_trackable_name == '' &
                           agrepl(unique_tns[i], fd_clean$trackable_name, max.distance = 2), trackable_name])
  top_hit <-
    fd_clean[trackable_name %in% tns &
               trackable_type == 'Condition', .N, by = trackable_name][order(-N)][1]$trackable_name
  output <-
    data.table(common_trackable_name = top_hit,
               trackable_names = list(tns))
  tns_for_audit <- rbind(tns_for_audit, output)
  fd_clean[trackable_name %in% tns
           & trackable_type == "Condition"
           &
             common_trackable_name == '', common_trackable_name := top_hit]
  
  print(
    paste0(
      'Completed iteration number ',
      i,
      ' in namer function for term ',
      unique_tns[i],
      ', which has a total of ',
      round(nrow(fd_clean[trackable_name == unique_tns[i] &
                            trackable_type == 'Condition'])
            / nrow(fd_clean[trackable_type == 'Condition']), 2) *
        100,
      '% of the Condition records'
    )
  )
  
}
tns_for_audit$trackable_names <-
  vapply(tns_for_audit$trackable_names,
         paste,
         collapse = ", ",
         character(1L))
write.csv(tns_for_audit, paste0('ldistance_algo_output_', Sys.Date(), '.csv'))

# scrub dosrcer and disease
# gerd/ acne
# 4 letter
# aid
# cidp
# cfs
# copd
# fnd
# gi
# ibsd
# mood
# pain (maybe drop that?
# pms
# sa
# sci
# tbi
# uctd
# uti
# emojis?
# chronis
# increase l distance to 1.5?


#gernal notes -- find new algorithm or set new paramters, i.e. distance to 2
# manual cleaning
#ocd
# pcos
#mecfs is # me = myalgic encephalitis
#pots
#ptsd
#scrubbing chronic from things other than chronic pain (?)
#headaches in general
#hashimotos
# chronic
# lupus
# ms/multiple sclorosis
#ibs/ irritable bowel syndome
# arthtitis/ keeping them separate
# adhd





# take out 4 and less
# run alogorithm separately
# separate out
