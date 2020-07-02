#### Run Controller####
source("functions.R")

fd_full <- data.table(read.csv("export.csv"), stringsAsFactors = F)

#make copy
fd_clean <- fd_full

# refactor the older than 90s and drop random stuff
fd_clean <- fd_clean[age > 0]
fd_clean[age >= 90, age := 90]
# write.csv2(fd_clean, "fd_clean_20200417.csv")

# scrub punctuation & make uniform capitalization & trailing whitespace
fd_clean[, trackable_name := as.character(trackable_name)]
fd_clean[, trackable_name := removePunctuation(trackable_name)]
fd_clean[, trackable_name := tolower(trackable_name)]
fd_clean[, trackable_name := gsub("[^\x01-\x7F]", "", trackable_name)] #drop emoji encodings
fd_clean[, trackable_name := trimws(trackable_name)] #drop emoji encodings

# get number of trackable names and pct
fd_clean[, count := .N, by = .(trackable_name, trackable_type)][order(-count)]
fd_clean[, trackable_type_count := .N, by = trackable_type] # gets count of each trackable_type
fd_clean[, pct := (count / trackable_type_count), by = trackable_name][order(-count)]
fd_clean[, pct := round(pct, 2)]



# Clean up trackable names
# drop any records that only have 20 matchs in condiiton for now
fd_clean <-
  fd_clean[!trackable_name %in% 
             fd_clean[trackable_type == 'Condition', .N, by = trackable_name][N <= 20]$trackable_name]
# some basic cleaning based on common acronyms
# scrub those acronyms and replace with full medical term cause it'll break the algo
fd_clean[trackable_name == 'me' & trackable_type == 'Condition',
         trackable_name := 'mecfs']


fd_clean[trackable_name == 'ms' & trackable_type == 'Condition',
         trackable_name := 'multiple sclerosis']

fd_clean[trackable_name == 'pcos' & trackable_type == 'Condition',
         trackable_name := 'polycystic ovarian syndrome']
#
fd_clean[(trackable_name == 'obsessivecompulsive disorder' |
            trackable_name == 'ocd')
         & trackable_type == 'Condition',
         trackable_name := 'obsessive compulsive disorder']

fd_clean[(trackable_name == 'ptsd' | trackable_name == 'ptbs') & trackable_type == 'Condition',
         trackable_name := 'post traumatic stress disorder']

fd_clean[trackable_name == 'cptsd'  & trackable_type == 'Condition',
         trackable_name := 'complex post traumatic stress disorder']

fd_clean[trackable_name == 'adhd'  & trackable_type == 'Condition',
         trackable_name := 'attention deficit disorder']

fd_clean[trackable_name == 'bpd'  & trackable_type == 'Condition',
         trackable_name := 'borderline personality disorder']

fd_clean[trackable_name == 'pots' & trackable_type == 'Condition',
         trackable_name := 'postural orthostatic tachycardia syndrome']

fd_clean[trackable_name == 'sibo' & trackable_type == 'Condition',
         trackable_name := 'small intestinal bacterial overgrowth']

fd_clean[(trackable_name == 'ibs' | trackable_name == 'ibd') & trackable_type == 'Condition',
         trackable_name := 'irritable bowel syndrome']

fd_clean[trackable_name %like% 'migraine'  &
           trackable_type == 'Condition',
         trackable_name := 'migraine']

fd_clean[trackable_name %like% 'lupus'  &
           trackable_type == 'Condition',
         trackable_name := 'lupus']

fd_clean[trackable_name == 'tmj'  & trackable_type == 'Condition',
         trackable_name := 'temporomandibular joint']

fd_clean[trackable_name == 'pmdd'  & trackable_type == 'Condition',
         trackable_name := 'premenstrual dysphoric disorder']

fd_clean[(trackable_name == 'eds'  |
            trackable_name %like% 'ehlersdanlos syndrome')
         & trackable_type == 'Condition',
         trackable_name := 'ehlersdanlos syndrome']

fd_clean[(trackable_name == 'mcas'  |
            trackable_name %like% 'mast cell activation disoder') &
           trackable_type == 'Condition',
         trackable_name := 'mast cell activation syndrome']

fd_clean[trackable_name == 'add' & trackable_type == 'Condition',
         trackable_name := 'attention deficit disorder']

fd_clean[trackable_name == 'gerd' & trackable_type == 'Condition',
         trackable_name := 'gastroesophageal reflux disease']

fd_clean[trackable_name == 'crps' & trackable_type == 'Condition',
         trackable_name := 'complex regional pain syndrome']

fd_clean[trackable_name == 'cidp' & trackable_type == 'Condition',
         trackable_name := 'chronic inflammatory demyelinating polyneuropathy']

fd_clean[trackable_name == 'ptbs' & trackable_type == 'Condition',
         trackable_name := 'chronic inflammatory demyelinating polyneuropathy']

fd_clean[trackable_name == 'tbi' & trackable_type == 'Condition',
         trackable_name := 'traumatic brain injury']

fd_clean[(trackable_name == 'csr' | trackable_name == 'csrc') & trackable_type == 'Condition',
         trackable_name := 'central serous retinopathy']

fd_clean[trackable_name == 'rls' & trackable_type == 'Condition',
         trackable_name := 'restless leg syndrome']

fd_clean[trackable_name == 'svt' & trackable_type == 'Condition',
         trackable_name := 'supraventricular tachycardia']

fd_clean[trackable_name == 'uti' & trackable_type == 'Condition',
         trackable_name := 'urinary tract infection']

fd_clean[trackable_name == 'pms' & trackable_type == 'Condition',
         trackable_name := 'premenstral syndrome']

fd_clean[trackable_name == 'pvfs' & trackable_type == 'Condition',
         trackable_name := 'postviral Fatigue Syndrome']

fd_clean[trackable_name == 'cvid' & trackable_type == 'Condition',
         trackable_name := 'common variable immune deficiency']

fd_clean[trackable_name == 'hsv1' & trackable_type == 'Condition',
         trackable_name := 'herpesvirus']

fd_clean[trackable_name == 'mctd' & trackable_type == 'Condition',
         trackable_name := 'mixed connective tissue disease']

fd_clean[trackable_name == 'cirs' & trackable_type == 'Condition',
         trackable_name := 'chronic inflammatory response syndrome']

fd_clean[trackable_name == 'rpgn' & trackable_type == 'Condition',
         trackable_name := 'rapidly progressive glomerulonephritis']

fd_clean[trackable_name == 'cfs' & trackable_type == 'Condition',
         trackable_name := 'chronic fatigue syndrome']

fd_clean[trackable_name == 'gad' & trackable_type == 'Condition',
         trackable_name := 'generalized anxiety disorder']

fd_clean[trackable_name == 'ald' & trackable_type == 'Condition',
         trackable_name := 'adrenoleukodystrophy']

fd_clean[trackable_name == 'chf' & trackable_type == 'Condition',
         trackable_name := 'congestive heart failure']

fd_clean[trackable_name == 'ebv' & trackable_type == 'Condition',
         trackable_name := 'idiopathic intracranial hypertension']

fd_clean[trackable_name == 'mdds' & trackable_type == 'Condition',
         trackable_name := 'mal de debarquement syndrome']

fd_clean[trackable_name == 'gord' & trackable_type == 'Condition',
         trackable_name := 'gastro-oesophageal reflux disease']

fd_clean[trackable_name == 'copd' & trackable_type == 'Condition',
         trackable_name := 'chronic obstructive pulmonary disease']

fd_clean[trackable_name == 'sob' & trackable_type == 'Condition',
         trackable_name := 'shortness of breath']

fd_clean[trackable_name == 'amps' & trackable_type == 'Condition',
         trackable_name := 'amplified musculoskeletal pain syndrome']

fd_clean[trackable_name == 'ndph' & trackable_type == 'Condition',
         trackable_name := 'new daily persistent headache']

fd_clean[trackable_name == 'aps' & trackable_type == 'Condition',
         trackable_name := 'antiphospholipid']

fd_clean[trackable_name == 'pssd' & trackable_type == 'Condition',
         trackable_name := 'post-ssri sexual dysfunction']

fd_clean[trackable_name == 'fnd' & trackable_type == 'Condition',
         trackable_name := 'functional neurological disorder']

fd_clean[trackable_name == 'ntos' & trackable_type == 'Condition',
         trackable_name := 'neurogenic thoracic outlet syndrome']

fd_clean[trackable_name == 'sci' & trackable_type == 'Condition',
         trackable_name := 'acute spinal cord injury']

fd_clean[trackable_name == 'mds' & trackable_type == 'Condition',
         trackable_name := 'myelodysplastic syndrome']

fd_clean[trackable_name == 'uc' & trackable_type == 'Condition',
         trackable_name := 'ulcerative colitis']

fd_clean[trackable_name == 'heds' & trackable_type == 'Condition',
         trackable_name := 'hypermobile Ehlers-Danlos syndrome']

fd_clean[trackable_name == 'uctd' & trackable_type == 'Condition',
         trackable_name := 'undifferentiated connective tissue disease']

fd_clean[trackable_name == 'sle' & trackable_type == 'Condition',
         trackable_name := 'systemic lupus erythematosus']

fd_clean[trackable_name == 'nf1' & trackable_type == 'Condition',
         trackable_name := 'neurofibromatosis']

fd_clean[trackable_name == 'asd' & trackable_type == 'Condition',
         trackable_name := 'atrial septal defect']

fd_clean[trackable_name == 'lh' & trackable_type == 'Condition',
         trackable_name := 'luteinizing hormone lightheadedness']

fd_clean[trackable_name == 'cpps' & trackable_type == 'Condition',
         trackable_name := 'chronic pelvic pain syndrome']

fd_clean[trackable_name == 'period' | trackable_name %in% c('period pain', 'period cramps') & trackable_type == 'Condition',
         trackable_name := 'menstruation']

fd_clean[trackable_name %like% 'major depressive disorder' & trackable_type == 'Condition',
         trackable_name := 'depression']

fd_clean[trackable_name== 'acne' & trackable_type == 'Condition',
         trackable_name := 'acne vulgaris']


# temporarily split out common words