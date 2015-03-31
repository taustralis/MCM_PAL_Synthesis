##Read in source data prepared by Jeff Bowman##
pal_compare <- read.table('pal_comparison_data.txt', sep = ',', header = T,colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
mcm_compare <- read.table('mcm_comparison_data.txt', sep = ',', header = T,colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
View(mcm_compare) #to check it out

## Run Jeff's code to remove rows containing NA's or Negatives##

mcm_bp_no_na <- data.frame(mcm_compare[,1], mcm_compare[,2], mcm_compare[,9], mcm_compare[,3], mcm_compare[,4], mcm_compare[,5], mcm_compare[,6], mcm_compare[,7], mcm_compare[,8], stringsAsFactors = F)
pal_bp_no_na <- data.frame(pal_compare[,1], pal_compare[,2], pal_compare[,9], pal_compare[,3], pal_compare[,4], pal_compare[,5], pal_compare[,6], pal_compare[,7], pal_compare[,8], stringsAsFactors = F)
colnames(mcm_bp_no_na) <- c('run', 'depth', 'doy', 'td', 'leu', 'pp', 'abund', 'chl', 'doc')
colnames(pal_bp_no_na) <- c('run', 'depth', 'doy', 'td', 'leu', 'pp', 'abund', 'chl', 'doc')

mcm_bp_no_na[mcm_bp_no_na <= 0] <- NA
pal_bp_no_na[pal_bp_no_na <= 0] <- NA

mcm_bp_no_na <- na.omit(mcm_bp_no_na)
pal_bp_no_na <- na.omit(pal_bp_no_na)

mcm_leu_pp_no_na <- data.frame(mcm_compare[,1], mcm_compare[,2], mcm_compare[,9], mcm_compare[,4], mcm_compare[,5], mcm_compare[,6], mcm_compare[,7], mcm_compare[,8], stringsAsFactors = F)
pal_leu_pp_no_na <- data.frame(pal_compare[,1], pal_compare[,2], pal_compare[,9], pal_compare[,4], pal_compare[,5], pal_compare[,6], pal_compare[,7], pal_compare[,8], stringsAsFactors = F)
colnames(mcm_leu_pp_no_na) <- c('run', 'depth', 'doy', 'leu', 'pp', 'abund', 'chl', 'doc')
colnames(pal_leu_pp_no_na) <- c('run', 'depth', 'doy', 'leu', 'pp', 'abund', 'chl', 'doc')

mcm_leu_pp_no_na[mcm_leu_pp_no_na <= 0] <- NA
pal_leu_pp_no_na[pal_leu_pp_no_na <= 0] <- NA

mcm_leu_pp_no_na <- na.omit(mcm_leu_pp_no_na)
pal_leu_pp_no_na <- na.omit(pal_leu_pp_no_na)


##check out the data###
summary(mcm_leu_pp_no_na)
summary(pal_leu_pp_no_na)
View(mcm_leu_pp_no_na)
View(pal_leu_pp_no_na)

##Jeff's plots
## plot Leu ~ pp for both sites## 

plot(pal_leu_pp_no_na$leu ~ pal_leu_pp_no_na$pp,
     log = 'xy',
     type = 'n',
     xlab = 'PP',
     ylab = 'Leu',
     xlim = c(1e-5,1e4),
     ylim = c(1e-4,10))

points(pal_leu_pp_no_na$leu ~ pal_leu_pp_no_na$pp,
       col = 'blue')

points(mcm_leu_pp_no_na$leu ~ mcm_leu_pp_no_na$pp,
       col = 'red')

pal_leu_pp_lm <- lm(pal_leu_pp_no_na$leu ~ pal_leu_pp_no_na$pp)
mcm_leu_pp_lm <- lm(mcm_leu_pp_no_na$leu ~ mcm_leu_pp_no_na$pp)

abline(pal_leu_pp_lm,
       col = 'blue',
       untf = T)

abline(mcm_leu_pp_lm,
       col = 'red',
       untf = T)

legend('bottomleft',
       pch = 1,
       col = c('blue', 'red'),
       legend = c('PAL', 'MCM'))

## plot Leu ~ doc ## These throw some warnings...TVM
plot(mcm_leu_pp_no_na$leu ~ mcm_leu_pp_no_na$doc,
     log = 'xy',
     type = 'n',
     xlab = 'doc',
     ylab = 'Leu',
     xlim = c(1e-5,1550),
     ylim = c(1e-3,10)
)


points(pal_leu_pp_no_na$leu ~ c(as.numeric(pal_leu_pp_no_na$doc) / 1000),
       col = 'blue')

points(mcm_leu_pp_no_na$leu ~ mcm_leu_pp_no_na$doc,
       col = 'red')


pal_leu_doc_lm <- lm(pal_leu_pp_no_na$leu ~ pal_leu_pp_no_na$doc/1000)
mcm_leu_doc_lm <- lm(mcm_leu_pp_no_na$leu ~ mcm_leu_pp_no_na$doc)

abline(pal_leu_doc_lm,
       col = 'blue',
       untf = T)

abline(mcm_leu_doc_lm,
       col = 'red',
       untf = T)

legend('bottomleft',
       pch = 1,
       col = c('blue', 'red'),
       legend = c('PAL', 'MCM'))