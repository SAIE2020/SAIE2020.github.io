#load data
################
#Open interview#
################
library(readxl)
opent <- read_excel("C:/Users/Sarah/Downloads/Transcript data.xlsx",sheet = "Open")
View(opent)
#paired t-test for ofc and osc
t.test(opent$OFC, opent$OSC, paired = TRUE, alternative = "two.sided")
mean(opent$OFC)
mean(opent$OSC)
#sig diff at .009014 for number of correct answers given between interview one and two
#more correct details given in the first interview 

#paired t-test for ofi and osi
t.test(opent$OFI, opent$OSI, paired= TRUE, alternative = "two.sided")
#no sig diff in number of incorrect answers given 

#paired t-test for ofd and osd
t.test(opent$OFD, opent$OSD, paired=TRUE, alternative = "two.sided")
#no sig diff found in number of details given 


###########################
#Semi Structured Interview#
###########################
semit <- read_excel("C:/Users/Sarah/Downloads/Transcript data.xlsx",sheet = "Semi")
View(semit)
#paired t test for sfc and ssc
t.test(semit$SFC, semit$SSC, paired=TRUE, alternative="two.sided")
#no significant difference between correct information given during first interview and second

#paired t test for sfi and ssi
t.test(semit$SFI, semit$SSI, paired=TRUE, alternative = "two.sided")
#no significant difference between amount of incorrect information given during first and second interview


#paired t test for sfd and ssd
t.test(semit$SFD, semit$SSD, paired=TRUE, alternative = "two.sided")
#no significant difference between amount of details given between first and second interview

#############
#Compairison#
#############
#t test for correct information in first interview
t.test(opent$OFC, semit$SFC, paired=FALSE, alternative = "two.sided")
#no sig diff for correct info given in first inteview between the formats

#t test for correct information in second interview
t.test(opent$OSC, semit$SSC, paired=FALSE, alternative = "two.sided")
#sig difference found between amount of correct info given in second interview between the formats
#more correct info was given in the semi structured format

#t test for incorrect information in first interview
t.test(opent$OFI, semit$SFI, paired=FALSE, alternative = "two.sided")
#no significant difference found between incorrect information given in first interview between formats

#t test for incorrect information in second interview
t.test(opent$OSI, semit$SSI, paired = FALSE, alternative = "two.sided")
#no sig diff for incorrect information given in second interview between format

#t test for details given in first interview
t.test(opent$OFD, semit$SFD, paired = FALSE, alternative = "two.sided")
#sig diff found between amount of details given in first interview between format
#more details given in first interview open ended

#t test for details given in second interview
t.test(opent$OSD, semit$SSD, paired = FALSE, alternative = "two.sided")
#no sig difference found between amount of details given in second interview between format

########
#Graphs#
########
ofcosc<-c(mean(opent$OFC), mean(opent$OSC))
sfcssc<-c(mean(semit$SFC), mean(semit$SSC))
both<-c(ofcosc,sfcssc)
par(family = "serif")
barplot(both, main="Correct Information in Open Ended/Semi Structured Interview", col=c("#D9ECF2","#D9ECF2", "#F0ADC2", "#F0ADC2"), 
        border = "#DC5084", xlab="Interview", ylab = "Count", names.arg=c("First", "Second","First", "Second" ),
        cex.names = .9, family = "serif", ylim = c(0,10),  axis.lty=1)
legend(3,10,legend=c("Open Ended", "Semi Structered"), cex=.9,fill=c("#D9ECF2", "#F0ADC2"),
       box.lty=1, col=c("#D9ECF2", "#F0ADC2"), box.col="white", border="#DC5084" )

together<-c(mean(opent$OFI),mean(opent$OSI), mean(semit$SFI), mean(semit$SSI))
par(family = "serif")
barplot(together, main="Incorrect Information in Open Ended Interview/Semi Structured", col=c("#D9ECF2","#D9ECF2", "#F0ADC2", "#F0ADC2"), 
        border = "#DC5084", xlab="Interview", ylab = "Count", names.arg=c("First", "Second","First", "Second" ),
        cex.names = .9, family = "serif", ylim = c(0,10),  axis.lty=1)
legend(3,10,legend=c("Open Ended", "Semi Structered"), cex=.9,fill=c("#D9ECF2", "#F0ADC2"),
       box.lty=1, col=c("#D9ECF2", "#F0ADC2"), box.col="white", border="#DC5084" )

allofit<-c(mean(opent$OFD), mean(opent$OSD), mean(semit$SFD), mean(semit$SSD))
par(family = "serif")
barplot(allofit, main="Details in Open Ended Interview/Semi Structured", col=c("#D9ECF2","#D9ECF2", "#F0ADC2", "#F0ADC2"), 
        border = "#DC5084", xlab="Interview", ylab = "Count", names.arg=c("First", "Second","First", "Second" ),
        cex.names = .9, family = "serif", ylim = c(0,10),  axis.lty=1)
legend(3,10,legend=c("Open Ended", "Semi Structered"), cex=.9,fill=c("#D9ECF2", "#F0ADC2"),
       box.lty=1, col=c("#D9ECF2", "#F0ADC2"), box.col="white", border="#DC5084" )

####################
#Sample Description#
####################
ages<-c(50,17,44,24,20,20,20,62,52,50)
boxplot(ages)
boxplot.stats(ages)
mean(ages)
