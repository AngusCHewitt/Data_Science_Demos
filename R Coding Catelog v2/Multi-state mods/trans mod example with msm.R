### R code from vignette source 'msm-manual.Rnw'

###################################################
### code chunk number 1: msm-manual.Rnw:41-43
###################################################
version <- gsub("Version: +", "",
                packageDescription("msm", lib.loc=c("../..",.libPaths()))$Version)


###################################################
### code chunk number 2: msm-manual.Rnw:48-49
###################################################
cat(version)


###################################################
### code chunk number 3: msm-manual.Rnw:52-53
###################################################
cat(format(Sys.time(), "%d %B, %Y"))


###################################################
### code chunk number 4: msm-manual.Rnw:861-862
###################################################
options(width = 60)


###################################################
#MSM 
###################################################
library(msm)

##--  cardiovascular data
cav[1:21,]

## struc table, cont table
statetable.msm(state, PTNUM, data=cav)


##--transition mod prob, est which pathways are possible
Q  <-  rbind ( c(0, 0.25, 0, 0.25),
               c(0.166, 0, 0.166, 0.166),
               c(0, 0.25, 0, 0.25),
               c(0, 0, 0, 0) )


##-- crude est of trans pathwys
Q.crude  <- crudeinits.msm(state ~ years, PTNUM, data=cav,
                           qmatrix=Q)

##  Multi stone model with no coefficients
cav.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                qmatrix = Q, deathexact = 4)


cav.msm

summary(cav.msm)
plot(cav.msm)
str(cav)

##--  I put for model with no cover it
pmatrix.msm(cav.msm, t=30)##- different transition probability is a different time interval
sojourn.msm(cav.msm) ## --  me a number of days for each stage
pnext.msm(cav.msm) ##   probabilities of transitioning to in the next stage
totlos.msm(cav.msm)  
qratio.msm(cav.msm, ind1=c(2,1), ind2=c(1,2))
qmatrix.msm(cav.msm) ##  quantities transitioning to each state, in cont table form

plot(cav.msm, legend.pos=c(8, 1))

##-- residual stats mod gooness ofr fit test for each trans stage
options(digits=3)
prevalence.msm(cav.msm, times=seq(0,20,2))
plot.prevalence.msm(cav.msm, mintime=0, maxtime=20)

##-- deviance stas fitted - obs
options(digits=2)
pearson.msm(cav.msm, timegroups=2,
            transitions=c(1,2,3,4,5,6,7,8,9,9,9,10))


#msm  with coefficients
cavsex.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                   qmatrix = Q, deathexact = 4, covariates = ~ sex)

#  outputs
options(digits = 3)
summary(cavsex.msm)
qmatrix.msm(cavsex.msm, covariates=list(sex=0)) # Male
qmatrix.msm(cavsex.msm, covariates=list(sex=1)) # Female




##--  for models with covariates
hazard.msm(cavsex.msm) ##  model requires covariate
qmatrix.msm(cavsex.msm, covariates = 0)
qmatrix.msm(cavsex.msm, covariates = list(sex = 1))


##== msm  with first observations record
Qm <- rbind(c(0, 0.148, 0, 0.0171),
            c(0, 0, 0.202, 0.081),
            c(0, 0, 0, 0.126),
            c(0, 0, 0, 0))
ematrix <- rbind(c(0, 0.1, 0, 0),
                 c(0.1, 0, 0.1, 0),
                 c(0, 0.1, 0, 0),
                 c(0, 0, 0, 0))
cavmisc.msm <- msm(state ~ years, subject = PTNUM, data = cav,
                   qmatrix = Qm, ematrix = ematrix, deathexact = 4,
                   obstrue = firstobs)
cavmisc.msm

## msm  with first observations and miss classification covariance
cavmiscsex.msm <- msm(state ~ years, subject = PTNUM, data = cav,
                      qmatrix = Qm, ematrix = ematrix,
                      deathexact = 4, misccovariates = ~sex,
                      obstrue=firstobs)
cavmiscsex.msm


ematrix.msm(cavmiscsex.msm, covariates=list(sex=0))
ematrix.msm(cavmiscsex.msm, covariates=list(sex=1))
pearson.msm(cavmisc.msm, timegroups=2,
            transitions=c(1,2,3,4,5,6,7,8,9,9,9,10))

vit <- viterbi.msm(cavmisc.msm)
vit[vit$subject==100103,]


###################################################
### code chunk number 38: msm-manual.Rnw:2347-2348
###################################################
three.q <- rbind(c(0, exp(-6), exp(-9)), c(0, 0, exp(-6)), c(0, 0, 0))


###################################################
### code chunk number 39: msm-manual.Rnw:2366-2378
###################################################
hmodel1 <- list(hmmNorm(mean=100, sd=16), hmmNorm(mean=54, sd=18),
                hmmIdent(999))

fev1.msm <- msm(fev ~ days, subject=ptnum, data=fev, qmatrix=three.q,
                deathexact=3, hmodel=hmodel1,
                hcovariates=list(~acute, ~acute, NULL),
                hcovinits = list(-8, -8, NULL),
                hconstraint = list(acute = c(1,1)))

fev1.msm

sojourn.msm(fev1.msm)


## msm  with coefficients and restricted pathway
cavsex.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                   qmatrix = Q, deathexact = 4,
                   covariates = list("1-2" = ~ sex, "1-4" = ~sex) )

## msm  with coefficients and constraints apply to transition probabilities
cav3.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                 qmatrix = Q, deathexact = 4,
                 covariates = ~ sex,
                 constraint = list(sex=c(1,2,3,1,2,3,2)) )

## msm  with fixed parameters
cav4.msm <- msm( state ~ years, subject=PTNUM, data = cav,
                 qmatrix = Q, deathexact = 4,
                 control = list(trace=2, REPORT=1),
                 fixedpars = c(6, 7) )


