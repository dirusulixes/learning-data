#dataset<-load("dataset.RData")
dataset<-evh.dataset.degree2.6m.unified
dataset$time0<-dataset$evTime0-min(dataset$evTime0)
dataset$time1<-dataset$evTime1-min(dataset$evTime0)
dataset$timeDiff<-dataset$time1-dataset$time0

# surv object
require(survival)
surv.obj.fixed <- Surv(dataset$time0,dataset$time1,dataset$fixed==1)
surv.obj.closed <- Surv(dataset$time0,dataset$time1,dataset$closed==1)
surv.obj.resolved <- Surv(dataset$time0,dataset$time1,dataset$resolved==1)
#attach(evh.dataset)
#cor(!is.na(evh.2path,evh.neigh.2path,evh.neigh.4c))
#cor(evh.dataset, use="pairwise.complete.obs", method="pearson") 
#cor(apache.collab, use="complete.obs", method="pearson") 

# recode variables

install.packages("car")
require(car)
dataset$bugSevOrd <- recode(dataset$bugSev,"c('Blocker','blocker')='7';c('Critical','critical')='6';c('Major','major')='5';c('Normal','normal')='4';c('Minor','minor')='3';c('regression','trivial')='2';c('Enhancement','enhancement')='1'",as.factor.result=FALSE)
dataset$component <- recode(dataset$Component,"'All'='0';'Core'='1';c('Build','Runtime Config','Win32 MSI Installer')='2'; 'Documentation'='3';else='4'",as.factor.result=TRUE)

dataset$neigh.4c.meandist.na<-as.numeric(dataset$neigh_4c_meandist)
#dataset$neigh.4c.meandist.na[is.nan(dataset$neigh.4c.meandist)]<-NA
dataset$neigh.4c.meandist.0<-as.numeric(dataset$neigh_4c_meandist)
dataset$neigh.4c.meandist.0[is.na(dataset$neigh.4c.meandist.0)]<-0

summary(dataset$neigh.4c.meandist.na)
summary(dataset$neigh.4c.meandist.0)

# std dataset

dataset.6m <- data.frame(comments=dataset$comments,cc=dataset$cc,assignee=dataset$Assignee,bugSev=dataset$bugSevOrd,HE=dataset$HE,inst=dataset$changedBy_inst,experience=dataset	$idContrR_catAttach,degree=dataset$degree,degreeW=dataset$degreeW,degreeDiff=dataset$degreeDiff,path2=dataset$gen_2path,neigh2path=dataset$neigh_2path,cy4=dataset$neigh_4c,distNA=dataset$neigh.4c.meandist.na,dist0=dataset$neigh.4c.meandist.0,ih=as.numeric(dataset$neigh_4c_meandist_ih),wih=as.numeric(dataset$neigh_4c_meandist_wih))

dataset.6m.std <- scale(dataset.6m)

dataset.6m <- data.frame(dataset.6m,component=dataset$component,resolution=dataset$resolution,status=dataset$status,idBug=dataset$idBugR)
dataset.6m.std <- data.frame(dataset.6m.std,component=dataset$component,resolution=dataset$resolution,status=dataset$status,idBug=dataset$idBugR)

dataset.6m$distNA2<-dataset.6m$distNA^2
dataset.6m$dist02<-dataset.6m$dist0^2
dataset.6m$cy42<-dataset.6m$cy4^2
dataset.6m.std$distNA2<-dataset.6m.std$distNA^2
dataset.6m.std$dist02<-dataset.6m.std$dist0^2
dataset.6m.std$cy42<-dataset.6m.std$cy4^2

dataset.6m.std.cor<-cbind(dataset.6m.std$path2,dataset.6m.std$neigh2path,dataset.6m.std$cy4,dataset.6m.std$dist)
cor<-cor(dataset.6m.std.cor,use="complete.obs")

pdf("scatterplot.pdf")
pairs(~neigh2path+cy4+dist,data=dataset.6m.std, main="Simple Scatterplot Matrix")
dev.off()

ans.6m.resolved.std.00 <- coxph(surv.obj.resolved ~ 1 + cluster(idBug), data=evh.6m.dataset.std)
ans.6m.resolved.std.01 <- coxph(surv.obj.resolved ~ comments + cc + assignee + bugSev + HE + as.factor(component) + cluster(idBug), data=evh.6m.dataset.std)
ans.6m.resolved.std.02 <- coxph(surv.obj.resolved ~ comments + cc + assignee + bugSev + degree + degreeW + as.factor(component), data=evh.6m.dataset.std)
ans.6m.resolved.std.03 <- coxph(surv.obj.resolved ~ comments + cc + assignee + bugSev + degree + degreeW + neigh.2path + cy4, data=evh.6m.dataset.std)

ans.6m.spelldup.resolved.std.01 <- coxph(surv.obj.resolved ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.resolved.std.01)
ans.6m.spelldup.fixed.std.01 <- coxph(surv.obj.fixed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.fixed.std.01)
ans.6m.spelldup.closed.std.01 <- coxph(surv.obj.closed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0, data=dataset.6m.std)
summary(ans.6m.spelldup.closed.std.01)

save.image();savehistory()

ans.6m.spelldup.fixed.std.02 <- coxph(surv.obj.fixed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.fixed.std.01)
ans.6m.spelldup.fixed.std.01 <- coxph(surv.obj.fixed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.fixed.std.01)
ans.6m.spelldup.fixed.std.01 <- coxph(surv.obj.fixed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.fixed.std.01)
ans.6m.spelldup.fixed.std.01 <- coxph(surv.obj.fixed ~ comments + cc+  assignee + bugSev + HE + inst + experience + degreeW + neigh2path + (cy4 + cy42) *  dist0 + cluster(idBug), data=dataset.6m.std)
summary(ans.6m.spelldup.fixed.std.01)
