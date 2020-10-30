#------ 2015. 7. 08
#----------------------- RVPA program -----------------------------
# written by Hiroshi Okamura (VPA & reference point) 
#                     and Momoko Ichinokawa (future projection & reference point)
#
# (*) mac�œǂޏꍇ�́A
# source("rvpa1.6.r",encoding="shift-jis")
# �Ƃ��āA�����R�[�h���w�肵�ēǂ�ł�������
# (*) vpa���g���ꍇ�A�����l�̎w��ɂ���Ă͎������Ȃ��ꍇ������܂��Bp.init�̒l�����낢��ς��Ď����ĉ�����
#
#
#
# �ύX����
# rvpa0.9 - 2013.7.3. �����\���֐��G�����\����determinisitc run�ł�rps�̎Q�Ɣ͈͂ƁA
#   stochastic run�ł�rps�̎Q�Ɣ͈͂��قȂ�ꍇ�̃I�v�V����(sample.year�Ŏw��)��ǉ�
#   ���T���v�����O�����RPS = RPS[sample.year�͈̔�]/mean(RPS[sample.year�͈̔�])*median(RPS[rps.year�͈̔�])
#   (bias.adjusted���Ă��Ȃ��ꍇ�͊֌W�Ȃ��Bsample.year��ݒ肵�Ȃ��ꍇ�A�����I��sample.year=rps.year�ƂȂ�)
# rvpa0.92 - vpa��sel.update�����̏C����profile likelihood�M����Ԋ֐��̏C���E�ǉ�
# rvpa0.95 - �Ǘ���l�v�Z�̂Ƃ���ɁAnlm�ł̌v�Z�񐔂̏����ݒ肷�������ǉ��i�f�t�H���g�ł͂P�O�O�O�j
# rvpa0.96 - likelihood profile�p�ɁACPUE���ƂɖړI�֐��̒l���o�͂���I�v�V������ǉ��B���̑�
# rvpa1.0 - �p�b�P�[�W�Ƃ��Č��J�����o�[�W����
# rvpa1.3 - �d�ݕt���v�Z��ǉ�
# rvpa1.6 -warnings���b�Z�[�W��ǉ��Dvpa��index�̈ꕔ�������p����I�v�V����(use.index)��ǉ��DMSY.EST2�֐���ǉ�
# rvpa1.7 - selupdate+F����I�v�V�����D����t�œK���D
# rvpa.1.8 - Retrospective analysis���s�����߂̊֐� (retro.est, retro.est2) �ƁA���b�W��A���s�����߂̈��� (lambda) ��ǉ�(2017/05/29)

##

data.handler <- function(
  caa,
  waa,
  maa,
  index=NULL,
  M = 0.4,
  maa2=NULL,
  catch.prop=NULL
)
{
  years <- as.numeric(sapply(strsplit(names(caa[1,]),"X"), function(x) x[2]))

  if (is.null(dim(waa)) | dim(waa)[2]==1) waa <- as.data.frame(matrix(unlist(waa), nrow=nrow(caa), ncol=ncol(caa)))
  if (is.null(dim(maa)) | dim(maa)[2]==1) maa <- as.data.frame(matrix(unlist(maa), nrow=nrow(caa), ncol=ncol(caa)))

  colnames(caa) <- colnames(waa) <- colnames(maa) <- years

  if (!is.null(maa2)) {
    if (is.null(dim(maa2)) | dim(maa2)[2]==1) maa2 <- as.data.frame(matrix(unlist(maa2), nrow=nrow(caa), ncol=ncol(caa)))
    colnames(maa2) <- years
  }

  if (!is.null(catch.prop)) colnames(catch.prop) <- years
  
  if (!is.null(index)) colnames(index) <- years

  if (is.null(dim(M))) M <- as.data.frame(matrix(M, nrow=nrow(caa), ncol=ncol(caa)))

  colnames(M) <- years
  rownames(M) <- rownames(caa)

  res <- list(caa=caa, maa=maa, waa=waa, index=index, M=M, maa2=maa2, catch.prop=catch.prop)

  invisible(res)
}

# miscellaneous functions

#max.age <- function(x) max(which(!is.na(x)))
max.age.func <- function(x) max(which(!is.na(x)))

vpa.core <- function(caa,faa,M,k){
  out <- caa[,k]/(1-exp(-faa[,k]-M[,k]))*(faa[,k]+M[,k])/faa[,k]
  return(out)
}

vpa.core.Pope <- function(caa,faa,M,k){
  out <- caa[, k]*exp(M[, k]/2)/(1-exp(-faa[, k]))
  return(out)
}

ik.est <- function(caa,naa,M,i,k,min.caa=0.01,maxit=5,d=0.0001){
  K <- 1
  it <- 0
  
  f0 <- 1
  f1 <- NA

  if (!is.na(naa[i+1,k+1])){
    while(it < maxit & K > d){
      it <- it + 1
      f1 <- log(1+max(caa[i,k],min.caa)/naa[i+1,k+1]*exp(-M[i,k])*(f0+M[i,k])*(1-exp(-f0))/(f0*(1-exp(-f0-M[i,k]))))
      K <- sqrt((f1-f0)^2)
      f0 <- f1
    }
  }
  
  return(f1)
}

hira.est <- function(caa,naa,M,i,k,alpha=1,min.caa=0.01,maxit=5,d=0.0001){
  K <- 1
  it <- 0
  
  f0 <- 1
  f1 <- NA

  if (!is.na(naa[i+1,k+1])){
    while(it < maxit & K > d){
      it <- it + 1
      f1 <- log(1+(1-exp(-f0))*exp(-M[i,k])/(naa[i+1,k+1]*f0)*(max(caa[i+1,k],min.caa)*(alpha*f0+M[i+1,k])/(alpha*(1-exp(-alpha*f0-M[i+1,k])))*exp((1-alpha)*f0)+max(caa[i,k],min.caa)*(f0+M[i,k])/(1-exp(-f0-M[i,k]))))
      K <- sqrt((f1-f0)^2)
      f0 <- f1
    }
  }
  
  return(f1)
}

f.forward.est <- function(caa,naa,M,i,k,maxit=5,d=0.0001){
  K <- 1
  it <- 0
  
  f0 <- f1 <- 1
  
  while(it < maxit & K > d){
    it <- it + 1
    f1 <- caa[i,k]/naa[i,k]*(f0+M[i,k])/f0*1/(1-exp(-f0-M[i,k]))
    K <- sqrt((f1-f0)^2)
    f0 <- f1
  }
  
  return(f1)
}

fp.forward.est <- function(caa,naa,M,i,k,alpha=1,maxit=5,d=0.0001){
  K <- 1
  it <- 0
  
  f0 <- f1 <- 1
  
  while(it < maxit & K > d){
    it <- it + 1
    f1 <- 1/(1+alpha)*(caa[i,k]/naa[i,k]*(f0+M[i,k])*1/(1-exp(-f0-M[i,k]))+caa[i+1,k]/naa[i+1,k]*(alpha*f0+M[i+1,k])*1/(1-exp(-alpha0*f0-M[i+1,k])))
    K <- sqrt((f1-f0)^2)
    f0 <- f1
  }
  
  return(f1)
}

backward.calc <- function(caa,naa,M,na,k,min.caa=0.001,plus.group=TRUE){
  out <- rep(NA, na[k])
  if(na[k+1] > na[k]){
    for (i in 1:na[k]){
      out[i] <- naa[i+1,k+1]*exp(M[i,k])+caa[i,k]*exp(M[i,k]/2)
    }
  }
  else{
    for (i in 1:(na[k+1]-2)){
      out[i] <- naa[i+1,k+1]*exp(M[i,k])+caa[i,k]*exp(M[i,k]/2)
    }
    if (isTRUE(plus.group)){
      out[(na[k+1]-1):na[k]] <- pmax(caa[(na[k+1]-1):na[k],k],min.caa)/sum(pmax(caa[(na[k+1]-1):na[k],k],min.caa))*naa[na[k+1],k+1]*exp(M[(na[k+1]-1):na[k],k])+caa[(na[k+1]-1):na[k],k]*exp(M[(na[k+1]-1):na[k],k]/2)
    }
    else{
      out[na[k+1]-1] <- naa[na[k+1],k+1]*exp(M[na[k+1]-1,k])+caa[na[k+1]-1,k]*exp(M[na[k+1]-1,k]/2)
      out[na[k]] <- out[na[k+1]-1]*caa[na[k+1],k]/caa[na[k+1]-1,k]*exp((M[na[k+1],k]-M[na[k+1]-1,k])/2)
    }
  }
  return(out)
}

forward.calc <- function(faa,naa,M,na,k){
  out <- rep(NA, na[k])
  for (i in 2:(na[k]-1)){
    out[i] <- naa[i-1,k-1]*exp(-faa[i-1,k-1]-M[i-1,k-1])
  }
  out[na[k]] <- sum(sapply(seq(na[k]-1,max(na[k], na[k-1])), plus.group.eq, naa=naa, faa=faa, M=M, k=k))
  return(out)
}

plus.group.eq <- function(x, naa, faa, M, k) naa[x,k-1]*exp(-faa[x,k-1]-M[x,k-1])

f.at.age <- function(caa,naa,M,na,k,alpha=1) {
  out <- -log(1-caa[1:(na[k]-1),k]*exp(M[1:(na[k]-1),k]/2)/naa[1:(na[k]-1),k])
  c(out, alpha*out[length(out)])
}

sel.func <- function(faa, def="maxage") {
  if(def=="maxage") saa <- apply(faa, 2, function(x) x/x[length(x[!is.na(x)])])
  if(def=="max") saa <- apply(faa, 2, function(x) x/max(x,na.rm=TRUE))
  if(def=="mean") saa <- apply(faa, 2, function(x) x/sum(x,na.rm=TRUE))

  return(saa)
}

ff <- function(x, z) get(x)(z)

abund.extractor <- function(
  abund="SSB",
  naa,
  faa,
  dat,
  min.age=0,
  max.age=0,
  link="id",
  base=exp(1),
  af=1,
  catch.prop=NULL,
  sel.def="maxage",
  scale=1000
){
# abund = "N": abundance
# abund = "Nm": abundance at the middle of the year
# abund = "B": biomass
# abund = "Bm": abundance at the middle of the year
# abund = "SSB": SSB
# # abund = "SSB": SSB at the middle of the year

  naa <- as.data.frame(naa)
  faa <- as.data.frame(faa)

  waa <- dat$waa/scale
  maa <- dat$maa

  min.age <- min.age + 1
  max.age <- max.age + 1

 if (abund=="N") res <- colSums(naa[min.age:max.age,], na.rm=TRUE)
 if (abund=="Nm") res <- colSums(naa[min.age:max.age,]*exp(-dat$M[min.age:max.age,]/2-af*faa[min.age:max.age,]/2), na.rm=TRUE)
 if (abund=="B") res <- colSums((naa*waa)[min.age:max.age,], na.rm=TRUE)
 if (abund=="Bm") res <- colSums((naa*waa)[min.age:max.age,]*exp(-dat$M[min.age:max.age,]/2-af*faa[min.age:max.age,]/2), na.rm=TRUE)
 if (abund=="SSB"){
   ssb <- naa*waa*maa
   res <- colSums(ssb,na.rm=TRUE)
 }
 if (abund=="Bs"){
       saa <- sel.func(faa, def=sel.def)
       res <- colSums((naa*waa*saa)[min.age:max.age,], na.rm=TRUE)
 }
 if (abund=="Ns"){
       saa <- sel.func(faa, def=sel.def)
       res <- colSums((naa*saa)[min.age:max.age,], na.rm=TRUE)
 } 
 if (abund=="SSBm"){
   ssb <- naa*waa*maa*exp(-dat$M/2-af*faa/2)
   res <- colSums(ssb,na.rm=TRUE)
 }
 if (abund=="SSBsj"){
   maa2 <- dat$maa2
   ssb <- naa*waa*maa2*exp(-dat$M/2) 
   res <- colSums(ssb,na.rm=TRUE)
 }
 if (abund=="N1sj") res <- colSums(cbind(naa[1,-1]*exp(dat$M[1,-1]),NA), na.rm=TRUE)
 if (abund=="N0sj") res <- colSums(cbind(naa[1,-1]*exp(dat$M[1,-1]*2),NA), na.rm=TRUE)
 if (abund=="F") if (is.null(catch.prop)) res <- colMeans(faa[min.age:max.age,], na.rm=TRUE) else res <- colMeans(catch.prop[min.age:max.age, ]*faa[min.age:max.age,], na.rm=TRUE)
 
 if (link=="log") res <- log(res, base=base)

  return(invisible(res))
}

#

tmpfunc2 <- function(x=1,y=2,z=3){
  argname <- ls()  # �֐����Ăяo���ꂽ�΂���̂Ƃ���ls()�͈����݂̂������Ă���
  arglist <- lapply(argname,function(xx) eval(parse(text=xx)))
  names(arglist) <- argname
  value <- x+y+z
  return(list(value=value,args=arglist))
}

#
##

qbs.f <- function(q.const, b.const, sigma.const, index, Abund, nindex, index.w, max.dd=0.0001, max.iter=100){
  
  np.q <- length(unique(q.const[q.const > 0])) 
  np.b <- length(unique(b.const[b.const > 0])) 
  np.s <- length(unique(sigma.const[sigma.const > 0]))
  
  q <- b <- sigma <- numeric(nindex)
  
  q[1:nindex] <- b[1:nindex] <- sigma[1:nindex] <- 1
  
  delta <- 1
  
  obj <- NULL
  
  NN <- 0
  
  while(delta > max.dd & NN < max.iter){
    NN <- NN+1
    q0 <- q
    b0 <- b
    sigma0 <- sigma
    
    if (np.q > 0){
      for(i in 1:np.q){
        id <- which(q.const==i)
        num <- den <- 0
        for (j in id){
          avail <- which(!is.na(as.numeric(index[j,])))
          num <- num+index.w[j]*mean(log(as.numeric(index[j,avail]))-b[j]*log(as.numeric(Abund[j,avail])))/sigma[j]^2
          den <- den+index.w[j]/sigma[j]^2 
        }
        q[i] <- num/den
      }
    }
    if (np.b > 0){
      for(i in 1:np.b){
        id <- which(b.const==i)
        num <- den <- 0
        for (j in id){
          avail <- which(!is.na(as.numeric(index[j,])))
          num <- num+index.w[j]*cov(log(as.numeric(index[j,avail])),log(as.numeric(Abund[j,avail])))/var(log(as.numeric(Abund[j,avail])))/sigma[j]^2
          den <- den+index.w[j]/sigma[j]^2 
        }
        b[i] <- num/den
      }  
    }
    if (np.s > 0){
      for(i in 1:np.s){
        id <- which(sigma.const==i)
        num <- den <- 0
        for (j in id){
          avail <- which(!is.na(as.numeric(index[j,])))
          nn <- length(avail)
          num <- num+index.w[j]*sum((log(as.numeric(index[j,avail]))-q[j]-b[j]*log(as.numeric(Abund[j,avail])))^2)
          den <- den+index.w[j]*nn 
        }
        sigma[i] <- sqrt(num/den)
      }      
    }
    
    q[which(q.const>0)] <- q[q.const[which(q.const>0)]]
    b[which(b.const>0)] <- b[b.const[which(b.const>0)]]
    sigma[which(sigma.const>0)] <- sigma[sigma.const[which(sigma.const>0)]]
    
    delta <- max(c(sqrt((q-q0)^2),sqrt((b-b0)^2),sqrt((sigma-sigma0)^2)))
  }

  for (i in 1:nindex){
    avail <- which(!is.na(as.numeric(index[i,])))  
    obj <- c(obj, index.w[i]*(-as.numeric(na.omit(dnorm(log(as.numeric(index[i,avail])),log(q[i])+b[i]*log(as.numeric(Abund[i,avail])),sigma[i],log=TRUE)))))
  }
  
  convergence <- ifelse(delta <= max.dd, 1, 0) 
  
  return(list(q=q, b=b, sigma=sigma, obj=sum(obj), convergence=convergence))
}

qbs.f2 <- function(p0,index, Abund, nindex, index.w, fixed.index.var=NULL){
  
  if (is.null(fixed.index.var)) fixed.index.var <- matrix(0, nrow=nrow(index), ncol=ncol(index))
  if (class(fixed.index.var)=="numeric") fixed.index.var <- matrix(fixed.index.var, nrow=1)
  if (class(fixed.index.var)=="matrix" | class(fixed.index.var)=="data.frame") fixed.index.var <- array(fixed.index.var, dim=c(dim(fixed.index.var),1))
  
  np <- min(nindex,sum(index.w>0))
  
  p <- vector(length=2*np)
  q <- sigma <- vector(length=np)

  obj.f <- function(p){
    obj <- j <- 0
    
    for (i in 1:nindex){
      if (index.w[i] > 0 ){
      j <- j + 1
      q[j] <- exp(p[2*j-1])
      sigma[j] <- exp(p[2*j])
      
      avail <- which(!is.na(as.numeric(index[i,])))
      obj <- obj+index.w[i]*(-as.numeric(na.omit(dmvnorm(log(as.numeric(index[i,avail])),log(q[j])+log(as.numeric(Abund[i,avail])),as.matrix(fixed.index.var[avail,avail,j])+sigma[j]^2*diag(length(avail)),log=TRUE))))
      }
    }
    
    sum(obj)
  }
 
  res <- nlm(obj.f,p0)
  
  q <- res$estimate[1:np]
  sigma <- exp(res$estimate[1:np+np])
  obj <- res$minimum
  convergence <- res$code
    
   return(list(q=q, b=rep(1,np), sigma=sigma, obj=obj, convergence=convergence))
}
    
#

# vpa 
#

vpa <- function(
  dat,  # data for vpa
  sel.f = NULL,  # �ŏI�N�̑I��
  tf.year = 2008:2010, # terminal F���ǂ̔N�̕��ςɂ��邩
  rec.new = NULL, # ���N�̉������O����^����
  rec=NULL, # rec.year�̉���
  rec.year=2010,  # ������������ۂ̔N
  rps.year = 2001:2010, # ���N��RPS���ǂ͈̔͂̕��ςɂ��邩
  fc.year = 2009:2011, # Fcurrent�łǂ͈̔͂��Q�Ƃ��邩
  last.year = NULL,   # vpa���v�Z����ŏI�N���w��iretrospective analysis�j
  last.catch.zero = FALSE,   # TRUE�Ȃ狭���I�ɍŏI�N�̋��l�ʂ�0�ɂ���
  faa0 = NULL,  # sel.update=TRUE�̂Ƃ��C�����l�ƂȂ�faa
  naa0 = NULL,    # sel.update=TRUE�̂Ƃ��C�����l�ƂȂ�naa
  f.new = NULL,
  Pope = TRUE,  # Pope�̋ߎ������g�����ǂ���
  tune = FALSE,  # tuning�����邩�ǂ���
  abund = "B",   # tuning�̍ہC���̎w�W�ɑΉ����邩
  min.age = 0,  # tuning�w�W�̔N��Q�Ɣ͈͂̉���
  max.age = 0,  # tuning�w�W�̔N��Q�Ɣ͈͂̏��
  link = "id",  # tuning��link�֐�
  base = NA,  # link�֐���"log"�̂Ƃ��C������ɂ��邩
  af = NA,  # �����ʎw�����N�̒����̂Ƃ��Caf=0�Ȃ狙���O�Caf=1�Ȃ狙���^�񒆁Caf=2�Ȃ狙����ƂȂ�
  index.w = NULL,  # tuning index�̏d��
  use.index = "all",
  scale = 1000,  # �d�ʂ�scaling
  hessian = TRUE,
  alpha = 1,  # �ō���ƍō���-1��F�̔� F_a = alpha*F_{a-1}
  maxit = 5,  # �Ή��E�ݓc/�����̕��@�̍ő�J��Ԃ���
  d = 0.0001,  # �Ή��E�ݓc/�����̕��@�̎�������
  min.caa = 0.001,   # caa��0������Ƃ��C0��min.caa�Œu��������
  plot = FALSE,   # tuning�Ɏg���������ʎw���ɑ΂���t�B�b�g�̃v���b�g
  plot.year = 1998:2015,   # ��̃v���b�g�̎Q�ƔN
  term.F = "max",   # terminal F�̉��𐄒肷�邩: "max" or "all"
  plus.group = TRUE,  
  stat.tf = "mean",  # �ŏI�N��F�𐄒肷�铝�v�ʁi�N��ʂɗ^���邱�Ɖj
  add.p.est = NULL,  # �ǉ��ōō���ȊO��faa�𐄒肷��ہD�N����w�肷��D
  add.p.ini = NULL, 
  sel.update=FALSE,  # �`���[�j���OVPA�ɂ����āC�I�𗦂��X�V���Ȃ��琄��
  sel.def = "max",  #  sel.update=TRUE�őI�𗦂��X�V���Ă����ۂɁC�I�𗦂��ǂ̂悤�Ɍv�Z���邩�D�ő�l��1�Ƃ��邩�C���ϒl��1�ɂ��邩...
  max.dd = 0.000001,  # sel.update�̍ۂ̎�������
  intercept = FALSE,   # �����ʎw���̗\���l�ɐؕЂ��g��
  intercept.p.init = NULL,   # �ؕЂ����ꂽ�Ƃ��̃p�����[�^�̏����l
  ti.scale = NULL,   # �����ʂ̌W���ƐؕЂ�scaling
  tf.mat = NULL,   # terminal F�̕��ς��Ƃ�N�̐ݒ�D0-1�s��D
  eq.tf.mean = FALSE, # terminal F�̕��ϒl���ߋ���F�̕��ϒl�Ɠ���������
  no.est = FALSE,   # �p�����[�^���肵�Ȃ��D
  est.method = "ls",  # ������@ �ils = �ŏ����@�Cml = �Ŗޖ@�j
  b.est = FALSE,  #  b�𐄒肷�邩�ǂ���
  est.constraint = FALSE,   # ����t����������邩�ǂ���
  q.const = 1:length(abund),   # q�p�����[�^�̐���i0�͐��肵�Ȃ���1��fix�j
  b.const = 1:length(abund),   # b�p�����[�^�̐���i0�͐��肵�Ȃ���1��fix�j
  q.fix = NULL,
  b.fix = NULL,
  sigma.const = 1:length(abund),   # sigma�p�����[�^�̐���i0�͐��肵�Ȃ���1��fix�j
  fixed.index.var = NULL,
  max.iter = 100,    # q,b,sigma�v�Z�̍ۂ̍ő�J��Ԃ���
  optimizer = "nlm",
  Lower = -Inf,
  Upper = Inf,
  p.fix = NULL,
  lambda = 0,   # ridge��A�W��
  beta = 2,   # penalty��exponent  (beta = 1: lasso, 2: ridge)
  penalty = "p",
  ssb.def = "i",  # i: �N�͂��߁Cm: �N����, l: �N�Ō�
  ssb.lag = 0,   # 0: no lag, 1: lag 1
  ADMB=FALSE,
  p.init = 0.2   # ����p�����[�^�̏����l
)
{

  # input�f�[�^�����X�g��

  argname <- ls()  # �֐����Ăяo���ꂽ�΂���̂Ƃ���ls()�͈����݂̂������Ă���
  arglist <- lapply(argname,function(xx) eval(parse(text=xx)))
  names(arglist) <- argname
  
  # data handling

  caa <- dat$caa    # catch-at-age
  waa <- dat$waa    # weight-at-age
  maa <- dat$maa    # maturity-at-age
  if (!is.null(dat$maa2)) maa2 <- dat$maa2
  if (!is.null(dat$catch.prop)) catch.prop <- dat$catch.prop
  index <- dat$index   # abundance indices
  M <- dat$M    # natural mortality-at-age

  years <- dimnames(caa)[[2]]  # �N
  ages <- dimnames(caa)[[1]]  # �N��

  if (class(index)=="numeric") index <- t(as.matrix(index))

  if (use.index[1]!="all") {
    index <- index[use.index,,drop=FALSE]
    if (length(use.index)!=length(abund)){
      if (length(abund)>1) abund <- abund[use.index]
      if (length(min.age)>1) min.age <- min.age[use.index]
      if (length(max.age)>1) max.age <- max.age[use.index]
      if (length(link)>1) link <- link[use.index]
      if (length(base)>1) base <- base[use.index]
      if (length(af)>1) af <- af[use.index]            
      if (length(index.w)>1) index.w <- index.w[use.index]  
    }
  }

  #
  
  if(!is.null(fixed.index.var)) require(mvtnorm)

  # �ŏI�N last.year�ɒl�������Ă���ꍇ�́C����ȍ~�̃f�[�^���폜����iretrospective analysis�j
  if (!is.null(last.year)) {
    caa <- caa[,years <= last.year]
    waa <- waa[,years <= last.year]
    maa <- maa[,years <= last.year]
    if (!is.null(dat$maa2)) maa2 <- maa2[,years <= last.year] 
    if (!is.null(dat$cathc.prop)) maa2 <- catch.prop[,years <= last.year] 
    M <- M[,years <= last.year]
    if(!is.null(index)) index <- index[,years <= last.year,drop=FALSE]
    years <- dimnames(caa)[[2]]
    dat <- list(caa=caa, waa=waa, maa=maa, M=M, index=index)
  }

  na <- apply(caa, 2, max.age.func)  # �N���Ƃ̍ő�N��i�N�ɂ���čő�N��Ⴄ�ꍇ�ɑΉ����邽�߁j
  ny <- ncol(caa)  # �N�̐�
  
  if (isTRUE(last.catch.zero)) {caa[,ny] <- 0; ny <- ny - 1; n.add <- 1; saa.new <- NULL} else n.add <- 0  # �ŏI�N�̋��l�ʂ�0�Ƃ��C�N��1���炷

  if (term.F=="max") {  # �ō����F�����𐄒肷��ꍇ
    p.init <- ifelse(is.na(p.init[1]), M[na[ny],ny], p.init[1])   # �����l��NA�Ȃ�C�ŏI�N�ō���̎��R���S�W���������l�Ƃ���
    if (!is.null(add.p.est) & is.null(add.p.ini)) {add.p.est <- add.p.est + 1; p.init <- rep(p.init, length(add.p.est)+1)}  # add.p.est�ɐ���������΁C���̔N���ǉ��̃p�����[�^�Ƃ��Đ��肷��
    if (!is.null(add.p.est) & !is.null(add.p.ini)) {add.p.est <- add.p.est + 1; p.init <- c(add.p.ini,p.init)}  # add.p.est�ɐ���������΁C���̔N���ǉ��̃p�����[�^�Ƃ��Đ��肷��
  }
  if (term.F=="all"){  # �ŏI�N�̂��ׂĂ̔N���F�𐄒�
    if(length(p.init)==0) p.init <- rep(M[na[ny],ny], na[ny]-1)  # �����l��NA�Ȃ�C�ŏI�N�ō���̎��R���S�W����0~na-1�̏����l�Ƃ���
    if(length(p.init) < na[ny]-1) p.init <- rep(p.init[1], na[ny]-1)  # �����l�̐��������N�-1��菬�����ꍇ�́C�����l�̍ŏ��̒l��v�f�Ɏ��N�-1�̑傫���̃x�N�g���������l�Ƃ���
    if(length(p.init) >= na[ny]-1) p.init <- p.init[1:na[ny]-1]  # �����l�̐��������N�-1�ȏ�ł���΁C�N��ȏ�̒l�͎g�p���Ȃ�
  }

  if (length(stat.tf)==1) stat.tf <- rep(stat.tf, na[ny]-1)  # stat.tf��1�����w�肳��Ă���Ƃ��́C�S�N��̓��v�ʂ��g��

  # tuning�̍ۂ̃p�����[�^��1�����w�肳��Ă���ꍇ�́Cnindex�̐��������₷
  if (isTRUE(tune)){
    nindex <- nrow(index)

    if (nindex > length(abund) & length(abund)==1) abund <- rep(abund, nindex)
    if (nindex > length(min.age) & length(min.age)==1) min.age <- rep(min.age, nindex)
    if (nindex > length(max.age) & length(max.age)==1) max.age <- rep(max.age, nindex)
    if (nindex > length(link) & length(link)==1) link <- rep(link, nindex)
    if (nindex > length(base) & length(base)==1) base <- rep(base, nindex)

    if (is.null(index.w)) index.w <- rep(1, nindex)
    if (!is.na(af)) if(nindex > length(af) & length(af)==1) af <- rep(af, nindex)

    q <- rep(NA, nindex)
  }

  if (isTRUE(intercept)) p.init <- exp(c(log(p.init), intercept.p.init))   # �����ʎw���̗\���l�̐��`�֐����ؕЂ����ꍇ�C�p�����[�^��ǉ�

  # selectivity���X�V����ꍇ��faa0�Cnaa0���^�����Ă���΁C������g��
   if (!isTRUE(sel.update)) faa <- naa <- matrix(NA, nrow=max(na), ncol=ny+n.add, dimnames=list(ages, years))
   else {
     if(is.null(faa0) | is.null(naa0)) faa <- naa <- matrix(1, nrow=max(na), ncol=ny+n.add, dimnames=list(ages, years))
     else {faa <- as.matrix(faa0); naa <- as.matrix(naa0)}
   }

  if (is.null(p.fix)) p.fix <- 1:length(p.init)

  # warnings
  
  if (!tune & sel.update) print("sel.update = TRUE but tune=FALSE. So, the results are unreliable.")
  if (tune & is.null(sel.f) & (!sel.update & term.F=="max")) print("sel.f=NULL although tune=TRUE & sel.update=FALSE & term.F=max. The results are unreliable.")
  if (tune & (length(abund)!=nrow(index))) print("Check!: The number of abundance definition is different from the number of indices.")

#  ssb.def

  if (ssb.def=="i") ssb.coef <- 0
  if (ssb.def=="m") ssb.coef <- 0.5
  if (ssb.def=="l") ssb.coef <- 1  
  
# core function for optimization

  p.est <- function(log.p, out=FALSE){
 
    if (!isTRUE(intercept)) p <- exp(log.p)   # intercept==FALSE�Ȃ�C�p�����[�^��exp(log.p)
    else {  # intercept==TRUE�Ȃ�C������A�Ɋւ���p�����[�^��ǉ�
      p <- exp(log.p[1])
      Q1 <- log.p[2:(length(intercept.p.init)/2+1)]
      D1 <- log.p[(length(intercept.p.init)/2+2):(length(intercept.p.init)+1)]
    }

    # sel.f==NULL�ŁC�p�����[�^p��1�Ȃ�C�ŏI�N�ō����faa��naa�𐄒�
    if (is.null(sel.f) & length(p) == 1){
      faa[na[ny], ny] <- p
      if (isTRUE(Pope)) naa[na[ny], ny] <- caa[na[ny], ny]*exp(M[na[ny], ny]/2)/(1-exp(-faa[na[ny], ny]))
      else  naa[na[ny], ny] <- caa[na[ny], ny]/(1-exp(-faa[na[ny], ny]-M[na[ny], ny]))*(faa[na[ny],ny]+M[na[ny],ny])/faa[na[ny],ny]
    }

    # sel.f!=NULL�ŁC�p�����[�^���N��-1��菭�Ȃ��ꍇ�Csel.f���g���āC�ŏI�N/�S�N���faa��naa���v�Z
    if (!is.null(sel.f) & length(p) < na[ny]-1){
      if(length(p)==1) faa[, ny] <- sel.f*p
      if(length(p) > 1) {   # �p�����[�^����1���傫���ꍇ�Cadd.p.est�̕��C����p�����[�^���𑝂₷
      faa[,ny] <- sel.f*p[length(p)]
        for (i in 1:(length(p)-1)){
          faa[add.p.est[i],ny] <- p[i]*p[length(p)]
        }
      }
      if (isTRUE(Pope)) naa[, ny] <- vpa.core.Pope(caa,faa,M,ny)
      else  naa[, ny] <- vpa.core(caa,faa,M,ny)
    }

   #
   if (is.null(sel.f) & isTRUE(sel.update)){
      if(length(p)==1) faa[, ny] <- p
      if(length(p) > 1) {   # �p�����[�^����1���傫���ꍇ�Cadd.p.est�̕��C����p�����[�^���𑝂₷
      faa[,ny] <- p[length(p)]
      }
    }
    
   # �p�����[�^���N��-1�ł���΁C�������p�����[�^�Ƃ��čŏI�N/�S�N���faa��naa���v�Z
   if (length(p) == na[ny]-1){
     faa[1:(na[ny]-1), ny] <- p[p.fix]
     faa[na[ny], ny] <- alpha*p[na[ny]-1]
     if (isTRUE(Pope)) naa[, ny] <- vpa.core.Pope(caa,faa,M,ny)
     else naa[, ny] <- vpa.core(caa,faa,M,ny)
   }
   
   # selctivity���X�V���Ȃ��琄�肷��ꍇ
   if (isTRUE(sel.update)){
     dd <- 1
     while(dd > max.dd){
      saa <- sel.func(faa, def=sel.def)   # sel.def�ɏ]���đI�𗦂��v�Z
      for (i in (na[ny]-1):1){
        saa[i, ny] <- get(stat.tf[i])(saa[i, years %in% tf.year]) 
      }
 
      saa[na[ny], ny] <- get(stat.tf[na[ny]-1])(saa[na[ny], years %in% tf.year])
      if(length(p)==1) faa[1:na[ny], ny] <- p*sel.func(saa, def=sel.def)[1:na[ny],ny] else faa[1:na[ny], ny] <- p[length(p)]*sel.func(saa, def=sel.def)[1:na[ny],ny] 
    
      if (isTRUE(Pope)) naa[ , ny] <- vpa.core.Pope(caa,faa,M,ny)
      else naa[, ny] <- vpa.core(caa,faa,M,ny)
  
      if (isTRUE(Pope)){
        for (i in (ny-1):1){
         naa[1:na[i], i] <- backward.calc(caa,naa,M,na,i,min.caa=min.caa,plus.group=plus.group)
         faa[1:na[i], i] <- f.at.age(caa,naa,M,na,i,alpha=alpha)
       }
     }
     else{
       for (i in (ny-1):1){
         for (j in 1:(na[i]-2)){
           faa[j, i] <- ik.est(caa,naa,M,j,i,min.caa=min.caa,maxit=maxit,d=d)
         }
         if (isTRUE(plus.group)){
           faa[na[i]-1, i] <- hira.est(caa,naa,M,na[i]-1,i,alpha=alpha,min.caa=min.caa,maxit=maxit,d=d)
         }
         else faa[na[i]-1, i] <- ik.est(caa,naa,M,na[i]-1,i,min.caa=min.caa,maxit=maxit,d=d)
         
         faa[na[i], i] <- alpha*faa[na[i]-1, i]
         naa[1:na[i], i] <- vpa.core(caa,faa,M,i)
       }
     }

     faa1 <- faa
     saa1 <- sel.func(faa1, def=sel.def)

     for (i in (na[ny]-1):1){
       saa1[i, ny] <- get(stat.tf[i])(saa1[i, years %in% tf.year]) 
     }
     saa1[na[ny], ny] <- get(stat.tf[na[ny]-1])(saa1[na[ny], years %in% tf.year]) 
     if(length(p)==1) faa1[1:na[ny], ny] <- p*sel.func(saa1, def=sel.def)[1:na[ny],ny] else  faa1[1:na[ny], ny] <- p[length(p)]*sel.func(saa1, def=sel.def)[1:na[ny],ny]
     faa1[na[ny], ny] <- alpha*faa1[na[ny]-1, ny]
     
     dd <- max(sqrt((saa1[,ny] - saa[,ny])^2))

     faa <- faa1
   }

   saa <- sel.func(faa, def=sel.def)
   
     if(length(p) > 1) {   # �p�����[�^����1���傫���ꍇ�Cadd.p.est�̕��C����p�����[�^���𑝂₷
      for (i in 1:(length(p)-1)){
        faa[add.p.est[i],ny] <- p[i]
      }
      naa[, ny] <- vpa.core.Pope(caa,faa,M,ny)
      for (i in (ny-1):(ny-na[ny]+1)){
        naa[1:na[i], i] <- backward.calc(caa,naa,M,na,i,min.caa=min.caa,plus.group=plus.group)
        faa[1:na[i], i] <- f.at.age(caa,naa,M,na,i,alpha=alpha)
      }
    }   
 }

   if (!isTRUE(sel.update)){
   if (isTRUE(Pope)){
     for (i in (ny-1):1){
       naa[1:na[i], i] <- backward.calc(caa,naa,M,na,i,min.caa=min.caa,plus.group=plus.group)
       faa[1:na[i], i] <- f.at.age(caa,naa,M,na,i,alpha=alpha)
      }
   }
  else{
     for (i in (ny-1):1){
       for (j in 1:(na[i]-2)){
         faa[j, i] <- ik.est(caa,naa,M,j,i,min.caa=min.caa,maxit=maxit,d=d)
       }
       if (isTRUE(plus.group)) faa[na[i]-1, i] <- hira.est(caa,naa,M,na[i]-1,i,alpha=alpha,min.caa=min.caa,maxit=maxit,d=d)
       else faa[na[i]-1, i] <- ik.est(caa,naa,M,na[i]-1,i,min.caa=min.caa,maxit=maxit,d=d)
       faa[na[i], i] <- alpha*faa[na[i]-1, i]
       naa[1:na[i], i] <- vpa.core(caa,faa,M,i)
     }
   }

    if (is.na(naa[na[ny]-1,ny])){
      if(isTRUE(Pope)){
        for (i in (na[ny]-1):1){
          if (is.null(tf.mat)) faa[i, ny] <- get(stat.tf[i])(faa[i, years %in% tf.year]) 
          else faa[i, ny] <- get(stat.tf[i])(faa[i, !is.na(tf.mat[i,])]) 
          naa[i, ny] <- caa[i, ny]*exp(M[i, ny]/2)/(1-exp(-faa[i, ny]))
          k <- 0
          for (j in (i-1):1){
            k <- k + 1
            if (i-k > 0){
              naa[j,ny-k] <- naa[j+1,ny-k+1]*exp(M[j,ny-k])+caa[j,ny-k]*exp(M[j,ny-k]/2)
              faa[j,ny-k] <- -log(1-caa[j,ny-k]*exp(M[j,ny-k]/2)/naa[j,ny-k])
            }  
          }
        }
      }
      else{
        for (i in (na[ny]-1):1){
          faa[i, ny] <- get(stat.tf[i])(faa[i, years %in% tf.year]) 
          naa[i, ny] <- caa[i, ny]/(1-exp(-faa[i, ny]-M[i, ny]))*(faa[i, ny]+M[i, ny])/faa[i, ny]
          k <- 0
          for (j in (i-1):1){
            k <- k + 1
            if (i-k > 0){
              faa[j,ny-k] <- ik.est(caa,naa,M,j,ny-k,min.caa=min.caa,maxit=maxit,d=d)
              naa[j,ny-k] <- caa[j, ny-k]/(1-exp(-faa[j, ny-k]-M[j, ny-k]))*(faa[j, ny-k]+M[j, ny-k])/faa[j, ny-k]
            }  
          }
        }
      }
    }
   }

   if (!is.null(rec)){
     naa[1, years %in% rec.year] <- rec
     if(isTRUE(Pope)) faa[1, years %in% rec.year] <- -as.numeric(log(1-caa[1, years %in% rec.year]/naa[1, years %in% rec.year]*exp(M[1, years %in% rec.year]/2)))
     else{ 
       for (j in which(years %in% rec.year)){
         faa[1,j] <- f.forward.est(caa,naa,M,1,j,maxit=maxit,d=d)
       }
     }

     terminal.year <- as.numeric(years[ny])
     for (kk in 1:length(rec.year)){
       for (i in rec.year[kk]:terminal.year){
         if(terminal.year-i > 0 & i-rec.year[kk]+1 <= max(ages)){
           naa[i-rec.year[kk]+2, years %in% (i+1)] <- naa[i-rec.year[kk]+1, years %in% i]*exp(-faa[i-rec.year[kk]+1, years %in% i]-M[i-rec.year[kk]+1, years %in% i])
           if (isTRUE(Pope)) faa[i-rec.year[kk]+2, years %in% (i+1)] <- -log(1-caa[i-rec.year[kk]+2, years %in% (i+1)]/naa[i-rec.year[kk]+2, years %in% (i+1)]*exp(M[i-rec.year[kk]+2, years %in% (i+1)]/2))
           else {
             for (j in which(years %in% (i+1))){
               if(i-rec.year[kk]+2 < na[j]-1) faa[i-rec.year[kk]+2, j] <- f.forward.est(caa,naa,M,i-rec.year[kk]+2,j,maxit=maxit,d=d)
               if (isTRUE(plus.group)){
                 if(i-rec.year[kk]+2 == na[j]-1) faa[i-rec.year[kk]+2, j] <- fp.forward.est(caa,naa,M,i-rec.year[kk]+2,j,alpha,maxit=maxit,d=d) 
                 if(i-rec.year[kk]+2 == na[j]) faa[i-rec.year[kk]+2, j] <- alpha*fp.forward.est(caa,naa,M,i-rec.year[kk]+1,j,alpha,maxit=maxit,d=d)
               } 
               else{
                 if(i-rec.year[kk]+2 == na[j]-1) faa[i-rec.year[kk]+2, j] <- f.forward.est(caa,naa,M,i-rec.year[kk]+2,j,maxit=maxit,d=d) 
                 if(i-rec.year[kk]+2 == na[j]) faa[i-rec.year[kk]+2, j] <- alpha*f.forward.est(caa,naa,M,i-rec.year[kk]+1,j,maxit=maxit,d=d) 
               }
             }
           }
         }
       }
     }
   }

  # next year

    if (isTRUE(tune)){
      if (n.add==1 & !is.na(mean(index[,ny+n.add],na.rm=TRUE))){
 
        new.naa <- forward.calc(faa,naa,M,na,ny+n.add)

        naa[,ny+n.add] <- new.naa
        baa <- naa*waa
        ssb <- baa*maa*exp(-ssb.coef*(faa+M))
    
        if (is.null(rec.new)) {
          new.naa[1] <- median((naa[1,]/colSums(ssb))[years %in% rps.year])*sum(ssb[,ny+n.add],na.rm=TRUE)
        }
        else new.naa[1] <- rec.new

        naa[1,ny+n.add] <- new.naa[1]
        baa[1,ny+n.add] <- naa[1,ny+n.add]*waa[1,ny+n.add]

        if (!is.null(f.new) & !is.null(saa.new)) faa[,ny+n.add] <- f.new*saa.new else faa[,ny+n.add] <- 0
         if (isTRUE(Pope)) caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]))*exp(-M[,ny+n.add]/2) else caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]-M[,ny+n.add]))*faa[,ny+n.add]/(faa[,ny+n.add]+M[,ny+n.add])
 
        ssb[1,ny+n.add] <- baa[1,ny+n.add]*maa[1,ny+n.add]*exp(-ssb.coef*(faa[1,ny+n.add]+M[1,ny+n.add]))

        if (ssb.lag==1) ssb <- cbind(NA, ssb[,-ncol(ssb)])
      }

  # tuning
  
    obj <- NULL

   if (tune){
     if (est.constraint | !is.null(fixed.index.var)){
   
       Abund <- NULL
       
       for (i in 1:nindex){
         abundance <- abund.extractor(abund=abund[i], naa, faa, dat, min.age=min.age[i], max.age=max.age[i], link=link[i], base=base[i], af=af[i], catch.prop=catch.prop, sel.def=sel.def, scale=scale)
         Abund <- rbind(Abund, abundance)
       }
   
       if (is.null(fixed.index.var)) est.qbs <- qbs.f(q.const, b.const, sigma.const, index, Abund, nindex, index.w, max.dd, max.iter) else {
       p00 <- c(log(q.const[which(index.w >0)]), log(sigma.const[which(index.w >0)]))
       est.qbs <- qbs.f2(p00, index, Abund, nindex, index.w, fixed.index.var)
       }
      
       q <- exp(est.qbs$q)
       b <- est.qbs$b     
       sigma <- est.qbs$sigma
       obj <- est.qbs$obj
       convergence <- est.qbs$convergence
       obj0 <- obj
       
       rownames(Abund) <- 1:nindex
     }
     else{
       Abund <- nn <- sigma <- b <- NULL

       for (i in 1:nindex){
         abundance <- abund.extractor(abund=abund[i], naa, faa, dat, min.age=min.age[i], max.age=max.age[i], link=link[i], base=base[i], af=af[i], catch.prop=catch.prop, sel.def=sel.def, scale=scale)
         Abund <- rbind(Abund, abundance)
        
       if (est.method=="ls"){
          if (!isTRUE(intercept)){
            avail <- which(!is.na(as.numeric(index[i,])))
            if (b.est) {
              if (is.null(b.fix)){
                b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail])))
              } else{
                if (is.na(b.fix[i])) b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail]))) else b[i] <- b.fix[i]
              }
            }
            else {
              if (is.null(b.fix)) b[i] <- 1 else b[i] <- b.fix[i]
            }
            if (is.null(q.fix)) q[i] <- exp(mean(log(as.numeric(index[i,avail]))-b[i]*log(as.numeric(abundance[avail])))) else q[i] <- q.fix[i]
            obj <- c(obj,index.w[i]*sum((log(as.numeric(index[i,avail]))-log(q[i])-b[i]*log(as.numeric(abundance[avail])))^2))
          }
          if (isTRUE(intercept)){
            obj <- c(obj,index.w[i]*sum((log(as.numeric(index[i,]))-log(Q1[i]*as.numeric(abundance)*ti.scale[1]+D1[i]*ti.scale[2]))^2, na.rm=TRUE))
          }
        }
        if (est.method=="ml"){
          if (!isTRUE(intercept)){
            avail <- which(!is.na(as.numeric(index[i,])))
            nn[i] <- length(avail)
            if (b.est) {
              if (is.null(b.fix)){
                b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail])))
              } else{
                if (is.na(b.fix[i])) b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail]))) else b[i] <- b.fix[i]
              }
            } else {
              if (is.null(b.fix)) b[i] <- 1 else b[i] <- b.fix[i]
            }
            if (is.null(q.fix)) q[i] <- exp(mean(log(as.numeric(index[i,avail]))-b[i]*log(as.numeric(abundance[avail])))) else q[i] <- q.fix[i]
            sigma[i] <- sqrt(sum((log(as.numeric(index[i,avail]))-log(q[i])-b[i]*log(as.numeric(abundance[avail])))^2)/nn[i])
            obj <- c(obj,index.w[i]*(-as.numeric(na.omit(dnorm(log(as.numeric(index[i,avail])),log(q[i])+b[i]*log(as.numeric(abundance[avail])),sigma[i],log=TRUE)))))
          }
       # if (isTRUE(intercept)){
       #   obj <- c(obj,index.w[i]*sum((log(as.numeric(index[i,]))-log(Q1[i]*as.numeric(abundance)*ti.scale[1]+D1[i]*ti.scale[2]))^2, na.rm=TRUE))
       # }
        }
      }      

      obj0 <- obj
      obj <- sum(obj)
      convergence <- 1
      saa <- sel.func(faa, def=sel.def)

      if (penalty=="p") obj <- (1-lambda)*obj + lambda*sum(p^beta)     
      
      if (penalty=="s") obj <- (1-lambda)*obj + lambda*sum((abs(saa[,ny]-apply(saa[, years %in% tf.year],1,get(stat.tf))))^beta)
      
      rownames(Abund) <- 1:nindex
      } 
    }
  }
  else {obj <- (p - alpha*faa[na[ny]-1, ny])^2; obj0 <- NA}
    
  #

    if (isTRUE(out)) {
        # next year

        if (n.add==1 & is.na(naa[1,ny+n.add])){
          new.naa <- forward.calc(faa,naa,M,na,ny+n.add)

          naa[,ny+n.add] <- new.naa
          baa <- naa*waa
          ssb <- baa*maa*exp(-ssb.coef*(faa+M))

          if (is.null(rec.new)) {
            new.naa[1] <- median((naa[1,]/colSums(ssb))[years %in% rps.year])*sum(ssb[,ny+n.add],na.rm=TRUE)
          } else new.naa[1] <- rec.new

          naa[1,ny+n.add] <- new.naa[1]
          baa[1,ny+n.add] <- naa[1,ny+n.add]*waa[1,ny+n.add]
    
          if (!is.null(f.new) & !is.null(saa.new)) faa[,ny+n.add] <- f.new*saa.new else faa[,ny+n.add] <- 0
          if (isTRUE(Pope)) caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]))*exp(-M[,ny+n.add]/2) else caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]-M[,ny+n.add]))*faa[,ny+n.add]/(faa[,ny+n.add]+M[,ny+n.add])
          
          ssb[1,ny+n.add] <- baa[1,ny+n.add]*maa[1,ny+n.add]*exp(-ssb.coef*(faa[1,ny+n.add]+M[1,ny+n.add]))

        if (ssb.lag==1) ssb <- cbind(NA, ssb[,-ncol(ssb)])
        } 
        else {
          baa <- naa*waa
          ssb <- baa*maa*exp(-ssb.coef*(faa+M))
          if (ssb.lag==1) ssb <- cbind(NA, ssb[,-ncol(ssb)])
        }

        if (isTRUE(intercept)) q <- cbind(Q1,D1)      
        
        obj <- list(minimum=obj, minimum.c=obj0, caa=caa, naa=naa, faa=faa, baa=baa, ssb=ssb)
        if (isTRUE(eq.tf.mean)) obj$p <- max(faa[,ny],na.rm=TRUE)

        if (isTRUE(tune)) {
          if (est.method=="ls"){
            if (use.index[1]=="all") Nindex <- sum(!is.na(index[index.w > 0,])) else Nindex <- sum(!is.na(index[index.w[use.index > 0] > 0,])) 
            Sigma2 <- obj$minimum/Nindex
            neg.logLik <- Nindex/2*log(2*pi*Sigma2)+Nindex/2
            obj$q <- q
            obj$b <- b
            obj$sigma <- sqrt(Sigma2)
            obj$convergence <- convergence  
            obj$Abund <- Abund
            obj$logLik <- -neg.logLik
          }
          if (est.method=="ml"|est.constraint| !is.null(fixed.index.var)){
            if (est.constraint){
              names(q) <- q.const
              names(b) <- b.const              
              names(sigma) <- sigma.const          
            }
            obj$convergence <- convergence  
            obj$q <- q
            obj$b <- b
            obj$sigma <- sigma
            obj$Abund <- Abund
            obj$logLik <- -obj$minimum
          }          
   
        }
    }
    
    return(obj)   # �ړI�֐���Ԃ�
  }

  # execution of optimization
  if (isTRUE(ADMB)){
    require(R2admb)
    
    index2 <- as.matrix(t(apply(index,1,function(x) ifelse(is.na(x),0,x))))
 
    Type <- ifelse(abund=="SSB", 1, ifelse(abund=="B",4,ifelse(abund=="N",2,3)))
    
    if(is.null(dat$maa2)) MAA <- as.matrix(dat$maa) else MAA <- as.matrix(dat$maa2)
    if (is.na(af[1])) af <- rep(0,nindex)
    
    data2 <- list(A=nrow(dat$caa),Y=ncol(dat$caa),K=length(use.index),Est=ifelse(est.method=="ls",0,1),b_est=as.numeric(b.est),alpha=alpha,lambda=lambda,beta=beta,Type=Type,w=index.w,af=af,CATCH=as.matrix(dat$caa),WEI=as.matrix(dat$waa/scale),MAT=MAA,M=as.matrix(dat$M),CPUE=index2,MISS=ifelse(index2==0,1,0))
    
    init <- log(p.init)
    
    write_dat("vpa",data2)
    write_pin("vpa",init)

    system("vpa -nohess")
 
    summary.p.est <- read_pars("vpa")
    summary.p.est$estimate <- exp(summary.p.est$coeflist$log_F)
    summary.p.est$minimum <- -summary.p.est$loglik  
    summary.p.est$gradient <- summary.p.est$maxgrad
    summary.p.est$code <- 0
    log.p.hat <- log(summary.p.est$estimate)
  } else {
  if (isTRUE(no.est)){
    if (isTRUE(eq.tf.mean)) {
      summary.p.est <- p.est(log(p.init), out=TRUE)
      summary.p.est <- list(estimate=summary.p.est$p, minimum=p.est(log(summary.p.est$p)), gradient=NA, code=NA)
      log.p.hat <- log(summary.p.est$estimate)
    }
    else{
      summary.p.est <- list(estimate=log(p.init), minimum=p.est(log(p.init)), gradient=NA, code=NA)
      log.p.hat <- summary.p.est$estimate
    }
  }
  else{
    if (optimizer=="nlm") summary.p.est <- nlm(p.est, log(p.init), hessian=hessian)
    if (optimizer=="nlminb") {
      summary.p.est <- nlminb(log(p.init), p.est, hessian=hessian, lower=Lower, upper=Upper)
      summary.p.est$estimate <- summary.p.est$par
      summary.p.est$minimum <- summary.p.est$objective    
      summary.p.est$gradient <- NA  
      summary.p.est$code <- summary.p.est$convergence
    }
    log.p.hat <- summary.p.est$estimate
  }
  }

  gradient <- summary.p.est$gradient
  code <- summary.p.est$code
  message.nlminb <- summary.p.est$message
      
  np <- length(summary.p.est$estimate)

  out <- p.est(log.p.hat, out=TRUE)

  term.f <- exp(log.p.hat)
  
  # 

  if(isTRUE(hessian)) hessian <- summary.p.est$hessian

  naa <- as.data.frame(out$naa)
  faa <- as.data.frame(out$faa)
  baa <- as.data.frame(out$baa)
  ssb <- as.data.frame(out$ssb)
  saa <- as.data.frame(sel.func(faa, def=sel.def))

  if(isTRUE(tune)){
    logLik <- out$logLik
    sigma <- out$sigma
    q <- out$q
    b <- out$b
    convergence <- out$convergence
    message <- message.nlminb
    if (!isTRUE(intercept))  pred.index <- q*out$Abund^b
    else pred.index <-  q[,1]*out$Abund*ti.scale[1]+q[,2]*ti.scale[2]
  }
 else logLik <- sigma <- q <- b <- convergence <- message <- pred.index <- NULL

  Ft <- mean(faa[,ny],na.rm=TRUE)
  Fc.at.age <- apply(faa[,years %in% fc.year,drop=FALSE],1,mean)  # drop=FALSE�ŁC�s��̃x�N�g������h��
  Fc.mean <- mean(Fc.at.age,na.rm=TRUE)
  Fc.max <- max(Fc.at.age,na.rm=TRUE)

  res <- list(input=arglist, term.f=term.f, np=np, minimum=out$minimum, minimum.c=out$minimum.c, logLik=logLik, gradient=gradient, code=code, q=q, b=b, sigma=sigma, convergence=convergence, message=message, hessian=hessian, Ft=Ft, Fc.at.age=Fc.at.age, Fc.mean=Fc.mean, Fc.max=Fc.max, last.year=last.year, Pope=Pope, ssb.coef=ssb.coef, pred.index=pred.index, naa=naa, faa=faa, baa=baa, ssb=ssb, saa=saa)

  if (isTRUE(plot) & isTRUE(tune)){
    for (i in 1:nindex){
      Y <- years %in% plot.year
      if (!isTRUE(intercept)) Pred <- (index[i,Y]/q[i])^(1/b[i]) else Pred <- (index[i,Y]-q[i,2]*ti.scale[2])/(q[i,1]*ti.scale[1])
      plot(years[Y], Pred, ylim=range(Pred, out$Abund[i,Y], na.rm=TRUE),col=3,pch=16,xlab="Year",ylab=paste("index", i), main=abund[i])
      lines(years[Y],out$Abund[i,Y],col=2,lwd=2)
   }
  }

  return(invisible(res))
}

# profile likelihood (one parameter)

profile.likelihood.vpa <- function(res,Alpha=0.95,min.p=1.0E-6,max.p=1,L=20,method="ci"){
   
   res.c <- res
   res.c$input$no.est <- TRUE
   res.c$input$plot <- FALSE

   like <- function(p,method="ci") {
     res.c$input$p.init <- p

     res1 <- do.call(vpa,res.c$input)

     if (method=="ci") obj <- (-2*(res1$logLik - res$logLik)-qchisq(Alpha,1))^2
#     if (method=="dist") obj <- res1$logLik
     if (method=="dist"){   # �s���ύX
          obj <- list(logLik=res1$logLik,LLs=res1$minimum.c)
     }
     return(obj)
  }

  if (method=="ci"){
    res.lo <- nlminb(start=res$term.f*0.5, like, lower=0.001, upper=0.999*res$term.f, method="ci")
    res.up <- nlminb(start=res$term.f*1.5, like, lower=1.001*res$term.f, upper=Inf, method="ci")
    out <- list(lower=res.lo,upper=res.up,ci=c(res.lo$par, res.up$par))
  }
  if (method=="dist"){
    p0 <- seq(min.p,max.p,len=L)
    tmp <- lapply(p0, like, method="dist")
    out <- list(logLik=sapply(tmp,function(x) x$logLik),
    			LLs = sapply(tmp,function(x) x$LLs))
    out$TLL <- -out$logLik - min(-out$logLik)
    out$RLLs <- sweep(out$LLs,1,apply(out$LLs,1,min),FUN="-")
    out$p0 <- p0
#    out <- p0 # �s���ύX
  }

  return(out)
}

dp.est <- function(p,res,Ref,target="F",beta=1){
    res.c <- res
    res.c$input$no.est <- TRUE
    res.c$input$plot <- FALSE

    res.c$input$p.init <- p

    ny <- length(res.c$faa[1,])
    na <- length(res.c$faa[,ny])

     res1 <- do.call(vpa,res.c$input)

     if (target=="B") out <- -res1$logLik+beta*(sum(res1$baa[,ny])-Ref)^2
     if (target=="F") out <- -res1$logLik+beta*(res1$faa[na,ny]-Ref)^2

     return(out)
}

pl.ci.dp <- function(res,target="F",beta=10^5,Alpha=0.8,lo.p=0.1,up.p=2.0,lo.Ref=0.5,up.Ref=3,method="ci"){
    res.c <- res
    res.c$input$no.est <- TRUE
    res.c$input$plot <- FALSE

     p.est <- function(Ref) optimize(dp.est,c(lo.p,up.p),res=res,Ref=Ref,target=target,beta=beta)$minimum

    ny <- length(res$faa[1,])
    na <- length(res$faa[,ny])

    if (target=="F") Ref0 <- res$faa[na,ny]
    if (target=="B") Ref0 <- sum(res$baa[,ny])

      like <- function(Ref,method="ci") {

        p <- p.est(Ref)

        res.c$input$p.init <- p

        res1 <- do.call(vpa,res.c$input)
        
  if (method=="ci") obj <- -2*(res1$logLik - res$logLik)-qchisq(Alpha,1)
     if (method=="dist") obj <- res1$logLik
     return(obj)
  }

  if (method=="ci"){
    res.lo <- uniroot(like, lower=Ref0*lo.Ref, upper=Ref0, method="ci")
    res.up <- uniroot(like, lower=Ref0, upper=Ref0*up.Ref, method="ci")
    out <- list(lower=res.lo,upper=res.up,ci=c(res.lo$root, res.up$root))
  }
  if (method=="dist"){
    p0 <- seq(Ref0*lo.Ref,Ref0*up.Ref,len=L)
    out <- sapply(p0, like, method="dist")
  }

  return(out)
}

profile.likelihood.vpa.B <- function(res,Alpha=0.95,min.p=1.0E-6,max.p=1,L=20,method="ci"){
   
   res.c <- res
   res.c$input$no.est <- TRUE

   like <- function(p,method="ci") {

     Bm <- exp(p)

     p0 <- res.c$term.f

     f1 <- function(p0){
       res.c$input$p.init <- p0
       res1 <- do.call(vpa,res.c$input)
       (sum(res1$baa[,37])-Bm)^2
     }

     res1 <- nlm(f1,p0)

     res.c$input$p.init <- res1$estimate
     res1 <- do.call(vpa,res.c$input)
      
     if (method=="ci") obj <- (-2*(res1$logLik - res$logLik)-qchisq(Alpha,1))^2
     if (method=="dist") obj <- res1$logLik
     return(obj)
  }

  if (method=="ci"){
    res.lo <- nlminb(start=log(sum(res$baa[,37])*0.5), like, lower=-Inf, upper=log(sum(res$baa[,37])), method="ci")
    res.up <- nlminb(start=log(sum(res$baa[,37])*1.5), like, lower=log(sum(res$baa[,37])), upper=Inf, method="ci")
    out <- list(lower=res.lo,upper=res.up,ci=c(res.lo$par, res.up$par))
  }
  if (method=="dist"){
    p0 <- seq(min.p,max.p,len=L)
    out <- sapply(p0, like, method="dist")
  }

  return(out)
}

# bootstrap

boo.vpa <- function(res,B=5,method="p",mean.correction=FALSE){
  ## method == "p": parametric bootstrap
  ## method == "n": non-parametric bootstrap
  ## method == "r": smoothed residual bootstrap-t

  index <- res$input$dat$index
  p.index <- res$pred.index
  resid <- log(as.matrix(index))-log(as.matrix(p.index))
  
  R <- nrow(resid)

  n <- apply(resid,1,function(x) sum(!is.na(x)))

  np <- res$np

  rs2 <- rowSums(resid^2, na.rm=TRUE)/(n-np)

  res.c <- res
  
  res.c$input$p.init <- res$term.f[1]
  
  b.index <- res$input$dat$index
  
  Res1 <- list()

  for (b in 1:B){
    print(b)

    for (i in 1:R){
      if (method=="p") b.index[i,!is.na(index[i,])] <- exp(log(p.index[i,!is.na(index[i,])]) + rnorm(sum(!is.na(index[i,])),0,sd=sqrt(rs2[i])))
      if (method=="n") b.index[i,!is.na(index[i,])] <- exp(log(p.index[i,!is.na(index[i,])]) + sample(resid[i,!is.na(index[i,])],length(index[i,!is.na(index[i,])]),replace=TRUE))
      if (isTRUE(mean.correction)) b.index[i,!is.na(index[i,])] <- b.index[i,!is.na(index[i,])]*exp(-rs2[i]/2)
      if (method=="r") {
        rs.d <- density(resid[i,!is.na(index[i,])])
        rs.db <- sample(rs.d$x,length(index[i,!is.na(index[i,])]),prob=rs.d$y,replace=TRUE)
        sd.j <- sd(rs.db)
        s.rs.b <- rs.db/sd.j
        b.index[i,!is.na(index[i,])] <- exp(log(p.index[i,!is.na(index[i,])]) + s.rs.b*sqrt(rs2[i]))
      }
      if (isTRUE(mean.correction)) b.index[i,!is.na(index[i,])] <- b.index[i,!is.na(index[i,])]*exp(-rs2[i]/2)
    }
  
    res.c$input$dat$index <- b.index

    res1 <- try(do.call(vpa,res.c$input))
    if(class(res1)=="try-error"){
      Res1[[b]] <- "try-error"
    }
    else{
      Res1[[b]] <- list(index=b.index,naa=res1$naa,baa=res1$baa,ssb=res1$ssb,faa=res1$faa,saa=res1$saa,
                      Fc.at.age=res1$Fc.at.age,q=res1$q,b=res1$b,sigma=res1$sigma) # 2013.8.20�ǋL(�s���)
    }
  }

  return(Res1)
}

# SR estimation

SR.est.old <- function(res, model="BH", method="log", scale=1){
  SSB <- colSums(res$ssb,na.rm=TRUE)/scale
  R <- unlist(res$naa[1,])
  
  if (model=="BH"){
    res0 <- lm(SSB/R ~ SSB)

    a0 <- 1/res0$coef[1]
    b0 <- res0$coef[2]*a0
  }
  if (model=="RI"){
    res0 <- lm(log(R/SSB) ~ SSB)

    a0 <- exp(res0$coef[1])
    b0 <- -res0$coef[2]
  }

  p0 <- log(c(a0,b0))  

  data <- data.frame(SSB=SSB,R=R)

  if (model=="BH"){
    if (method=="id") res <- nls(R~exp(log.a)*SSB/(1+exp(log.b)*SSB), data, start=list(log.a=p0[1],log.b=p0[2]))
    if (method=="log") res <- nls(log(R)~log.a+log(SSB)-log(1+exp(log.b)*SSB), data, start=list(log.a=p0[1],log.b=p0[2]))
   }

  if (model=="RI"){
    if (method=="id") res <- nls(R~exp(log.a)*SSB*exp(-exp(log.b)*SSB), data, start=list(log.a=p0[1],log.b=p0[2]))
    if (method=="log") res <- nls(log(R)~log.a+log(SSB)-exp(log.b)*SSB, data, start=list(log.a=p0[1],log.b=p0[2]))
   }

  par <- exp(coef(res))
  names(par) <- c("a","b")

  out <- list(res=res, model=model, method=method, par=par)

  return(out)
}

# MSY estimation

MSY.est <- function(res,model="schaefer",r.fix=NULL,K.fix=NULL,p.init=NULL,scale=1,main=""){
  B <- colSums(res$baa,na.rm=TRUE)/scale
  C <- colSums(res$input$dat$caa*res$input$dat$waa,na.rm=TRUE)/scale

  n <- length(B)

  if (C[n]==0) {n <- n-1; B <- B[1:n]; C <- C[1:n]}

  B2 <- B[2:n]
  B1 <- B[1:(n-1)]
  C1 <- C[1:(n-1)]
  
  S1 <- B2 - B1 + C1

  if (is.null(p.init)){
    if (model=="schaefer") {
      res0 <- lm(S1/B1 ~ B1)
      r0 <- res0$coef[1]
      K0 <- -r0/res0$coef[2]
    }
    if (model=="fox") {
      res0 <- lm(S1/B1 ~ log(B1))
      r0 <- res0$coef[1]
      K0 <- exp(-r0/res0$coef[2])
    }

    p0 <- log(c(max(r0, 0.001), max(K0, 100)))
  }
  else p0 <- p.init

  data <- data.frame(S1=S1,B1=B1)

  if (is.null(r.fix) & is.null(K.fix)){
    if (model=="schaefer") res <- nls(S1~exp(log.r)*B1*(1-B1/exp(log.K)), data, start=list(log.r=p0[1],log.K=p0[2]))
    if (model=="fox") res <- nls(S1~exp(log.r)*B1*(1-log(B1)/log.K), data, start=list(log.r=p0[1],log.K=p0[2]))

    p <- exp(coef(res))
    names(p) <- c("r","K")
  }
  else {
    if (!is.null(r.fix) & is.null(K.fix)){
      if (model=="schaefer") res <- nls(S1~r.fix*B1*(1-B1/exp(log.K)), data, start=list(log.K=p0[2]))
      if (model=="fox") res <- nls(S1~r.fix*B1*(1-log(B1)/log.K), data, start=list(log.K=p0[2]))

      p <- c(r.fix, exp(coef(res)))
      names(p) <- c("r","K")
    }
    if (is.null(r.fix) & !is.null(K.fix)){
      log.K <- log(K.fix)
      if (model=="schaefer") res <- nls(S1~exp(log.r)*B1*(1-B1/exp(log.K)), data, start=list(log.r=log(0.2)))
      if (model=="fox") res <- nls(S1~exp(log.r)*B1*(1-log(B1)/log.K), data, start=list(log.r=log(0.2)))

      p <- c(exp(coef(res)),exp(log.K))
      names(p) <- c("r","K")
    }
    if (!is.null(r.fix) & !is.null(K.fix)){
      res <- list()

      p <- c(r.fix, B[1])
      names(p) <- c("r","K")
    }
  }

  r <- p[1]
  K <- p[2]

  p.S1 <- predict(res)

  if (model=="schaefer") MSY <- c(r*K/4, K/2, r/2)
  if (model=="fox") MSY <- c(r*K/(log(K)*exp(1)), K/exp(1), r/log(K))
  names(MSY) <- c("MSY","Bmsy","Fmsy")

  Assess <- c(B[n]/MSY[2], (C[n]/B[n])/MSY[3])
  names(Assess) <- c("Bcur/Bmsy","Fcur/Fmsy")

  # SP plot
  std.S <- (S1-mean(S1,na.rm=TRUE))/sd(S1,na.rm=TRUE)
  std.pS <- (p.S1-mean(S1,na.rm=TRUE))/sd(S1,na.rm=TRUE)
  std.MSY <- (MSY[1]-mean(S1,na.rm=TRUE))/sd(S1,na.rm=TRUE)
  std.C <- (C1-mean(S1,na.rm=TRUE))/sd(S1,na.rm=TRUE)

  plot(names(B1), std.S, pch=16, col="blue", xlab="Year", ylab="Standardized Surplus Production", main=main, cex=1.5)
  lines(names(B1), std.pS , col="red", lwd=2)
  abline(h=std.MSY, col="green", lty=2, lwd=2)
  points(names(B1), std.C , pch=17, col="orange", cex=1.5)

  Res <- list(B=B, C=C, S=S1, std.S=std.S, std.pS=std.pS, std.MSY=std.MSY, std.C=std.C, res=res, log.p = coef(res), p = p, vcov = vcov(res), MSY=MSY, Assess=Assess)

  return(Res)
}

SR.est.old <- function(res,model="BH",k=1,p.init=NULL,lower.limit=-25,scale=1,main=NULL,log=FALSE){
  SSB <- colSums(res$ssb,na.rm=TRUE)/scale
  R <- res$naa[1,]/scale

  n <- length(R)

  R1 <- R[(1+k):n]
  SSB1 <- SSB[1:(n-k)]

  if (is.null(p.init)){
    if (model=="BH") {
      Y <- as.numeric(SSB1/R1)
      res0 <- lm(Y ~ SSB1)
      alpha <- 1/res0$coef[1]
      beta <- res0$coef[2]*alpha
    }
    if (model=="RI") {
      Y <- as.numeric(log(R1/SSB1))
      res0 <- lm(Y ~ SSB1)
      alpha <- exp(res0$coef[1])
      beta <- -res0$coef[2]
    }

    p0 <- log(c(max(alpha, 0.00000000001), max(beta, 0.00000000001)))
  }
  else p0 <- p.init

  data <- data.frame(R1=as.numeric(R1),SSB1=as.numeric(SSB1))

  if (isTRUE(log)){
    if (model=="BH") res <- nls(log(R1)~log.a+log(SSB1)-log(1+exp(log.b)*SSB1), data, start=list(log.a=p0[1],log.b=p0[2]), control=list(warnOnly=TRUE), lower=rep(lower.limit,2), algorithm="port")
    if (model=="RI") res <- nls(log(R1)~log.a+log(SSB1)-exp(log.b)*SSB1, data, start=list(log.a=p0[1],log.b=p0[2]), control=list(warnOnly=TRUE), lower=rep(lower.limit,2), algorithm="port")
  }
  else{
    if (model=="BH") res <- nls(R1~exp(log.a)*SSB1/(1+exp(log.b)*SSB1), data, start=list(log.a=p0[1],log.b=p0[2]), control=list(warnOnly=TRUE), lower=rep(lower.limit,2), algorithm="port")
    if (model=="RI") res <- nls(R1~exp(log.a)*SSB1*exp(-exp(log.b)*SSB1), data, start=list(log.a=p0[1],log.b=p0[2]), control=list(warnOnly=TRUE), lower=rep(lower.limit,2), algorithm="port")
  }

  p <- exp(coef(res))
  names(p) <- c("alpha","beta")

  if(is.null(main)) main <- model 
  plot(SSB1,R1,xlab="SSB",ylab="R",xlim=c(0,max(SSB1)*1.05),ylim=c(0,max(R1)*1.05),main=main)
  
  x <- seq(0,max(SSB1),len=100)

  if(model=="BH") pred <- p[1]*x/(1+p[2]*x)
  if(model=="RI") pred <- p[1]*x*exp(-p[2]*x)

  lines(x,pred,col="red",lwd=2)

  Res <- list(SSB=SSB1,R=R1, k=k, p=p)

  return(Res)
}


logit <- function(x) log(x/(1-x))

##

cv.est <- function(res,n=5){

   nr <- ifelse(res$input$use.index=="all", 1:nrow(res$input$dat$index), res$input$use.index)
   nc <- ncol(res$input$dat$index)
   
   obj <- NULL
   
   for (i in 0:(n-1)){
     res.c <- res

     res.c$input$dat$index[,nc-i] <- NA
     res.c$input$plot <- FALSE
#     res.c$input$p.init <- res$term.f

     res1 <- do.call(vpa,res.c$input)

     if (abs(res1$gradient) < 10^(-3)){
       obj <- c(obj,mean(dnorm(log(res$input$dat$index[nr,nc-i]),log(res1$pred[,nc-i]),res1$sigma,log=TRUE),na.rm=TRUE))
     }
   }
   
   return(mean(obj,na.rm=TRUE))
}

retro.est <- function(res,n=5,stat="mean",init.est=FALSE, b.fix=TRUE){
   res.c <- res
   res.c$input$plot <- FALSE
   Res <- list()
   obj.n <- obj.b <- obj.s <- obj.r <- obj.f <- NULL
   
   if (isTRUE(b.fix)){
     res.c$input$b.fix <- res$b
     res.c$input$b.est <- FALSE
   }
   
   if (res$input$last.catch.zero) res.c$input$last.catch.zero <- FALSE
     
   for (i in 1:n){
     nc <- ncol(res.c$input$dat$caa)
     
     res.c$input$dat$caa <- res.c$input$dat$caa[,-nc]
     res.c$input$dat$maa <- res.c$input$dat$maa[,-nc]
     res.c$input$dat$maa2 <- res.c$input$dat$maa2[,-nc]
     res.c$input$dat$waa <- res.c$input$dat$waa[,-nc]
     res.c$input$dat$M <- res.c$input$dat$M[,-nc]
     res.c$input$dat$index <- res.c$input$dat$index[,-nc,drop=FALSE]
     res.c$input$dat$catch.prop <- res.c$input$dat$catch.prop[,-nc]
     
     res.c$input$tf.year <- res.c$input$tf.year-1
     res.c$input$fc.year <- res.c$input$fc.year-1
     
     if (isTRUE(init.est)) res.c$input$p.init <- res.c$term.f
     
     res1 <- do.call(vpa,res.c$input)

     Res[[i]] <- res1
     
     if ((max(abs(res1$gradient)) < 10^(-3) & !isTRUE(res1$input$ADMB)) | (max(abs(res1$gradient)) > 0 & max(abs(res1$gradient)) < 10^(-3) & isTRUE(res1$input$ADMB)) | (is.na(max(abs(res1$gradient))) & res1$input$optimizer=="nlminb")){
        obj.n <- c(obj.n, (sum(res1$naa[,nc-1])-sum(res$naa[,nc-1]))/sum(res$naa[,nc-1]))
        obj.b <- c(obj.b, (sum(res1$baa[,nc-1])-sum(res$baa[,nc-1]))/sum(res$baa[,nc-1]))
        obj.s <- c(obj.s, (sum(res1$ssb[,nc-1])-sum(res$ssb[,nc-1]))/sum(res$ssb[,nc-1]))
        obj.r <- c(obj.r, (res1$naa[1,nc-1]-res$naa[1,nc-1])/res$naa[1,nc-1])
        obj.f <- c(obj.f, (sum(res1$faa[,nc-1])-sum(res$faa[,nc-1]))/sum(res$faa[,nc-1]))
     } else {
       obj.n <- c(obj.n, NA)
       obj.b <- c(obj.b, NA)
       obj.s <- c(obj.s, NA)
       obj.r <- c(obj.r, NA)
       obj.f <- c(obj.f, NA)
     }
   }
   
   mohn <- c(get(stat)(obj.n,na.rm=TRUE),get(stat)(obj.b,na.rm=TRUE),get(stat)(obj.s,na.rm=TRUE),get(stat)(obj.r,na.rm=TRUE),get(stat)(obj.f,na.rm=TRUE))
   
   names(mohn) <- c("N","B","SSB","R","F")
   
   return(list(Res=Res,retro.n=obj.n, retro.b=obj.b, retro.s=obj.s, retro.r=obj.r, retro.f=obj.f, mohn=mohn))
}

#�ŐV�N�̋��l�ʂ�0�̏ꍇ (last.zero.catch=0)�A�ŐV�N��F��0�ƂȂ�A�����ʂ�����ł��Ȃ����߁A����1�N�O�̐���l��Mohn's rho���v�Z���邽�߂̊֐�
retro.est2 <- function(res,n=5,stat="mean",init.est=FALSE, b.fix=TRUE){
   res.c <- res
   res.c$input$plot <- FALSE
   Res <- list()
   obj.n <- obj.b <- obj.s <- obj.r <- obj.f <- NULL
   
   if (isTRUE(b.fix)){
     res.c$input$b.fix <- res$b
     res.c$input$b.est <- FALSE
   }
        
   for (i in 1:n){
     nc <- ncol(res.c$input$dat$caa)
     
     res.c$input$dat$caa <- res.c$input$dat$caa[,-nc]
     res.c$input$dat$maa <- res.c$input$dat$maa[,-nc]
     res.c$input$dat$maa2 <- res.c$input$dat$maa2[,-nc]
     res.c$input$dat$waa <- res.c$input$dat$waa[,-nc]
     res.c$input$dat$M <- res.c$input$dat$M[,-nc]
     res.c$input$dat$index <- res.c$input$dat$index[,-nc,drop=FALSE]
     res.c$input$dat$catch.prop <- res.c$input$dat$catch.prop[,-nc]
     
     res.c$input$tf.year <- res.c$input$tf.year-1
     res.c$input$fc.year <- res.c$input$fc.year-1
     
     if (isTRUE(init.est)) res.c$input$p.init <- res.c$term.f
     
     if (res.c$input$last.catch.zero) {res.c$input$dat$caa[,nc-1] <- 0; Y <- nc-2} else Y <- nc-1
     
     res1 <- do.call(vpa,res.c$input)

     Res[[i]] <- res1

     if ((max(abs(res1$gradient)) < 10^(-3) & !isTRUE(res1$input$ADMB)) | (max(abs(res1$gradient)) > 0 & max(abs(res1$gradient)) < 10^(-3) & isTRUE(res1$input$ADMB)) | (is.na(max(abs(res1$gradient))) & res1$input$optimizer=="nlminb")){
        obj.n <- c(obj.n, (sum(res1$naa[,Y])-sum(res$naa[,Y]))/sum(res$naa[,Y]))
        obj.b <- c(obj.b, (sum(res1$baa[,Y])-sum(res$baa[,Y]))/sum(res$baa[,Y]))
        obj.s <- c(obj.s, (sum(res1$ssb[,Y])-sum(res$ssb[,Y]))/sum(res$ssb[,Y]))
        obj.r <- c(obj.r, (res1$naa[1,Y]-res$naa[1,Y])/res$naa[1,Y])
        obj.f <- c(obj.f, (sum(res1$faa[,Y])-sum(res$faa[,Y]))/sum(res$faa[,Y]))
     } else {
       obj.n <- c(obj.n, NA)
       obj.b <- c(obj.b, NA)
       obj.s <- c(obj.s, NA)
       obj.r <- c(obj.r, NA)
       obj.f <- c(obj.f, NA)
     }
   }
   
   mohn <- c(get(stat)(obj.n,na.rm=TRUE),get(stat)(obj.b,na.rm=TRUE),get(stat)(obj.s,na.rm=TRUE),get(stat)(obj.r,na.rm=TRUE),get(stat)(obj.f,na.rm=TRUE))
   
   names(mohn) <- c("N","B","SSB","R","F")
   
   return(list(Res=Res,retro.n=obj.n, retro.b=obj.b, retro.s=obj.s, retro.r=obj.r, retro.f=obj.f, mohn=mohn))
}

