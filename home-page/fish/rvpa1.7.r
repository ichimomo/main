#------ 2015. 7. 08
#----------------------- RVPA program -----------------------------
# written by Hiroshi Okamura (VPA & reference point) 
#                     and Momoko Ichinokawa (future projection & reference point)
#
# (*) macで読む場合は、
# source("rvpa1.6.r",encoding="shift-jis")
# として、文字コードを指定して読んでください
# (*) vpaを使う場合、初期値の指定によっては収束しない場合があります。p.initの値をいろいろ変えて試して下さい
#
#
#
# 変更履歴
# rvpa0.9 - 2013.7.3. 将来予測関数；将来予測のdeterminisitc runでのrpsの参照範囲と、
#   stochastic runでのrpsの参照範囲が異なる場合のオプション(sample.yearで指定)を追加
#   リサンプリングされるRPS = RPS[sample.yearの範囲]/mean(RPS[sample.yearの範囲])*median(RPS[rps.yearの範囲])
#   (bias.adjustedしていない場合は関係ない。sample.yearを設定しない場合、自動的にsample.year=rps.yearとなる)
# rvpa0.92 - vpaのsel.update部分の修正とprofile likelihood信頼区間関数の修正・追加
# rvpa0.95 - 管理基準値計算のところに、nlmでの計算回数の上限を設定する引数を追加（デフォルトでは１０００）
# rvpa0.96 - likelihood profile用に、CPUEごとに目的関数の値を出力するオプションを追加。その他
# rvpa1.0 - パッケージとして公開したバージョン
# rvpa1.3 - 重み付け計算を追加
# rvpa1.6 -warningsメッセージを追加．vpaでindexの一部だけ利用するオプション(use.index)を追加．MSY.EST2関数を追加

##

data.handler <- function(
  caa,
  waa,
  maa,
  index=NULL,
  M = 0.4
)
{
  years <- as.numeric(sapply(strsplit(names(caa[1,]),"X"), function(x) x[2]))

  if (is.null(dim(waa)) | dim(waa)[2]==1) waa <- as.data.frame(matrix(unlist(waa), nrow=nrow(caa), ncol=ncol(caa)))
  if (is.null(dim(maa)) | dim(maa)[2]==1) maa <- as.data.frame(matrix(unlist(maa), nrow=nrow(caa), ncol=ncol(caa)))

  colnames(caa) <- colnames(waa) <- colnames(maa) <- years

  if (!is.null(index)) colnames(index) <- years

  if (is.null(dim(M))) M <- as.data.frame(matrix(M, nrow=nrow(caa), ncol=ncol(caa)))

  colnames(M) <- years
  rownames(M) <- rownames(caa)

  res <- list(caa=caa, maa=maa, waa=waa, index=index, M=M)

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
 if (abund=="SSBm"){
   ssb <- naa*waa*maa*exp(-dat$M/2-af*faa/2)
   res <- colSums(ssb,na.rm=TRUE)
 }
 if (abund=="F") res <- colMeans(faa[min.age:max.age,], na.rm=TRUE)

 if (link=="log") res <- log(res, base=base)

  return(invisible(res))
}

#

tmpfunc2 <- function(x=1,y=2,z=3){
  argname <- ls()  # 関数が呼び出されたばかりのときのls()は引数のみが入っている
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
    obj <- c(obj, index.w[i]*(-as.numeric(na.omit(dnorm(log(as.numeric(index[i,avail])),q[i]+b[i]*log(as.numeric(Abund[i,avail])),sigma[i],log=TRUE)))))
  }
  
  convergence <- ifelse(delta <= max.dd, 1, 0) 
  
  return(list(q=q, b=b, sigma=sigma, obj=sum(obj), convergence=convergence))
}

#

# vpa 
#

vpa <- function(
  dat,  # data for vpa
  sel.f = NULL,  # 最終年の選択率
  tf.year = 2008:2010, # terminal Fをどの年の平均にするか
  rec.new = NULL, # 翌年の加入を外から与える
  rec=NULL, # rec.yearの加入
  rec.year=2010,  # 加入を代入する際の年
  rps.year = 2001:2010, # 翌年のRPSをどの範囲の平均にするか
  fc.year = 2009:2011, # Fcurrentでどの範囲を参照するか
  last.year = NULL,   # vpaを計算する最終年を指定（retrospective analysis）
  last.catch.zero = FALSE,   # TRUEなら強制的に最終年の漁獲量を0にする
  faa0 = NULL,  # sel.update=TRUEのとき，初期値となるfaa
  naa0 = NULL,    # sel.update=TRUEのとき，初期値となるnaa
  f.new = NULL,
  Pope = TRUE,  # Popeの近似式を使うかどうか
  tune = FALSE,  # tuningをするかどうか
  abund = "B",   # tuningの際，何の指標に対応するか
  min.age = 0,  # tuning指標の年齢参照範囲の下限
  max.age = 0,  # tuning指標の年齢参照範囲の上限
  link = "id",  # tuningのlink関数
  base = NA,  # link関数が"log"のとき，底を何にするか
  af = NA,  # 資源量指数が年の中央のとき，af=0なら漁期前，af=1なら漁期後となる
  index.w = NULL,  # tuning indexの重み
  use.index = "all",
  scale = 1000,  # 重量のscaling
  hessian = TRUE,
  alpha = 1,  # 最高齢と最高齢-1のFの比 F_a = alpha*F_{a-1}
  maxit = 5,  # 石岡・岸田/平松の方法の最大繰り返し数
  d = 0.0001,  # 石岡・岸田/平松の方法の収束判定基準
  min.caa = 0.001,   # caaに0があるとき，0をmin.caaで置き換える
  plot = FALSE,   # tuningに使った資源量指数に対するフィットのプロット
  plot.year = 2005:2012,   # 上のプロットの参照年
  term.F = "max",   # terminal Fの何を推定するか: "max" or "all"
  plus.group = TRUE,  
  stat.tf = "mean",  # 最終年のFを推定する統計量（年齢別に与えること可）
  add.p.est = NULL,  # 追加で最高齢以外のfaaを推定する際．年齢を指定する．
  add.p.ini = NULL,
  sel.update=FALSE,  # チューニングVPAにおいて，選択率を更新しながら推定
  sel.def = "max",  #  sel.update=TRUEで選択率を更新していく際に，選択率をどのように計算するか．最大値を1とするか，平均値を1にするか...
  max.dd = 0.000001,  # sel.updateの際の収束判定基準
  intercept = FALSE,   # 資源量指数の予測値に切片を使う
  intercept.p.init = NULL,   # 切片も入れたときのパラメータの初期値
  ti.scale = NULL,   # 資源量の係数と切片のscaling
  tf.mat = NULL,   # terminal Fの平均をとる年の設定．0-1行列．
  eq.tf.mean = FALSE, # terminal Fの平均値を過去のFの平均値と等しくする
  no.est = FALSE,   # パラメータ推定しない．
  est.method = "ls",  # 推定方法 （ls = 最小二乗法，ml = 最尤法）
  b.est = FALSE,  #  bを推定するかどうか
  est.constraint = FALSE,   # 制約付き推定をするかどうか
  q.const = 1:length(abund),   # qパラメータの制約（0は推定しないで1にfix）
  b.const = 1:length(abund),   # bパラメータの制約（0は推定しないで1にfix）
  sigma.const = 1:length(abund),   # sigmaパラメータの制約（0は推定しないで1にfix）
  max.iter = 100,    # q,b,sigma計算の際の最大繰り返し数
  p.init = 0.2   # 推定パラメータの初期値
)
{

  # inputデータをリスト化

  argname <- ls()  # 関数が呼び出されたばかりのときのls()は引数のみが入っている
  arglist <- lapply(argname,function(xx) eval(parse(text=xx)))
  names(arglist) <- argname
  
  # data handling

  caa <- dat$caa    # catch-at-age
  waa <- dat$waa    # weight-at-age
  maa <- dat$maa    # maturity-at-age
  index <- dat$index   # abundance indices
  M <- dat$M    # natural mortality-at-age

  years <- dimnames(caa)[[2]]  # 年
  ages <- dimnames(caa)[[1]]  # 年齢

  if (use.index[1]!="all") {
    index <- index[use.index,]
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

  # 最終年 last.yearに値が入っている場合は，それ以降のデータを削除する（retrospective analysis）
  if (!is.null(last.year)) {
    caa <- caa[,years <= last.year]
    waa <- waa[,years <= last.year]
    maa <- maa[,years <= last.year]
    M <- M[,years <= last.year]
    if(!is.null(index)) index <- index[,years <= last.year]
    years <- dimnames(caa)[[2]]
    dat <- list(caa=caa, waa=waa, maa=maa, M=M, index=index)
  }

  na <- apply(caa, 2, max.age.func)  # 年ごとの最大年齢（年によって最大年齢が違う場合に対応するため）
  ny <- ncol(caa)  # 年の数
  
  if (isTRUE(last.catch.zero)) {caa[,ny] <- 0; ny <- ny - 1; n.add <- 1; saa.new <- NULL} else n.add <- 0  # 最終年の漁獲量を0とし，年を1個減らす

  if (term.F=="max") {  # 最高齢のFだけを推定する場合
    p.init <- ifelse(is.na(p.init[1]), M[na[ny],ny], p.init[1])   # 初期値がNAなら，最終年最高齢の自然死亡係数を初期値とする
    if (!is.null(add.p.est) & is.null(add.p.ini)) {add.p.est <- add.p.est + 1; p.init <- rep(p.init, length(add.p.est)+1)}  # add.p.estに数字があれば，その年齢を追加のパラメータとして推定する
    if (!is.null(add.p.est) & !is.null(add.p.ini)) {add.p.est <- add.p.est + 1; p.init <- c(add.p.ini,p.init)}  # add.p.estに数字があれば，その年齢を追加のパラメータとして推定する
  }
  if (term.F=="all"){  # 最終年のすべての年齢のFを推定
    if(length(p.init)==0) p.init <- rep(M[na[ny],ny], na[ny]-1)  # 初期値がNAなら，最終年最高齢の自然死亡係数を0~na-1の初期値とする
    if(length(p.init) < na[ny]-1) p.init <- rep(p.init[1], na[ny]-1)  # 初期値の成分数が年齢数-1より小さい場合は，初期値の最初の値を要素に持つ年齢数-1の大きさのベクトルを初期値とする
    if(length(p.init) >= na[ny]-1) p.init <- p.init[1:na[ny]-1]  # 初期値の成分数が年齢数-1以上であれば，年齢数以上の値は使用しない
  }

  if (length(stat.tf)==1) stat.tf <- rep(stat.tf, na[ny]-1)  # stat.tfが1個だけ指定されているときは，全年齢その統計量を使う

  # tuningの際のパラメータが1個だけ指定されている場合は，nindexの数だけ増やす
  if (isTRUE(tune)){
    nindex <- nrow(index)

    if (nindex > length(abund) & length(abund)==1) abund <- rep(abund, nindex)
    if (nindex > length(min.age) & length(min.age)==1) min.age <- rep(min.age, nindex)
    if (nindex > length(max.age) & length(max.age)==1) max.age <- rep(max.age, nindex)
    if (nindex > length(link) & length(link)==1) link <- rep(link, nindex)
    if (nindex > length(base) & length(base)==1) base <- rep(base, nindex)

    if (is.null(index.w)) index.w <- rep(1, nindex)
    if (!is.null(af)) if(nindex > length(af) & length(af)==1) af <- rep(af, nindex)

    q <- rep(NA, nindex)
  }

  if (isTRUE(intercept)) p.init <- exp(c(log(p.init), intercept.p.init))   # 資源量指数の予測値の線形関数が切片を持つ場合，パラメータを追加

  # selectivityを更新する場合にfaa0，naa0が与えられていれば，それを使う
   if (!isTRUE(sel.update)) faa <- naa <- matrix(NA, nrow=max(na), ncol=ny+n.add, dimnames=list(ages, years))
   else {
     if(is.null(faa0) | is.null(naa0)) faa <- naa <- matrix(1, nrow=max(na), ncol=ny+n.add, dimnames=list(ages, years))
     else {faa <- as.matrix(faa0); naa <- as.matrix(naa0)}
   }

  # warnings
  
  if (!tune & sel.update) print("sel.update = TRUE but tune=FALSE. So, the results are unreliable.")
  if (tune & is.null(sel.f) & (!sel.update & term.F=="max")) print("sel.f=NULL although tune=TRUE & sel.update=FALSE & term.F=max. The results are unreliable.")
  if (tune & (length(abund)!=nrow(index))) print("Check!: The number of abundance definition is different from the number of indices.")
  
# core function for optimization

  p.est <- function(log.p, out=FALSE){
 
    if (!isTRUE(intercept)) p <- exp(log.p)   # intercept==FALSEなら，パラメータはexp(log.p)
    else {  # intercept==TRUEなら，直線回帰に関するパラメータを追加
      p <- exp(log.p[1])
      Q1 <- log.p[2:(length(intercept.p.init)/2+1)]
      D1 <- log.p[(length(intercept.p.init)/2+2):(length(intercept.p.init)+1)]
    }

    # sel.f==NULLで，パラメータpが1個なら，最終年最高齢のfaaとnaaを推定
    if (is.null(sel.f) & length(p) == 1){
      faa[na[ny], ny] <- p
      if (isTRUE(Pope)) naa[na[ny], ny] <- caa[na[ny], ny]*exp(M[na[ny], ny]/2)/(1-exp(-faa[na[ny], ny]))
      else  naa[na[ny], ny] <- caa[na[ny], ny]/(1-exp(-faa[na[ny], ny]-M[na[ny], ny]))*(faa[na[ny],ny]+M[na[ny],ny])/faa[na[ny],ny]
    }

    # sel.f!=NULLで，パラメータが年齢-1より少ない場合，sel.fを使って，最終年/全年齢のfaaとnaaを計算
    if (!is.null(sel.f) & length(p) < na[ny]-1){
      if(length(p)==1) faa[, ny] <- sel.f*p
      if(length(p) > 1) {   # パラメータ数が1より大きい場合，add.p.estの分，推定パラメータ数を増やす
      faa[,ny] <- sel.f*p[length(p)]
        for (i in 1:(length(p)-1)){
          faa[add.p.est[i],ny] <- p[i]*p[length(p)]
        }
      }
      if (isTRUE(Pope)) naa[, ny] <- vpa.core.Pope(caa,faa,M,ny)
      else  naa[, ny] <- vpa.core(caa,faa,M,ny)
    }

   # パラメータが年齢-1であれば，それらをパラメータとして最終年/全年齢のfaaとnaaを計算
   if (length(p) == na[ny]-1){
     faa[1:(na[ny]-1), ny] <- p
     faa[na[ny], ny] <- alpha*p[na[ny]-1]
     if (isTRUE(Pope)) naa[, ny] <- vpa.core.Pope(caa,faa,M,ny)
     else naa[, ny] <- vpa.core(caa,faa,M,ny)
   }

   # selctivityを更新しながら推定する場合
   if (isTRUE(sel.update)){
     dd <- 1
     while(dd > max.dd){
      saa <- sel.func(faa, def=sel.def)   # sel.defに従って選択率を計算
      for (i in (na[ny]-1):1){
        saa[i, ny] <- get(stat.tf[i])(saa[i, years %in% tf.year]) 
      }
 
      saa[na[ny], ny] <- get(stat.tf[na[ny]-1])(saa[na[ny], years %in% tf.year])
      faa[1:na[ny], ny] <- p*sel.func(saa, def=sel.def)[1:na[ny],ny]

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
     faa1[1:na[ny], ny] <- p*sel.func(saa1, def=sel.def)[1:na[ny],ny]
    
     dd <- max(sqrt((saa1[,ny] - saa[,ny])^2))

     faa <- faa1
   }
   saa <- sel.func(faa, def=sel.def)
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
        ssb <- baa*maa
    
        if (is.null(rec.new)) {
          new.naa[1] <- median((naa[1,]/colSums(ssb))[years %in% rps.year])*sum(ssb[,ny+n.add],na.rm=TRUE)
        }
        else new.naa[1] <- rec.new

        naa[1,ny+n.add] <- new.naa[1]
        baa[1,ny+n.add] <- naa[1,ny+n.add]*waa[1,ny+n.add]
        ssb[1,ny+n.add] <- baa[1,ny+n.add]*maa[1,ny+n.add]

        if (!is.null(f.new) & !is.null(saa.new)) faa[,ny+n.add] <- f.new*saa.new else faa[,ny+n.add] <- 0
        caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]))*exp(-M[,ny+n.add]/2)
      }

  # tuning
  
    obj <- NULL

   if (tune){
     if (est.constraint){
   
       Abund <- NULL

       for (i in 1:nindex){
         abundance <- abund.extractor(abund=abund[i], naa, faa, dat, min.age=min.age[i], max.age=max.age[i], link=link[i], base=base[i], af=af[i], scale=scale)
         Abund <- rbind(Abund, abundance)
       }
   
       est.qbs <- qbs.f(q.const, b.const, sigma.const, index, Abund, nindex, index.w, max.dd, max.iter)
             
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
         abundance <- abund.extractor(abund=abund[i], naa, faa, dat, min.age=min.age[i], max.age=max.age[i], link=link[i], base=base[i], af=af[i], scale=scale)
         Abund <- rbind(Abund, abundance)
        
       if (est.method=="ls"){
          if (!isTRUE(intercept)){
            avail <- which(!is.na(as.numeric(index[i,])))
            if (b.est) b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail])))
            else b[i] <- 1
            q[i] <- exp(mean(log(as.numeric(index[i,avail]))-b[i]*log(as.numeric(abundance[avail]))))
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
            if (b.est) b[i] <- cov(log(as.numeric(index[i,avail])),log(as.numeric(abundance[avail])))/var(log(as.numeric(abundance[avail])))
            else b[i] <- 1
            q[i] <- exp(mean(log(as.numeric(index[i,avail]))-b[i]*log(as.numeric(abundance[avail]))))
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
          ssb <- baa*maa

          if (is.null(rec.new)) {
            new.naa[1] <- median((naa[1,]/colSums(ssb))[years %in% rps.year])*sum(ssb[,ny+n.add],na.rm=TRUE)
          }
          else new.naa[1] <- rec.new

          naa[1,ny+n.add] <- new.naa[1]
          baa[1,ny+n.add] <- naa[1,ny+n.add]*waa[1,ny+n.add]
          ssb[1,ny+n.add] <- baa[1,ny+n.add]*maa[1,ny+n.add]
    
          if (!is.null(f.new) & !is.null(saa.new)) faa[,ny+n.add] <- f.new*saa.new else faa[,ny+n.add] <- 0
          caa[,ny+n.add] <- naa[,ny+n.add]*(1-exp(-faa[,ny+n.add]))*exp(-M[,ny+n.add]/2)
        } 
        else {
          baa <- naa*waa
          ssb <- baa*maa
        }

        if (isTRUE(intercept)) q <- cbind(Q1,D1)      
        
        obj <- list(minimum=obj, minimum.c=obj0, caa=caa, naa=naa, faa=faa, baa=baa, ssb=ssb)
        if (isTRUE(eq.tf.mean)) obj$p <- max(faa[,ny],na.rm=TRUE)

        if (isTRUE(tune)) {
          if (est.method=="ls"){
            Nindex <- sum(!is.na(index))
            Sigma2 <- obj$minimum/Nindex
            neg.logLik <- Nindex/2*log(2*pi*Sigma2)+Nindex/2
            obj$q <- q
            obj$b <- b
            obj$sigma <- sqrt(Sigma2)
            obj$convergence <- convergence  
            obj$Abund <- Abund
            obj$logLik <- -neg.logLik
          }
          if (est.method=="ml"|est.constraint){
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
    
    return(obj)   # 目的関数を返す
  }

  # execution of optimization

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
    summary.p.est <- nlm(p.est, log(p.init), hessian=hessian)
    log.p.hat <- summary.p.est$estimate
  }

  gradient <- summary.p.est$gradient
  code <- summary.p.est$code

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
    if (!isTRUE(intercept))  pred.index <- q*out$Abund
    else pred.index <-  q[,1]*out$Abund*ti.scale[1]+q[,2]*ti.scale[2]
  }
 else logLik <- sigma <- q <- b <- convergence <- pred.index <- NULL

  Ft <- mean(faa[,ny],na.rm=TRUE)
  Fc.at.age <- apply(faa[,years %in% fc.year,drop=FALSE],1,mean)  # drop=FALSEで，行列のベクトル化を防ぐ
  Fc.mean <- mean(Fc.at.age,na.rm=TRUE)
  Fc.max <- max(Fc.at.age,na.rm=TRUE)

  res <- list(input=arglist, term.f=term.f, np=np, minimum=out$minimum, minimum.c=out$minimum.c, logLik=logLik, gradient=gradient, code=code, q=q, b=b, sigma=sigma, convergence=convergence, hessian=hessian, Ft=Ft, Fc.at.age=Fc.at.age, Fc.mean=Fc.mean, Fc.max=Fc.max, last.year=last.year, Pope=Pope, pred.index=pred.index, naa=naa, faa=faa, baa=baa, ssb=ssb, saa=saa)

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
     if (method=="dist"){   # 市野川変更
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
#    out <- p0 # 市野川変更
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
                      Fc.at.age=res1$Fc.at.age) # 2013.8.20追記(市野川)
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

#---------------- rfuture & Fref
# 2012. 7. 28.; calc.rel.abund導入。SPR、YPRを計算する関数。
# 2012. 8. 1; 加入の関数を大幅改善
# 0.3f; Frec1, Frec2, Frecの計算オプションをfuture.vpaに追加
# 0.4: スケトウ対応
# 0.5: 2013.1.18 全種対応のためのバージョン
# 0.5c: 対馬のサバに対応するため、将来予測の部分にtimestepを導入
#       資源量の単位はトン、尾数の単位は百万尾
# 2014.7.4; 黒田さん指摘により，Popeの引数を追加

#---------------- 管理基準値計算のための関数 ------------------------
# ref.F
ref.F <- function(
  res, # VPAの結果のオブジェクト
  sel=NULL, # 仮定する選択率．NULLの場合，res$Fc.at.ageが使われる
  waa=NULL, # 仮定する生物パラメータ．直接の値を入れるか，年を指定するやり方のどちらでも動く。直接指定するほうが優先。
  maa=NULL,
  M=NULL,
  M.year=NULL, 
  waa.year=NULL, # 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
  maa.year=NULL,
  rps.year = NULL, # Fmedの計算に使うRPSの年の範囲．NULLの場合，全範囲が用いられる
  max.age = Inf, # SPR計算を打ち切りにする年齢（この年齢より上は，存在しないとみなす）
  d = 0.001,
  Fspr.init = 0.5, # F%SPRの初期値
  Fmax.init = 1.5, # Fmaxの初期値
  F0.1.init = 0.7, # F0.1の初期値
  pSPR = seq(10,90,by=10), # F%SPRを計算するときの％SPR
  iterlim=1000,
  plot=TRUE,
  Pope=FALSE, # 2014.7.4追加
  F.range = seq(from=0,to=2,length=101)  # YPR, SPR曲線を書くときのFの範囲
){

  argname <- ls()
  arglist <- lapply(argname,function(x) eval(parse(text=x)))
  names(arglist) <- argname
  
#  caa <- res$input$dat$caa
  naa <- res$naa
  ssb <- res$ssb
  ny <- ncol(naa)
  years <- dimnames(naa)[[2]]
  ages <- dimnames(naa)[[1]]

  
  if(is.null(sel)){
    Fc.at.age <- res$Fc.at.age
    sel <- Fc.at.age/max(Fc.at.age,na.rm=TRUE)
  }
  else{
    Fc.at.age <- sel
  }
  sel <- sel/max(sel,na.rm=T)
  
  
  na <- sum(!is.na(sel))

  if(is.null(waa.year)) waa.year <- rev(years)[1]
  if(is.null(maa.year)) maa.year <- rev(years)[1]
  if(is.null(M.year)) M.year <- rev(years)[1]
  if(is.null(rps.year)) rps.year <- as.numeric(colnames(res$naa))
  
  if(is.null(waa))  waa <- apply(as.matrix(as.data.frame(res$input$dat$waa)[as.character(waa.year)]),1,mean)
  if(is.null(M))  M <- apply(as.matrix(as.data.frame(res$input$dat$M)[as.character(M.year)]),1,mean)
  if(is.null(maa))  maa <- apply(as.matrix(as.data.frame(res$input$dat$maa)[as.character(maa.year)]),1,mean)

  

  min.age <- min(as.numeric(rownames(res$naa)))
  if(min.age==0) slide.tmp <- TRUE else slide.tmp <- -1:-min.age
  rps.data <- data.frame(year=as.numeric(names(colSums(ssb,na.rm=T))),
                         ssb=as.numeric(colSums(ssb,na.rm=T)),
                         recruit=as.numeric(c(naa[1,slide.tmp],rep(NA,min.age))))
  rps.data$rps <- rps <- rps.data$recruit/rps.data$ssb
#  rps <- as.numeric(naa[1,]/colSums(ssb,na.rm=TRUE))

#  if (is.null(rps.year)) rps.year <- years

  tmp <- rps.data$year %in% rps.year
  rps.q <- quantile(rps[tmp], na.rm=TRUE, probs=c(0.1,0.5,0.9))
  rps.q <- c(rps.q,mean(rps[tmp], na.rm=TRUE))  
  names(rps.q)[4] <- "mean"
  spr.q <- 1/rps.q

#  browser()
  # F.spr

  spr.f.est <- function(log.p, out=FALSE, sub="med", spr0=NULL){
    Fr <- exp(log.p)

    tmp <- calc.rel.abund(sel,Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)
    rel.abund <- tmp$rel.abund
    spr <- sum(tmp$spr)
#    rel.abund <- rep(NA, na)
#    rel.abund[1] <- 1
#    for (i in 2:(na-1)) {
#      rel.abund[i] <- rel.abund[i-1]*exp(-M[i-1]-sel[i-1]*Fr)
#    }
#    rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-(max.age-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))
 
#    spr <- sum(rel.abund*waa*maa)

    if (isTRUE(out)) obj <- spr
    else{
#      browser()
     if(sub=="mean") obj <- (spr-spr.q[4])^2       
     if(sub=="low") obj <- (spr-spr.q[3])^2 
     if(sub=="med") obj <- (spr-spr.q[2])^2
     if(sub=="high") obj <- (spr-spr.q[1])^2
     if(is.numeric(sub)) obj <- (spr/spr0-sub/100)^2

    }

    return(obj)
  }

  spr0 <- spr.f.est(-Inf, out=TRUE)

  Fmed.res <- nlm(spr.f.est, Fspr.init, out=FALSE, sub="med", iterlim = iterlim)
  Fmean.res <- nlm(spr.f.est, Fspr.init, out=FALSE, sub="mean", iterlim = iterlim)
#  browser()
  Flow.res <- nlm(spr.f.est, Fspr.init, out=FALSE, sub="low", iterlim = iterlim)
  Fhigh.res <- nlm(spr.f.est, Fspr.init, out=FALSE, sub="high", iterlim = iterlim)

  Fmean <- exp(Fmean.res$estimate)  
  Fmed <- exp(Fmed.res$estimate)
  Flow <- exp(Flow.res$estimate)
  Fhigh <- exp(Fhigh.res$estimate)

  if (!is.null(pSPR)){
    FpSPR <- NULL

    for (i in pSPR){
      FpSPR.res <- nlm(spr.f.est, Fspr.init, out=FALSE, sub=i, spr0=spr0, iterlim=iterlim)
#      print(FpSPR.res)
#       cat("i", FpSPR.res$code," ")
      FpSPR <- c(FpSPR, exp(FpSPR.res$estimate))
    }
    names(FpSPR) <- paste(pSPR,"%SPR",sep="")
  }

  # Fmax

  ypr.f.est <- function(log.p, out=FALSE){
    Fr <- exp(log.p)
  
#    rel.abund <- rep(NA, na)
#    rel.abund[1] <- 1
#    for (i in 2:(na-1)) {
#      rel.abund[i] <- rel.abund[i-1]*exp(-M[i-1]-sel[i-1]*Fr)
#    }
#    rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-(max.age-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))
#    ypr <- sum(rel.abund*waa*(1-exp(-sel*Fr))*exp(-M/2))
    tmp <- calc.rel.abund(sel,Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)
    rel.abund <- tmp$rel.abund
    ypr <- sum(tmp$ypr)    

    if (isTRUE(out)) obj <- ypr else obj <- -ypr

   return(obj)
  }

  Fmax.res <- nlm(ypr.f.est, log(Fmax.init), out=FALSE)

  Fmax <- exp(Fmax.res$estimate)

  # F0.1

  Fp <- function(log.p, out=FALSE){
    Fr <- exp(log.p)

    tmp <- calc.rel.abund(sel,Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)
    rel.abund <- tmp$rel.abund
    ypr <- sum(tmp$ypr)
    
#    rel.abund <- rep(NA, na)
#    rel.abund[1] <- 1
#    for (i in 2:(na-1)) {
#      rel.abund[i] <- rel.abund[i-1]*exp(-M[i-1]-sel[i-1]*Fr)
#    }
#   rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-(max.age-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))
#   ypr <- sum(rel.abund*waa*(1-exp(-sel*Fr))*exp(-M/2))

   if (isTRUE(out)) obj <- ypr else obj <- -ypr

   return(obj)
  }

  F0.1.est <- function(log.p){
    p <- exp(log.p)
    ref.trend <- (Fp(log(d))-Fp(log(0)))/d
    trend <- (Fp(log(p+d)) - Fp(log(p)))/d

    obj <- (ref.trend/10 - trend)^2

    obj
  }

  F0.1.res <- nlm(F0.1.est,log(F0.1.init))
 
  F0.1 <- exp(F0.1.res$estimate)

  # Fcurrent
  Fcurrent <- c(max(Fc.at.age,na.rm=T), mean(Fc.at.age,na.rm=T))
  
  # output
  f.mean <- function(x) mean(x*sel, na.rm=T)

  Fmean <- c(Fmean, f.mean(Fmean))  
  Fmed <- c(Fmed, f.mean(Fmed))
  Flow <- c(Flow, f.mean(Flow))
  Fhigh <- c(Fhigh, f.mean(Fhigh))
  Fmax <- c(Fmax, f.mean(Fmax))
  F0.1 <- c(F0.1, f.mean(F0.1))

  names(Fcurrent) <- names(Fmed) <- names(Fmean) <- names(Flow) <- names(Fhigh) <- names(Fmax) <- names(F0.1) <- c("max","mean")

  Res <- list(sel=sel, max.age=max.age, rps.q=rps.q, spr.q=spr.q, Fcurrent=Fcurrent, Fmed=Fmed, Flow=Flow, Fhigh=Fhigh, Fmax=Fmax, F0.1=F0.1, Fmean=Fmean,rps.data=rps.data)
  
  if (!is.null(pSPR)){
    FpSPR <- rbind(FpSPR, sapply(FpSPR, f.mean))
    rownames(FpSPR) <- c("max","mean")
    Res$FpSPR <- FpSPR
  }

  #-----  YPR & SPR figure -----

  spr0 <- sum(calc.rel.abund(sel,0,na,M,waa,maa,max.age=max.age,Pope=Pope)$spr)  
  tmp <- lapply(F.range, function(x) calc.rel.abund(sel,x,na,M,waa,maa,max.age=max.age,Pope=Pope))
  ypr <- sapply(tmp,function(x) sum(x$ypr))
  spr <- sapply(tmp,function(x) sum(x$spr))/spr0*100

  if (isTRUE(plot)){
  plot(F.range,spr,xlab="F at selectivity=1",ylab="%SPR",type="l",ylim=c(0,max(spr)))
  par(new=T)
  plot(F.range,ypr,axes=F,xlab="",ylab="",lty=2,type="l",ylim=c(0,max(ypr)))
  axis(side=4)
  mtext("YPR",side=4,line=2)
  abline(v=xx <- c(Res$Fmax[1],Res$Fcurrent[1],Res$F0.1[1],Res$Fmed[1]))
  text(xx,rep(max(ypr)*c(0.4,0.3,0.2,0.1),length(xx)),c("Fmax","Fcur","F0.1","Fmed"))
  }

  ypr.spr <- data.frame(F.range=F.range,ypr=ypr,spr=spr)
  Res$ypr.spr  <- ypr.spr
  Res$waa <- waa
  Res$maa <- maa
  #------------------------------

  Res$summary <- as.data.frame(Res[substr(names(Res),1,1)=="F"])
  Res$summary <- rbind(Res$summary,Res$summary[1,]/Res$summary[1,1])
  dimnames(Res$summary)[[1]][3] <- "Fref/Fcur"
  Res$arglist <- arglist
  Res$spr0 <- spr0
  class(Res) <- "ref"
#  print(Res)
  return(Res)
}

#print.ref <- function(x){
#  x$summary
#}


#print.ref <- function(x){
#  x$summary
#}


#----------------------- 将来予測関数 ----------------------------
# multiのオプションは管理後のFのmultiplier（管理前後でselectivityが同じ）

future.vpa <- function(res0,
                       currentF=NULL, # 管理前のF
                       multi=1, # 管理後（ABC.yearから）のF (current F x multi)
                       nyear=10,Pope=res0$input$Pope,
                       
                       multi.year=1,#ある特定の年だけFを変えたい場合。デフォルトは1。変える場合は、指定した年またはタイムステップの要素数のベクトルで指定。
                       # 年数の指定
                       start.year=NULL, # 将来予測の開始年，NULLの場合はVPA計算の最終年の次の年
                       ABC.year=NULL, # ABC yearを計算する年。NULLの場合はVPA計算の最終年の次の次の年
                       waa.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                                      # NULLの場合，VPAの最終年のパラメータを持ってくる
                       maa.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                       M.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                       
                       plus.group=res0$input$plus.group,
                       N=1000,# 確率的なシミュレーションをする場合の繰り返し回数。
                              # N+1の結果が返され、1列目に決定論的な結果が                       
                              # 0を与えると決定論的な結果のみを出力
                       silent=FALSE, is.plot=TRUE, # 計算条件を出力、プロットするか
                       
                       pre.catch=NULL, #list(year=2012,wcatch=13000), 漁獲重量をgivenで与える場合                       
                       #-------- 加入に関する設定 -----------------
                       rec.new=NULL, # 指定した年の加入量
                                     # 年を指定しないで与える場合は、自動的にスタート年の加入になる。
                                     # list(year=, rec=)で与える場合は、対応する年の加入を置き換える。
                       #--- 加入関数
                       recfunc=RPS.simple.rec, # 太平洋マサバ、ゴマサバ以外はRPS.simple.recを使う
                       rec.arg=list(upper.ssb=Inf,upper.recruit=Inf), # 加入の各種設定
                       #--- Frecオプション；Frec計算のための設定リストを与えると、指定された設定でのFrecに対応するFで将来予測を行う
                       Frec=NULL, # list(stochastic=TRUE, # TRUEの場合、stochastic simulationで50%の確率でBlimitを越す(PMS, TMI)
                                                          # FALSEの場合、RPS固定のprojectionがBilmitと一致する(NSK)
                                 #      future.year=2018, # 何年の資源量を見るか？
                                 #      Blimit=450*1000,  # Blimit (xトン)
                            #      seed=100) # 乱数のシード
                       # 対馬サバに対応するオプション。ts=2のとき、1年を2つの季節に分けて将来予測する
                       ts=1, # 時間ステップ。1年間の季節の数。普通は１。対馬サバの場合2。ts=1の場合、以下の引数はすべて無視される。
                #---- 以下、ts>2のときに必要な引数 -----
                       waa=NULL,maa=NULL,M=NULL, # 季節毎の生物パラメータ
                       rec.season=1, # 加入がおこる季節
                       waa.multi="opt", # waa.optyearに対応する年について、暦年漁獲量と一致するようにwaaを最適化するか？ "opt"の場合、内部で最適化。waa.optyearの長さ分のベクトルを与えて指定することもできる
                       waa.optyear=2011:2013, # waa.optyearをするときに、置き換えるwaaの年
                       replace.rec.year=2012, # 加入量を暦年の将来予測での加入量に置き換えるか？
                       partial.F=NULL         # 季節毎のpartial F
                       ){
   
  argname <- ls()
  arglist <- lapply(argname,function(x) eval(parse(text=x)))
  names(arglist) <- argname

  if(is.null(res0$input$unit.waa)) res0$input$unit.waa <- 1
  if(is.null(res0$input$unit.caa)) res0$input$unit.caa <- 1
  if(is.null(res0$input$unit.biom)) res0$input$unit.biom <- 1  
    
  if(ts>1 && is.null(partial.F)){
    stop("When ts>1, partial.F should be given")
  }
  #--------------------------------------------------
  N <- N + 1
  years <- as.numeric(dimnames(res0$naa)[[2]])
    
  #------------- set default options
  if(is.null(currentF)) currentF <- res0$Fc.at.age
  if(is.null(waa.year)) waa.year <- rev(years)[1]
  if(is.null(maa.year)) maa.year <- rev(years)[1]
  if(is.null(M.year)) M.year <- rev(years)[1]
  if(is.null(start.year)) start.year <- rev(years)[1]+1
  if(is.null(ABC.year)) ABC.year <- rev(years)[1]+1
  arglist$ABC.year <- ABC.year
  #-------------
  
#  fyears <- start.year:(start.year+nyear-1)
  fyears <- seq(from=start.year,to=start.year+nyear-1,by=1/ts)
  fyear.year <- floor(fyears)
  fyear.season <- #fyears-fyear.year
                  rep(1:ts,nyear)
  fyear.season <- fyear.season[1:length(fyears)]
  ntime <- length(fyears)
#  if(is.null(multi.year)) multi.year <- rep(1,nyear)*ts  
  ages <- as.numeric(dimnames(res0$naa)[[1]])
#  nage <- length(ages)
  min.age <- min(as.numeric(ages))
  if(ts>1){
    ages <- seq(from=min(ages),to=max(ages)+1/ts,by=1/ts)
    nage <- length(ages) # naaにNAが入っていて、かつ、半年毎の将来予測をする場合対応できない可能性がある
  }
  if(any(is.na(res0$naa[,ncol(res0$naa)]))){
    nage <- sum(!is.na(res0$naa[,ncol(res0$naa)])) # naaにNAが入っている対馬マイワシ対応
  }
  else{
    nage <- length(ages)
  }  

  if(!silent)  cat("F multiplier= ", multi,"\n")
  #------------Frecオプションの場合 -------------
  if(!is.null(Frec)){
    if(is.null(Frec$stochastic)) Frec$stochastice <- TRUE
    if(is.null(Frec$method)) Frec$method <- "nibun"
    if(is.null(Frec$seed)) Frec$seed <- as.numeric(Sys.time())
    
    getFrec <- function(x,arglist){
      set.seed(Frec$seed)
      arglist.tmp <- arglist
      arglist.tmp$multi <- x
      arglist.tmp$Frec <- NULL
      arglist.tmp$is.plot <- FALSE
      if(Frec$stochastic==FALSE){
        arglist.tmp$N <- 0
      }      
      fres.tmp <- do.call(future.vpa,arglist.tmp)
      tmp <- rownames(fres.tmp$vssb)==Frec$future.year
      if(all(tmp==FALSE)) stop("nyear should be longer than Frec$future.year.")
      if(Frec$stochastic==TRUE){
        is.lower.ssb <- fres.tmp$vssb<Frec$Blimit
        probs <- (sum(is.lower.ssb[tmp,],na.rm=T)-1)/
          (length(is.lower.ssb[tmp,])-1)*100
        return.obj <- probs-50
      }
      else{
        return.obj <- Frec$Blimit-fres.tmp$vssb[tmp,1]
      }
      return(ifelse(Frec$method=="nibun",return.obj,return.obj^2))        
    }

    if(Frec$method=="nibun"){
      # 二分法
      eps <- ifelse(Frec$stochastic==TRUE,0.5,0.001)
      x.high <- 2 ; x.low <- 0.01;  fx <- Inf
      max.count <- 1000
      s <- 1
      while(abs(fx)>eps && s<max.count){
        x <- (x.high+x.low)/2
        fx <- getFrec(x,arglist)
        if(fx>0) x.high <- x
        if(fx<0) x.low <- x
        cat("fx =",fx,"\n")
        s <- s+1
      }
      multi <- x
    }
    else{
      # optimizeを使う場合=>収束基準が厳しいので時間がかかる
      res <- optimize(getFrec,interval=c(0.01,2),arglist=arglist) 
      multi <- res$minimum        
    }
  }

  #-------------- main function ---------------------
  # ts>1 (半年毎の将来予測の場合、半年毎のwaa, maa, Mを別に与える必要がある)
  if(ts>1 && ((any(sapply(list(waa,maa,M),is.null))) || (any(sapply(list(waa,maa,M),length)!=length(ages))))){
    stop("Appropriate biological paramters of waa, maa, M should be given when ts>1.")
  }
  else{
    waa.org <- waa
    maa.org <- maa
    M.org <- M
  }
  
  faa <- naa <- waa <- maa <- M <- caa <- 
          array(NA,dim=c(length(ages),ntime,N),dimnames=list(ages,fyears,1:N))
  # future biological patameter
  if(!is.null(M.org))  M[] <- M.org  else M[] <- apply(as.matrix(res0$input$dat$M[,years %in% M.year]),1,mean)
  if(!is.null(waa.org))  waa[] <- waa.org  else waa[] <- apply(as.matrix(res0$input$dat$waa[,years %in% waa.year]),1,mean)
  if(!is.null(maa.org))  maa[] <- maa.org  else maa[] <- apply(as.matrix(res0$input$dat$maa[,years %in% maa.year]),1,mean)  

  # time step definition (change F and M)
  M <- M/ts
  if(ts>1){
    currentF <- as.numeric(sweep(matrix(partial.F,ts,nage/ts),2,currentF,FUN="*"))
  }

  # future F matrix
  faa[] <- currentF*multi
  faa[,fyears<min(ABC.year),] <- currentF
#  browser()  
  if(length(tmp <- which(fyear.year %in% years))>0){  
      tmp0 <- which(years %in% fyear.year)
#      tmp1 <- which(fyear.year %in% years)
      for(jj in 1:length(tmp0)){
        for(j in 1:length(tmp)){
          if(ts>1){
            # VPAデータを季節で展開
            faa[,tmp[j],] <- 
              as.numeric(sweep(matrix(partial.F,ts,nage/ts),2,res0$faa[,tmp0[jj]],FUN="*"))
          }
          else{
            if(any(res0$faa[,tmp0[jj]]>0)){ # もしfaaがゼロでないなら（PMIの場合、2012までデータが入っているが、faaはゼロになっているので
              faa[,tmp[j],] <- res0$faa[,tmp0[jj]]
              waa[,tmp[j],] <- res0$input$dat$waa[,tmp0[jj]]
            }
          }
        }}}

  if(ts>1){
    for(j in 1:ts){
      for(kk in 1:N){
#          faa[max(floor(ages))==floor(ages),fyear.season==j,][j,,kk]
        # plus goupのFやwaaは季節によって変えないといけない
        # (plus groupに限らない？。1年に複数回の加入がある場合、季節によるFの違いなのか、加入群に対するFの違いなのかによって仕様を変える必要がある)
        tmp <- t(faa[max(floor(ages))==floor(ages),fyear.season==j,kk])
        tmp[] <- faa[max(floor(ages))==floor(ages),fyear.season==j,,drop=F][j,,kk]
        faa[max(floor(ages))==floor(ages),fyear.season==j,kk] <- t(tmp)

        tmp <- t(waa[max(floor(ages))==floor(ages),fyear.season==j,kk])
        tmp[] <- waa[max(floor(ages))==floor(ages),fyear.season==j,,drop=F][j,,kk]
        waa[max(floor(ages))==floor(ages),fyear.season==j,kk] <- t(tmp)        
      }
    }

    #waaは歴年の漁獲量と同じになるように最適化する

    arglist.tmp <- arglist
    arglist.tmp$ts <- 1
    arglist.tmp$N <- 0
    arglist.tmp$silent <- TRUE
    arglist.tmp$is.plot <- FALSE
    arglist.tmp$waa <- arglist.tmp$maa <- arglist.tmp$M <- NULL
    # SSB用
    fres.cyear <- do.call(future.vpa,arglist.tmp)
    # waaの補正用
    arglist.tmp2 <- arglist.tmp
    arglist.tmp2$multi <- 1
    a <- do.call(future.vpa,arglist.tmp2)    
    if(!is.numeric(waa.multi)){ # if waa="opt"    
      optfunc <- function(x,arglist,a,waa.optyear,replace.rec.year){
        opt.catch <- a$vwcaa[names(a$vwcaa[,1])%in%waa.optyear]
        arglist.tmp <- arglist
        arglist.tmp$N <- 0
        arglist.tmp$silent <- TRUE
        arglist.tmp$is.plot <- FALSE        
        arglist.tmp$waa.multi <- x
          #        browser()        
        arglist.tmp$rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])
#        cat(arglist.tmp$rec.new$rec,"\n")
        a.tmp <- do.call(future.vpa,arglist.tmp)
        pre.catch <- tapply(a.tmp$vwcaa[,1],a.tmp$fyear.year,sum)
        
        xx <- sum((pre.catch[names(pre.catch)%in%waa.optyear]-opt.catch)^2)
#        cat(xx,"\n")
        return(xx)
      }
#      browser()
#      tmp <- optfunc(c(1,1,1),arglist=arglist,opt.catch=opt.catch,waa.optyear=waa.optyear)
#      debug(future.vpa)
      est <- optim(rep(1,length(waa.optyear)),optfunc,
                   arglist=arglist,a=a,waa.optyear=waa.optyear,replace.rec.year=replace.rec.year)
      waa.multi <- est$par
      cat(waa.multi,"\n")
      rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])      
    }
    for(kk in 1:length(waa.optyear)){
      waa[,fyear.year==waa.optyear[kk],] <- waa[,fyear.year==waa.optyear[kk],] * waa.multi[kk]
    }
  }
  tmp <- aperm(faa,c(2,1,3))
  tmp <- tmp*multi.year
  faa <- aperm(tmp,c(2,1,3))

  #  vpa.multi <- ifelse(is.null(vpa.mode),1,vpa.mode$multi)
  # rps assumption
  rps.mat <- array(NA,dim=c(ntime,N),dimnames=list(fyears,1:N))
#  rps.mat[] <- sample(rps.range2,nyear*N,replace=TRUE)  # 平均を揃えたもの
#  rps.mat[,1] <- rps.med
  rec.tmp <- list(rec.resample=NULL,tmparg=NULL)

  if(!is.null(Frec$seed)) set.seed(Frec$seed)    
  for(k in 1:N){
    # future N matrix
    if(sum(start.year==years)==0){
      # VPA結果が2011年まで、将来予測が2012年からだったら、VPA結果を使って2011年まで1年前進計算を行う
      if(start.year==(max(years)+1)){ 
#        tmp <- forward.calc(res0$faa,res0$naa,res0$input$dat$M,
#                            rep(nage,length(years)+1),length(years)+1)
        tmp <- forward.calc.simple(res0$faa[,length(years)],
                                     res0$naa[,length(years)],
                                     res0$input$dat$M[,length(years)],
                                   plus.group=plus.group)
        if(ts==1){        
          naa[1:nage,1,k] <- tmp
        }
        else{
          naa[1:nage,1,k] <- 0
          naa[(ages-floor(ages))==0,1,k] <- tmp
        }
        if(is.na(naa[1,1,k])){
          if(fyears[1]-min.age < start.year){
            thisyear.ssb <- sum(res0$ssb[,as.character(fyears[1]-min.age)],na.rm=T)

          }
          else{
              # 単位を揃える
              thisyear.ssb <- sum(naa[,1,k]*waa[,1,k]*maa[,1,k],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
          }
          rec.tmp <- recfunc(thisyear.ssb,res0,
                             rec.resample=rec.tmp$rec.resample,
                             rec.arg=rec.arg,
                             deterministic=ifelse(k==1,TRUE,FALSE))
          if(!is.null(rec.tmp$rec.arg)) rec.arg <- rec.tmp$rec.arg
          naa[1,1,k] <- rec.tmp$rec
          rps.mat[1,k] <- naa[1,1,k]/thisyear.ssb          
        }
      }
      else{
        stop("ERROR Set appropriate year to start projection\n")
      }
    }
    else{
      if(any(ts==rec.season)){
        naa[,1,k] <- res0$naa[,start.year==years]
      }
      else{
        naa[,1,k] <- 0
        naa[(ages-floor(ages))==0,1,k] <- res0$naa[,start.year==years]
      }
    }

    if(!is.null(rec.new)){
      if(!is.list(rec.new)){
        naa[1,1,k] <- rec.new
      }
      else{ # rec.newがlistの場合
        naa[1,fyears==rec.new$year,k] <- rec.new$rec
      }}

    # ここ、並列化してみる
#    a <- foreach(i=1:(ntime-1)) %do% {naa[,i,k]*(1-exp(-faa[,i,k]))*exp(-M[,i,k]/2)}
    for(i in 1:(ntime-1)){
      if(Pope){
       caa[,i,k] <- naa[,i,k]*(1-exp(-faa[,i,k]))*exp(-M[,i,k]/2)
     }
      else{
        caa[,i,k] <- naa[,i,k]*(1-exp(-faa[,i,k]-M[,i,k]))*faa[,i,k]/(faa[,i,k]+M[,i,k])
      }
        
      #漁獲量がgivenの場合
      if(!is.null(pre.catch) && fyears[i]==pre.catch$year){
          tmp <- caa.est(naa[,i,k],faa[,i,k]/max(faa[,i,k]),
                         waa[,i,k],M[,i,k],pre.catch$wcatch*1000,Pope=Pope)
          faa.new <- tmp$x * faa[,i,k]/max(faa[,i,k])
          caa[,i,k] <- tmp$caa
          faa[,i,k] <- faa.new
        }

      tmp <- forward.calc.simple(faa[,i,k],naa[,i,k],M[,i,k],plus.group=plus.group)
      naa[is.na(naa[,i+1,k]),i+1,k ] <- tmp[is.na(naa[,i+1,k])]      
     
      # 当年の加入の定義
#      if(ifelse(is.null(vpa.mode),TRUE, sum(years==fyears[i+1])==0|vpa.mode$rec=="recfun")){
      if(fyears[i+1]-min.age < start.year){
        thisyear.ssb <- sum(res0$ssb[,as.character(fyears[i+1]-min.age)],na.rm=T)
      }
      else{
        if(ts==1){
          thisyear.ssb <- sum(naa[,i+1-min.age,k]*waa[,i+1-min.age,k]*
                              maa[,i+1-min.age,k],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
        }
        else{
          # 暦年の将来予測での親魚資源量から加入量を推定する（もしかしたらreplace.rec.yearはこれがあれば必要ないのかもしれない）
          # min.ageが0才より大きく、半年毎の計算をする場合対応できない
           # stochasticのときはどうする？
          cssb <- fres.cyear$vssb[,1]
          thisyear.ssb <- cssb[as.numeric(names(cssb))==fyears[i+1]]
          if(length(thisyear.ssb)==0) thisyear.ssb <- 0
        }          
      }
      rec.tmp <- recfunc(thisyear.ssb,res0,
                         rec.resample=rec.tmp$rec.resample,
                         rec.arg=rec.arg,
                         deterministic=ifelse(k==1,TRUE,FALSE))
      if(!is.null(rec.tmp$rec.arg)) rec.arg <- rec.tmp$rec.arg      
      if(is.na(naa[1,i+1,k]) && (floor(fyears[i+1])-fyears[i+1])==0){ # 加入は最初の季節にのみおこる
        naa[1,i+1,k] <- rec.tmp$rec
      }
      else{
        if(is.na(naa[1,i+1,k])) naa[1,i+1,k] <- 0
      }
      rps.mat[i+1,k] <- naa[1,i+1,k]/thisyear.ssb
    }
    if(Pope){
      caa[,ntime,k] <- naa[,ntime,k]*(1-exp(-faa[,ntime,k]))*exp(-M[,ntime,k]/2)
    }
    else{
      caa[,ntime,k] <- naa[,ntime,k]*(1-exp(-faa[,ntime,k]-M[,ntime,k]))*faa[,ntime,k]/(faa[,ntime,k]+M[,ntime,k])
    }
  }
#    for(j in 1:nage) caa[j,nyear,k] <- naa[j,nyear,k]*(1-exp(-faa[j,nyear,k]))*exp(-M[j,nyear,k]/2)

  biom <- naa*waa*res0$input$unit.waa/res0$input$unit.biom
  ssb <- naa*waa*maa*res0$input$unit.waa/res0$input$unit.biom
  
  wcaa <- caa*waa*res0$input$unit.waa/res0$input$unit.biom
  vwcaa <- apply(wcaa,c(2,3),sum,na.rm=T)

  ABC <- apply(as.matrix(vwcaa[fyears%in%ABC.year,,drop=F]),2,sum)
#  colnames(ABC) <- ABC.year
#  browser()
  
  fres <- list(faa=faa,naa=naa,biom=biom,ssb=ssb,wcaa=wcaa,caa=caa,M=M,rps=rps.mat,
               maa=maa,vbiom=apply(biom,c(2,3),sum,na.rm=T),
               waa=waa,currentF=currentF,
               vssb=apply(ssb,c(2,3),sum,na.rm=T),vwcaa=vwcaa,
               years=fyears,fyear.year=fyear.year,ABC=ABC,recfunc=recfunc,rec.arg=rec.arg,
               waa.year=waa.year,maa.year=maa.year,multi=multi,multi.year=multi.year,
               Frec=Frec,rec.new=rec.new,pre.catch=pre.catch,input=arglist)
  class(fres) <- "future"
  if(is.plot){
    par(mfrow=c(2,2))
    plot.future(fres)
  }
  invisible(fres)
}

future.vpa2 <- function(res0,
                       currentF=NULL, # 管理前のF
                       multi=1, # 管理後（ABC.yearから）のF (current F x multi)
                       nyear=10,Pope=res0$input$Pope,
                        seed=NULL,
                       multi.year=1,#ある特定の年だけFを変えたい場合。デフォルトは1。変える場合は、指定した年またはタイムステップの要素数のベクトルで指定。
                       # 年数の指定
                       start.year=NULL, # 将来予測の開始年，NULLの場合はVPA計算の最終年の次の年
                       ABC.year=NULL, # ABC yearを計算する年。NULLの場合はVPA計算の最終年の次の次の年
                       waa.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                                      # NULLの場合，VPAの最終年のパラメータを持ってくる
                       maa.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                       M.year=NULL, # VPA結果から生物パラメータをもってきて平均する期間
                       
                       plus.group=res0$input$plus.group,
                       N=1000,# 確率的なシミュレーションをする場合の繰り返し回数。
                              # N+1の結果が返され、1列目に決定論的な結果が                       
                              # 0を与えると決定論的な結果のみを出力
                        silent=FALSE, is.plot=TRUE, # 計算条件を出力、プロットするか
                       
                        pre.catch=NULL, #list(year=2012,wcatch=13000), 漁獲重量をgivenで与える場合
                        outtype="FULL", # 結果の出力を小さくするか。FULL=しない。それ以外＝する。
                       #-------- 加入に関する設定 -----------------
                       rec.new=NULL, # 指定した年の加入量
                                     # 年を指定しないで与える場合は、自動的にスタート年の加入になる。
                                     # list(year=, rec=)で与える場合は、対応する年の加入を置き換える。
                       #--- 加入関数
                       recfunc=RPS.simple.rec, # 太平洋マサバ、ゴマサバ以外はRPS.simple.recを使う
                       rec.arg=list(upper.ssb=Inf,upper.recruit=Inf), # 加入の各種設定
                       #--- Frecオプション；Frec計算のための設定リストを与えると、指定された設定でのFrecに対応するFで将来予測を行う
                       Frec=NULL, # list(stochastic=TRUE, # TRUEの場合、stochastic simulationで50%の確率でBlimitを越す(PMS, TMI)
                                                          # FALSEの場合、RPS固定のprojectionがBilmitと一致する(NSK)
                                 #      future.year=2018, # 何年の資源量を見るか？
                                 #      Blimit=450*1000,  # Blimit (xトン)
                            #      seed=100) # 乱数のシード
                       # 対馬サバに対応するオプション。ts=2のとき、1年を2つの季節に分けて将来予測する
                       ts=1, # 時間ステップ。1年間の季節の数。普通は１。対馬サバの場合2。ts=1の場合、以下の引数はすべて無視される。
                #---- 以下、ts>2のときに必要な引数 -----
                       waa=NULL,maa=NULL,M=NULL, # 季節毎の生物パラメータ
                       rec.season=1, # 加入がおこる季節
                       waa.multi="opt", # waa.optyearに対応する年について、暦年漁獲量と一致するようにwaaを最適化するか？ "opt"の場合、内部で最適化。waa.optyearの長さ分のベクトルを与えて指定することもできる
                       waa.optyear=2011:2013, # waa.optyearをするときに、置き換えるwaaの年
                       replace.rec.year=2012, # 加入量を暦年の将来予測での加入量に置き換えるか？
                       partial.F=NULL         # 季節毎のpartial F
                       ){

  if(!is.null(seed)) set.seed(seed)
  argname <- ls()
  arglist <- lapply(argname,function(x) eval(parse(text=x)))
  names(arglist) <- argname

  if(is.null(res0$input$unit.waa)) res0$input$unit.waa <- 1
  if(is.null(res0$input$unit.caa)) res0$input$unit.caa <- 1
  if(is.null(res0$input$unit.biom)) res0$input$unit.biom <- 1  
    
  if(ts>1 && is.null(partial.F)){
    stop("When ts>1, partial.F should be given")
  }
  #--------------------------------------------------
  N <- N + 1
  years <- as.numeric(dimnames(res0$naa)[[2]])
    
  #------------- set default options
  if(is.null(currentF)) currentF <- res0$Fc.at.age
  if(is.null(waa.year)) waa.year <- rev(years)[1]
  if(is.null(maa.year)) maa.year <- rev(years)[1]
  if(is.null(M.year)) M.year <- rev(years)[1]
  if(is.null(start.year)) start.year <- rev(years)[1]+1
  if(is.null(ABC.year)) ABC.year <- rev(years)[1]+1
  arglist$ABC.year <- ABC.year
  #-------------
  
#  fyears <- start.year:(start.year+nyear-1)
  fyears <- seq(from=start.year,to=start.year+nyear-1,by=1/ts)
  fyear.year <- floor(fyears)
  fyear.season <- #fyears-fyear.year
                  rep(1:ts,nyear)
  fyear.season <- fyear.season[1:length(fyears)]
  ntime <- length(fyears)
#  if(is.null(multi.year)) multi.year <- rep(1,nyear)*ts  
  ages <- as.numeric(dimnames(res0$naa)[[1]])
#  nage <- length(ages)
  min.age <- min(as.numeric(ages))
  if(ts>1){
    ages <- seq(from=min(ages),to=max(ages)+1/ts,by=1/ts)
    nage <- length(ages) # naaにNAが入っていて、かつ、半年毎の将来予測をする場合対応できない可能性がある
  }
  if(any(is.na(res0$naa[,ncol(res0$naa)]))){
    nage <- sum(!is.na(res0$naa[,ncol(res0$naa)])) # naaにNAが入っている対馬マイワシ対応
  }
  else{
    nage <- length(ages)
  }  

  if(!silent)  cat("F multiplier= ", multi,"\n")
  #------------Frecオプションの場合 -------------
  if(!is.null(Frec)){
    if(is.null(Frec$stochastic)) Frec$stochastice <- TRUE
    if(is.null(Frec$method)) Frec$method <- "nibun"
    if(is.null(Frec$seed)) Frec$seed <- as.numeric(Sys.time())
    
    getFrec <- function(x,arglist){
      set.seed(Frec$seed)
      arglist.tmp <- arglist
      arglist.tmp$multi <- x
      arglist.tmp$Frec <- NULL
      arglist.tmp$is.plot <- FALSE
      if(Frec$stochastic==FALSE){
        arglist.tmp$N <- 0
      }      
      fres.tmp <- do.call(future.vpa,arglist.tmp)
      tmp <- rownames(fres.tmp$vssb)==Frec$future.year
      if(all(tmp==FALSE)) stop("nyear should be longer than Frec$future.year.")
      if(Frec$stochastic==TRUE){
        is.lower.ssb <- fres.tmp$vssb<Frec$Blimit
        probs <- (sum(is.lower.ssb[tmp,],na.rm=T)-1)/
          (length(is.lower.ssb[tmp,])-1)*100
        return.obj <- probs-50
      }
      else{
        return.obj <- Frec$Blimit-fres.tmp$vssb[tmp,1]
      }
      return(ifelse(Frec$method=="nibun",return.obj,return.obj^2))        
    }

    if(Frec$method=="nibun"){
      # 二分法
      eps <- ifelse(Frec$stochastic==TRUE,0.5,0.001)
      x.high <- 2 ; x.low <- 0.01;  fx <- Inf
      max.count <- 1000
      s <- 1
      while(abs(fx)>eps && s<max.count){
        x <- (x.high+x.low)/2
        fx <- getFrec(x,arglist)
        if(fx>0) x.high <- x
        if(fx<0) x.low <- x
        cat("fx =",fx,"\n")
        s <- s+1
      }
      multi <- x
    }
    else{
      # optimizeを使う場合=>収束基準が厳しいので時間がかかる
      res <- optimize(getFrec,interval=c(0.01,2),arglist=arglist) 
      multi <- res$minimum        
    }
  }

  #-------------- main function ---------------------
  # ts>1 (半年毎の将来予測の場合、半年毎のwaa, maa, Mを別に与える必要がある)
  if(ts>1 && ((any(sapply(list(waa,maa,M),is.null))) || (any(sapply(list(waa,maa,M),length)!=length(ages))))){
    stop("Appropriate biological paramters of waa, maa, M should be given when ts>1.")
  }
  else{
    waa.org <- waa
    maa.org <- maa
    M.org <- M
  }
  
  faa <- naa <- waa <- maa <- M <- caa <- 
          array(NA,dim=c(length(ages),ntime,N),dimnames=list(ages,fyears,1:N))
  # future biological patameter
  if(!is.null(M.org))  M[] <- M.org  else M[] <- apply(as.matrix(res0$input$dat$M[,years %in% M.year]),1,mean)
  if(!is.null(waa.org))  waa[] <- waa.org  else waa[] <- apply(as.matrix(res0$input$dat$waa[,years %in% waa.year]),1,mean)
  if(!is.null(maa.org))  maa[] <- maa.org  else maa[] <- apply(as.matrix(res0$input$dat$maa[,years %in% maa.year]),1,mean)  

  # time step definition (change F and M)
  M <- M/ts
  if(ts>1){
    currentF <- as.numeric(sweep(matrix(partial.F,ts,nage/ts),2,currentF,FUN="*"))
  }

  # future F matrix
  faa[] <- currentF*multi
  faa[,fyears<min(ABC.year),] <- currentF
#  browser()  
  if(length(tmp <- which(fyear.year %in% years))>0){  
      tmp0 <- which(years %in% fyear.year)
#      tmp1 <- which(fyear.year %in% years)
      for(jj in 1:length(tmp0)){
        for(j in 1:length(tmp)){
          if(ts>1){
            # VPAデータを季節で展開
            faa[,tmp[j],] <- 
              as.numeric(sweep(matrix(partial.F,ts,nage/ts),2,res0$faa[,tmp0[jj]],FUN="*"))
          }
          else{
            if(any(res0$faa[,tmp0[jj]]>0)){ # もしfaaがゼロでないなら（PMIの場合、2012までデータが入っているが、faaはゼロになっているので
              faa[,tmp[j],] <- res0$faa[,tmp0[jj]]
              waa[,tmp[j],] <- res0$input$dat$waa[,tmp0[jj]]
            }
          }
        }}}

  if(ts>1){
    for(j in 1:ts){
      for(kk in 1:N){
#          faa[max(floor(ages))==floor(ages),fyear.season==j,][j,,kk]
        # plus goupのFやwaaは季節によって変えないといけない
        # (plus groupに限らない？。1年に複数回の加入がある場合、季節によるFの違いなのか、加入群に対するFの違いなのかによって仕様を変える必要がある)
        tmp <- t(faa[max(floor(ages))==floor(ages),fyear.season==j,kk])
        tmp[] <- faa[max(floor(ages))==floor(ages),fyear.season==j,,drop=F][j,,kk]
        faa[max(floor(ages))==floor(ages),fyear.season==j,kk] <- t(tmp)

        tmp <- t(waa[max(floor(ages))==floor(ages),fyear.season==j,kk])
        tmp[] <- waa[max(floor(ages))==floor(ages),fyear.season==j,,drop=F][j,,kk]
        waa[max(floor(ages))==floor(ages),fyear.season==j,kk] <- t(tmp)        
      }
    }

    #waaは歴年の漁獲量と同じになるように最適化する

    arglist.tmp <- arglist
    arglist.tmp$ts <- 1
    arglist.tmp$N <- 0
    arglist.tmp$silent <- TRUE
    arglist.tmp$is.plot <- FALSE
    arglist.tmp$waa <- arglist.tmp$maa <- arglist.tmp$M <- NULL
    # SSB用
    fres.cyear <- do.call(future.vpa,arglist.tmp)
    # waaの補正用
    arglist.tmp2 <- arglist.tmp
    arglist.tmp2$multi <- 1
    a <- do.call(future.vpa,arglist.tmp2)    
    if(!is.numeric(waa.multi)){ # if waa="opt"    
      optfunc <- function(x,arglist,a,waa.optyear,replace.rec.year){
        opt.catch <- a$vwcaa[names(a$vwcaa[,1])%in%waa.optyear]
        arglist.tmp <- arglist
        arglist.tmp$N <- 0
        arglist.tmp$silent <- TRUE
        arglist.tmp$is.plot <- FALSE        
        arglist.tmp$waa.multi <- x
          #        browser()        
        arglist.tmp$rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])
#        cat(arglist.tmp$rec.new$rec,"\n")
        a.tmp <- do.call(future.vpa,arglist.tmp)
        pre.catch <- tapply(a.tmp$vwcaa[,1],a.tmp$fyear.year,sum)
        
        xx <- sum((pre.catch[names(pre.catch)%in%waa.optyear]-opt.catch)^2)
#        cat(xx,"\n")
        return(xx)
      }
#      browser()
#      tmp <- optfunc(c(1,1,1),arglist=arglist,opt.catch=opt.catch,waa.optyear=waa.optyear)
#      debug(future.vpa)
      est <- optim(rep(1,length(waa.optyear)),optfunc,
                   arglist=arglist,a=a,waa.optyear=waa.optyear,replace.rec.year=replace.rec.year)
      waa.multi <- est$par
      cat(waa.multi,"\n")
      rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])      
    }
    for(kk in 1:length(waa.optyear)){
      waa[,fyear.year==waa.optyear[kk],] <- waa[,fyear.year==waa.optyear[kk],] * waa.multi[kk]
    }
  }
  tmp <- aperm(faa,c(2,1,3))
  tmp <- tmp*multi.year
  faa <- aperm(tmp,c(2,1,3))

  #  vpa.multi <- ifelse(is.null(vpa.mode),1,vpa.mode$multi)
  # rps assumption
  rps.mat <- array(NA,dim=c(ntime,N),dimnames=list(fyears,1:N))
#  rps.mat[] <- sample(rps.range2,nyear*N,replace=TRUE)  # 平均を揃えたもの
#  rps.mat[,1] <- rps.med
  rec.tmp <- list(rec.resample=NULL,tmparg=NULL)

  if(!is.null(Frec$seed)) set.seed(Frec$seed)    

#  for(k in 1:N){  #k loopを消す
    # future N matrix
    if(sum(start.year==years)==0){
      # VPA結果が2011年まで、将来予測が2012年からだったら、VPA結果を使って2011年まで1年前進計算を行う
      if(start.year==(max(years)+1)){ 
#        tmp <- forward.calc(res0$faa,res0$naa,res0$input$dat$M,
#                            rep(nage,length(years)+1),length(years)+1)
        tmp <- forward.calc.simple(res0$faa[,length(years)],
                                     res0$naa[,length(years)],
                                     res0$input$dat$M[,length(years)],
                                   plus.group=plus.group)
        if(ts==1){        
          naa[1:nage,1,] <- tmp
        }
        else{
          naa[1:nage,1,] <- 0
          naa[(ages-floor(ages))==0,1,] <- tmp
        }
        if(all(is.na(naa[1,1,]))){
          if(fyears[1]-min.age < start.year){
            thisyear.ssb <- rep(sum(res0$ssb[,as.character(fyears[1]-min.age)],na.rm=T),N)
          }
          else{
              thisyear.ssb <- colSums(naa[,1,]*waa[,1,]*maa[,1,],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
          }
          rec.tmp0 <- recfunc(thisyear.ssb[1],res0,
                             rec.resample=rec.tmp$rec.resample,
                             rec.arg=rec.arg,
                             deterministic=TRUE)
          rec.tmp1 <- recfunc(thisyear.ssb[-1],res0,
                             rec.resample=rec.tmp$rec.resample,
                             rec.arg=rec.arg,
                             deterministic=FALSE)          
          if(!is.null(rec.tmp$rec.arg)) rec.arg <- rec.tmp$rec.arg
          naa[1,1,] <- c(rec.tmp0$rec,rec.tmp1$rec)
          rps.mat[1,] <- naa[1,1,]/thisyear.ssb          
        }
      }
      else{
        stop("ERROR Set appropriate year to start projection\n")
      }
    }
    else{
      if(any(ts==rec.season)){
        naa[,1,] <- res0$naa[,start.year==years]
      }
      else{
        naa[,1,] <- 0
        naa[(ages-floor(ages))==0,1,] <- res0$naa[,start.year==years]
      }
    }

    if(!is.null(rec.new)){
      if(!is.list(rec.new)){
        naa[1,1,] <- rec.new
      }
      else{ # rec.newがlistの場合
        naa[1,fyears==rec.new$year,] <- rec.new$rec
      }}

    for(i in 1:(ntime-1)){
      if(Pope){
       caa[,i,] <- naa[,i,]*(1-exp(-faa[,i,]))*exp(-M[,i,]/2)
     }
      else{
        caa[,i,] <- naa[,i,]*(1-exp(-faa[,i,]-M[,i,]))*faa[,i,]/(faa[,i,]+M[,i,])
      }
        
      #漁獲量がgivenの場合
      if(!is.null(pre.catch) && fyears[i]==pre.catch$year){
        for(k in 1:N){
          tmp <- caa.est(naa[,i,k],faa[,i,k]/max(faa[,i,k]),
                         waa[,i,k],M[,i,k],pre.catch$wcatch*1000,Pope=Pope)
          faa.new <- tmp$x * faa[,i,k]/max(faa[,i,k])
          caa[,i,k] <- tmp$caa
          faa[,i,k] <- faa.new
        }}

      tmp <- forward.calc.mat(faa[,i,],naa[,i,],M[,i,],plus.group=plus.group)
      naa[,i+1,][is.na(naa[,i+1,])] <- tmp[is.na(naa[,i+1,])]      
     
      # 当年の加入の定義
#      if(ifelse(is.null(vpa.mode),TRUE, sum(years==fyears[i+1])==0|vpa.mode$rec=="recfun")){
      if(fyears[i+1]-min.age < start.year){
        thisyear.ssb <- rep(sum(res0$ssb[,as.character(fyears[i+1]-min.age)],na.rm=T),N)
      }
      else{
        if(ts==1){
          thisyear.ssb <- colSums(naa[,i+1-min.age,]*waa[,i+1-min.age,]*
                              maa[,i+1-min.age,],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
        }
        else{
          # 暦年の将来予測での親魚資源量から加入量を推定する（もしかしたらreplace.rec.yearはこれがあれば必要ないのかもしれない）
          # min.ageが0才より大きく、半年毎の計算をする場合対応できない
           # stochasticのときはどうする？
          cssb <- fres.cyear$vssb[,1]
          thisyear.ssb <- cssb[as.numeric(names(cssb))==fyears[i+1]]
          if(length(thisyear.ssb)==0) thisyear.ssb <- 0
        }          
      }
      rec.tmp0 <- recfunc(thisyear.ssb[1],res0,
                         rec.resample=rec.tmp$rec.resample,
                         rec.arg=rec.arg,
                         deterministic=TRUE)      
      rec.tmp <- recfunc(thisyear.ssb[-1],res0,
                         rec.resample=rec.tmp$rec.resample,
                         rec.arg=rec.arg,
                         deterministic=FALSE)
      if(!is.null(rec.tmp$rec.arg)) rec.arg <- rec.tmp$rec.arg      
      if(all(is.na(naa[1,i+1,])) && (floor(fyears[i+1])-fyears[i+1])==0){ # 加入は最初の季節にのみおこる
        naa[1,i+1,] <- c(rec.tmp0$rec,rec.tmp$rec)
      }
      else{
        if(all(is.na(naa[1,i+1,]))) naa[1,i+1,] <- 0
      }
      rps.mat[i+1,] <- naa[1,i+1,]/thisyear.ssb
    }
    if(Pope){
      caa[,ntime,] <- naa[,ntime,]*(1-exp(-faa[,ntime,]))*exp(-M[,ntime,]/2)
    }
    else{
      caa[,ntime,] <- naa[,ntime,]*(1-exp(-faa[,ntime,]-M[,ntime,]))*faa[,ntime,]/(faa[,ntime,]+M[,ntime,])
    }

  biom <- naa*waa*res0$input$unit.waa/res0$input$unit.biom
  ssb <- naa*waa*maa*res0$input$unit.waa/res0$input$unit.biom
  
  wcaa <- caa*waa*res0$input$unit.waa/res0$input$unit.biom
  vwcaa <- apply(wcaa,c(2,3),sum,na.rm=T)

  ABC <- apply(as.matrix(vwcaa[fyears%in%ABC.year,,drop=F]),2,sum)
#  colnames(ABC) <- ABC.year
#  browser()

  if(outtype=="FULL"){
      fres <- list(faa=faa,naa=naa,biom=biom,ssb=ssb,wcaa=wcaa,caa=caa,M=M,rps=rps.mat,
                   maa=maa,vbiom=apply(biom,c(2,3),sum,na.rm=T),
                   waa=waa,currentF=currentF,
                   vssb=apply(ssb,c(2,3),sum,na.rm=T),vwcaa=vwcaa,
                   years=fyears,fyear.year=fyear.year,ABC=ABC,recfunc=recfunc,rec.arg=rec.arg,
                   waa.year=waa.year,maa.year=maa.year,multi=multi,multi.year=multi.year,
                   Frec=Frec,rec.new=rec.new,pre.catch=pre.catch,input=arglist)
  }
  else{
      fres <- list(faa=faa[,,1],M=M[,,1],recruit=naa[1,,],
                   maa=maa[,,1],vbiom=apply(biom,c(2,3),sum,na.rm=T),
                   waa=waa[,,1],currentF=currentF,
                   vssb=apply(ssb,c(2,3),sum,na.rm=T),vwcaa=vwcaa,
                   years=fyears,fyear.year=fyear.year,ABC=ABC,recfunc=recfunc,rec.arg=rec.arg,
                   waa.year=waa.year,maa.year=maa.year,multi=multi,multi.year=multi.year,
                   Frec=Frec,rec.new=rec.new,pre.catch=pre.catch,input=arglist)
  }
  class(fres) <- "future"
  if(is.plot){
    par(mfrow=c(2,2))
    plot.future(fres)
  }
  invisible(fres)
}


plot.future <- function(fres0,ylim.tmp=NULL,xlim.tmp=NULL,vpares=NULL,what=c(TRUE,TRUE,TRUE),
                        label=c("Biomass","SSB","Catch"),is.legend=TRUE,add=FALSE,col=NULL){
  if(is.null(col)) col <- 1                        
  matplot2 <- function(x,add=FALSE,...){
    if(add==FALSE) matplot(rownames(x),x,type="l",lty=c(2,1,2),col=col,xlab="Year",...)
    if(add==TRUE) matpoints(rownames(x),x,type="l",lty=c(2,1,2),col=col,xlab="Year",...)    
  }

  if(is.null(xlim.tmp)) xlim.tmp <- range(as.numeric(colnames(fres0$naa)))

  if(what[1]){
    matplot2(x <- t(apply(fres0$vbiom,1,quantile,probs=c(0.1,0.5,0.9))),
           ylim=c(0,ifelse(is.null(ylim.tmp),max(x),ylim.tmp[1])),
           xlim=xlim.tmp,
           ylab=label[1],main=label[1],add=add)
    points(rownames(fres0$vbiom),apply(fres0$vbiom,1,mean),type="b",pch=1)
    points(rownames(fres0$vbiom),as.numeric(fres0$vbiom[,1]),type="b",pch=3)
    if(!is.null(vpares)){
      points(colnames(vpares$baa),colSums(vpares$baa),type="o",pch=20)
    }
  }

  if(what[2]){
    matplot2(x <- t(apply(fres0$vssb,1,quantile,probs=c(0.1,0.5,0.9))),
             ylim=c(0,ifelse(is.null(ylim.tmp),max(x),ylim.tmp[2])),
             xlim=xlim.tmp,           
             ylab=label[2],main=label[2],add=add)
    points(rownames(fres0$vssb),apply(fres0$vssb,1,mean),type="b",pch=1)    
    points(rownames(fres0$vssb),as.numeric(fres0$vssb[,1]),type="b",pch=3)    
    if(!is.null(vpares)){
      points(colnames(vpares$ssb),colSums(vpares$ssb),type="o",pch=20)
    }
  }

  if(what[3]){
    matplot2(x <- t(apply(fres0$vwcaa,1,quantile,probs=c(0.1,0.5,0.9))),
             ylim=c(0,ifelse(is.null(ylim.tmp),max(x),ylim.tmp[3])),
             xlim=xlim.tmp,           
             ylab=label[3],main=label[3],add=add)
    points(rownames(fres0$vwcaa),apply(fres0$vwcaa,1,mean),type="b",pch=1)        
    points(rownames(fres0$vwcaa),as.numeric(fres0$vwcaa[,1]),type="b",pch=3)        
    if(!is.null(vpares)){
      points(colnames(vpares$baa),colSums(vpares$input$dat$caa*vpares$input$dat$waa),type="o",pch=20)
    }
  }
  if(is.legend){
    plot(1:10,type = "n",ylab = "", xlab = "", axes = F)
    legend("topleft",lty=c(1,1,1,2),legend=c("Deterministic","Mean","Median","80%conf"),pch=c(3,1,NA,NA))
  }
  
}

#print.future <- function(fres){ # S3 method を使いたいんですが、まだいまいちわかりません
#  cat(fres$ABC[1])
#}
#

ref.F2 <- function(res0,target.year=c(2018,2023),current.year=2011,Blim,
                   interval=c(0,3),...){
  ssb <- apply(res0$ssb,2,sum)
  Frec <- numeric()
  Frec[1] <- ssb[current.year]/Blim

  for(i in 1:length(target.year)){
    tmpfunc <- function(x,res0,Blim,...){
      fres <- future.vpa(res0=res0,multi=x,...)
      cat(x," ")    
      return((fres$vssb[rownames(fres$vssb)==target.year[i]]-Blim)^2)
    }
    Frec[i+1] <- optimize(tmpfunc,interval=interval,res0=res0,Blim=Blim,...)$minimum
  }
  return(Frec)
}

# 2012. 8. 3 -- 管理基準値計算は外に出す
getABC <- function(res.vpa, # VPAの結果
                   res.ref, # 管理基準値計算の結果
                   res.future, # 将来予測計算の結果
                   ref.case="all",
                   multi=NULL,
                   N=NULL,                   
                   SSBcur=1000,
                   Blim=1000,
                   Bban=0, # Kobe matrixを作るときに参照
                   ssb=TRUE, # limitがSSBかどうか。biomassのときはFALSE                 
                   target.year=NULL, # NULLの場合，ABC.year+4
                   catch.year=NULL, # 2013:2017など、漁獲量の平均を出したい期間、NULLの場合、ABC.year:ABC.year+4
                   is.plot=TRUE){
  if(all(ref.case=="all")) ref.case <- names(res.ref$summary)
  if(all(is.null(multi))) multi <- rep(1,length(ref.case))
                                       
  nref <- length(ref.case)

  ABC.year <- res.future$input$ABC.year
  if(is.null(target.year)) target.year <- ABC.year+4
  ABC <- wariai <- aveF <- catch5u <- catch5l <- upperSSBlim <- upperSSBcur <- SSBlim <- SSBcur.tmp <- rep(NA,nref)
  names(ABC) <- names(wariai) <- names(aveF) <- paste(ref.case,"x",round(multi,3))
  wcatch <- matrix(NA,5,nref,dimnames=list(((min(ABC.year)):(min(ABC.year)+4)),names(aveF)))

  fres <- list()
  i.tmp <- match(ref.case,names(res.ref$summary))
  
  if(any(is.na(i.tmp)))
    stop(paste("ref.case specification of is wrong!"))

  years <- res.future$year
  currentF <- res.ref$Fcurrent["max"] * res.ref$sel
  N <- ifelse(is.null(N),dim(res.future$naa)[[3]],N)

  for(i in 1:nref){
    tmp <- res.ref$summary[i.tmp[i]][1,1] * res.ref$sel
    tmp <- max(tmp,na.rm=T)/max(currentF,na.rm=T)*multi[i]
    tmpF <- tmp * currentF
    aveF[i] <- mean(tmpF,na.rm=T)
    input.tmp <- res.future$input        
    input.tmp$multi <- tmp
    input.tmp$is.plot <- FALSE
    input.tmp$N <- N
    input.tmp$Frec <- NULL
    fres[[i]] <- do.call(future.vpa, input.tmp)
    ABC[i] <- fres[[i]]$ABC[1]
#    browser()    
    if(res.future$input$ts>1){ # ts>2の場合、漁獲量などの計算は暦年を使う
      input.tmp <- res.future$input
      input.tmp$multi <- tmp      
      input.tmp$ts <- 1
      input.tmp$is.plot <- FALSE      
      input.tmp$ABC.year <- ABC.year <- floor(min(input.tmp$ABC.year))
      input.tmp$waa <- input.tmp$maa <- input.tmp$M <- NULL
      input.tmp$N <- N
      fres[[i]] <- do.call(future.vpa, input.tmp)
      years <- fres[[i]]$year
    }
    wariai[i] <- sum(fres[[i]]$wcaa[,years==ABC.year,1],na.rm=T)/
            sum(fres[[i]]$biom[,years==ABC.year,1],na.rm=T)
    catch.year <- (ABC.year):(ABC.year+4)
    wcatch[,i] <- apply(fres[[i]]$vwcaa[years %in% (catch.year),-1],1,mean,na.rm=T)
    catch5u[i] <- quantile(fres[[i]]$vwcaa[years==max(catch.year),-1],probs=0.9) # catchは2017年
    catch5l[i] <- quantile(fres[[i]]$vwcaa[years==max(catch.year),-1],probs=0.1) 

    tmp.year <- years %in% target.year
    if(is.null(SSBcur)) SSBcur <- fres[[i]]$vssb[years==(ABC.year),1]    
      
    SSBcur.tmp[i] <- SSBcur
    upperSSBlim[i] <- sum(fres[[i]]$vssb[tmp.year,-1]>Blim)/N*100 # SSBは2018年当初まで
    upperSSBcur[i] <- sum(fres[[i]]$vssb[tmp.year,-1]>SSBcur)/N*100
    SSBlim[i] <- Blim
  }

  if(is.plot){
    par(mfrow=c(1,2),mar=c(4,4,2,1))
    vssb <- apply(res.vpa$ssb,2,sum,na.rm=T)/1000
    x <- sapply(fres,function(x) x$vssb[,1])/1000
    plot(range(c(as.numeric(names(vssb)),years)),
         c(0,max(x)*1.1),type="n",xlab="Year",ylab="SSB (x1000)")
    matpoints(years,x,col=1:nref,type="l",lty=1,
            ylim=c(0,max(x)))
    points(as.numeric(names(vssb)),vssb,type="b")
    abline(h=c(SSBlim/1000,SSBcur/1000),col="gray")
    title("SSB in deterministic runs")
    plot(0,axes=F,xlab="",ylab="")
    legend("topleft",col=1:nref,lty=1,legend=names(ABC))
  }
  average <- apply(wcatch,2,mean)
  res.ref$ABC <- rbind(aveF,wariai,catch5l,catch5u,average,
                         upperSSBcur,SSBcur.tmp,upperSSBlim,SSBlim,ABC)
  rownames(res.ref$ABC)[3] <- paste("catch5l during ",min(catch.year),"-",max(catch.year),sep="")
  rownames(res.ref$ABC)[4] <- paste("catch5u during ",min(catch.year),"-",max(catch.year),sep="")  
  rownames(res.ref$ABC)[5] <- paste("average catch during ",min(catch.year),"-",max(catch.year),sep="")    
  rownames(res.ref$ABC)[6] <- paste("upperSSBcur at",target.year)
  rownames(res.ref$ABC)[8] <- paste("upperSSBlim at",target.year)  
  fres0 <- fres
  write.table(round(res.ref$ABC,2),sep="\t")
  save(fres0,file="fres0.R") # 将来予測の全結果はfres0.Rにてセーブされている

  # Kobe chartの作成
  kobe.array <- array(NA,dim=c(length(fres),nrow(fres[[1]]$vssb),5))
  dimnames(kobe.array) <- list(names(ABC),rownames(fres[[1]]$vssb),
                               c("catch","Biomass","SSB","upperBlimit","upperBban"))
  for(i in 1:length(fres)){
      kobe.array[i,,] <- as.matrix(get.kobematrix(fres[[i]],
                                   Blim=Blim,Bban=Bban,ssb=TRUE))
  }
  return(list(ABC=res.ref$ABC,kobe.matrix=kobe.array))
}  

#----------------------------------------------------------------------
#----------   加入に関する関数。魚種specific        -------------------
#----------------------------------------------------------------------
#---- 期間内のRPSをサンプリング。平均値と中央値の差を補正するかどうか？など 
RPS.simple.rec <- function(ssb,vpares,
                           rec.arg=list(rps.year=NULL, # 点推定値のrpsを計算する期間
                             upper.ssb=Inf, # 親魚資源量の上限（単位はトン？）
                             upper.recruit=Inf,
			     sample.year = NULL, # リサンプリング期間。rps.yearと異なる範囲を使う場合、設定する
                             bias.corrected=TRUE, # stochasticのときに平均値と中央値の比率を使うもの)
                             rpsmean=FALSE),# deterministicのときに、RPSの平均を使うか、中央値を使うか。（スケトウでは平均、その他の魚種は中央値）
                           deterministic=FALSE,rec.resample=NULL # ここは外から指定する必要ない
                           ){ 
  argname <- c("rps.year","upper.ssb","upper.recruit","sample.year","bias.corrected","rpsmean")
  tmp <- !(names(rec.arg) %in% argname)
  if(sum(tmp)>0) stop(paste(names(rec.arg)[tmp],"no such arguments in RPS.simple.rec"))
  
  if(is.null(rec.arg$bias.corrected)) rec.arg$bias.corrected <- TRUE
  if(is.null(rec.arg$rpsmean)) rec.arg$rpsmean <- FALSE
  if(is.null(rec.arg$rps.year)) rec.arg$rps.year <- as.numeric(colnames(vpares$naa))
  if(is.null(rec.arg$sample.year)) rec.arg$sample.year <- rec.arg$rps.year

#  browser()
  names(rec.arg)

  if(is.null(rec.resample)|deterministic){
    min.age <- min(as.numeric(rownames(vpares$ssb)))
    if(min.age==0) slide.tmp <- TRUE else slide.tmp <- -1:-min.age    
    rps.data <- data.frame(year=years <- as.numeric(names(colSums(vpares$ssb,na.rm=T))),
                           ssb=SSB <- as.numeric(colSums(vpares$ssb,na.rm=T)),
                           recruit=rec <- as.numeric(c(vpares$naa[1,slide.tmp],
                             rep(NA,min.age))))
    rps.data$rps <- rps <- rps.data$recruit/rps.data$ssb
    
    rps.range <- as.numeric(rps[years %in% rec.arg$rps.year])
    rps.med <- median(rps.range) # 点推定のためのrps
    rps.mean <- mean(rps.range) # 点推定のためのrps

    sample.range <- as.numeric(rps[years %in% rec.arg$sample.year]) # リサンプリングのためのrps
    sample.mean <- mean(sample.range) # リサンプリングのためのrps
    if(rec.arg$bias.corrected==TRUE){
#      rec.resample <- sample.range/rps.mean*rps.med
      rec.resample <- sample.range/sample.mean*rps.med # ここは本当にsample.meanで良いのか？(サンプル期間が同じ場合、sample.mean=rps=meanなので問題ない。期間が異なる場合、rps.meanを使うとsample.range/rps.meanの平均が1にならないため、やはりsample.meanを使うのが適切)
 
    }
    else{
      rec.resample <- sample.range
    }
  }
  ssb.tmp <- min(ssb,rec.arg$upper.ssb)
  if(deterministic){
    if(rec.arg$rpsmean){
      rec <- ssb.tmp * rps.mean
    }
    else{
      rec <- ssb.tmp * rps.med
    }
  }
  else{
    rec <- ssb.tmp * sample(rec.resample,1)
  }
  rec2 <- min(rec,rec.arg$upper.recruit)  
  return(list(rec=rec2,rec.resample=rec.resample))
}

#----- ゴマサバ（太平洋用）
gomasaba.rec <- function(ssb,vpares,
                         rec.arg=list(Blim.rec=38*1000,a=14.6887049,b=-0.0055629,tmparg=NULL,max.ratio=2),
                         rec.resample=NULL,
                         deterministic=FALSE){
  Blim.rec <- rec.arg$Blim.rec/1000
  tmpfunc <- function(ssb,a,b){
#    14.6887049*exp(-0.0055629*ssb/1000)
    rec.arg$a*exp(rec.arg$b*ssb/1000)        
  }
  rps <- tmpfunc(ssb)

  
  if(is.null(rec.arg$tmparg)) rec.arg$tmparg <- 1
  if(is.null(rec.resample)){
    vpa.ssb <- apply(vpares$ssb,2,sum,na.rm=T)
    pred.rps <- tmpfunc(vpa.ssb)
    obs.rps <- vpares$naa[1,]/vpa.ssb*1000
    rec.resample <- obs.rps/pred.rps # 回帰では1996, 2004年は除いていたが、使う。ただし、卓越年級は2年続けて発生しない、Blim以下では発生しないという制約がある
#    rec.resample[rec.resample>rec.arg$max.ratio] <- 1
  }

  if(ssb/1000<Blim.rec | rec.arg$tmparg > rec.arg$max.ratio){
    # 川端さんexcel seelt仕様。max.ratio以上の年は１に置き換える
    tmp.resample <- rec.resample
    tmp.resample[tmp.resample>rec.arg$max.ratio] <- 1
    tmp <- as.numeric(sample(tmp.resample,1))
  }
  else{
    tmp <- as.numeric(sample(rec.resample,1))
  }

  if(deterministic==TRUE) tmp <- 1
  rec <- rps*ssb/1000*tmp
  rec.arg$tmparg <- tmp
  return(list(rec=rec,rec.resample=rec.resample,rec.arg=rec.arg))  
}

#---- マサバ・マイワシ太平洋用 (Blimの上と下で、RPSの平均に乗じる比率のサンプル範囲を変える）
masaba.rec <- function(ssb,vpares,
                       rec.arg=list(rps.years=list(1970:1985,1986:2011), # year < Blim, year > Blim # rps.yearではなく，"s"がつく
                         Blim.rec=450,upper.ssb=1380*1000,
												 rps.years.med=NULL, # RPSmedの期間がrps.yearsで指定した全期間と異なる場合
                         upper.recruit=14200),
                       deterministic=FALSE,rec.resample=NULL
                           ){ 
  if(is.null(rec.arg$rps.years.med)) rec.arg$rps.years.med <- unique(unlist(rec.arg$rps.years))
  
  if(is.null(rec.resample)|deterministic){
    allyears <- as.numeric(dimnames(vpares$naa)[[2]])      
    SSB <- apply(vpares$ssb,2,sum,na.rm=T)      
    rec <- vpares$naa[1,]
    rps <- rec/SSB
	  # define overall RPSmed 
    rps.range.med <- as.numeric(rps[allyears %in% rec.arg$rps.years.med])
    rps.med.all <- median(rps.range.med)

    # define rps ratio for resampling
#    years <- c(rec.arg$rps.years[[1]],rec.arg$rps.years[[2]])
#    rps.range <- as.numeric(rps[allyears %in% years])
#    rps.med <- median(rps.range)
#    rps.mean <- mean(rps.range)  
#    tmp <- rps.range/rps.mean*rps.med.all  
    rps.range <- as.numeric(rps[allyears %in% rec.arg$rps.years[[1]]])
    resample1 <- rps.range/mean(rps.range) * rps.med.all
    rps.range <- as.numeric(rps[allyears %in% rec.arg$rps.years[[2]]])    
    resample2 <- rps.range/mean(rps.range) * rps.med.all
    rec.resample <- list(resample1=resample1,resample2=resample2)
    rps <-  rps.med.all
  }
  if(deterministic){
    rps <-  rps.med.all
  }
  else{
    if(ssb/1000>rec.arg$Blim.rec){
#      rps <- sample(rec.resample[years %in% rec.arg$rps.years[[1]]],1)
      rps <- sample(rec.resample$resample1,1)      
    }
    else{
      #      rps <- sample(rec.resample[years %in% rec.arg$rps.years[[2]]],1)
      rps <- sample(rec.resample$resample2,1)      
    }
  }
  
  ssb.tmp <- min(ssb,rec.arg$upper.ssb)
  rec <- ssb.tmp * rps #sample(rec.resample,1)
  rec2 <- min(rec,rec.arg$upper.recruit)
  return(list(rec=rec2,
              rec.resample=rec.resample))
}

#---- マサバ太平洋用
masaba.rec.old <- function(ssb,vpares,
                       rec.arg=list(rps.years=list(1970:1985,1986:2011),
                         Blim.rec=450,upper.ssb=1380*1000,
                         upper.recruit=14200),
                       deterministic=FALSE,rec.resample=NULL
                           ){ # 平均値と中央値の比率を使うもの){
  if(is.null(rec.resample)|deterministic){
    years <- as.numeric(dimnames(vpares$naa)[[2]])      
    SSB <- apply(vpares$ssb,2,sum,na.rm=T)      
    rec <- vpares$naa[1,]
    rps <- rec/SSB
    rps.range <- as.numeric(rps[years %in% c(rec.arg$rps.years[[1]],rec.arg$rps.years[[2]])])
    rps.med <- median(rps.range)
    rps.mean <- mean(rps.range)  
    tmp <- rps.range/rps.mean*rps.med  
    resample1 <- tmp[years %in% rec.arg$rps.years[[1]]]
    resample2 <- tmp[years %in% rec.arg$rps.years[[2]]]
    rec.resample <- list(resample1=resample1,resample2=resample2)
    rps <-  rps.med
#    cat("1 ")
  }

  if(deterministic){
    rps <-  rps.med
  }
  else{
    if(ssb/1000>rec.arg$Blim.rec){
#      rps <- sample(rec.resample[years %in% rec.arg$rps.years[[1]]],1)
      rps <- sample(rec.resample$resample1,1)      
    }
    else{
      #      rps <- sample(rec.resample[years %in% rec.arg$rps.years[[2]]],1)
      rps <- sample(rec.resample$resample2,1)      
    }
  }
  
#  browser()
  ssb.tmp <- min(ssb,rec.arg$upper.ssb)
  rec <- ssb.tmp * rps #sample(rec.resample,1)
  rec2 <- min(rec,rec.arg$upper.recruit)
  return(list(rec=rec2,
              rec.resample=rec.resample))
}

#-------------- VPA mode 用関数 -------------------
caa.est <- function(naa,saa,waa,M,catch.obs,Pope){
  saa <- saa/max(saa)
  tmpfunc <- function(x,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,out=FALSE,Pope=Pope){
    if(isTRUE(Pope)){
      caa <- naa*(1-exp(-saa*x))*exp(-M/2)
    }
    else{
      caa <- naa*(1-exp(-saa*x-M))*saa*x/(saa*x+M)
    }
    wcaa <- caa*waa
    if(out==FALSE){
      return((sum(wcaa,na.rm=T)-catch.obs)^2)
    }
    else{
      return(caa)
    }
  }
  tmp <- optimize(tmpfunc,c(0,5),catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=FALSE)
  tmp2 <- tmpfunc(x=tmp$minimum,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=TRUE)
  return(list(x=tmp$minimum,caa=tmp2))
}

# とらふぐ用; constant recruitment (no stochastic)
constant.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(rec=1000)){ # 加入量
  return(list(rec=rec.arg$rec,rec.resample=1))
}

# HS用; 
HS.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,gamma=0.01,sd=0.1)){
    if(is.null(rec.arg$gamma)) rec.arg$gamma <- 0.01

    rec <- rec.arg$a*(ssb+sqrt(rec.arg$b^2+(rec.arg$gamma^2)/4)-sqrt((ssb-rec.arg$b)^2+(rec.arg$gamma^2)/4))
    if(!isTRUE(deterministic)){
      rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
    }
  return(list(rec=rec,rec.resample=1))
}


# BH; 
BH.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,sd=0.1)){

    rec <- rec.arg$a*ssb/(1+rec.arg$b*ssb)
    if(!isTRUE(deterministic)){
        rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
    }
  return(list(rec=rec,rec.resample=1))
}

# RI; 
RI.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,sd=0.1)){

    rec <- rec.arg$a*ssb*exp(-rec.arg$b*ssb) # rec.arg$a*ssb/(1+rec.arg$b*ssb)
    if(!isTRUE(deterministic)){
        rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
    }
  return(list(rec=rec,rec.resample=1))
}


#---------------- 結果の確かめ用関数 ---------------------
# --------USAGE-------
# tdata <- get.tdata("vpa_results.csv")
# check.res(res.pms,list(fres,fres),tdata,digits=2,type="%")

get.tdata <- function(tfile){
  tmpdata <- read.csv(tfile,header=F,as.is=F,colClasses="character")
  flags <- which(substr(tmpdata[,1],1,1)=="#")
  tlist <- list()
  for(i in 1:(length(flags)-1)){
      tmp <- tmpdata[(flags[i]+1):(flags[i+1]-1),]
      if(dim(tmp)[[1]]>1){
        dimnames(tmp) <- list(tmp[,1],tmp[1,])
        tmp <- tmp[,!apply(tmp=="",2,all)]
        tlist[[i]] <- sapply((tmp[-1,-1]),as.numeric)
      }
     else{
        tlist[[i]] <- as.numeric(tmp[tmp!=""])
      }
  }
  names(tlist)[1:4] <- c("naa","faa","Biomass","Fc.at.age")
  dimnames(tlist[[3]])[[1]] <- c("SSB","Biomass")
  for(i in 1:tlist[[5]]){
    names(tlist)[(4+(i-1)*4+1):(4+(i*4))] <- c("fnaa","ffaa","fwcaa","ABC")
  }
  return(tlist)
}

#type="TorF" # true or false
#type="diff" # excel-RVPA
#type="%" # (excel-RVPA)/excel
check.res <- function(res,fres,tdata,digits=3,type="%"){
    
  check.twomats <- function(mat1,mat2,digits=3,type="%"){
    if(!is.null(colnames(mat1))){
      tmp1 <- mat1[,colnames(mat1)%in%colnames(mat2)]
      tmp2 <- mat2[,colnames(mat2)%in%colnames(mat1)]
    }
    else{
      tmp1 <- mat1
      tmp2 <- mat2
    }
    if(type=="TorF"){
      tmp <- round(tmp1,digits) == round(tmp2,digits)
    }
    if(type=="diff"){
      tmp <- round(tmp1-tmp2,digits)
    }
    if(type=="%"){
      tmp <- round((tmp1-tmp2)/tmp1*100,digits)
    }
    return(tmp)
  }
  
  naa.res <- check.twomats(tdata$naa,res$naa,digits=digits,type=type)
  faa.res <- check.twomats(tdata$faa,res$faa,digits=digits,type=type)
  fcaa.res <- check.twomats(tdata$Fc.at.age,res$Fc.at.age,digits=digits,type=type)
    
  tmp.list <- list(naa=naa.res,faa=faa.res,Fc.at.age=fcaa.res)
  return(tmp.list)       
}


solv.Feq <- function(cvec,nvec,mvec){

  Fres <- rep(0,length(cvec))
 # cat(nvec," ")
  for(i in 1:length(cvec)){
    F0 <- cvec[i]/nvec[i]
    F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
    if(round(cvec[i],6)<round(nvec[i],6)){
      while(abs(F0-F1)>0.0001 ){
        F0 <- F1
        F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
        if(F0-F1==-Inf) cat("\n",cvec[i]," ",nvec[i]," \n")
      }
      Fres[i] <- F1
    }
    else{
      Fres[i] <- 10
      cat("Warning: catch exceeded tot_num at: ",i," ",
          round(cvec[i],6)," ",round(nvec[i],6),"\n")
      }
  }
  Fres
}

forward.calc.simple.old <- function(fav,nav,Mv,plus.group=TRUE){
  nage <- max(which(!is.na(nav)))#length(fav)
  naa <- rep(NA,nage)
  for(a in 2:(nage-1)){
    naa[a] <- nav[a-1]*exp(-fav[a-1]-Mv[a-1])
  }
  naa[nage] <- nav[nage-1]*exp(-fav[nage-1]-Mv[nage-1]) 
  pg <- nav[nage]*exp(-fav[nage]-Mv[nage])
  if(plus.group) naa[nage] <- naa[nage] + pg

  return(naa)
}

forward.calc.simple <- function(fav,nav,Mv,plus.group=TRUE){
  nage <- max(which(!is.na(nav)))#length(fav)
  naa <- rep(NA,nage)
#  for(a in 2:(nage-1)){
    naa[c(-1,-nage)] <- nav[c(-nage,-(nage-1))]*exp(-fav[c(-nage,-(nage-1))]-Mv[c(-nage,-(nage-1))])
#  }
  naa[nage] <- nav[nage-1]*exp(-fav[nage-1]-Mv[nage-1]) 
  pg <- nav[nage]*exp(-fav[nage]-Mv[nage])
  if(plus.group) naa[nage] <- naa[nage] + pg
  return(naa)
}

forward.calc.mat <- function(fav,nav,Mv,plus.group=TRUE){
  nage <- max(which(!is.na(nav[,1])))#length(fav)
  na.age <- which(is.na(nav[,1]))
#  naa <- matrix(NA,nage,dim(nav)[[2]])
  naa <- matrix(NA,dim(nav)[[1]],dim(nav)[[2]])  
#  for(a in 2:(nage-1)){
    naa[c(-1,-nage,-na.age),] <- nav[c(-nage,-(nage-1),-na.age),]*
        exp(-fav[c(-nage,-(nage-1),-na.age),]-Mv[c(-nage,-(nage-1),-na.age),])
#  }
  naa[nage,] <- nav[nage-1,]*exp(-fav[nage-1,]-Mv[nage-1,]) 
  pg <- nav[nage,]*exp(-fav[nage,]-Mv[nage,])
  if(plus.group) naa[nage,] <- naa[nage,] + pg
  return(naa)
}

calc.rel.abund <- function(sel,Fr,na,M,waa,maa,max.age=Inf,Pope=TRUE){
  rel.abund <- rep(NA, na)
  rel.abund[1] <- 1
  for (i in 2:(na-1)) {
    rel.abund[i] <- rel.abund[i-1]*exp(-M[i-1]-sel[i-1]*Fr)
  }
  rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-(max.age-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))

  if(isTRUE(Pope)){
    ypr1 <- rel.abund*waa[1:na]*(1-exp(-sel[1:na]*Fr))*exp(-M[1:na]/2)
  }
  else{
  # use Baranov catch equation
    ypr1 <- rel.abund*(1-exp(-sel[1:na]*Fr-M[1:na]))*sel[1:na]*Fr/
                                      (sel[1:na]*Fr+M[1:na])*waa[1:na]
#    ypr1 <- numeric()
#    for(i in 1:(na-1)){
#      ypr1[i] <- (rel.abund[i]-rel.abund[i+1])*Fr*sel[i]/(Fr*sel[i]+M[i])
#    }
#    ypr1[na] <- (rel.abund[na])*Fr*sel[i]/(Fr*sel[i]+M[i])
#    ypr1 <- ypr1*waa
  }
  spr <- rel.abund*waa[1:na]*maa[1:na] # waaとmaaにNAが入る対馬マイワシのために、!is.na(waa)を追加
  return(list(rel.abund=rel.abund,ypr=ypr1,spr=spr))
}

out.vpa <- function(res=NULL, # VPA result 
                    rres=NULL, # reference point 
                    fres=NULL, # future projection result (not nessesarily)
                    ABC=NULL,
                    filename="vpa" # filename without extension
                    ){
  old.par <- par()  
  exit.func <- function(){
#    par(old.par)    
    dev.off()
    options(warn=0)      
  }
  on.exit(exit.func())

  csvname <- paste(filename,".csv",sep="")
  pdfname <- paste(filename,".pdf",sep="")
  pdf(pdfname)
  par(mfrow=c(3,2),mar=c(3,3,2,1))  
  options(warn=-1)
  
  write.table2 <- function(x,title.tmp="",is.plot=TRUE,...){
    if(is.plot){
	    if(!is.null(dim(x))){
    	  matplot(colnames(x),t(x),type="b",ylim=c(0,max(x,na.rm=T)),pch=substr(rownames(x),1,1))
	    }
    	else{
	      barplot(x)
    	}
    title(title.tmp)
    }
    if(!is.null(dim(x))){
      tmp <- matrix("",nrow(x)+1,ncol(x)+1)
      tmp[-1,-1] <- as.character(unlist(x))
      tmp[-1,1] <- rownames(x)
      tmp[1,-1] <- colnames(x)
    }
    else{
      tmp <- x
    }
    write.table(tmp,append=T,sep=",",quote=FALSE,file=csvname,col.names=F,row.names=F,...)
  }

  write(paste("# RVPA outputs at ",date()," & ",getwd()),file=csvname)  
  
  if(!is.null(res)){
    write("# VPA results",file=csvname, append=T)
 
    write("\n# catch at age",file=csvname,append=T)    
    write.table2(res$input$dat$caa,title.tmp="Catch at age")

    write("\n# maturity at age",file=csvname,append=T)    
    write.table2(res$input$dat$maa,title.tmp="Maturity at age")

    write("\n# weight at age",file=csvname,append=T)    
    write.table2(res$input$dat$waa,title.tmp="Weight at age")

    write("\n# M at age",file=csvname,append=T)    
    write.table2(res$input$dat$M,title.tmp="M at age")          

    write("\n# fishing mortality at age",file=csvname,append=T)    
    write.table2(res$faa,title.tmp="F at age")

    write("\n# Current F",file=csvname,append=T)    
    write.table2(res$Fc.at.age,title.tmp="Current F")

    write("\n# numbers at age",file=csvname,append=T)    
    write.table2(res$naa,title.tmp="Numbers at age")

    write("\n# total and spawning biomass ",file=csvname,append=T)
    x <- rbind(colSums(res$ssb),colSums(res$baa),colSums(res$input$dat$caa*res$input$dat$waa))
    rownames(x) <- c("Spawning biomass","Total biomass","Catch biomass")
    write.table2(x,title.tmp="Total and spawning biomass")
  }  
  
  if(!is.null(rres)){
    write("\n# Reference points",file=csvname,append=T)
    write.table2(rres$summary,title.tmp="Future F at age",is.plot=F)
  }

  if(!is.null(fres)){
    write("\n# future projection results",file=csvname,append=T)  
    write("\n# future F at age",file=csvname,append=T)
    write.table2(fres$faa[,,1],title.tmp="Future F at age")
    
    write("\n# future numbers at age",file=csvname,append=T)
    write.table2(fres$naa[,,1],title.tmp="Future numbers at age")

    write("\n# future total and spawning biomass",file=csvname,append=T)
    x <- rbind(fres$vssb[,1],fres$vbiom[,1],fres$vwcaa[,1])
    rownames(x) <- c("Spawning biomass","Total biomass","Catch biomass")
    write.table2(x,title.tmp="Future total, spawning and catch biomass")    
  }
  
  if(!is.null(ABC)){
    write("\n# ABC summary",file=csvname,append=T)
    write.table2(ABC$ABC,title.tmp="Future F at age",is.plot=F)
    write("\n# Kobe matrix",file=csvname,append=T)
    for(i in 1:dim(ABC$kobe.matrix)[[3]]){
        write(paste("\n# ",dimnames(ABC$kobe.matrix)[[3]][i]),
              file=csvname,append=T)        
        write.table2(ABC$kobe.matrix[,,i],
                     title.tmp=dimnames(ABC$kobe.matrix)[[3]][i],is.plot=T)        
    }
  }  
}


## MSY for HS with recruitment variation

MSY.EST2 <- function(res,
  SR.rel="HS",
  waa=NULL,
  maa=NULL,
  M=NULL,
  waa.year=NULL, 
  maa.year=NULL,
  M.year=NULL,
  rps.year = NULL, 
  transformation="log",
  g1 = 0.01,
  sel=NULL,
  max.age = Inf,
  sigma=0.5,
  n.gi=30,
  bcor=TRUE,
  min.F=0,
  max.F=2,
  int.F=0.01,
  F0=0.1,
  est=TRUE,
  k = 0,
  statmod=FALSE,
  p.init=NULL){
  #

  argname <- ls()
  arglist <- lapply(argname,function(x) eval(parse(text=x)))
  names(arglist) <- argname
  
  caa <- res$input$dat$caa
  naa <- res$naa
  ssb <- res$ssb
  ny <- ncol(naa)
  years <- dimnames(naa)[[2]]
  ages <- dimnames(caa)[[1]]
  
  if(is.null(waa.year)) waa.year <- rev(years)[1]
  if(is.null(maa.year)) maa.year <- rev(years)[1]
  if(is.null(M.year)) M.year <- rev(years)[1]
  if(is.null(rps.year)) rps.year <- as.numeric(colnames(res$naa))

  if(is.null(waa))  waa <- apply(as.matrix(as.data.frame(res$input$dat$waa)[as.character(waa.year)]),1,mean)
  if(is.null(M))  M <- apply(as.matrix(as.data.frame(res$input$dat$M)[as.character(M.year)]),1,mean)
  if(is.null(maa))  maa <- apply(as.matrix(as.data.frame(res$input$dat$maa)[as.character(maa.year)]),1,mean)
    
  if(is.null(sel)){
    sel <- sweep(res$faa,2,apply(res$faa,2,max),FUN="/")
  }
   
  na <- nrow(caa)

  Pope <- res$input$Pope

  nY <- length(rps.year)
  
    SSB <- as.numeric(colSums(ssb)[1:(nY-k)])
    R <- as.numeric(res$naa[1,][(1+k):nY])
 
   min.SSB <- min(SSB)
   max.SSB <- max(SSB)
   
    SR.func <- function(p) {
      a <- exp(p[1])
      if (SR.rel=="HS"){
        b <- (max.SSB-min.SSB)/(1+exp(-p[2]))+min.SSB
        pred.R <- ifelse(SSB >= b, a*b, a*SSB)
        if (transformation=="id") obj <- sum((R-pred.R)^2)
        if (transformation=="log")  obj <- sum((log(R)-log(pred.R))^2)
      }
      if (SR.rel=="MR"){
        b <- (max.SSB-min.SSB)/(1+exp(-p[2]))+min.SSB
        pred.R <- a*(SSB+sqrt(b^2+g1^2/4)-sqrt((SSB-b)^2+g1^2/4))
        if (transformation=="id") obj <- sum((R-pred.R)^2)
        if (transformation=="log")  obj <- sum((log(R)-log(pred.R))^2)
      }
      if (SR.rel=="BH"){
        b <- exp(p[2])
        pred.R <- a*SSB/(1+b*SSB)
        if (transformation=="id") obj <- sum((R-pred.R)^2)
        if (transformation=="log")  obj <- sum((log(R)-log(pred.R))^2)
      }
      if (SR.rel=="RI"){
        b <- exp(p[2])
        pred.R <- a*SSB*exp(-b*SSB)
        if (transformation=="id") obj <- sum((R-pred.R)^2)
        if (transformation=="log")  obj <- sum((log(R)-log(pred.R))^2)
      }
      obj
    }
    
    if(is.null(p.init)){
      if (SR.rel=="HS" | SR.rel=="MR") p.init <- c(log(median(R/SSB)),logit((median(SSB)-min.SSB)/(max.SSB-min.SSB)))
      if (SR.rel=="BH" | SR.rel=="RI") p.init <- c(log(median(R/SSB)),-log(max.SSB))
    }
        
    out <- nlm(SR.func,p.init)
    p <- out$estimate
    a <- exp(p[1])
    
    if (SR.rel=="HS" | SR.rel=="MR") b <- (max.SSB-min.SSB)/(1+exp(-p[2]))+min.SSB
    if (SR.rel=="BH" | SR.rel=="RI") b <- exp(p[2])
        
    out$sr.par <- c(a,b)
    
    #
   
     if (statmod){
       require(statmod)
       gi <- gauss.quad(n.gi,kind="hermite") 
       x.gi <- gi$nodes
       wex2.gi <- gi$weights*exp(x.gi^2)
    } else{
     x.gi <- c(
     -6.8633453, -6.1382792, -5.5331472, -4.9889190, -4.4830554, -4.0039086,
     -3.5444439, -3.0999705, -2.6671321, -2.2433915, -1.8267411, -1.4155278,
     -1.0083383, -0.6039211, -0.2011286,  0.2011286,  0.6039211,  1.0083383,
      1.4155278,  1.8267411,  2.2433915,  2.6671321,  3.0999705,  3.5444439,
      4.0039086,  4.4830554,  4.9889190,  5.5331472,  6.1382792,  6.8633453)

      wex2.gi <- c(
      0.8342475,0.6490980,0.5694027,0.5225257,0.4910580,0.4683748,0.4513210,
      0.4381770,0.4279181,0.4198950,0.4136794,0.4089816,0.4056051,0.4034198,
      0.4023461,0.4023461,0.4034198,0.4056051,0.4089816,0.4136794,0.4198950,
      0.4279181,0.4381770,0.4513210,0.4683748,0.4910580,0.5225257,0.5694027,
      0.6490980,0.8342475) 
    }
    
    Fr <- seq(min.F,max.F,by=int.F)
      
    if (SR.rel=="HS" | SR.rel=="MR"){
      R0 <- a*b
               
      YPR.f0 <- function(Fr,x,sel){
        N.HS <- R0*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
        SSB.HS <- sum(N.HS*waa*maa)*exp(sigma*x-bcor*sigma^2/2)
        ifelse(SSB.HS < b,0,R0*exp(sigma*x-bcor*sigma^2/2)*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$ypr))
      }
      
      YPR.f <- function(Fr,x) mean(apply(sel,2,YPR.f0,Fr=Fr,x=x))
      
      if (est){
        YPR <- sapply(Fr, function(Fr) sum(YPR.f(Fr,x.gi)*dnorm(x.gi)*wex2.gi))
       
        Fmsy <- Fr[which.max(YPR)]
        MSY <- max(YPR,na.rm=TRUE)
      } else{
        YPR <- sum(YPR.f(F0,x.gi)*dnorm(x.gi)*wex2.gi)
        Fmsy <- F0
        MSY <- YPR      
      }
      
     
      Abund0 <- function(Fr,sel)  R0*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
      Abund <- function(Fr)  rowMeans(apply(sel,2,Abund0,Fr=Fr))

      Nmsy <-  Abund(Fmsy)
      Bmsy <- Nmsy*waa
      SSBmsy <- Bmsy*maa
      
      N0 <- Abund(0)
      B0 <- N0*waa
      SSB0 <- B0*maa 
    }    
    
    if (SR.rel=="BH"){    
       YPR.f0 <- function(Fr,x,sel){
        R <- (a*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa)-1)/(b*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))
        R <- ifelse(R >=0, R, 0)
        N.BH <- R*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
        SSB.BH <- sum(N.BH*waa*maa)*exp(sigma*x-bcor*sigma^2/2)
        a*SSB.BH/(1+b*SSB.BH)*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$ypr)
      }
            
      YPR.f <- function(Fr,x) mean(apply(sel,2,YPR.f0,Fr=Fr,x=x))

      YPR <- sapply(Fr, function(Fr) sum(YPR.f(Fr,x.gi)*dnorm(x.gi)*wex2.gi))
      
      Fmsy <- Fr[which.max(YPR)]
      MSY <- max(YPR,na.rm=TRUE)
      
      Abund0 <- function(Fr,sel)  {
        R <- (a*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa)-1)/(b*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))
        R*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
      }
      Abund <- function(Fr)  rowMeans(apply(sel,2,Abund0,Fr=Fr))

      Nmsy <-  Abund(Fmsy)
      Bmsy <- Nmsy*waa
      SSBmsy <- Bmsy*maa
      
      N0 <- Abund(0)
      B0 <- N0*waa
      SSB0 <- B0*maa 
    }       

    if (SR.rel=="RI"){    
       YPR.f0 <- function(Fr,x,sel){
        R <- log(a*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))/(b*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))
        R <- ifelse(R >=0, R, 0)
        N.RI <- R*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
        SSB.RI <- sum(N.RI*waa*maa)*exp(sigma*x-bcor*sigma^2/2)
        a*SSB.RI*exp(-b*SSB.RI)*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$ypr)
      }
            
      YPR.f <- function(Fr,x) mean(apply(sel,2,YPR.f0,Fr=Fr,x=x))

      YPR <- sapply(Fr, function(Fr) sum(YPR.f(Fr,x.gi)*dnorm(x.gi)*wex2.gi))
      
      Fmsy <- Fr[which.max(YPR)]
      MSY <- max(YPR,na.rm=TRUE)
      
      Abund0 <- function(Fr,sel)  {
        R <- log(a*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))/(b*sum(calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund*waa*maa))
        R*calc.rel.abund(t(sel),Fr,na,M,waa,maa,max.age=max.age,Pope=Pope)$rel.abund
      }
      Abund <- function(Fr)  rowMeans(apply(sel,2,Abund0,Fr=Fr))

      Nmsy <-  Abund(Fmsy)
      Bmsy <- Nmsy*waa
      SSBmsy <- Bmsy*maa
      
      N0 <- Abund(0)
      B0 <- N0*waa
      SSB0 <- B0*maa 
    }       

   out$sigma <- sigma
   out$bcor <- bcor
   
   msy <- c(MSY, Fmsy)
   names(msy) <- c("MSY","Fmsy")
   
   out$rps.year <- rps.year
   out$nY <- nY
   
   out$MSY <- msy
   out$Nmsy <- Nmsy
   out$Bmsy <- Bmsy
   out$SSBmsy <- SSBmsy

  out$N0 <- N0
  out$B0 <- B0
  out$SSB0 <- SSB0
  
  return(out)
}

logit <- function(x) log(x/(1-x))


get.kobematrix <- function(fres,Blim=0,Bban=0,ssb=TRUE){
    if(isTRUE(ssb))  tmp <- fres$vssb[,-1]
    else  tmp <- fres$vbiom[,-1]
    
    res <- data.frame(
        # 漁獲量
        catch=fres$vwcaa[,1],
        # 資源量
        biom=fres$vbiom[,1],
        # 親魚量
        ssb=fres$vssb[,1],
        # Blim回復確率
        uptoBlim=apply(tmp>Blim,1,mean)*100,
        # Bban以上確率
        uptoBban=apply(tmp>Bban,1,mean)*100)

    return(res)
}
