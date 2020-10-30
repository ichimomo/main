#---------------- rfuture & Fref
# 2012. 7. 28.; calc.rel.abund導入。SPR、YPRを計算する関数。
# 2012. 8. 1; 加入の関数を大幅改善
# 0.3f; Frec1, Frec2, Frecの計算オプションをfuture.vpaに追加
# 0.4: スケトウ対応
# 0.5: 2013.1.18 全種対応のためのバージョン
# 0.5c: 対馬のサバに対応するため、将来予測の部分にtimestepを導入
#       資源量の単位はトン、尾数の単位は百万尾
# 2014.7.4; 黒田さん指摘により，Popeの引数を追加
# 2016.6.12; vpaのほうとファイルを分ける→future1.0

## MSY計算関数(SRest)を追加
## 2017.08.10 SR.estにssbとRだけを単純に与えて再生産関係をフィットする機能を追加。引数SSB.datとR.datを与えると、他のデータは無視して,与えたデータにフィットさせる
## 2017.08.28 将来予測関数(future.vpa)にBbanオプションを追加
## 2017.11.24 ref.Fにmin.ageを追加
## 2017.11.24 ref.F, future.vpaにwaa.catch引数（漁獲量計算時のwaa。通常のwaa引数は資源量・親魚資源量計算時に使われる）。また、vpaへの引数にcaa.catchを特別に与えている場合には、将来予測や管理基準値、MSYの漁獲量の計算時にvpaに与えたcaa.catchを使うように変更

## 2017.12.25 Frecの引数に "scenario"と"Frange"オプションを追加
##   scenario="catch.mean"とすると、将来のstochastic simulationにおける平均漁獲量Blimitで指定した値と一致するようになる
##   Frange=c(0.01,2) などと指定する。探索するFの範囲。特に、scenario="catch.mean"を使う場合、解が２つ出てくるので、
##   Frange=c(0.01,Fmsy), c(Fmsy,2) のように、２種類のFの範囲で探索することになる



#---------------- 管理基準値計算のための関数 ------------------------
# ref.F
ref.F <- function(
  res, # VPAの結果のオブジェクト
  sel=NULL, # 仮定する選択率．NULLの場合，res$Fc.at.ageが使われる
  waa=NULL, # 仮定する生物パラメータ．直接の値を入れるか，年を指定するやり方のどちらでも動く。直接指定するほうが優先。
  maa=NULL,
  M=NULL,
  waa.catch=NULL,
  M.year=NULL, 
  waa.year=NULL, # 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
  maa.year=NULL,
  rps.year = NULL, # Fmedの計算に使うRPSの年の範囲．NULLの場合，全範囲が用いられる
  max.age = Inf, # 加入年齢を０歳としたときに、SPR計算で考慮される最大の年齢（年齢の数ではないことに注意）。加入年齢が１歳以上のときは、SPR計算で考慮したい年齢-加入年齢を入力する、またはmin.ageの引数に加入年齢を設定する。
  min.age = 0, # 加入年齢が0歳でないときに指定できる
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

  if(is.null(waa.catch)){
      if(is.null(res$input$dat$waa.catch)){
          waa.catch <- waa
      }
      else{
          waa.catch <- apply(as.matrix(as.data.frame(res$input$dat$waa.catch)[as.character(waa.year)]),1,mean)
          }
  }

  ssb.coef <- ifelse(is.null(res$ssb.coef),0,res$ssb.coef)
    
  min.age <- min(as.numeric(rownames(res$naa)))
  if(min.age==0) slide.tmp <- TRUE else slide.tmp <- -1:-min.age
  rps.data <- data.frame(year=as.numeric(names(colSums(ssb,na.rm=T))),
                         ssb=as.numeric(colSums(ssb,na.rm=T)),
                         recruit=as.numeric(c(naa[1,slide.tmp],rep(NA,min.age))))
  if (sum(is.na(rps.data$year))>0) rps.data <- rps.data[-which(is.na(rps.data$year)),]
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

    tmp <- calc.rel.abund(sel,Fr,na,M,waa,waa.catch,maa,min.age=min.age,max.age=max.age,Pope=Pope,ssb.coef=ssb.coef)
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
    tmp <- calc.rel.abund(sel,Fr,na,M,waa,waa.catch,maa,max.age=max.age,Pope=Pope,ssb.coef=ssb.coef)
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

    tmp <- calc.rel.abund(sel,Fr,na,M,waa,waa.catch,maa,max.age=max.age,Pope=Pope,ssb.coef=ssb.coef)
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

  Res <- list(sel=sel, min.age=min.age, max.age=max.age, rps.q=rps.q, spr.q=spr.q, Fcurrent=Fcurrent, Fmed=Fmed, Flow=Flow, Fhigh=Fhigh, Fmax=Fmax, F0.1=F0.1, Fmean=Fmean,rps.data=rps.data)
  
  if (!is.null(pSPR)){
    FpSPR <- rbind(FpSPR, sapply(FpSPR, f.mean))
    rownames(FpSPR) <- c("max","mean")
    Res$FpSPR <- FpSPR
  }

  #-----  YPR & SPR figure -----

  spr0 <- sum(calc.rel.abund(sel,0,na,M,waa,waa.catch,maa,min.age=min.age,max.age=max.age,Pope=Pope,ssb.coef=ssb.coef)$spr)  
  tmp <- lapply(F.range, function(x) calc.rel.abund(sel,x,na,M,waa,waa.catch,maa,min.age=min.age,max.age=max.age,Pope=Pope,ssb.coef=ssb.coef))
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
  Res$waa.catch <- waa.catch  
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

calc.rel.abund <- function(sel,Fr,na,M,waa,waa.catch=NULL,maa,min.age=0,max.age=Inf,Pope=TRUE,ssb.coef=0){
    if(is.null(waa.catch)) waa.catch <- waa
    rel.abund <- rep(NA, na)
    rel.abund[1] <- 1
    for (i in 2:(na-1)) {
        rel.abund[i] <- rel.abund[i-1]*exp(-M[i-1]-sel[i-1]*Fr)
    }
    rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-((max.age-min.age)-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))

    if(isTRUE(Pope)){
        ypr1 <- rel.abund*waa.catch[1:na]*(1-exp(-sel[1:na]*Fr))*exp(-M[1:na]/2)
    }
    else{
                                        # use Baranov catch equation
        ypr1 <- rel.abund*(1-exp(-sel[1:na]*Fr-M[1:na]))*sel[1:na]*Fr/
            (sel[1:na]*Fr+M[1:na])*waa.catch[1:na]
    }
    spr <- rel.abund*waa[1:na]*maa[1:na]*exp(-ssb.coef*(sel[1:na]*Fr+M[1:na])) 
  return(list(rel.abund=rel.abund,ypr=ypr1,spr=spr))
}



##----------------------- 将来予測関数 ----------------------------
## multiのオプションは管理後のFのmultiplier（管理前後でselectivityが同じ）

future.vpa <-
    function(res0,
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
             seed=NULL,
             strategy="F", # F: 漁獲係数一定, E: 漁獲割合一定、C: 漁獲量一定（pre.catchで漁獲量を指定）   
             plus.group=res0$input$plus.group,
             N=1000,# 確率的なシミュレーションをする場合の繰り返し回数。
                   # N+1の結果が返され、1列目に決定論的な結果が                       
                   # 0を与えると決定論的な結果のみを出力
             silent=FALSE, is.plot=TRUE, # 計算条件を出力、プロットするか
             random.select=NULL, # 選択率をランダムリサンプリングする場合、ランダムリサンプリングする年を入れる
                                 # strategy="C"または"E"のときのみ有効
             pre.catch=NULL, # list(year=2012,wcatch=13000), 漁獲重量をgivenで与える場合
             # list(year=2012:2017,E=rep(0.5,6)), 漁獲割合をgivenで与える場合                       
             ##-------- 加入に関する設定 -----------------
             rec.new=NULL, # 指定した年の加入量
                           # 年を指定しないで与える場合は、自動的にスタート年の加入になる。
                           # list(year=, rec=)で与える場合は、対応する年の加入を置き換える。
             ##--- 加入関数
             recfunc=RPS.simple.rec, # 太平洋マサバ、ゴマサバ以外はRPS.simple.recを使う
             rec.arg=list(upper.ssb=Inf,upper.recruit=Inf), # 加入の各種設定

             ##--- Frecオプション；Frec計算のための設定リストを与えると、指定された設定でのFrecに対応するFで将来予測を行う
             Frec=NULL,
                 # list(stochastic=TRUE, # TRUEの場合、stochastic simulationで50%の確率でBlimitを越す(PMS, TMI)
                                         # FALSEの場合、RPS固定のprojectionがBilmitと一致する(NSK)
                                        #      future.year=2018, # 何年の資源量を見るか？
                                        #      Blimit=450*1000,  # Blimit (xトン)
                                        #      scenario="catch.mean" or "blimit" (デフォルトはblimit; "catch.mean"とするとstochastic simulationにおける平均漁獲量がBlimitで指定した値と一致するようになる)
                                        #      Frange=c(0.01,2*mult) # Fの探索範囲
                                        #      method="optimize") # 二分法かoptimizeか。パラメータの探索範囲は0.001～multi*2となる。何も指定しない場合はoptimizeが使われる。   
                                        # 対馬サバに対応するオプション。ts=2のとき、1年を2つの季節に分けて将来予測する
             ts=1, # 時間ステップ。1年間の季節の数。普通は１。対馬サバの場合2。ts=1の場合、以下の引数はすべて無視される。
                                        #---- 以下、ts>2のときに必要な引数 -----
             ## Bbanオプション。こちらも設定はlistで与える
             ##  設定する場合はBban=list(Bban1=禁漁になるときのSSB, Bban2=禁漁から回復するときのSSB, time.lag1=何年後から漁獲をゼロとするか.time.lag2=SSB>Bbanとなって何年後から漁獲をはじめるか(1年後の場合は1, 2年後の場合は2など))
             Bban=NULL,
             ## 
             waa=NULL,waa.catch=NULL,maa=NULL,M=NULL, # 季節毎の生物パラメータ、または、生物パラメータを外から与える場合
             rec.season=1, # 加入がおこる季節
             waa.multi="opt", # waa.optyearに対応する年について、暦年漁獲量と一致するようにwaaを最適化するか？ "opt"の場合、内部で最適化。waa.optyearの長さ分のベクトルを与えて指定することもできる（ts>1のときのオプション）
             waa.optyear=2011:2013, # waa.optyearをするときに、置き換えるwaaの年
             replace.rec.year=2012, # 加入量を暦年の将来予測での加入量に置き換えるか？
                                        #                       new.ssb="hokke",
             partial.F=NULL,         # 季節毎のpartial F
             F.sigma=0
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
  if(!is.null(Bban)) Bban$is.Bban <- rep(FALSE,N)        
  arglist$ABC.year <- ABC.year
  #-------------
  
#  fyears <- seq(from=start.year,to=start.year+nyear-1,by=1/ts)
  fyears <- seq(from=start.year,to=start.year+nyear,by=1/ts)
        
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

  if(!silent)  cat("F multiplier= ", multi,"seed=",seed,"\n")

    # シードの設定
    if(is.null(seed)) arglist$seed <- as.numeric(Sys.time())
    
  #------------Frecオプションの場合 -------------
  if(!is.null(Frec)){
      multi.org <- multi
      if(is.null(Frec$stochastic)) Frec$stochastice <- TRUE
      if(is.null(Frec$method)) Frec$method <- "optimize"
      if(is.null(Frec$target.probs)) Frec$target.probs <- 50

      if(is.null(Frec$target)) Frec$scenario <- "blimit" # 2017/12/25追記 
      if(is.null(Frec$Frange)) Frec$Frange <- c(0.01,multi.org*2)   # 2017/12/25追記(探索するFの範囲の指定)      
    
    getFrec <- function(x,arglist){
      set.seed(arglist$seed)
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
        if(Frec$scenario=="blimit"){          
            is.lower.ssb <- fres.tmp$vssb<Frec$Blimit
            probs <- (sum(is.lower.ssb[tmp,-1],na.rm=T)-1)/
                (length(is.lower.ssb[tmp,-1])-1)*100
            return.obj <- probs-Frec$target.probs
        }
        # stochastic projectionにおける平均漁獲量を目的の値に一致させる 
        if(Frec$scenario=="catch.mean"){
            return.obj <- (log(Frec$Blimit)-log(mean(fres.tmp$vwcaa[tmp,-1])))^2
        }
        # stochastic projectionにおける平均親魚資源量を目的の値に一致させる 
        if(Frec$scenario=="ssb.mean"){
            return.obj <- (log(Frec$Blimit)-log(mean(fres.tmp$vssb[tmp,-1])))^2
        }                
      }
      else{
        return.obj <- Frec$Blimit-fres.tmp$vssb[tmp,1]
      }
      return(ifelse(Frec$method=="nibun",return.obj,return.obj^2))        
    }

    if(Frec$method=="nibun"){
      # 二分法
      eps <- ifelse(Frec$stochastic==TRUE,0.01,0.001)
#      x.high <- multi.org*2 ; x.low <- 0.01;  fx <- Inf
        x.high <- Frec$Frange[2] ; x.low <- Frec$Frange[1];
        fx <- Inf        
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
#      res <- optimize(getFrec,interval=c(0.01,multi.org*2),arglist=arglist)
      res <- optimize(getFrec,interval=Frec$Frange,arglist=arglist)        
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
    waa.catch.org <- waa.catch
    maa.org <- maa
    M.org <- M
  }

  if(strategy=="C"|strategy=="E") multi.catch <- multi else multi.catch <- 1
  
  faa <- naa <- waa <- waa.catch <- maa <- M <- caa <- 
          array(NA,dim=c(length(ages),ntime,N),dimnames=list(ages,fyears,1:N))
  # future biological patameter
  if(!is.null(M.org))  M[] <- M.org  else M[] <- apply(as.matrix(res0$input$dat$M[,years %in% M.year]),1,mean)
  if(!is.null(waa.org))  waa[] <- waa.org  else waa[] <- apply(as.matrix(res0$input$dat$waa[,years %in% waa.year]),1,mean)
  if(!is.null(maa.org))  maa[] <- maa.org  else maa[] <- apply(as.matrix(res0$input$dat$maa[,years %in% maa.year]),1,mean)
  if(!is.null(waa.catch.org))  waa.catch[] <- waa.catch.org
  else{
      if(!is.null(res0$input$dat$waa.catch)) waa.catch[] <- apply(as.matrix(res0$input$dat$waa.catch[,years %in% waa.year]),1,mean)
      else waa.catch <- waa
  }

  # time step definition (change F and M)
  M <- M/ts
  if(ts>1){
    currentF <- as.numeric(sweep(matrix(partial.F,ts,nage/ts),2,currentF,FUN="*"))
  }

  # future F matrix
  faa[] <- currentF*multi*exp(rnorm(length(faa),0,F.sigma))
  # ABCyear以前はcurrent Fを使う。
  faa[,fyears<min(ABC.year),] <- currentF*exp(rnorm(length(faa[,fyears<min(ABC.year),]),0,F.sigma))

  ## VPA期間と将来予測期間が被っている場合、VPA期間のFはVPAの結果を使う
  if(length(tmp <- which(fyear.year %in% years))>0){  
      tmp0 <- which(years %in% fyear.year)
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
              if(!is.null(res0$input$dat$waa.catch)){
                  waa.catch[,tmp[j],] <- res0$input$dat$waa.catch[,tmp0[jj]]
              }
              else{
                  waa.catch[,tmp[j],] <- res0$input$dat$waa[,tmp0[jj]]
                  }
            }
          }
        }}}

  ############### ts>1用の設定(ここから) ###########        
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
        arglist.tmp$rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])
        a.tmp <- do.call(future.vpa,arglist.tmp)
        pre.catch <- tapply(a.tmp$vwcaa[,1],a.tmp$fyear.year,sum)
        
        xx <- sum((pre.catch[names(pre.catch)%in%waa.optyear]-opt.catch)^2)
        return(xx)
      }
     
      est <- optim(rep(1,length(waa.optyear)),optfunc,
                   arglist=arglist,a=a,waa.optyear=waa.optyear,replace.rec.year=replace.rec.year)
      waa.multi <- est$par
      cat(waa.multi,"\n")
      rec.new <- list(year=replace.rec.year,rec=a$naa[1,a$year==replace.rec.year,1])      
    }
    for(kk in 1:length(waa.optyear)){
      waa[,fyear.year==waa.optyear[kk],] <- waa[,fyear.year==waa.optyear[kk],] * waa.multi[kk]
    }
  } ############### ts>1用の設定ここまで ###########
        
  tmp <- aperm(faa,c(2,1,3))
  tmp <- tmp*multi.year
  faa <- aperm(tmp,c(2,1,3))

  #  vpa.multi <- ifelse(is.null(vpa.mode),1,vpa.mode$multi)
  # rps assumption
  rps.mat <- array(NA,dim=c(ntime,N),dimnames=list(fyears,1:N))
#  rps.mat[] <- sample(rps.range2,nyear*N,replace=TRUE)  # 平均を揃えたもの
#  rps.mat[,1] <- rps.med
  rec.tmp <- list(rec.resample=NULL,tmparg=NULL)

  #if(!is.null(Frec$seed)) set.seed(Frec$seed)
  set.seed(arglist$seed)        
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
              if(is.null(res0$ssb.coef)) res0$ssb.coef <- 0
              ly <- length(years)
              {if(res0$ssb.coef >= 0)  thisyear.ssb <- sum(naa[,1,k]*waa[,1,k]*maa[,1,k]*exp(-res0$ssb.coef*(M[,1,k]+faa[,1,k])),na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
              else thisyear.ssb <- sum(res0$naa[,ly]*res0$input$dat$waa[,ly]*res0$input$dat$maa[,ly]*exp(-res0$ssb.coef*(res0$input$dat$M[,ly]+res0$faa[,ly])),na.rm=T)*res0$input$unit.waa/res0$input$unit.biom
              }
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

          ## Bbanの判断
          if(!is.null(Bban)){
              if(Bban$is.Bban[k]==FALSE){
                  time.lag <- Bban$time.lag1+1
              }
              else{
                  time.lag <- Bban$time.lag2+1                  
              }
              if(fyears[i+1]-time.lag < start.year){
                  ssb.tmp <- sum(res0$ssb[,as.character(fyears[i+1]-time.lag)],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom            
              }
              else{
                  ssb.tmp <- sum(naa[,i+1-time.lag,k]*waa[,i+1-time.lag,k]*maa[,i+1-time.lag,k],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom            
              }
              if(Bban$is.Bban[k]==FALSE){              
                  if(ssb.tmp<Bban$Bban1){
                      faa[,i:dim(faa)[[2]],k] <- 0
                      Bban$is.Bban[k] <- TRUE
                  }
              }
              else{
                  if(ssb.tmp>Bban$Bban2){
                      faa[,i:dim(faa)[[2]],k] <- currentF*multi
                      Bban$is.Bban[k] <- FALSE
                  }
              }
          }

          ## 最終年の漁獲量を入れる
          if(Pope){
              caa[,i,k] <- naa[,i,k]*(1-exp(-faa[,i,k]))*exp(-M[,i,k]/2)
          }
          else{
              caa[,i,k] <- naa[,i,k]*(1-exp(-faa[,i,k]-M[,i,k]))*faa[,i,k]/(faa[,i,k]+M[,i,k])
          }
        
      #漁獲量がgivenの場合
          if(!is.null(pre.catch) && fyears[i]%in%pre.catch$year){
              if(!is.null(pre.catch$wcatch)){
                  if(fyears[i]<ABC.year){
                      tmpcatch <- as.numeric(pre.catch$wcatch[pre.catch$year==fyears[i]]) 
                  }
                  else{
                      tmpcatch <- as.numeric(pre.catch$wcatch[pre.catch$year==fyears[i]]) * multi.catch                  
                  }
              }
              if(!is.null(pre.catch$E)){
                  biom <- sum(naa[,i,k]*waa[,i,k]*res0$input$unit.waa/res0$input$unit.biom)
                  if(fyears[i]<ABC.year){
                      tmpcatch <- as.numeric(pre.catch$E[pre.catch$year==fyears[i]])  * biom
                  }
                  else{
                      tmpcatch <- as.numeric(pre.catch$E[pre.catch$year==fyears[i]]) * biom * multi.catch                  
                  }
              }

              # 選択率をランダムサンプリングする場合
              if(!is.null(random.select)) saa.tmp <- as.numeric(res0$saa[,colnames(res0$saa)==sample(random.select,1)])
              else saa.tmp <- faa[,i,k]/max(faa[,i,k])
                  
              tmp <- caa.est(naa[,i,k],saa.tmp,
                         waa.catch[,i,k],M[,i,k],tmpcatch,Pope=Pope)
              faa.new <- tmp$x * faa[,i,k]/max(faa[,i,k])
              caa[,i,k] <- tmp$caa
              faa[,i,k] <- faa.new
      }


      ## 漁獲して１年分前進（加入はまだいれていない）
      tmp <- forward.calc.simple(faa[,i,k],naa[,i,k],M[,i,k],plus.group=plus.group)
      naa[is.na(naa[,i+1,k]),i+1,k ] <- tmp[is.na(naa[,i+1,k])]      
     
      ## 当年の加入の計算
      if(fyears[i+1]-min.age < start.year){
        thisyear.ssb <- sum(res0$ssb[,as.character(fyears[i+1]-min.age)],na.rm=T)*res0$input$unit.waa/res0$input$unit.biom            
      }
      else{
        if(ts==1){
            if(is.null(res0$ssb.coef)) res0$ssb.coef <- 0
            thisyear.ssb <- sum(naa[,i+1-min.age,k]*waa[,i+1-min.age,k]*maa[,i+1-min.age,k]*exp(-res0$ssb.coef*(M[,i+1-min.age,k]+faa[,i+1-min.age,k])),na.rm=T)*res0$input$unit.waa/res0$input$unit.biom            
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
#      if(k>2) browser()
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

        caa <- caa[,-ntime,,drop=F]
        waa.catch <- waa.catch[,-ntime,,drop=F]
        waa <- waa[,-ntime,,drop=F]
        maa <- maa[,-ntime,,drop=F]                
        naa <- naa[,-ntime,,drop=F]
        faa <- faa[,-ntime,,drop=F]
        M <- M[,-ntime,,drop=F]
        fyears <- fyears[-ntime]
        
        biom <- naa*waa*res0$input$unit.waa/res0$input$unit.biom
        ssb <- naa*waa*maa*exp(-res0$ssb.coef*(M+faa))*res0$input$unit.waa/res0$input$unit.biom

        ## SSB計算をするタイミングをずらすオプション。岡村さん書き換え
        if(res0$input$ssb.lag==1){
            ssb[,2:dim(ssb)[2],] <- ssb[,1:(dim(ssb)[2]-1),]
        }
        if(res0$ssb.coef!=0){    
            ssb[,1,] <- res0$naa[,length(years)]*
                exp(-res0$ssb.coef*res0$faa[,length(years)]-res0$ssb.coef*res0$input$dat$M[,length(years)])*
                res0$input$dat$waa[,length(years)]*res0$input$dat$maa[,length(years)]*
                res0$input$unit.waa/res0$input$unit.biom
        }

        wcaa <- caa*waa.catch*res0$input$unit.waa/res0$input$unit.biom
        vwcaa <- apply(wcaa,c(2,3),sum,na.rm=T)

        ABC <- apply(as.matrix(vwcaa[fyears%in%ABC.year,,drop=F]),2,sum)
  
        fres <- list(faa=faa,naa=naa,biom=biom,ssb=ssb,wcaa=wcaa,caa=caa,M=M,rps=rps.mat,
                     maa=maa,vbiom=apply(biom,c(2,3),sum,na.rm=T),
                     waa=waa,waa.catch=waa.catch,currentF=currentF,
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


plot.future <- function(fres0,ylim.tmp=NULL,xlim.tmp=NULL,vpares=NULL,what=c(TRUE,TRUE,TRUE),conf=0.1,
                        label=c("Biomass","SSB","Catch"),is.legend=TRUE,add=FALSE,col=NULL){
  if(is.null(col)) col <- 1                        
  matplot2 <- function(x,add=FALSE,...){
    if(add==FALSE) matplot(rownames(x),x,type="l",lty=c(2,1,2),col=col,xlab="Year",...)
    if(add==TRUE) matpoints(rownames(x),x,type="l",lty=c(2,1,2),col=col,xlab="Year",...)    
  }

  if(is.null(xlim.tmp)) xlim.tmp <- range(as.numeric(colnames(fres0$naa)))

  if(what[1]){
    matplot2(x <- t(apply(fres0$vbiom,1,quantile,probs=c(conf,0.5,1-conf))),
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
    matplot2(x <- t(apply(fres0$vssb,1,quantile,probs=c(conf,0.5,1-conf))),
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
    matplot2(x <- t(apply(fres0$vwcaa,1,quantile,probs=c(conf,0.5,1-conf))),
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
    legend("topleft",lty=c(1,1,1,2),legend=c("Deterministic","Mean","Median",paste(100-(conf*2)*100,"%conf")),pch=c(3,1,NA,NA))
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
                   Blim=1000,Bban=0,                   
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

    # Frecで使われたシードはとっておかないといけない=> seedはFrecの引数の外に出すこと！
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
      input.tmp$waa <- input.tmp$maa <- input.tmp$M <- input.tmp$waa.catch <- NULL
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


constant.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(rec=1000)){ # 加入量
  ## とらふぐ用; constant recruitment (no stochastic)        
  return(list(rec=rec.arg$rec,rec.resample=1))
}

# HS用; 
HS.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,gamma=0.01,sd=0.1, # Mesnil関数のparameter
                                        resample=FALSE,resid=0, # 残差リサンプリングする場合、resample=TRUEにして、residにリサンプリングする残差（対数）を入れる
                                        bias.correction=TRUE)){
    if(is.null(rec.arg$gamma)) rec.arg$gamma <- 0.01
    if(is.null(rec.arg$bias.correction)) rec.arg$bias.correction <- TRUE
    if(is.null(rec.arg$resample)) rec.arg$resample <- FALSE

    rec <- rec.arg$a*(ssb+sqrt(rec.arg$b^2+(rec.arg$gamma^2)/4)-sqrt((ssb-rec.arg$b)^2+(rec.arg$gamma^2)/4))
    if(!isTRUE(deterministic)){
        if(!isTRUE(rec.arg$resample)){
            if(isTRUE(rec.arg$bias.correction)){
                rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
            }
            else{
                rec <- rec*exp(rnorm(length(ssb),0,rec.arg$sd))
            }
        }
        else{
            if(isTRUE(rec.arg$bias.correction)){
                rec <- exp(log(rec)+sample(rec.arg$resid,1))/mean(exp(rec.arg$resid))
            }
            else{
                rec <- exp(log(rec)+sample(rec.arg$resid,1))
            }
        }
    }
  return(list(rec=rec,rec.resample=1))
}

# HS (two line)
HS.rec2 <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=0.2,b=1000,sd=0.1,
                                        resample=TRUE,
                                        resid=0, # 残差の過去の時系列
                                        hssb=0, # SSBの過去の時系列
                                        threshold=c(0, 50000,10000,Inf), # 指定したSSBのしきい値でリサンプリングする残差を変える
                                        nblock=0)){ # block resamplingするか。上のオプションと同時にはできない

    if(is.null(rec.arg)) nblock <- 0
    if(is.null(rec.resample)) rec.resample <- list(count=1)
    if(ssb>rec.arg$b) rec <- rec.arg$a*rec.arg$b
    else rec <- rec.arg$a*ssb
    
    if(!isTRUE(deterministic)){
        if(isTRUE(rec.arg$resample)){
            if(is.null(rec.arg$threshold)){
                if(rec.arg$nblock==0){
                                        # normal resampling
                    rec <- exp(log(rec)+sample(rec.arg$resid,1))
                }
                else{ # block resampling
                    rec.resample$sample.year <- ifelse(rec.resample$count==1,
                                                       sample(1:length(rec.arg$resid),1),
                                                       rec.resample$sample.year+1)
                    if(rec.resample$sample.year>length(rec.arg$resid)){
                        rec.resample$sample.year <- rec.resample$sample.year-length(rec.arg$resid)
                    }
                    rec.resample$count <- ifelse(rec.resample$count==rec.arg$nblock,1,rec.resample$count + 1)
                    rec <- exp(log(rec)+rec.arg$resid[rec.resample$sample.year])
                }
            }
            else{   # thresholdに値が入っている場合
                rset <- tapply(rec.arg$resid,as.numeric(cut(rec.arg$hssb,breaks=rec.arg$threshold)),
                                        function(x) x)
                rec <- exp(log(rec)+sample(rset[[as.numeric(cut(ssb,breaks=rec.arg$threshold))]],1))
            }
        }
        else{
            # ラプラス分布
            require(VGAM)
            phai <- sqrt(var(rec.arg$resid)/2)
       #     if(isTRUE(rec.arg$raplace)){
                rec <- rec * exp(rlaplace(1,0,phai)) #rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))

        #    }
        #   else{
        #       rec <- rec*exp(rnorm(length(ssb),-0.5*(phai*2)^2,phai*2)) # 対数正規分布
        #     }
        }
    }
  return(list(rec=rec,rec.resample=rec.resample))
}

# HS; ラプラスーガウス
HS.rec3 <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=0.2,b=1000,sd=0.1,
                                        resid=0, # 残差の過去の時系列
                                        hssb=0, # SSBの過去の時系列
                                        LGRB=lm(rec.arg$resid~log(rec.arg$hssb)))
                                        ){
    HS <- function(x,a,b)  ifelse(x>b,a*b,a*x)

    rec <- exp(log(HS(ssb,rec.arg$a,rec.arg$b)) + predict(lres,data.frame(SSB=ssb)))
    
    if(!isTRUE(deterministic)){
        rec <- rec * exp(sample(lres$resid,1))
    }
  return(list(rec=rec,rec.resample=rec.resample))
}

# BH; 
BH.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,sd=0.1,bias.correction=TRUE)){
    if(is.null(rec.arg$bias.correction)) rec.arg$bias.correction <- TRUE
    rec <- rec.arg$a*ssb/(1+rec.arg$b*ssb)
    if(!isTRUE(deterministic)){
        if(isTRUE(rec.arg$bias.correction)){
            rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
            }
        else{
            rec <- rec*exp(rnorm(length(ssb),0,rec.arg$sd))                
            }
    }
  return(list(rec=rec,rec.resample=1))
}

# RI; 
RI.rec <- function(ssb,vpares,deterministic=FALSE,rec.resample=NULL,
                           rec.arg=list(a=1000,b=1000,sd=0.1,bias.correction=TRUE)){                   
    if(is.null(rec.arg$bias.correction)) rec.arg$bias.correction <- TRUE
    rec <- rec.arg$a*ssb*exp(-rec.arg$b*ssb) # rec.arg$a*ssb/(1+rec.arg$b*ssb)
#    if(!isTRUE(deterministic)){
#        rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
#    }
    if(isTRUE(rec.arg$bias.correction)){
        rec <- rec*exp(rnorm(length(ssb),-0.5*(rec.arg$sd)^2,rec.arg$sd))
    }
    else{
        rec <- rec*exp(rnorm(length(ssb),0,rec.arg$sd))                
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

    write("\n# weight at age for biomass calculation",file=csvname,append=T)    
    write.table2(res$input$dat$waa,title.tmp="Weight at age (for biomass)")

    write("\n# weight at age for catch calculation",file=csvname,append=T)    
    write.table2(res$input$dat$waa.catch,title.tmp="Weight at age (for catch)")    

    write("\n# M at age",file=csvname,append=T)    
    write.table2(res$input$dat$M,title.tmp="M at age")          

    write("\n# fishing mortality at age",file=csvname,append=T)    
    write.table2(res$faa,title.tmp="F at age")

    write("\n# Current F",file=csvname,append=T)    
    write.table2(res$Fc.at.age,title.tmp="Current F")

    write("\n# numbers at age",file=csvname,append=T)    
    write.table2(res$naa,title.tmp="Numbers at age")

    write("\n# total and spawning biomass ",file=csvname,append=T)
    x <- rbind(colSums(res$ssb),colSums(res$baa),colSums(res$wcaa))
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

get.kobematrix <- function(fres,Blim=0,Bban=0,ssb=TRUE){
    if(isTRUE(ssb))  tmp <- fres$vssb[,-1]
    else  tmp <- fres$vbiom[,-1]
    
    res <- data.frame(
        # 漁獲量
        catch.deterministic=fres$vwcaa[,1],
        # 資源量
        biom.deterministic=fres$vbiom[,1],
        # 親魚量
        ssb.deterministic=fres$vssb[,1],
        # Blim回復確率
        probability.upper.Blim=apply(tmp>Blim,1,mean)*100,
        # Bban以上確率
        probability.upper.Bban=apply(tmp>Bban,1,mean)*100)

    return(res)
}

############
# RVPAの結果からMSYを計算する関数
# 主に使うのはSR.est(再生産関係をフィットし、MSYを計算)とSR.plot（フィットした結果をプロット）
############

############
# 使い方
############
if(0){
                                        # マサバ太平洋のデータを読み込み; modelAはvpaの帰り値
    modelA <- readRDS("modelA_res.Rdata")
                                        # MSY計算    
    res1 <- SR.est(modelA, 
                   what.est=c(TRUE,TRUE,TRUE), # HS,BH,RIのどれをフィットするか。
                   bref.year=2013:2015, # 生物パラメータを用いる期間
                   years=c(1970:2013), # 観測されたSR関係を用いる期間
                   er.log=TRUE, # 誤差。TRUEで対数正規誤差
                   fc.year=2013:2015, # MSY計算のさいに選択率を平均する期間
                   seed=1 # 乱数の種。この値を変えると乱数が変わるので結果も変わる
                   )
    
    res1$summary # 推定パラメータ、管理基準値の確認
                                        # 再生産パラメータa,bはエクセルとほぼ一致するはずだが、管理基準値は確率的シミュレーションをもとに計算しているので、エクセルとは必ずしも一致しない。±５％くらいの違いはあるみたい

                                        # 結果のプロット(HSのみ)
    res1.pred <- plot.SR(res1,what.plot=c("hs"))
                                        # 結果のプロット(HS,BH,RIを全て)
    res1.pred <- plot.SR(res1,what.plot=c("hs","bh","ri"))
    allplot(res1) # 要約表・グラフの出力

}

############
# fit to S-R relationship
############

SR.est <- function(vpares,SSB.dat=NULL,R.dat=NULL,gamma1=0.01,er.log=TRUE,
                   years=as.numeric(colnames(vpares$naa)), # 親子関係に推定に使う年のベクトル
                   bref.year=2011:2013,# B0やMSYを計算するさいの生物パラメータの範囲(2011-2013に変更、2016-06-06）
                   fc.year=bref.year, # 将来予測をするときに仮定する選択率をとる年の範囲                   
                   seed=1,
                   nyear=100,
                   bias.corrected=TRUE, # 確率的な将来予測を行うときにbias correctionをするかどうか
                   eyear=0, # 将来予測の最後のeyear+1年分を平衡状態とする
#                   FUN=median, # 漁獲量の何を最大化するか？
                   FUN=mean, # 漁獲量の何を最大化するか？                   
                   sigma=-1, #加入変動のCV。-1の場合にはobservedの値を使う
                   N=1000, # stochastic計算するときの繰り返し回数
                   is.small=FALSE, # 将来予測の結果を返さない。
                   is.boot=1000,# 正の値であれば、SRフィットのノンパラメトリックブートストラップを行う
                   is.Kobe=c(FALSE,FALSE,FALSE), # Kobeの計算をするかどうか。順番に、HS, BH, RIの順
                   is.5perlower=FALSE, # HSの折れ点を5%の確率で下回るときの親魚資源量
                   PGY=NULL, # PGY管理基準値を計算するかどうか。計算しない場合はNULLを、計算する場合はc(0.8,0.9,0.95)のように割合を入れる
                   what.est=c(TRUE,TRUE,TRUE) # MSY等を推定するか。順番に、HS, BH, RIの順
                   ){

    #####-------- 内部で使う関数の定義
    HS <- function(p,R,SSB,gamma=gamma1,er.log=er.log,MLE=FALSE,a=NULL,b=NULL){
        if(!is.null(a)) p[1] <- a 
        if(!is.null(b)) p[2] <- b

        a <- exp(p[1])
        b <- max(SSB)/(1+exp(-p[2]))
        if(isTRUE(MLE)) sigma <- exp(p[3])
        Pred <- function(SSB) a*(SSB+sqrt(b^2+gamma^2/4)-sqrt((SSB-b)^2+gamma^2/4))
        if(!isTRUE(MLE)){
            if(er.log==FALSE)   return(sum((R-Pred(SSB))^2))
            else return(sum((log(R)-log(Pred(SSB)))^2))
        }
        else{
            if(er.log==FALSE){
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (R-Pred(SSB))^2 )
                return(-obj)
            }
            else{
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (log(R)-log(Pred(SSB)))^2 )
                return(-obj)
            }
        }                
    }

    BH <- function(p,R,SSB,er.log=er.log,MLE=FALSE){
        a <- exp(p[1])
        b <- exp(p[2])
        if(isTRUE(MLE)) sigma <- exp(p[3])
        
        Pred <- function(SSB) a*SSB/(1+b*SSB)

        if(!isTRUE(MLE)){
            if(er.log==FALSE) return(sum((R-Pred(SSB))^2))
            else return(sum((log(R)-log(Pred(SSB)))^2))
        }
        else{
            if(er.log==FALSE){
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (R-Pred(SSB))^2 )
                return(-obj)
            }
            else{
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (log(R)-log(Pred(SSB)))^2 )
                return(-obj)
            }
        }        
    }


    SL <- function(p,R,SSB,er.log=er.log,MLE=FALSE){
        a <- exp(p[1])
#        b <- exp(p[2])
        if(isTRUE(MLE)) sigma <- exp(p[2])
        
        Pred <- function(SSB) a*SSB

        if(!isTRUE(MLE)){
            if(er.log==FALSE) return(sum((R-Pred(SSB))^2))
            else return(sum((log(R)-log(Pred(SSB)))^2))
        }
        else{
            if(er.log==FALSE){
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (R-Pred(SSB))^2 )
                return(-obj)
            }
            else{
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (log(R)-log(Pred(SSB)))^2 )
                return(-obj)
            }
        }        
    }


    RI <- function(p,R,SSB,er.log=er.log,MLE=FALSE){
        a <- exp(p[1])
        b <- exp(p[2])
        if(isTRUE(MLE)) sigma <- exp(p[3])
        
        Pred <- function(SSB) a*SSB*exp(-b*SSB)

        if(!isTRUE(MLE)){
            if(er.log==FALSE) return(sum((R-Pred(SSB))^2))
            else return(sum((log(R)-log(Pred(SSB)))^2))
        }
        else{
            if(er.log==FALSE){
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (R-Pred(SSB))^2 )
                return(-obj)
            }
            else{
                obj <- length(R)*log(1/(sqrt(2*pi)*sigma))-1/2/sigma^2*sum( (log(R)-log(Pred(SSB)))^2 )
                return(-obj)
            }
        }
    }

    # HSを推定するための関数
    get.HS <- function(R,SSB,er.log,gamma1,do.profile=TRUE){
        reg0 <- lm(R~SSB-1)
        a0 <- reg0$coef
        b0 <- 0.9
        # hockey-stick
        res.HS <-  optim(c(log(a0),logit(b0)),HS,R=R,SSB=SSB,er.log=er.log,gamma=gamma1)
        s <- 1
        for (j in seq(0.95,0.1,by=-0.05)){
            res.HS0 <-  optim(c(log(a0),logit(j)),HS,R=R,SSB=SSB,er.log=er.log,gamma=gamma1)
            if (res.HS0$value < res.HS$value) res.HS <- res.HS0
        }
        res.HS <-  optim(res.HS$par,HS,R=R,SSB=SSB,method="BFGS",er.log=er.log,gamma=gamma1)


        # 最尤法で計算しなおしたもので上書き
        res.HS <-  optim(c(res.HS$par,log(sqrt(res.HS$value/length(R)))),HS,R=R,SSB=SSB,method="BFGS",er.log=er.log,gamma=gamma1,MLE=TRUE)
        
        a.HS <- exp(res.HS$par[1])
        names(a.HS) <- NULL
        b.HS <- max(SSB)/(1+exp(-res.HS$par[2])) # 曲がるところのx軸(ssb_hs)
        # r0の計算
        r0.HS <- pred.HS(b.HS,a=a.HS,b=b.HS,gamma=gamma1)
        # もし、b.HSが最大・最小SSBよりも大きい・小さかったら
        if(b.HS>max(SSB)|b.HS<min(SSB)){
            b.HS <- ifelse(b.HS>max(SSB),max(SSB),b.HS)
            b.HS <- ifelse(b.HS<min(SSB),min(SSB),b.HS)
            tmpfunc <- function(x,r0,...) (pred.HS(a=x,...)-r0)^2
            tmp <- optimize(tmpfunc,c(0,a.HS*10),b=b.HS,gamma=gamma1,SSB=b.HS,r0=r0.HS)
            a.HS <- tmp$minimum
        }

        # 尤度surfaceの計算
        if(isTRUE(do.profile)){
            a.grid <- c(seq(from=0.1,to=0.9,by=0.1),seq(from=0.91,to=1.09,by=0.02),seq(from=1.1,to=1.5,by=0.1)) * a.HS
            b.grid <- c(seq(from=0.1,to=0.9,by=0.1),seq(from=0.91,to=1.09,by=0.02),seq(from=1.1,to=1.5,by=0.1)) * b.HS
            b.grid <- b.grid[b.grid<max(SSB)]
            obj.data <- expand.grid(a=a.grid,b=b.grid)
            obj.data$obj <- NA
            obj.data$log.a <- log(obj.data$a)
            obj.data$conv.b <- -log(max(SSB)/obj.data$b-1)
            for(i in 1:nrow(obj.data))
            {
                obj.data$obj[i] <- HS(c(obj.data$log.a[i],obj.data$conv.b[i]),R=R,SSB=SSB,
                                      MLE=FALSE,er.log=er.log,gamma=gamma1)
                                      
            }
        }
        else{
            obj.data <- NA
        }

        return(list(a=a.HS,b=b.HS,r0=r0.HS,res=res.HS,obj.data=obj.data))
    }

   
    # Beverton-Holt
    get.BH <- function(R,SSB,er.log){
        reg0 <- lm(R~SSB-1)
        a0 <- reg0$coef                    
        b0 <- max(SSB)    
        res.BH <-  optim(c(log(a0),log(1/b0)),BH,R=R,SSB=SSB,method="BFGS",er.log=er.log)
        for (j in seq(0.9,0.1,by=-0.1)){
            res.BH0 <-  optim(c(log(a0),log(j/b0)),BH,R=R,SSB=SSB,er.log=er.log)
            if (res.BH0$value < res.BH$value) res.BH <- res.BH0
        }

        # 最尤法で計算しなおしたもので上書き
        res.BH <-  optim(c(res.BH$par,log(sqrt(res.BH$value/length(R)))),BH,R=R,SSB=SSB,method="BFGS",er.log=er.log,MLE=TRUE)
        
        a.BH <- exp(res.BH$par[1])
        b.BH <- exp(res.BH$par[2])
        return(list(a=a.BH,b=b.BH,res=res.BH))
    }


    get.RI <- function(R,SSB,er.log){
        reg0 <- lm(R~SSB-1)
        a0 <- reg0$coef            
        b0 <- max(SSB)    
        # Ricker
        res.RI <- optim(c(log(a0),log(1/b0)),RI,R=R,SSB=SSB,method="BFGS",er.log=er.log)
        for (j in seq(0.9,0.1,by=-0.1)){
            res.RI0 <-  optim(c(log(a0),log(j/b0)),RI,R=R,SSB=SSB,er.log=er.log)
            if (res.RI0$value < res.RI$value) res.RI <- res.RI0
        }
        #　最尤法
        res.RI <- optim(c(res.RI$par,log(sqrt(res.RI$value/length(R)))),
                        RI,R=R,SSB=SSB,method="BFGS",er.log=er.log,MLE=TRUE)
        a.RI <- exp(res.RI$par[1])
        b.RI <- exp(res.RI$par[2])
        return(list(a=a.RI,b=b.RI,res=res.RI))        
    }
    ##### 関数定義終わり

    # R.datとSSB.datだけが与えられた場合、それを使ってシンプルにフィットする
    if(!is.null(R.dat) & !is.null(SSB.dat)){
        dat <- data.frame(R=R.dat,SSB=SSB.dat,years=1:length(R.dat))
    }
    else{
        vpares$Fc.at.age <- rowMeans(vpares$faa[as.character(fc.year)])
    
    # データの整形
        n <- ncol(vpares$naa)
        L <- as.numeric(rownames(vpares$naa)[1])

        dat <- list()
        dat$R <- as.numeric(vpares$naa[1,])
        dat$SSB <- as.numeric(colSums(vpares$ssb))
        dat$year <- as.numeric(colnames(vpares$ssb))
    # 加入年齢分だけずらす
        dat$R <- dat$R[(L+1):n]
        dat$SSB <- dat$SSB[1:(n-L)]
        dat$year <- dat$year[(L+1):n]

                                        # データの抽出
        dat <- as.data.frame(dat)
        dat <- dat[dat$year%in%years,]
    }

    R <- dat$R
    SSB <- dat$SSB

    # HS推定
#    if(what.est[1]==TRUE){
        tmp <- get.HS(R,SSB,er.log,gamma1)
        a.HS <- tmp$a; b.HS <- tmp$b ; r0.HS <- tmp$r0
        sd.HS <- exp(tmp$res$par[3])
        surface.HS <- tmp$obj.data
        res.HS <- tmp$res
        boot.HS <- matrix(NA,is.boot,3)
        jack.HS <- matrix(NA,length(R),3)
        colnames(boot.HS) <- colnames(jack.HS) <-  c("a","b","r0")            
        if(what.est[1]==TRUE&&is.boot>0){ # ブートストラップ
            for(i in 1:is.boot){
                rand <- sample(length(R),replace=TRUE)
                tmp <- get.HS(R[rand],SSB[rand],er.log,gamma1,do.profile=FALSE)
                boot.HS[i,] <- unlist(tmp[c("a","b","r0")])
            }
            for(i in 1:length(R)){
                tmp <- get.HS(R[-i],SSB[-i],er.log,gamma1,do.profile=FALSE)
                jack.HS[i,] <- unlist(tmp[c("a","b","r0")])
            }                        
        }
        # 予測値
        dat$pred.HS <- pred.HS(dat$SSB,a=a.HS,b=b.HS,gamma=gamma1)
        dat$log.resid.HS <- log(dat$R) - log(dat$pred.HS)
#    }

    if(0){
        # 直線回帰
        reg0 <- lm(R~SSB-1)
        a0 <- reg0$coef    
        res.SL <-  optimize(SL,c(0,log(a0)*10),R=R,SSB=SSB,er.log=er.log)
        res.SL <-  optim(c(res.SL$minimum,log(sqrt(res.SL$objective/length(R)))),
                         SL,R=R,SSB=SSB,er.log=er.log,MLE=TRUE)
        #    res.SL$value <- res.SL$objective
        a.SL <- exp(res.SL$par[1])
        boot.SL <- rep(NA,is.boot)
        jack.SL <- rep(NA,length(R))
        if(is.boot>0){
            for(i in 1:is.boot){
                rand <- sample(length(R),replace=TRUE)
                tmp <-  optimize(SL,c(0,log(a0)*10),R=R[rand],SSB=SSB[rand],er.log=er.log)
                boot.SL[i] <- exp(tmp$minimum[1])
            }
            for(i in 1:length(R)){
                tmp <-  optimize(SL,c(0,log(a0)*10),R=R[-i],SSB=SSB[-i],er.log=er.log)
                jack.SL[i] <- exp(tmp$minimum[1])            
            }
        }
    }

    if(what.est[2]==TRUE){
        # BH推定
        tmp <- get.BH(R,SSB,er.log)
        a.BH <- tmp$a; b.BH <- tmp$b; res.BH <- tmp$res
        sd.BH <- exp(tmp$res$par[3])        
        boot.BH <- matrix(NA,is.boot,2)
        jack.BH <- matrix(NA,length(R),2)
        colnames(boot.BH) <- colnames(jack.BH) <-  c("a","b")            
        if(is.boot>0){ # ブートストラップ
            for(i in 1:is.boot){
                rand <- sample(length(R),replace=TRUE)
                tmp <- get.BH(R[rand],SSB[rand],er.log)
                boot.BH[i,] <- unlist(tmp[c("a","b")])
            }
            # ジャックナイフも
            for(i in 1:length(R)){
                tmp <- get.BH(R[-i],SSB[-i],er.log)
                jack.BH[i,] <- unlist(tmp[c("a","b")])
            }                
        }
        ## 
        dat$pred.BH <- pred.BH(dat$SSB,a=a.BH,b=b.BH)
        dat$log.resid.BH <- log(dat$R) - log(dat$pred.BH)        
    }

    if(what.est[3]==TRUE){
        # RI推定
        tmp <- get.RI(R,SSB,er.log)
        a.RI <- tmp$a ; b.RI <- tmp$b ; res.RI <- tmp$res
        sd.RI <- exp(tmp$res$par[3])                
        boot.RI <- matrix(NA,is.boot,2)
        jack.RI <- matrix(NA,length(R),2)
        colnames(boot.RI) <- colnames(jack.RI) <-  c("a","b")        
        if(is.boot>0){ # ブートストラップ
            for(i in 1:is.boot){
                rand <- sample(length(R),replace=TRUE)
                tmp <- get.RI(R[rand],SSB[rand],er.log)
                boot.RI[i,] <- unlist(tmp[c("a","b")])
            }
            # ジャックナイフも
            for(i in 1:length(R)){
                tmp <- get.RI(R[-i],SSB[-i],er.log)
                jack.RI[i,] <- unlist(tmp[c("a","b")])
            }        
        }
        ## 
        dat$pred.RI <- pred.RI(dat$SSB,a=a.RI,b=b.RI)
        dat$log.resid.RI <- log(dat$R) - log(dat$pred.RI)                
    }

    # 単に回帰だけする場合
    if(!is.null(R.dat) & !is.null(SSB.dat)){
        res <- list()
        paste2 <- function(x,...) paste(x,...,sep="")
        for(j in which(what.est)){
            SR <- c("HS","BH","RI")
            xx <- c(get(paste2("a.",SR[j])),
                    get(paste2("b.",SR[j])),
                    get(paste2("sd.",SR[j])),
                    get(paste2("res.",SR[j]))$value)
            names(xx) <- c("a","b","sd","value")
            res[[j]] <- list(parameter=xx,
                             boot=get(paste2("boot.",SR[j])),
                             jack=get(paste2("jack.",SR[j])))
            }
        names(res) <- SR[what.est]
        return(res)
    }
        
    #-------------------- B0 & MSY for HS --------------------
    # function to minimize

    # シミュレーション回数ぶんの漁獲量のFUN（mean, geomean, median）を最大化するFを選ぶ
    tmpfunc <- function(x,f.arg,FUN=FUN,eyear=eyear){
      f.arg$multi <- x
      fout <- do.call(future.vpa2,f.arg)
      return(-FUN(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1]))
    }

    tmpfunc2 <- function(x,f.arg,FUN=FUN,eyear=eyear,hsp=0){
      f.arg$multi <- x
      fout <- do.call(future.vpa2,f.arg)
      tmp <- as.numeric(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1])
      lhs <- sum(tmp<hsp)/length(tmp)
      return( (lhs-0.05)^2 + as.numeric(lhs==0) + as.numeric(lhs==1)  )
    }

    get.Fhist <- function(farg,vpares,eyear,trace,hsp=0){
        Fhist <- NULL
        original.sel <- farg$res0$Fc.at.age # original F
        for(j in 1:ncol(vpares$faa)){
            farg$res0$Fc.at.age <- vpares$faa[,j] # change the selectivity
            farg$multi <- 1            
            tmp <- do.call(future.vpa2,farg)
            tmp2 <- get.stat(tmp,eyear=eyear,hsp=hsp)
#            browser()
            xx <- which.min(abs(trace$ssb.median-tmp2$ssb.median))+c(-1,1)
            range.tmp <- trace$fmulti[xx]
            if(is.na(range.tmp[2])) range.tmp[2] <- max(trace$fmulti)*2
            if(xx[1]==0) range.tmp <- c(0,range.tmp[1])
            tmpfunc <- function(x,farg,ssb.target,eyear){
                farg$multi <- x
                return((get.stat(do.call(future.vpa2,farg),eyear=eyear,hsp=hsp)$ssb.mean-ssb.target)^2)                
            }
            farg$res0$Fc.at.age <- original.sel # current Fをもとにもどす
            # originalな選択率のもとで、それを何倍にすればi年目のFで漁獲した時の親魚資源量と同じになるか
            ores <- optimize(tmpfunc,range.tmp,farg=farg,ssb.target=tmp2$ssb.mean,eyear=eyear)            
#            farg$multi <- ores$minimum
#            tmp3 <- do.call(future.vpa2,farg)
            tmp2$fmulti <- ores$minimum
            Fhist <- rbind(Fhist,tmp2)            
        }
        return(as.data.frame(Fhist))
    }

    trace.func <- function(farg,eyear,hsp=0,
                           fmulti=c(seq(from=0,to=0.9,by=0.1),1,seq(from=1.1,to=2,by=0.1),3:5,7,20,100)){
        trace.res <- NULL
        farg$outtype <- "FULL"
        for(i in 1:length(fmulti)){
            farg$multi <- fmulti[i]
            tmp <- do.call(future.vpa2,farg)
#            trace.res <- rbind(trace.res,get.stat(tmp,eyear=eyear,hsp=hsp))
            tmp2 <- cbind(get.stat(tmp,eyear=eyear,hsp=hsp),get.stat2(tmp,eyear=eyear,hsp=hsp))
            trace.res <- rbind(trace.res,tmp2)
            if(tmp2$"ssb.mean"<trace.res$"ssb.mean"[1]/1000){
                fmulti <- fmulti[1:i]
                break()
            }
          }
        trace.res <- as.data.frame(trace.res)
        trace.res$fmulti <- fmulti
        return(trace.res)
    }    

    b0.HS <- b0.BH <- b0.RI <- numeric() # B0
    fout.HS <- fout.BH <- fout.RI <- list()
    fout0.HS <- fout0.BH <- fout0.RI <- list()    
    trace.HS <- trace.BH <- trace.RI <- list()
    Fhist.HS <- Fhist.BH <- Fhist.RI <- list()
    fout.HS.5per <- list()
    
    for(kk in 1:length(sigma)){
      ref.year <- as.numeric(rev(colnames(vpares$naa))[1])
      if(sigma[kk]==-1){
          if(isTRUE(what.est[1])){
              sigma.tmp <- exp(res.HS$par[3])
          }
          else{
              if(isTRUE(what.est[2]))  sigma.tmp <- exp(res.BH$par[3])
              if(isTRUE(what.est[3]))  sigma.tmp <- exp(res.RI$par[3])              
          }
      }
      else{
          sigma.tmp <- sigma[kk]
      }
      
      #--------- Hockey stick
      fout0.HS[[kk]] <- future.vpa2(vpares,multi=0,nyear=nyear,start.year=ref.year,
                          N=ifelse(sigma[kk]==0,1,N),
                          ABC.year=ref.year+1,waa.year=bref.year,maa.year=bref.year,
                          M.year=bref.year,is.plot=FALSE,
                          recfunc=HS.rec,seed=seed,outtype="simple",
                          rec.arg=list(a=a.HS,b=b.HS,gamma=gamma1,sd=sigma.tmp,bias.corrected=bias.corrected))

#      b0.HS[kk] <- fout0$vssb[nrow(fout0$vssb),1] #static b0
      farg.HS <- fout0.HS[[kk]]$input
      
      which.min2 <- function(x){
          max(which(min(x)==x))
      }

      if(isTRUE(what.est[1])){
          trace.HS[[kk]] <- trace.func(farg.HS,eyear,hsp=b.HS)

          xx <- which.max(trace.HS[[kk]]$catch.median)+c(-1,1)
          range.tmp <- trace.HS[[kk]]$fmulti[xx]
          if(xx[1]==0) range.tmp <- c(0,range.tmp)
#          if(is.na(xx[2])) range.tmp[2] <- max(trace.HS[[kk]]$fmulti)*10
          if(is.na(range.tmp[2])) range.tmp[2] <- max(trace.HS[[kk]]$fmulti)*10          
      
          tmp <- optimize(tmpfunc,range.tmp,f.arg=farg.HS,eyear=eyear,FUN=FUN)
          farg.HS$multi <- tmp$minimum # Fc.at.a * multiがFmsy
          fout.HS[[kk]] <- do.call(future.vpa2,farg.HS)
          Fmsy.HS <- tmp$minimum * farg.HS$res0$Fc.at.age

          ## ここでtraceを追加
          trace.HS[[kk]] <- rbind(trace.HS[[kk]],trace.func(farg.HS,eyear,hsp=b.HS,
                                  fmulti=tmp$minimum+c(-0.025,-0.05,-0.075,0,0.025,0.05,0.075)))
          trace.HS[[kk]] <- trace.HS[[kk]][order(trace.HS[[kk]]$fmulti),]
          ###
          
          if(is.Kobe[1]) Fhist.HS[[kk]] <- get.Fhist(farg.HS,vpares,eyear=eyear,trace=trace.HS[[kk]])
          if(is.5perlower){
              xx <- which.min2((trace.HS[[kk]]$lower-0.05)^2)+c(-1,1)
              range.tmp <- trace.HS[[kk]]$fmulti[xx]
              if(xx[1]==0) range.tmp <- c(0,range.tmp)
              if(is.na(xx[2])) range.tmp[2] <- max(trace.HS[[kk]]$fmulti)*10
              tmp <- optimize(tmpfunc2,range.tmp,f.arg=farg.HS,eyear=eyear,FUN=FUN,hsp=b.HS)
              farg.HS$multi <- tmp$minimum
              fout.HS.5per[[kk]] <- do.call(future.vpa2,farg.HS)
          }
      }

      #---------------------- calculation of MSY for BH
      if(isTRUE(what.est[2])){
          if(sigma[kk]==-1){
              sigma.tmp <- exp(res.BH$par[3])
          }
          else{
              sigma.tmp <- sigma[kk]
          }          
          farg.BH <- farg.HS
          farg.BH$recfunc <- BH.rec
          farg.BH$rec.arg <- list(a=a.BH,b=b.BH,sd=sigma.tmp,bias.corrected=bias.corrected)
          farg.BH$multi <- 0
          fout0.BH[[kk]] <- do.call(future.vpa2,farg.BH)
          #      b0.BH[kk] <- fout0.BH$vssb[nrow(fout0$vssb),1] #static b0

          trace.BH[[kk]] <- trace.func(farg.BH,eyear)
          #      tmp <- optimize(tmpfunc,c(0,10),f.arg=farg.BH,eyear=eyear,FUN=FUN)
          xx <- which.max(trace.BH[[kk]]$catch.median)+c(-1,1)
          range.tmp <- trace.BH[[kk]]$fmulti[xx]
          if(xx[1]==0) range.tmp <- c(0,range.tmp)
#          if(is.na(xx[2])) range.tmp[2] <- max(trace.BH[[kk]]$fmulti)*10
          if(is.na(range.tmp[2])) range.tmp[2] <- max(trace.BH[[kk]]$fmulti)*10          
          tmp <- optimize(tmpfunc,range.tmp,f.arg=farg.BH,eyear=eyear,FUN=FUN)

     
          farg.BH$multi <- tmp$minimum
          fout.BH[[kk]] <- do.call(future.vpa2,farg.BH)
          Fmsy.BH <- tmp$minimum * farg.BH$res0$Fc.at.age          
          if(is.Kobe[2])  Fhist.BH[[kk]] <- get.Fhist(farg.BH,vpares,eyear=eyear,trace.BH[[kk]])

          ## ここでtraceを追加
          trace.BH[[kk]] <- rbind(trace.BH[[kk]],
                                  trace.func(farg.BH,eyear,hsp=b.BH,fmulti=tmp$minimum+c(-0.025,-0.05,-0.075,0,0.025,0.05,0.075)))
          trace.BH[[kk]] <- trace.BH[[kk]][order(trace.BH[[kk]]$fmulti),]
          ###                    
      }
      
      #------------------- calculation of MSY for RI
      if(isTRUE(what.est[3])){
          if(sigma[kk]==-1){
              sigma.tmp <- exp(res.RI$par[3])
          }
          else{
              sigma.tmp <- sigma[kk]
          }                    
          farg.RI <- farg.HS
          farg.RI$recfunc <- RI.rec
          farg.RI$rec.arg <- list(a=a.RI,b=b.RI,sd=sigma.tmp,bias.corrected=bias.corrected)
          farg.RI$multi <- 0      
          fout0.RI[[kk]] <- do.call(future.vpa2,farg.RI)
          #      b0.RI[kk] <- fout0$vssb[nrow(fout0$vssb),1] #static b0

          trace.RI[[kk]] <- trace.func(farg.RI,eyear)

          xx <- which.max(trace.RI[[kk]]$catch.median)+c(-1,1)
          range.tmp <- trace.RI[[kk]]$fmulti[xx]
          if(xx[1]==0) range.tmp <- c(0,range.tmp)
#          if(is.na(xx[2])) range.tmp[2] <- max(trace.RI[[kk]]$fmulti)*10
          if(is.na(range.tmp[2])) range.tmp[2] <- max(trace.RI[[kk]]$fmulti)*10          
          
          tmp <- optimize(tmpfunc,range.tmp,f.arg=farg.RI,eyear=eyear,FUN=FUN)
      
          farg.RI$multi <- tmp$minimum
          fout.RI[[kk]] <- do.call(future.vpa2,farg.RI)
          Fmsy.RI <- tmp$minimum * farg.RI$res0$Fc.at.age
          
          if(is.Kobe[3])  Fhist.RI[[kk]] <- get.Fhist(farg.RI,vpares,eyear=eyear,trace.RI[[kk]])

          ## ここでtraceを追加
          trace.RI[[kk]] <- rbind(trace.RI[[kk]],
                                  trace.func(farg.RI,eyear,hsp=b.RI,
                                             fmulti=tmp$minimum+c(-0.025,-0.05,-0.075,0,0.025,0.05,0.075)))
          trace.RI[[kk]] <- trace.RI[[kk]][order(trace.RI[[kk]]$fmulti),]
          ###                              
      }
    }
    #--------------------------------------

    if(isTRUE(is.5perlower)){
        tmp <- as.data.frame(t(sapply(fout.HS.5per,get.stat,eyear=eyear,hsp=b.HS)))
        tmp$f <- sapply(fout.HS.5per,function(x)x$multi)
    }
    else{
        tmp <- NA
    }

    # 関数を返すとsaveしたときに異常にファイルサイズが大きくなる。原因は不明。
    # とりあえず、関数を返すのをやめる
    output <- list(dat=dat,sigma=sigma,vpares=vpares)
    if(what.est[1]==TRUE)
        output$hs <- list(a=a.HS,b=b.HS,sd=sd.HS,gamma=gamma1,ofv=res.HS$value,
                           res=res.HS,r0=r0.HS,Fhist=Fhist.HS,
                           trace=trace.HS,boot=as.data.frame(boot.HS),
                           jack=as.data.frame(jack.HS),farg=farg.HS,
                           f.msy=sapply(fout.HS,function(x)x$multi),
                          Fmsy=Fmsy.HS,surface=surface.HS,
                          fout=fout.HS,
                         # 最大化したときのFを使って将来予測したときのサマリーをMSYのreference pointとする
                           MSY=as.data.frame(t(sapply(fout.HS,get.stat,eyear=eyear,hsp=b.HS))),
                           B0=as.data.frame(t(sapply(fout0.HS,get.stat,eyear=eyear,hsp=b.HS))),
                           per5=tmp)

#                   sl=list(a=a.SL,
#                       res=res.SL,jack=jack.SL,boot=boot.SL),
    if(what.est[2]==TRUE)
        output$bh <- list(a=a.BH,b=b.BH,sd=sd.BH,
                       res=res.BH,r0=NA,#R0を入れないといけない
                       Fhist=Fhist.BH,ofv=res.BH$value,
                       trace=trace.BH,b0=b0.BH,boot=as.data.frame(boot.BH),jack=as.data.frame(jack.BH),
                       f.msy=sapply(fout.BH,function(x)x$multi),
                       fout=fout.BH,                       
                       Fmsy=Fmsy.BH,farg=farg.BH,                       
                       MSY=as.data.frame(t(sapply(fout.BH,get.stat,eyear=eyear))),
                          B0=as.data.frame(t(sapply(fout0.BH,get.stat,eyear=eyear))))

    if(what.est[3]==TRUE)
        output$ri <- list(a=a.RI,b=b.RI,sd=sd.RI,ofv=res.RI$value,
                           res=res.RI,r0=NA,#R0を入れないといけない,
                           Fhist=Fhist.RI,farg=farg.RI,                       
                           trace=trace.RI,b0=b0.RI,boot=as.data.frame(boot.RI),
                          jack=as.data.frame(jack.RI),
                          fout=fout.RI,                                                 
                           f.msy=sapply(fout.RI,function(x)x$multi),
                       Fmsy=Fmsy.RI,                                                 
                     MSY=as.data.frame(t(sapply(fout.RI,get.stat,eyear=eyear))),
                          B0=as.data.frame(t(sapply(fout0.RI,get.stat,eyear=eyear))))
    index <- c("a","b","R0","sd","MSY","B0","f.msy","Fmsy")    
    tmp <- NULL
    if(what.est[1]==TRUE) tmp <- rbind(tmp,unlist(output$hs[index]))
    if(what.est[2]==TRUE) tmp <- rbind(tmp,unlist(output$bh[index]))
    if(what.est[3]==TRUE) tmp <- rbind(tmp,unlist(output$ri[index]))

    tmp <- as.data.frame(tmp)
    rownames(tmp) <- c("hs","bh","ri")[what.est]
#    tmp$nLL <- output$ofv 
    output$summary0 <- tmp
    colnames(output$summary0)[1] <- "a"
    output$summary <- output$summary0[c("a","b","sd","MSY.ssb.mean.ssb.mean",
                                        "MSY.biom.mean.biom.mean",
                                        "MSY.U.mean.U.mean",
                                        "MSY.catch.mean.catch.mean",
                                        "B0.ssb.mean.ssb.mean",
                                        "B0.biom.mean.biom.mean","f.msy")]
    colnames(output$summary) <- c("a","b","sd","SSB_MSY","B_MSY","U_MSY","MSY","B0(SSB)","B0(Biomass)","FMSY/Fcurrent")
    output$summary <- cbind(output$summary,output$summary0[,substr(colnames(output$summary0),1,4)=="Fmsy"])
    class(output) <- "SR"

    ##--- PGY管理基準値を計算する
    if(!is.null(PGY)){
        k.tmp <- which(what.est)
        for(k in 1:length(k.tmp)){
            fout.list2 <- list()
            s <- 1
            for(j in 1:length(PGY)){
                outtmp <- output[[which(names(output)==c("hs","bh","ri")[k.tmp[k]])[1]]]
#                outtmp$trace
#                frange.list <- list(c(output[[which(names(output)==c("hs","bh","ri")[1])[1]]]$f.msy,2),
#                                    c(0.01,output[[which(names(output)==c("hs","bh","ri")[1])[1]]]$f.msy))
                ttmp <- outtmp$trace[[1]]$catch.mean-PGY[j]*output$summary$MSY[k]
                ttmp <- which(diff(sign(ttmp))!=0)
                frange.list <- list(outtmp$trace[[1]]$fmulti[ttmp[1]+0:1],
                                   outtmp$trace[[1]]$fmulti[ttmp[2]+0:1])
#                browser()
                for(i in 1:2){
                    if(k.tmp[k]==1) farg.tmp <- farg.HS
                    if(k.tmp[k]==2) farg.tmp <- farg.BH
                    if(k.tmp[k]==3) farg.tmp <- farg.RI                    
                    farg.tmp$outtype <- NULL
                    farg.tmp$Frec <- list(stochastic=TRUE,
                                          future.year=rev(rownames(outtmp$fout[[1]]$vssb))[1],
                                          Blimit=PGY[j]*output$summary$MSY[k],
                                          scenario="catch.mean",Frange=frange.list[[i]])
                    fout.list2[[s]] <- do.call(future.vpa,farg.tmp)
                    s <- s+1
                }}
            PGY.biom <- as.data.frame(t(sapply(fout.list2,get.stat,eyear=eyear)))
            rownames(PGY.biom) <- paste("PGY",rep(PGY,each=2),rep(c("upper","lower"),length(PGY)),c("hs","bh","ri")[k.tmp[k]],sep="_")
            PGY.biom$target.catch <- rep(PGY*output$summary$MSY[k],each=2)
            if(k.tmp[k]==1) output$PGY.biom.hs <- PGY.biom
            if(k.tmp[k]==2) output$PGY.biom.bh <- PGY.biom
            if(k.tmp[k]==3) output$PGY.biom.ri <- PGY.biom                        
        }
    }
    ##---

    if(isTRUE(is.small)){
        output$hs$fout <- NULL
        output$bh$fout <- NULL
        output$ri$fout <- NULL        
        }

    
    return(output)
}

pred.RI <- function(SSB,a,b) a*SSB*exp(-b*SSB)
pred.BH <- function(SSB,a,b) a*SSB/(1+b*SSB)
pred.HS <- function(SSB,a,b,gamma) a*(SSB+sqrt(b^2+gamma^2/4)-sqrt((SSB-b)^2+gamma^2/4))
pred.SL <- function(SSB,a) a*SSB

##
get.stat <- function(fout,eyear=NULL,hsp=NULL){
        tmp <- as.numeric(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1])
        lhs <- sum(tmp<hsp)/length(tmp)
            
        a <- data.frame("catch.mean"=mean(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1]),
                        "catch.sd"=sd(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1]),
                        "catch.geomean"=geomean(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1]),
                        "catch.median"=median(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1],na.rm=T),
                        "catch.det"=mean(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),1],na.rm=T),
                        "catch.L10"=quantile(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1],na.rm=T,probs=0.1),
                        "catch.H10"=quantile(fout$vwcaa[(nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa),-1],na.rm=T,probs=0.9),
                        "ssb.mean"=mean(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1]),
                        "ssb.sd"=sd(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1]),                        
                        "ssb.geomean"=geomean(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1]),
                        "ssb.median"=median(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1],na.rm=T),
                        "ssb.det"=mean(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),1],na.rm=T),
                        "ssb.L10"=quantile(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1],na.rm=T,probs=0.1),
                        "ssb.H10"=quantile(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),-1],na.rm=T,probs=0.9),

                        "biom.mean"=mean(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1]),
                        "biom.sd"=sd(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1]),                        
                        "biom.geomean"=geomean(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1]),
                        "biom.median"=median(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1],na.rm=T),
                        "biom.det"=mean(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),1],na.rm=T),
                        "biom.L10"=quantile(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1],na.rm=T,probs=0.1),
                        "biom.H10"=quantile(fout$vbiom[(nrow(fout$vbiom)-eyear):nrow(fout$vbiom),-1],na.rm=T,probs=0.9),
                        "lower.HSpoint"=lhs
                        )
        a$U.mean <- a$catch.mean/a$biom.mean
        a$U.median <- a$catch.median/a$biom.median
        a$U.geomean <- a$catch.geomean/a$biom.geomean
        a$U.det <- a$catch.det/a$biom.det

        a$catch.CV <- a$catch.sd/a$catch.mean
        a$ssb.CV <- a$ssb.sd/a$ssb.mean
        a$biom.CV <- a$biom.sd/a$biom.mean
        return(a)
    }

get.stat2 <- function(fout,eyear=NULL,hsp=NULL){
        tmp <- (nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa)
        nage <- dim(fout$naa)[[1]]
        tb <- fout$naa * fout$waa
        if(is.null(fout$waa.catch)) fout$waa.catch <- fout$waa
        tc <- fout$caa * fout$waa.catch
        ssb <- fout$naa * fout$waa *fout$maa 
        tb.mat <- tc.mat <- ssb.mat <- matrix(0,nage,6)
        for(i in 1:nage){
            tb.mat[i,1] <- mean(tb[i,tmp,-1])
            tb.mat[i,2] <- median(tb[i,tmp,-1])
            tb.mat[i,3] <- geomean(tb[i,tmp,-1])
            tb.mat[i,4] <- mean(tb[i,tmp,1])
            tb.mat[i,5:6] <- quantile(tb[i,tmp,-1],probs=c(0.1,0.9),na.rm=T)
            
            tc.mat[i,1] <- mean(tc[i,tmp,-1])
            tc.mat[i,2] <- median(tc[i,tmp,-1])
            tc.mat[i,3] <- geomean(tc[i,tmp,-1])
            tc.mat[i,4] <- mean(tc[i,tmp,1])
            tc.mat[i,5:6] <- quantile(tc[i,tmp,-1],probs=c(0.1,0.9),na.rm=T)            

            ssb.mat[i,1] <- mean(ssb[i,tmp,-1])
            ssb.mat[i,2] <- median(ssb[i,tmp,-1])
            ssb.mat[i,3] <- geomean(ssb[i,tmp,-1])
            ssb.mat[i,4] <- mean(ssb[i,tmp,1])
            ssb.mat[i,5:6] <- quantile(ssb[i,tmp,-1],probs=c(0.1,0.9),na.rm=T)                        
        }
        tc.mat <- as.numeric(tc.mat)
        tb.mat <- as.numeric(tb.mat)
        ssb.mat <- as.numeric(ssb.mat)        

        # MA; mean, ME; median, GM; geometric mean
        names(tc.mat) <- c(paste("TC-MA-A",1:nage,sep=""),paste("TC-ME-A",1:nage,sep=""),
                           paste("TC-GM-A",1:nage,sep=""),paste("TC-DE-A",1:nage,sep=""),
                           paste("TC-L10-A",1:nage,sep=""),paste("TC-H10-A",1:nage,sep=""))
        names(tb.mat) <- c(paste("TB-MA-A",1:nage,sep=""),paste("TB-ME-A",1:nage,sep=""),
                           paste("TB-GM-A",1:nage,sep=""),paste("TB-DE-A",1:nage,sep=""),
                           paste("TB-L10-A",1:nage,sep=""),paste("TB-H10-A",1:nage,sep=""))
        names(ssb.mat) <- c(paste("SSB-GA-A",1:nage,sep=""),paste("SSB-ME-A",1:nage,sep=""),
                            paste("SSB-GM-A",1:nage,sep=""),paste("SSB-DE-A",1:nage,sep=""),
                            paste("SSB-L10-A",1:nage,sep=""),paste("SSB-H10-A",1:nage,sep=""))        
            
        return(as.data.frame(t(c(tb.mat,tc.mat,ssb.mat))))
    }    



## est.SR用の将来予測関数。future.vpaとどう違うのかよくわからないが、なんか違うみたい。
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
                       waa=NULL,waa.catch=NULL,maa=NULL,M=NULL, # 季節毎の生物パラメータ
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

  if(!silent)  cat("F multiplier= ", multi,"seed=",seed,"\n")
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
    waa.catch.org <- waa.catch    
    maa.org <- maa
    M.org <- M
  }
  
  faa <- naa <- waa <- waa.catch <- maa <- M <- caa <- 
          array(NA,dim=c(length(ages),ntime,N),dimnames=list(ages,fyears,1:N))
  # future biological patameter
  if(!is.null(M.org))  M[] <- M.org  else M[] <- apply(as.matrix(res0$input$dat$M[,years %in% M.year]),1,mean)
  if(!is.null(waa.org))  waa[] <- waa.org  else waa[] <- apply(as.matrix(res0$input$dat$waa[,years %in% waa.year]),1,mean)
  if(!is.null(maa.org))  maa[] <- maa.org  else maa[] <- apply(as.matrix(res0$input$dat$maa[,years %in% maa.year]),1,mean)
  if(!is.null(waa.catch.org)){
      waa.catch[] <- waa.catch.org
  }
  else{
      if(!is.null(res0$input$dat$waa.catch)) waa.catch[] <- apply(as.matrix(res0$input$dat$waa.catch[,years %in% waa.year]),1,mean)
      else waa.catch <- waa
  }    

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
              if(!is.null(res0$input$dat$waa.catch)){
                  waa.catch[,tmp[j],] <- res0$input$dat$waa.catch[,tmp0[jj]]
              }
              else{
                  waa.catch[,tmp[j],] <- res0$input$dat$waa[,tmp0[jj]]
                  }              
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
                         waa.catch[,i,k],M[,i,k],pre.catch$wcatch*1000,Pope=Pope)
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
  
  wcaa <- caa*waa.catch*res0$input$unit.waa/res0$input$unit.biom
  vwcaa <- apply(wcaa,c(2,3),sum,na.rm=T)

  ABC <- apply(as.matrix(vwcaa[fyears%in%ABC.year,,drop=F]),2,sum)

  if(outtype=="FULL"){
      fres <- list(faa=faa,naa=naa,biom=biom,ssb=ssb,wcaa=wcaa,caa=caa,M=M,rps=rps.mat,
                   maa=maa,vbiom=apply(biom,c(2,3),sum,na.rm=T),
                   waa=waa,waa.catch=waa.catch,currentF=currentF,
                   vssb=apply(ssb,c(2,3),sum,na.rm=T),vwcaa=vwcaa,
                   years=fyears,fyear.year=fyear.year,ABC=ABC,recfunc=recfunc,rec.arg=rec.arg,
                   waa.year=waa.year,maa.year=maa.year,multi=multi,multi.year=multi.year,
                   Frec=Frec,rec.new=rec.new,pre.catch=pre.catch,input=arglist)
  }
  else{
      fres <- list(faa=faa[,,1],M=M[,,1],recruit=naa[1,,],
                   maa=maa[,,1],vbiom=apply(biom,c(2,3),sum,na.rm=T),
                   waa=waa[,,1],waa.catch=waa.catch[,,1],currentF=currentF,
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


geomean <- function(x)
{
  ifelse(all(x > 0), exp(mean(log(x))), NA)
}


plot.SR <- function(srres,what.plot=c("hs","bh","ri","sl"),xyscale=c(1.3,1.3),xscale=FALSE,is.legend=TRUE,what.sigma=1,FUN="mean",is.MSYline=TRUE,
                    pick="SSB_MSY"){

    col.tmp <- c(rgb(0.3,0.8,0.3,alpha=0.8),rgb(0.8,0.3,0.3,alpha=0.8),rgb(0.3,0.3,0.8,alpha=0.8))
    
    # xscale=TRUEの場合、B0が再生産関係によって異なってくるので、複数の重ね書きはしないこSと！
#    tmp <- which(names(srres)==what.plot)
    tmp <- which(names(srres)%in%what.plot)
    resid <- list()

    if(isTRUE(xscale)){
        ssb0 <- srres$summary$B0.ssb.mean1[tmp]
        xrange <- seq(from=0,to=ssb0,length=100)
    }
    else{
        ssb0 <- 1
        xrange <- seq(from=0,to=xyscale[1]*max(srres$dat$SSB,na.rm=T),length=100)
    }

    plot(x <- srres$dat$SSB/ssb0,y <- srres$dat$R,type="l",pch=20,xlim=range(xrange/ssb0),col="gray",
         ylim=c(0,xyscale[2]*max(y,na.rm=T)),xaxs="i",yaxs="i",xlab=ifelse(!xscale,"Spawning biomass (MT)","SB/SB0"),
         ylab="Number of recruits",lwd=1)
    points(x,y,type="p",pch=20,col=gray(c(seq(from=0.7,to=0,length=length(x)))))
    points(rev(x)[1],rev(y)[1],type="p",pch=20,cex=2.5)
    for(i in 1:length(what.plot)){
        Bmsy <- srres$summary[pick][which(what.plot[i]==rownames(srres$summary)),]        
        if(what.plot[i]=="hs"){
            points(xpred <- xrange/ssb0,
                   ypred <- pred.HS(SSB=xrange,
                                       a=srres[what.plot[i]][[1]]$a,b=srres[what.plot[i]][[1]]$b,gamma=srres[what.plot[i]][[1]]$gamma),
                   type="l",lwd=2,col=col.tmp[i],lty=1)
            resid[[i]] <- pred.HS(SSB=x,
                             a=srres[what.plot[i]][[1]]$a,
                             b=srres[what.plot[i]][[1]]$b,
                             gamma=srres[what.plot[i]][[1]]$gamma)
            resid[[i]] <- log(y)-log(resid[[i]])
            Rmsy <- pred.HS(SSB=Bmsy,
                            a=srres[what.plot[i]][[1]]$a,b=srres[what.plot[i]][[1]]$b,gamma=srres[what.plot[i]][[1]]$gamma)
        }
        if(what.plot[i]=="bh"|what.plot[i]=="ri"){
          if(what.plot[i]=="bh") tmpfunc <- pred.BH
          if(what.plot[i]=="ri") tmpfunc <- pred.RI        
          points(xpred <- xrange/ssb0,
                 ypred <- tmpfunc(SSB=xrange,
                                     a=srres[what.plot[i]][[1]]$a,b=srres[what.plot[i]][[1]]$b),type="l",lwd=2,col=col.tmp[i],lty=ifelse(what.plot[i]=="bh",2,3))
          resid[[i]] <- tmpfunc(SSB=x,
                                a=srres[what.plot[i]][[1]]$a,
                                b=srres[what.plot[i]][[1]]$b)
          resid[[i]] <- log(y)-log(resid[[i]])
          Rmsy <- tmpfunc(SSB=Bmsy,
                          a=srres[what.plot[i]][[1]]$a,b=srres[what.plot[i]][[1]]$b)          
        }
        if(what.plot[i]=="sl")
          points(xrange/ssb0,pred.SL(SSB=xrange,
                    a=srres[what.plot[i]][[1]]$a),type="l",lwd=1,col=col.tmp[i])



        if(is.MSYline){ #abline(v=Bmsy/ssb0,col=i+1,lty=2)
            arrows(Bmsy/ssb0,Rmsy*1.2,Bmsy/ssb0,Rmsy,col=col.tmp[i],lty=1,lwd=2,length=.1)
            if(Bmsy/ssb0>rev(xrange)[1]){
                ymax <- xyscale[2]*max(y,na.rm=T)
                arrows(rev(xrange)[2],ifelse(ymax<Rmsy,ymax*0.8,Rmsy*0.8),
                       rev(xrange)[2],ifelse(ymax<Rmsy,ymax,Rmsy),col=col.tmp[i],lty=1,lwd=2,length=.1)
            }
        }
    }

    neg.LL <- sapply(srres[what.plot],function(x) x$res$value)
    k <- sapply(srres[what.plot],function(x) length(x$res$par)-1)
    n <- length(srres$dat$R)
    AICc <- 2*neg.LL+2*k+2*k*(k+1)/(n-k-1)
    if(is.legend){
        legend("topright",legend=paste(toupper(what.plot[order(AICc)]),round(AICc[order(AICc)],2)),
               col=col.tmp[order(AICc)],lwd=1,title="AICc",bg="white",ncol=3)
    }

    return(list(AICc=AICc,resid=resid,x=xpred,y=ypred))
}

# MSY計算で仮定されている選択率で漁獲したとき何倍になるか？ => 計算時間が、、。
# %SPR？MSYを達成した時のFを%SPR換算
plot.Kobe0 <- function(srres,pickB="",what.plot="hs",plot.history=FALSE){
    tmp <- which(names(srres)==what.plot)-3
    years <- colnames(srres$vpares$ssb)
    y <- srres[what.plot][[1]]$Fhist[[1]]$fmulti/srres[what.plot][[1]]$f.msy
    x <- as.numeric(colSums(srres$vpares$ssb))/
                      srres$summary[pickB][[1]][tmp]
    x <- x[y>0.001]
    years <- years[y>0.001]
    y <- y[y>0.001]    
    plot(x,y,type="n",xlim=c(0,ifelse(max(x)<3,3,max(x,na.rm=T))),
#         ylim=c(0,ifelse(max(y)<3,3,max(y))),
         ylim=c(0,4),
         pch=c(3,rep(1,length(y)-2),20),col=c(1,rep(1,length(y)-2),2),
         cex=c(1,rep(1,length(y)-2),2),ylab="F/Fmsy",xlab="B/Bmsy")
    polygon(c(-1,1,1,-1),c(-1,-1,1,1),col="khaki1",border=NA)
    polygon(c(1,4,4,1),c(-1,-1,1,1),col="olivedrab2",border=NA)
    polygon(c(1,4,4,1),c(1,1,6,6),col="khaki1",border=NA)
    polygon(c(-1,1,1,-1),c(1,1,6,6),col="indianred1",border=NA)
    axis(side=1:2)
    
    points(x,y,type="o",
           pch=c(3,rep(1,length(y)-1),20),
           col=c(1,rep(1,length(y)-1),1),
           cex=c(1,rep(1,length(y)-1),2),ylab="F/Fmsy",xlab="B/Bmsy")
    points(rev(x)[1],rev(y)[1],pch=20)

    if(isTRUE(plot.history)){
      plot(years,y,type="b",ylab="F/Fmsy",ylim=c(0,max(y)))
      abline(h=1)    
      plot(years,x,type="b",xlab="B/Bmsy",ylim=c(0,max(y)))
      abline(h=1)
    }

    invisible(data.frame(years=years,F=y,B=x))    
}

plot.Kobe2 <- get.trend <- function(srres,UBdata=NULL,SR="hs",plot.history=FALSE,is.plot=FALSE,pickU="",pickB="",ylab.tmp="U/Umsy",xlab.tmp="SSB/SSBmsy"){
    
    dres <- srres$vpares
    tmp <- which(names(srres)==SR)-3

    if(is.null(dres$TC.MT)) dres$TC.MT <- as.numeric(colSums(dres$wcaa))

    if(is.null(UBdata)){
    U <- data.frame(years=as.numeric(colnames(dres$baa)),
                    U=as.numeric(dres$TC.MT)/as.numeric(colSums(dres$baa,na.rm=T)))
    B <- data.frame(years=as.numeric(colnames(dres$ssb)),
                    B=as.numeric(colSums(dres$ssb)))
    Catch <- data.frame(years=as.numeric(colnames(dres$baa)),
                    C=as.numeric(dres$TC.MT))
    UBdata <- merge(U,B)
    UBdata <- merge(UBdata,Catch)
    
#    U <- data.frame(years=as.numeric(ts$YEAR),
#                    U=as.numeric(ts$"TC-MT")/as.numeric(ts$"TB-MT"))

#    UBdata$Umsy <- srres$summary$MSY.U.median2[tmp]
#    UBdata$Bmsy <- srres$summary$MSY.ssb.median2[tmp]

    UBdata$Umsy <- srres$summary[pickU][tmp,]
    UBdata$Bmsy <- srres$summary[pickB][tmp,]
    
    UBdata$Uratio <- UBdata$U/UBdata$Umsy
    UBdata$Bratio <- UBdata$B/UBdata$Bmsy
    }

    if(is.plot){
      plot(x <- UBdata$Bratio,
           y <- UBdata$Uratio,type="n",xlim=c(0,ifelse(max(x)<2,2,max(x,na.rm=T))),
           ylim=c(0,ifelse(max(y,na.rm=T)<3,3,max(y,na.rm=T))),
           cex=c(1,rep(1,length(y)-2),3),ylab=ylab.tmp,xlab=xlab.tmp)
      polygon(c(-1,1,1,-1),c(-1,-1,1,1),col="khaki1",border=NA)
      polygon(c(1,6,6,1),c(-1,-1,1,1),col="olivedrab2",border=NA)
      polygon(c(1,6,6,1),c(1,1,6,6),col="khaki1",border=NA)
      polygon(c(-1,0.5,0.5,-1),c(1,1,6,6),col="indianred1",border=NA)
      polygon(c(0.5,1,1,0.5),c(1,1,6,6),col="tan1",border=NA)
      polygon(c(-1,0.5,0.5,-1),c(-1,-1,1,1),col="khaki2",border=NA)
      polygon(c(0.5,1,1,0.5),c(-1,-1,1,1),col="khaki1",border=NA)            
      axis(side=1:2)

#      points(x,y,type="o",pch=c(3,rep(1,length(y)-2),20),col=c(1,rep(1,length(y)-2),1),cex=c(1,rep(1,length(y)-2),1.5))

      points(x,y,type="l",pch=20,col=1,lwd=1)
      points(x,y,type="p",pch=20,col=gray(c(seq(from=0.7,to=0,length=length(x)))),cex=1.2)
      points(rev(x)[1],rev(y)[1],type="p",pch=20,cex=2.5)

    if(isTRUE(plot.history)){
      plot(UBdata$years,y,type="b",ylab="F/Fmsy",ylim=c(0,max(y)))
      abline(h=1)    
      plot(UBdata$years,x,type="b",xlab="B/Bmsy",ylim=c(0,max(y)))
      abline(h=1)
    }}

    invisible(UBdata)    
}


show.likeprof <- function(res){
    x <- tapply(res$hs$surface$obj,list(res$hs$surface$b,res$hs$surface$a),function(x) x)
    image(as.numeric(rownames(x)),as.numeric(colnames(x)),log(x/min(x)),col=rev(heat.colors(12)),ylab="a",xlab="b")
    contour(as.numeric(rownames(x)),as.numeric(colnames(x)),log(x/min(x)),add=T,nlevels=10,zlim=c(0,0.3))
    points(res$hs$b,res$hs$a)
    title("Diagnostics")    
}

# 単位はcatch at ageの尾数が100万尾、waaがgの場合、重量の単位がちょうどトンになるようになっている。

allplot <- function(res0,target="hs",biomass.scale=1000,
                    pngfile="../buri.png",detail.plot=1:3){
    dmodel <- res0$vpares
    summary <- res0$summary[rownames(res0$summary)==target,]
    res00 <- res0[names(res0)==target][[1]]
    if(is.null(dmodel$Bban))  dmodel$Bban <- NA
    if(is.null(dmodel$Blimit))  dmodel$Blimit <- NA
    if(is.null(dmodel$ABC))  dmodel$ABC <- NA
#    summary$Bban <- dmodel$Bban
    summary[1] <- res00$r0
    colnames(summary)[1] <- "R0"
    summary$Blimit <- ifelse(length(dmodel$Blimit)>0,dmodel$Blimit,NA)
    summary$"Fmsy/Fcurrent" <- res00$f.msy
    summary$ABC2017 <- dmodel$ABC2017
    summary$ABC2015 <- dmodel$ABC2015    
    summary$"newABC2015" <- rev(colSums(dmodel$baa))[1] * summary$"U_MSY"

    summary$"Pr(SSB_2021>SSB_MSY) (%)" <- mean(res00$fout[[1]]$vssb[6,]>summary$"SSB_MSY")*100
    summary$"Pr(SSB_2021>SSB_hs) (%)" <- mean(res00$fout[[1]]$vssb[6,]>summary$"b")*100

    colnames(summary)[2] <- "SSB_HS"
    colnames(summary)[8:9] <- c("SSB0","B0")

    
    summary1 <- data.frame(parameter=names(summary),value=as.numeric(summary))

    ## currentF projection
    arg.tmp <- res0[names(res0)==target][[1]]$farg
    arg.tmp$multi <- 1
    fout0 <- do.call(future.vpa2,arg.tmp) # currentFでの将来予測の結果
    fout1 <- res0[names(res0)==target][[1]]$fout[[1]] # Fmsyでの将来予測の結果

    layout(t(matrix(c(1,2,3,4,5,6,7,8),2,4)),heights=c(0.7,1,1,1))
    
    if(1%in%detail.plot){            
        ## plot talbe
        par(mar=c(1,4.3,3,1))
        n <- floor(nrow(summary1)/2)
        plot.info(summary1[1:n,])
        title(dmodel$jstockid)    
        plot.info(summary1[(n+1):nrow(summary1),])    
       
        ## SR plot(all)
        par(mar=c(4.3,4.3,3,1))
        aa <- summary[c("SSB_MSY","Blimit","SSB_HS")]    
        fit.tmp <- plot.SR(res0,what.plot=rownames(res0$summary),pick="SSB_MSY",xyscale=c(1.8,1.3))
#        abline(v=aa,col=c("chartreuse3","orange","red"))
        title("R vs SSB (HS, BH, RI)")

        ## SR plot(HS)
        par(mar=c(4.3,4.3,3,1))
        aa <- summary[c("SSB_MSY","Blimit","SSB_HS")]    
        fit.tmp <- plot.SR(res0,what.plot=target,pick="SSB_MSY",xyscale=c(1.8,1.3),is.legend=FALSE)
#        abline(v=aa,col=c("chartreuse3","orange","red"))
        title(paste("R vs SSB (only ",target,")",sep=""))        

        ## Residual plot
        plot(x <- res0$dat$year,
             y <- fit.tmp$resid[[1]],
             ylim=c(-1.5,1.5),type="p",pch=20,xlab="Year",ylab="log(Obs)-log(Pred)")
        abline(h=0,lty=2)
        xx <- loess(y~x)
        points(x,xx$fitted,type="l",col=2,lwd=2)
        title("Residual to HS prediction")

        ## plot diagnostics
        if(target=="hs"){
            show.likeprof(res0)
            lines(c(res0$hs$b,res0$hs$b),quantile(res0$hs$boot$a,probs=c(0.05,0.95)))
            lines(quantile(res0$hs$boot$b,probs=c(0.05,0.95)),c(res0$hs$a,res0$hs$a))
            points(res0$hs$jack$b,res0$hs$jack$a)
            legend("topleft",lty=c(1,NA),pch=c(NA,1),legend=c("Bootstrap 90%", "Jackknife"),ncol=2,bg="white")
        }

        ## yield curve
        par(mar=c(4.3,4.3,3,4.3))
        plotyield(res00)

    }

    if(2%in%detail.plot){
        layout(t(matrix(c(1,2,3,4,5,6),2,3)),heights=c(1.2,1,1))        
                                        # Kobe
        par(mar=c(4.3,4.3,5,1))        
        a <- plot.Kobe2(res0,SR=target,pickU="U_MSY",pickB="SSB_MSY",is.plot=T)
        abline(v=summary$"SSB_HS"/summary$"SSB_MSY",lty=2)
        title("Kobe chart",line=0.5)
        title(dmodel$jstockid,line=2)

        # plot selectivity
        matplot(rownames(res0$vpares$faa),
                res0$vpares$faa,pch=20,col="gray",xlab="Age",ylab="F",ylim=c(0,max(res0$vpares$faa,na.rm=T)))
        points(rownames(res0$vpares$faa),res00$fout[[1]]$faa[,1],type="b")
        legend("topleft",pch=c(1,20),legend=c("Current F","Past Fs"),col=c(1,"gray"))
        title("Current F")

        ## plot SSB
        par(mar=c(4.3,4.3,3,1))                                
        years <- as.numeric(colnames(dmodel$ssb))
        y <- as.numeric(colSums(dmodel$ssb))
        plot(years,y,ylim=c(0,1.1*max(c(y,unlist(aa)),na.rm=T)),xlab="Years",ylab="SSB",type="o",
             xlim=c(min(years),max(years)+10))

        ## projection
        menplot(rownames(fout0$vssb),t(apply(fout0$vssb,1,quantile,probs=c(0.1,0.9))),
                col=rgb(40/255,96/255,163/255,0.2),border="blue",lty=2)
        menplot(rownames(fout1$vssb),t(apply(fout1$vssb,1,quantile,probs=c(0.1,0.9))),
                col=rgb(40/255,96/255,163/255,0.2),border="red",lty=2)        
        points(rownames(fout0$vssb),apply(fout0$vssb,1,mean),type="o",pch=20,
               col="blue")
        points(rownames(fout1$vssb),apply(fout1$vssb,1,mean),type="o",pch=20,
               col=2)                
        
        abline(h=aa,col=c("chartreuse3","orange","red"))
        legend("topleft",col=c("chartreuse3","orange","red","blue","red"),lty=1,pch=c(NA,NA,NA,20,20),
               legend=c("SSB_MSY","SSB_limit","SSB_HS","Projection (Fcur)","Projection (Fmsy)"),cex=0.8,
               bg="white",ncol=2)
        title("SSB",line=0.5)
        
                                        # plot catch
#        y <- as.numeric(colSums(dmodel$input$dat$caa * dmodel$input$dat$waa,na.rm=T)) # * N.unit * waa.unit
        y <- as.numeric(colSums(dmodel$wcaa))
        aa <- summary[c("MSY")]
        plot(years,y,ylim=c(0,1.1*max(c(y,unlist(aa)))),xlab="Years",ylab="Total catch",type="o",
             xlim=c(min(years),max(years)+10))

        ## projection
        menplot(rownames(fout0$vssb),t(apply(fout0$vwcaa,1,quantile,probs=c(0.1,0.9))),
                col=rgb(210/255,94/255,44/255,0.3),border="blue",lty=2)
        menplot(rownames(fout1$vssb),t(apply(fout1$vwcaa,1,quantile,probs=c(0.1,0.9))),
                col=rgb(210/255,94/255,44/255,0.3),border="red",lty=2)
        points(rownames(fout0$vssb),apply(fout0$vwcaa,1,mean),type="o",pch=20,
               col="blue")
        points(rownames(fout1$vssb),apply(fout1$vwcaa,1,mean),type="o",pch=20,
               col=2)
        
        abline(h=aa,col="chartreuse3")
        tmp <- names(summary)%in%c("ABC2017","ABC2015","newABC2015")
        tmp2 <- c("ABC2017","ABC2015","newABC2015")%in%names(summary)
        points(c(2017,2015,2015)[tmp2],summary[tmp],pch=c(3,2,1)[tmp2],col=c(1,1,2)[tmp2])
        points(c(2017,2015,2015)[tmp2],summary[tmp],pch=c(3,2,1)[tmp2],col=c(1,1,2)[tmp2])
        tmp3 <- c(TRUE,tmp2,rep(TRUE,3))
        legend("topleft",col=c("chartreuse3",1,1,2,"blue","red")[tmp3],
               bg="white",
               pch=c(NA,3,2,1,20,20)[tmp3],legend=c("MSY","ABC2017","ABC2015","newABC2015","Projection (Fcur)","Projection (Fmsy)")[tmp3],ncol=2,cex=0.8,
               lty=c(1,NA,NA,NA,1,1)[tmp3])


        
        title("Catch")    
        
                                        # plot exploitation rates
        y <- y/as.numeric(colSums(dmodel$baa))
        aa <- summary[c("U_MSY")]
        plot(years,y,ylim=c(0,1.1*max(c(y,unlist(aa)))),xlab="Years",ylab="Exploitation rates (Catch/B)",type="o",
             xlim=c(min(years),max(years)+10))

        # projection
        points(rownames(fout0$vssb),apply(fout0$vwcaa,1,mean)/apply(fout0$vbiom,1,mean),type="o",pch=20,
               col="blue")
        points(rownames(fout1$vssb),apply(fout1$vwcaa,1,mean)/apply(fout1$vbiom,1,mean),type="o",pch=20,
               col=2)

        
        abline(h=aa,col=c("chartreuse3"))
        legend("topright",col=c("chartreuse3"),lty=1,legend=c("U_MSY"))
        title("Exploitation rates (U)")

        ## plot %SPR
        #y <- dmodel$SPR$ysdata$perSPR
        y <- get.SPR(res.pma)$ysdata$perSPR
        plot(years,y,ylim=c(0,100),xlab="Years",ylab="%SPR",type="o",
             xlim=c(min(years),max(years)))

        aa <- summary[c("SSB_MSY","SSB0")]                    
        abline(h=aa[1]/aa[2]*100,col=c("chartreuse3"))
        legend("topright",col=c("chartreuse3"),lty=1,legend=c("SSB_MSY/SSB0"))
        title("%SPR")
        
    }

    if(3%in%detail.plot){        
        layout(t(matrix(c(1,2,3,3,4,4,5,5),2,4)),heights=c(1,2,1,1))
        tres0 <- res00$trace[[1]]
        ssb <- res00$trace[[1]]$ssb.mean/biomass.scale
        tmp <- substr(colnames(tres0),1,5)=="TB-MA"
        tb <- tres0[,tmp]/biomass.scale
        tb2 <- sapply(1:ncol(tb),function(x) apply(tb[,1:x,drop=F],1,sum,na.rm=T))
        tmp <- substr(colnames(tres0),1,5)=="TC-MA"
        tc <- tres0[,tmp]/biomass.scale
        tc2 <- sapply(1:ncol(tc),function(x) apply(tc[,1:x,drop=F],1,sum,na.rm=T))
        library(png)
        if(file.exists(pngfile)) image <- readPNG(pngfile)
        else image <- NULL

#        library(RColorBrewer)
#        if(ncol(tc)<10)    col.tmp <- brewer.pal(ncol(tc),"Greens")
#        else col.tmp <- c(brewer.pal(9,"Greens"),rev(brewer.pal(ifelse(ncol(tc)-9<3,3,ncol(tc)-9),"GnBu")))
        col.tmp1 <- rgb(40/255,96/255,163/255,seq(from=0.1,to=0.9,length=ncol(tc)))
        col.tmp2 <- rgb(210/255,94/255,44/255,seq(from=0.1,to=0.9,length=ncol(tc)))                        

        ## plot table
        par(mar=c(1,4.3,3,2))
        plot.info(summary1[c(2,4,5,6,10,11,12,14),])    
        title(dmodel$jstockid)

        plot.SR(res0,what.plot=target,pick="SSB_MSY",xyscale=c(1.8,1.3),
                is.legend=FALSE)    

        par(mar=c(2,4.3,3,4.3))    
        scale <- max(ssb,na.rm=T) * 0.1
                                        #    ysize <- max(ssb,na.rm=T)/max(tb,na.rm=T)
        year.tmp <- rev(colnames(res0$vpares$ssb))[1:5]
        range1 <- range(res0$vpares$ssb)/biomass.scale
        range2 <- range(as.data.frame(res0$vpares$ssb)[as.character(year.tmp)])/biomass.scale


        ### plot of SSB
        ssb.max <- max(c(range1,summary$"SSB_MSY"),na.rm=T) *1.5 /biomass.scale
        tb3 <- tb2[which(ssb<ssb.max),]
        matplot(ssb,tb2,type="n",xlab="",ylab=paste("Biomass (",biomass.scale,")",sep=""),xaxs="i",yaxs="i",
                ylim=c(0,max(tb2[which(ssb<ssb.max),])*1.2),xlim=c(0,ssb.max))
                                        #    menplot(range1,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.9),border=NA)
                                        #    menplot(range2,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.7),border=NA)
        non.na <- !is.na(ssb)
        for(i in 1:ncol(tb2)) menplot(ssb[non.na], cbind(0,tb2)[non.na,i:(i+1)],col=col.tmp1[i],border=NA)
        waa.tmp <- (apply(res0$vpares$input$dat$waa,1,mean))^(1/3)*10
        waa.tmp <- waa.tmp/max(waa.tmp) * 0.9
        x <- tb3[1,]
        if(!is.null(image)){
            plotfish(image,x=rep(ssb.max*0.88,ncol(tb)),y=x-diff(c(0,x))/2,
                     size=waa.tmp,scale=scale,ysize=1)
        }
        text(rep(ssb.max*0.93,ncol(tb)),x-diff(c(0,x))/2,
             paste(0:(ncol(tb2)-1),"y/o: ",round(apply(res0$vpares$input$dat$waa,1,mean),0),""),cex=1)

        matpoints(ssb,tb2[,1],type="l",lwd=2,col="gray",lty=3)
        points(x <- colSums(res0$vpares$ssb)/biomass.scale,
               y <- colSums(res0$vpares$baa)/biomass.scale,type="o",
               col=gray(c(seq(from=0.7,to=0,length=length(x)))),pch=20,cex=1.2,
               lwd=3)
        text(x[1],y[1],colnames(x)[1],adj=0)
        text(rev(x[1]),rev(y)[1],rev(colnames(x))[1],adj=0)
                                        #    browser()    
        abline(v=summary$"SSB_MSY"/biomass.scale,lty=2)
        abline(v=summary$"Blimit"/biomass.scale,lty=2)
        abline(v=summary$"SSB_HS"/biomass.scale,lty=2)            
        text(summary$"SSB_MSY"/biomass.scale,max(tb3)*1.1,
             paste("SSB_MSY=",format(round(summary$"SSB_MSY"/biomass.scale),big.mark=","),"",sep=""),adj=0)
        text(summary$"Blimit"/biomass.scale,max(tb3)*1.0,
             paste("SSB_limit=",format(round(summary$"Blimit"/biomass.scale),big.mark=","),
                   "",sep=""),adj=0)
        text(summary$"SSB_HS"/biomass.scale,max(tb3)*1.05,
             paste("SSB_HS=",format(round(summary$"SSB_HS"/biomass.scale),big.mark=","),
                   "",sep=""),adj=0)
                                        #    text(max(ssb)*0.8,max(tb[,1],na.rm=T)*1.05,"予測加入量",col=2)
        ## plot of SSB CV
        par(new=T)
        y <- res00$trace[[1]]$ssb.CV
        plot(ssb,y,type="l",lwd=3,col=rgb(0.8,0.8,0,0.6),axes=F,xlab="",ylab="",
             ylim=c(0,ifelse(max(y,na.rm=T)>1.5,1.5,max(y,na.rm=T))))
        axis(side=4)
        mtext(side=4,"CV of SSB",line=3,col=rgb(0.8,0.8,0,0.6),cex=0.8)
        
        ##  catch
        par(mar=c(2,4.3,1,4.3))
        if(!is.null(res0$vpares$wcaa)) wcatch <- as.numeric(colSums(res0$vpares$wcaa))
        else{
            wcatch <- as.numeric(colSums(res0$vpares$input$dat$caa * res0$vpares$input$dat$waa/biomass.scale,na.rm=T))
        }
        matplot(ssb,tc2,type="n",,xaxs="i",yaxs="i",ylab=paste("Catch (",biomass.scale,")",sep=""),
                                        #            ylim=c(0,max(tc2,wcatch)*1.2),xlim=c(0,ssb.max))
                ylim=c(0,max(tc2)*1.2),xlim=c(0,ssb.max))

                                        #    menplot(range1,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.9),border=NA)
                                        #    menplot(range2,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.7),border=NA)    
        scale <- max(ssb)/max(tc2)
        for(i in 1:ncol(tc2)) menplot(ssb[non.na], cbind(0,tc2)[non.na,i:(i+1)],col=col.tmp2[i],border=NA)
        abline(v=summary$"SSB_MSY"/biomass.scale,lty=2)
        abline(v=summary$"Blimit"/biomass.scale,lty=2)
        abline(v=summary$"SSB_HS"/biomass.scale,lty=2)                    
        points(x <- as.numeric(colSums(res0$vpares$ssb))/biomass.scale,
               y <- wcatch,pch=20,lwd=3,
               type="o",col=gray(c(seq(from=0.7,to=0,length=length(x)))))
        text(x[1],y[1],colnames(res0$vpares$ssb)[1],adj=0)
        text(rev(x)[1],rev(y)[1],rev(colnames(res0$vpares$ssb))[1],adj=0)
        points(x <- apply(res00$fout[[1]]$vssb,1,mean)[1:10]/biomass.scale,
               y <- apply(res00$fout[[1]]$vwcaa,1,mean)[1:10]/biomass.scale,col=2,
               type="o",pch=20,lwd=3)
        text(rev(x)[1],rev(y)[1],
             paste("Projection ",rownames(res00$fout[[1]]$vssb)[10],"(F_MSY)",sep=""),adj=-0.1,col=2)

        points(x <- apply(fout0$vssb,1,mean)[1:10]/biomass.scale,
               y <- apply(fout0$vwcaa,1,mean)[1:10]/biomass.scale,col="blue",type="o",pch=20,lwd=3)
        text(rev(x)[1],rev(y)[1],
             paste("Projection ",rownames(res00$fout[[1]]$vssb)[10],"(F_current)",sep=""),adj=-0.1,col="blue")

        ## plot of catch CV
        par(new=T)
        y <- res00$trace[[1]]$catch.CV
        plot(ssb,y,type="l",lwd=2,col=rgb(0.8,0.8,0,0.6),axes=F,xlab="",ylab="",
             ylim=c(0,ifelse(max(y,na.rm=T)>1.5,1.5,max(y,na.rm=T))))
        axis(side=4)
        mtext(side=4,"CV of Catch",line=3,col=rgb(0.8,0.8,0,0.6),cex=0.8)        
        
        ## 努力量 plot
        par(mar=c(4.3,4.3,2,4.3))
        tmp <- round(ssb*biomass.scale)>0 & !is.na(ssb)
        matplot(ssb,tres0$fmulti,type="n",ylab="Fishing efforts (current=1)",xaxs="i",yaxs="i",xlab=paste("SSB(",biomass.scale,")",sep=""),xlim=c(0,ssb.max),
                ylim=c(0,max(tres0$fmulti[tmp]*1.2)))
                                        #    menplot(range1,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.9),border=NA)
                                        #    menplot(range2,cbind(c(-100,-100),rep(max(tb2)*1.5,2)),col=gray(0.7),border=NA)
        menplot(ssb[tmp],cbind(0,tres0$fmulti[tmp]),col=rgb(221/255,159/255,33/255,0.5),border=NA)
        if(length(res0$hs$Fhist)>0){
            points(x <- unlist(colSums(res0$vpares$ssb))/biomass.scale,
                   y <- res0$hs$Fhist[[1]]$fmulti,pch=20,lwd=3,
                   type="o",col=gray(c(seq(from=0.7,to=0,length=length(x)))))
        }
        abline(h=1,lty=2)
        abline(v=summary$"SSB_MSY"/biomass.scale,lty=2)
        abline(v=summary$"Blimit"/biomass.scale,lty=2)
        abline(v=summary$"SSB_HS"/biomass.scale,lty=2)                            
                                        #    points(x <- ssb[which.min(abs(tres0$fmulti-1))],y <- tres0$fmulti[which.min(abs(tres0$fmulti-1))],pch=4)
                                        #    text(x,y,"Recent 3 years",adj=-0.1)

    }
    else{
        a <- NULL
    }

    Fcurrent <- list(wcatch=mean(fout0$vwcaa[nrow(fout0$vwcaa),],na.rm=T),
                     ssb=mean(fout0$vssb[nrow(fout0$vssb),],na.rm=T))
    
    invisible(list(a=a,summary=summary,Fcurrent=Fcurrent))
    
}

plot.info <- function(a,xpos=7){
    plot(1:(nrow(a)+2),type="n",ylab="",xlab="",axes=F)
    units <- ceiling(-1*log10(a[,2]))
    units <- units + 2
    units <- ifelse(units<0,0,units)
    for(i in 1:nrow(a)){
      text(1,nrow(a)-i+2,a[i,1],adj=c(0,1),cex=1)
      text(xpos,nrow(a)-i+2,format(round(a[i,2],units[i]),big.mark=",",
                                   scientific=F),adj=c(1,1))
    }
}

plotfish <- function(image,x,y,size,scale=1,ysize=1){
#    image <- readJPEG("../buri.jpg")
    xx <- dim(image)[1]/dim(image)[2]
    rasterImage(image, 
                x-size*xinch(1), y-size*yinch(1)*xx*ysize, x+size*xinch(1), y+size*yinch(1)*xx*ysize)
}

menplot <- function(x,y,line.col=1,...){
    polygon(c(x,rev(x)),c(y[,1],rev(y[,2])),...)
    if(dim(y)[[2]]>2) points(x,y[,3],type="l",lwd=2,col=line.col)
}

menplot2 <- function(xy,probs=c(0.1,0.9),new=FALSE,xlab=NULL,ylab=NULL,...){
    xx <- rownames(xy)
    yy <- t(apply(xy,1,quantile,probs=c(0.1,0.9)))
    if(isTRUE(new)) matplot(xx,yy,type="n",xlab=xlab,ylab=ylab)
    menplot(xx,yy,...)
}


plotyield <- function(res00,int.res=NULL,detail.plot=FALSE){
#    par(mfrow=c(2,1))
    arg.tmp <- res00$farg
    arg.tmp$rec.arg$sd <- 0
    arg.tmp$N <- 1    
#    fout.tmp <- do.call(future.vpa2,arg.tmp)

    # average
    plot(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$catch.mean,type="n",xlim=c(0,max(x)),
         xlab="Multiplier to current F",ylab="Catch weight",ylim=c(0,max(res00$trace[[1]]$catch.det,y)))
    menplot(res00$trace[[1]]$fmulti,cbind(res00$trace[[1]]$catch.L10,res00$trace[[1]]$catch.H10),
            col=rgb(210/255,94/255,44/255,0.3),border=NA)    

    ## integrate
    if(!is.null(int.res)){
        points(x,y <- int.res$yield,lty=2,type="o",lwd=1,col="gray")
        points(fmax5 <- x[which.max(y)],y[which.max(y)],pch=20,col="gray")
    }

    points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$catch.mean,type="l",xlim=c(0,max(x)),
           xlab="Multiplier to current F",ylab="Catch weight",ylim=c(0,max(res00$trace[[1]]$catch.det,y)))    
    points(fmax1 <- x[which.max(y)],y[which.max(y)],pch=20,col=1)    

    if(isTRUE(detail.plot)){    
    # geomean
        points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$catch.geomean,col=2,type="l",xlim=c(0,2))
    points(fmax2 <- x[which.max(y)],y[which.max(y)],pch=20,col=2)

    # median
        points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$catch.median,col=3,type="l",xlim=c(0,2))
        points(fmax3 <- x[which.max(y)],y[which.max(y)],pch=20,col=3)
    }

    # deteministic
    points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$catch.det,col=4,
           type="l",xlim=c(0,2))
    points(fmax4 <- x[which.max(y)],y[which.max(y)],pch=20,col=4)

    title("Yield vs. F")

    ## plot CV of yield
    par(new=T)
    y <- res00$trace[[1]]$catch.CV
    plot(x,y,type="l",lwd=3,
         col=rgb(0.8,0.8,0,0.6),axes=F,xlab="",ylab="",
         ylim=c(0,ifelse(max(y,na.rm=T)>1.5,1.5,max(y,na.rm=T))))
    axis(side=4)
    mtext(side=4,"CV of catch",line=2.5,col=rgb(0.8,0.8,0,0.6),cex=0.8)    

    ### plot SSB
    plot(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$ssb.mean,type="n",xlim=c(0,max(x)),
         xlab="Relative F (to current F)",ylab="SSB")
    menplot(res00$trace[[1]]$fmulti,cbind(res00$trace[[1]]$ssb.L10,res00$trace[[1]]$ssb.H10),
            col=rgb(40/255,96/255,163/255,0.3),border=NA)        

    ## integrate
    if(!is.null(int.res)){
        points(x,y <- int.res$ssb,lty=2,type="o",lwd=1,col="gray")
        points(fmax5,y[x==fmax5],pch=20,col="gray")
    }
    
    points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$ssb.mean,type="l",xlim=c(0,max(x)),
         xlab="Relative F (to current F)",ylab="SSB")
    points(fmax1,y[x==fmax1],pch=20,col=1)

    if(isTRUE(detail.plot)){        
        points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$ssb.geomean,col=2,type="l",xlim=c(0,2))
        points(fmax2,y[x==fmax2],pch=20,col=2)
    
        points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$ssb.median,col=3,type="l",xlim=c(0,2))
        points(fmax3,y[x==fmax3],pch=20,col=3)
    }

    points(x <- res00$trace[[1]]$fmulti,y <- res00$trace[[1]]$ssb.det,
           col=4,type="l")
    points(fmax4,y[x==fmax4],pch=20,col=4)
    title("SSB vs. F")
    if(!is.null(int.res)){
        legend("topright",lty=c(1,1,1,1,2,NA),col=c(1:4,"gray",NA),legend=c("Simple mean","Geometric mean","Median","Deterministic","Integrate","fill: 80% conf"),bty="n")        
    }
    else{
        if(isTRUE(detail.plot)){
            legend("topright",lty=c(1,1,1,1,NA),col=c(1:4,NA),
                   legend=c("Simple mean","Geometric mean","Median","Deterministic","fill: 80% conf"))
        }
        else{
            legend("topright",lty=c(1,1,NA),col=c(c(1,4),NA),
                   legend=c("Simple mean","Deterministic","fill: 80% conf"))
        }
    }

    #### CV plot
    par(new=T)
    y <- res00$trace[[1]]$ssb.CV
    plot(x,y,type="l",lwd=3,
         col=rgb(0.8,0.8,0,0.6),axes=F,xlab="",ylab="",
         ylim=c(0,ifelse(max(y,na.rm=T)>1.5,1.5,max(y,na.rm=T))))
    axis(side=4)
    mtext(side=4,"CV of SSB",line=2.5,col=rgb(0.8,0.8,0,0.6),cex=0.8)        
    
#    points(fout.tmp$multi,fout.tmp$vssb[100,1],pch=4)
}

get.SPR <- function(dres){
    # Fの歴史的な%SPRを見てみる                                                                             
    # 毎年異なるFや生物パラメータに対して、YPR,SPR、SPR0がどのくらい変わっているのか見る(Rコード例2)        
    dres$ysdata <- matrix(0,ncol(dres$faa),4)
    dimnames(dres$ysdata) <- list(colnames(dres$faa),c("perSPR","YPR","SPR","SPR0"))
    for(i in 1:ncol(dres$faa)){
	dres$Fc.at.age <- dres$faa[,i] # Fc.at.ageに対象年のFAAを入れる                                     
        byear <- colnames(dres$faa)[i] # 何年の生物パラメータを使うか                                       
        # RVPAのref.F関数でYPRなどを計算。                                                                  
        # 配布している1.3から1.4にアップデートしているので、新しいほうの関数を使うこと(返り値がちょっと違う)
	a <- ref.F(dres,waa.year=byear,maa.year=byear,M.year=byear,rps.year=2000:2011,
                   F.range=c(seq(from=0,to=ceiling(max(dres$Fc.at.age,na.rm=T)*2),
                       length=101),max(dres$Fc.at.age,na.rm=T)),plot=FALSE)
        # YPRと%SPR                                                                                         
        dres$ysdata[i,1:2] <- (as.numeric(rev(a$ypr.spr[nrow(a$ypr.spr),-1])))
        # SPR                                                                                               
        dres$ysdata[i,3] <- a$spr0*dres$ysdata[i,1]/100
        # SPR0                                                                                              
        dres$ysdata[i,4] <- a$spr0
    }
    dres$ysdata <- as.data.frame(dres$ysdata)
    return(dres)
}
