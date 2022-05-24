#'
#' ## Rで使えるデバッグ技まとめ
#'
#' 自分で関数を作っていたがバグがありそう、、。エラーが出て止まってしまった、、。そんなときに役立つ関数をまとめました。
#'
#' ### mainfuncの定義
#'
#' とりあえず実行するとerrorを返す関数mainfuncを定義します。このあと、debug関数を試すのに使っていきます。
#'
#' 

#+ eval=FALSE

# example function

add_func <- function(x,y){
  return(x+y)
}

log_func <- function(x,y){
  res <- sum(list(log(x),log(y)))
  return(res)
}

mainfunc <- function(nsim){
  res1 <- res2 <- list()
  for(i in 1:nsim){
    x <- rnorm(1,mean=2)
    y <- rnorm(1,mean=2)
    res1[i] <- add_func(x,y)
    res2[i] <- log_func(x,y)
  }
  allres <- c(res1,res2)
  return(allres)
}

mainfunc(10)

#'
#' ## `traceback()`
#'
#' - エラーが出た直後に打つコマンド
#' - 実行した関数の中でどの部分でエラーが出たのかわかる
#' - 深い入れ子状になっている関数のときに便利
#' - 単純なバグならこれだけで原因がわかる場合も
#' 

#+ eval=FALSE

# エラーを出す関数を実行
mainfunc(10)
# tracebackを実行
traceback()

#'
#' ## `debug()`
#'
#' - `debug()`で指定した関数を上から１行づつ実行していく
#' - `undebug()`でデバッグモードを開くのを停止する
#' - 関数が入れ子状になっている場合、内側の関数を指定してそこだけをdebugすることも可能
#' - 自作関数でなくても、どんな関数でも適用できるため、パッケージに搭載されている関数の挙動を調べたいときにも有用
#' - 関数が実行されると「デバッグモード」に入る
#'    - デバッグモードでは関数が一行一行実行される
#'    - `Browse[2]> ` みたいなコマンドプロンプトに変わり、次に実行するRコードが出力される
#'    - for loopがある場合にはfor loopの中も一つづつ実行される
#'    - デバッグモード中の特殊キー（ほかにもあるかも）
#'       - 「c」for loop内の計算をすべて実行して次の行に移る
#'       - 「Q」デバッグモードを終了する 
#' 

#+ eval=FALSE

# mainfuncをdebugモードで実行する
debug(mainfunc)
# そのあとに該当する関数を実行すると「デバッグモード」に入る
mainfunc(10)
undebug(mainfunc)

# 実際にエラーがおこる関数のみデバッグモードをかけると、その関数が実行されたところからデバッグモードに入る
debug(log_func)
mainfunc(10)
undebug(mainfunc)

# 自作関数以外でも適用可能なので、その関数が何をやっているのかを理解するのにも有用
library(spict)
debug(fit.spict)
a <- fit.spict(rnorm(100))

#'
#'
#' ## `options(error=recover)`
#'
#' - エラーがおこったタイミングで、そのエラーを吐く行を実行する直前に戻ってデバッグモードに入る
#' - 特定の関数に適用しているのではなく環境全体を変えているので、どんな関数を実行しても同じ挙動になる
#' - やめたい場合は`options(error=NULL)`
#' - 挙動
#'    1. 入れ子状の関数のどの階層でdebug modeに入るか聞かれるので番号で入りたい階層を選ぶ
#'    2. debug modeに入ると、一行一行実行するモードに入る→Enterを押すとエラーを吐いて1の状態に戻る。Enterを押さないように、現状がどんな感じになっているのか調べる
#'    3. 調べるのが終わったらEnterを押して1に戻って0を押すとデバッグモードが終了する
#' - 使いどころ
#'    - `traceback()`見たけどどこでエラーがおこっているのかよくわからない...
#'    - かといって`debug()`で上から一行づつ実行していくと時間がめちゃくちゃかかる（プログラムが長すぎる、入れ子構造が多すぎるなど）
#'

#+  eval=FALSE

options(error=recover)
mainfunc(10)
options(error=NULL)


#'
#' ## `browser()`
#' - この行が実行されるとデバッグモードに入る
#' - 使いどころ
#'    - 特殊な条件でのみエラーがおこる場合
#'    - デバッグ目的でなくても、ある条件になった場合に新しいコードを追加したい場合
#'    - 関数定義をし直さなければいけないので若干ハードルは高いかも
#'    - エラーは吐かないが明らかに値がおかしい場合などに怪しい場所をつきとめるのに使ったり
#'

#+  eval=FALSE

# log_funcにbrowser()を挿入して定義しなおす
log_func <- function(x,y){
  browser()  
  res <- sum(list(log(x),log(y)))
  return(res)
}

mainfunc(10)

#'
#' ## 番外編：`try()`
#' - エラーが出ても気にせずに計算を進めてくれる
#'


#+ eval=FALSE

log_func <- function(x,y){
  res <- sum(list(log(x),log(y)))
  return(res)
}

mainfunc <- function(nsim){
  res1 <- res2 <- list()
  for(i in 1:nsim){
    x <- rnorm(1,mean=2)
    y <- rnorm(1,mean=2)
    res1[i] <- add_func(x,y)
    tmp <- try(log_func(x,y))
    if(class(tmp)=="try-error") res2[i] <- NA else res2[i] <- tmp
  }
  allres <- c(res1,res2)
  return(allres)
}

mainfunc(10)

#'
#' ## 番外編2：`options(warn=1)`
#'
#' - `warning()`は普段は特に気にしなくても良いものだが、気にしなければいけないwaningもある
#'    - NAになるはずがないのにNAになってしまっている
#'    - `置き換えるべき項目数が、置き換える数の倍数ではありませんでした`みたいな
#' - でもどのタイミングでwarningが出ているかわからない→`options(warn=1)` この数字: warningをどれだけ重大なこととして扱うか：（0: ちょっと引っかかるけど関数をストップさせるほどではない, 1: エラーでしょ）　→ `options(warn=1); options(error=recover) ; ` とすればwarningの箇所も特定できる
#' 
