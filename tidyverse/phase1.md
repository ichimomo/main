1週目
=====

１回目：趣旨
------------

tidyverseって知ってますか？Rから呼び出せる一連の「便利パッケージ」を集めたメタパッケージのことです。非常に便利なので、VPA計算や将来予測ツールを提供しているfrasyrも、今後、tidyverseをどんどん取り入れていく方針に転換しています。普通にRを使って自分のデータ解析をしている人だけでなく、frasyrパッケージを使って資源計算されている人も、ぜひtidyverseの世界を知ってほしいと思っています。

それによって

-   整理されたデータ（整然データ、今後解説）を使った一連のデータ処理プロセスの固定化・単純化→研究の効率アップ
-   tidyverseの標準グラフィックスシステムであるggplot2を使った効率的なグラフ作成→研究の効率アップ
-   frasyr,
    frasyr\_toolのバグ発見や修正依頼、さらには拡張コードを書くことへの障壁が少なく→皆でfrasyrを共同開発

のような効果が期待できます。

ここではできるだけ1回で１トピック、tidyverseで使うパッケージや関数の使い方、従来のR（裸のRと呼ぶことも）との違いなんかを、書きき込んでいきます。その後、何か思うところあったら、または追加の情報などをコメントとして返信してもらうことで、勉強を進めていければと思います。

今後想定するトピックとしては - tidy data（整然データ）とは何か？ -
パイプ %&gt;%の使い方 - ggplot2 - データ成型用パッケージ dplyr
を使ったデータの整理 - などなど
です。私も細かいことあまり知らないので、より分かっている人にさらに解説してもらったりできればうれしいです。

初回は参考URLを示すことで終わりにします。来週からぽつぽつアップしますので、よろしくお願いします。

-   本家のサイト　<a href="https://r4ds.had.co.nz/" class="uri">https://r4ds.had.co.nz/</a>　英語だけど本一冊分が無料で読めます。これを１冊通して読めばtidyverseマスター！
-   上のサイトの日本語訳　<a href="https://www.amazon.co.jp/R%E3%81%A7%E3%81%AF%E3%81%98%E3%82%81%E3%82%8B%E3%83%87%E3%83%BC%E3%82%BF%E3%82%B5%E3%82%A4%E3%82%A8%E3%83%B3%E3%82%B9-Hadley-Wickham/dp/487311814X" class="uri">https://www.amazon.co.jp/R%E3%81%A7%E3%81%AF%E3%81%98%E3%82%81%E3%82%8B%E3%83%87%E3%83%BC%E3%82%BF%E3%82%B5%E3%82%A4%E3%82%A8%E3%83%B3%E3%82%B9-Hadley-Wickham/dp/487311814X</a>
-   tidyverseの概説とさらなる参考情報　<a href="https://heavywatal.github.io/rstats/programming.html" class="uri">https://heavywatal.github.io/rstats/programming.html</a>

2回目：インストールと使い方
---------------------------

tidyverseの世界に入るときに一番最初にやること、、それは、tidyverseパッケージのインストールです。パッケージのインストール方法はいろいろありますが、コピペで済むのでここでは以下のコマンドでインストールすることにします。コマンドを打つと、「どこのサーバーからパッケージをダウンロードするか？」を聞かれることがあるので、そこはJapan(Tokyo)を選んで（多分どこを選んでもよい）、インストールします。複数のパッケージを同時にインストールするので時間がかかります。

    install.packages("tidyverse")

無事インストールが終わったら、以下のコマンドを打ってパッケージを呼び出してみましょう。

    library(tidyverse)

すると、あまり見慣れない -- Attaching packages --
とかいう表記が出てきます。

-- Attaching
packagesの下は、tidyverseをlibraryしたことで呼び出されるパッケージの一覧（tidyverseは複数のパッケージを寄せ集めたメタパッケージなので）とそれぞれのパッケージのバージョン番号を示しています。

--
Conflictの下には、tidyverseパッケージを呼び出すことで生じた関数名のコンフリクト（異なるパッケージ間で同じ関数名が使われている）の一覧が示されています。
x dplyr::filter() masks stats::filter()
は、「パッケージdplyrとstatの両方でfilterという関数が定義されているけど、ここでは単にfilterを呼ぶ場合はdplyrのfilterを使いますよ」という意味です。

また、ここで出てくる「::」は、用いる関数名がなんのパッケージ由来のものかを明示的に示すための記号です（つまり　**パッケージ名::関数名**
）。この方法で関数を指定すれば、library(パッケージ名)という形であらかじめパッケージを呼び出しておかなくても、自分の環境内にインストールされたパッケージの中の関数を利用することができます。逆に、**パッケージ名::**
なんてつけるのめんどくさいという人は、あらかじめ自分が使う関数をlibraryで呼び出しておけばいいわけです。ただし、上記のように、複数のパッケージ間で同じ関数名を使う場合はコンフリクトがおこるので（どういう状況でどちらのパッケージが優先されるかは？なんですが）、関数が意図下通りに動かずにエラーやバグが生じる恐れがあります。特に、dplyrのfilterは非常によく使い関数なんですが、statsパッケージのfilterと同名の関数になっているため、ライブラリの呼び出しの順番によってfilterがstatsのfilterになったりdplyrのfilterになったりします。ので、filterだけにはdplyr::filterとして使ったほうが良いかと思います。

    ## ::の例 (まだtidyverseをlibraryしていない状況下でお試しください）

    # パッケージtibble内にある関数tibbleを使う ::をつければlibraryする必要はない
    data_test <- tibble::tibble(a = 1:5, b = 2:6)

    # パッケージ内の関数を呼び出してから使う場合 libraryしてあるので::は必要ない
    library(tibble)  # or library(tidyverse)
    data_test <- tibble(ax = 1:5, bx = 2:6)

（**注**
frasyrやfrasyr\_toolでは、tidyverse内の関数をすべて「パッケージ名::関数名」としてはいません（かわりに、Rコードの最初でlibrary(tidyverse)をする必要があります）。ただし、dplyr::filterなど、コンフリクトが起こりそうな関数については::をつけています。また、gridExtraやforeachなど、使用頻度の高くないパッケージについては::をつけて呼び出しています。）

最後、ちょっとマニアックな話になりましたが、たまに目にする::の意味を知らないと、Rのコードを読んでいて::に遭遇したときの不安感が非常に大きいと思いますので、少し説明してみました。

次回は、tidyverseの中のパッケージがそれぞれ何をしているかの概要をお話しします。

3日目：tidyverseの中身と代表的な便利関数
----------------------------------------

いよいよtidyverseをインストールしました。3回目は、tidyverseで呼び出される複数のパッケージの中で、従来関数と互換性のある、でも、tidyverseのほうがちょっと便利な関数を紹介します。既存のR関数との比較はRをすでにけっこう知っている人向けの解説なので、R初修者はわからなくても気にしなくてもいいです。

### パッケージ：tibble

データを格納する形式である **tibble (←→ data.frame)** を提供。

**Q1**: tibbleとdata.frameの違いを比較してみよう！

    dat_f <- data.frame(a = 1:100, b = rnorm(100))  # 100行2列のデータフレームを作る
    dat_t <- tibble::tibble(a = 1:100, b = rnorm(100))  # 上と同じ操作
    dat_f

        a           b
    1   1  0.30388952
    2   2 -0.28320606
    3   3  0.39485070
    4   4 -0.50007695
    5   5  0.61257203
    6   6  0.46164096
    7   7  0.77802718
    8   8  1.29078667
    9   9  1.21527241
    10 10 -1.25362596
    11 11 -1.40932321
    12 12  0.80773917
    13 13 -1.34928100
    14 14  0.17386337
    15 15  0.03362471
    16 16 -0.26651315
    17 17  1.69983884
    18 18 -0.57846829
    19 19 -2.60807959
    20 20  0.69548702
    21 21 -0.80740260
    22 22  1.80110637
    23 23  0.72372422
    24 24  0.52786269
    25 25  0.41258205
    26 26 -0.93934356
    27 27  1.36146694
    28 28 -0.71352824
    29 29  1.39939691
    30 30  0.94473351
    31 31 -0.35860825
    32 32  0.67804109
    33 33 -0.48234909
    34 34 -0.24762564
    35 35 -0.53408146
    36 36 -1.35747584
    37 37  1.08569302
     [ reached 'max' / getOption("max.print") -- omitted 63 rows ]

    dat_t

    # A tibble: 100 x 2
           a      b
       <int>  <dbl>
     1     1  0.520
     2     2 -0.389
     3     3  0.253
     4     4  1.68 
     5     5  0.935
     6     6  1.09 
     7     7 -1.55 
     8     8 -1.43 
     9     9 -2.30 
    10    10  1.54 
    # ... with 90 more rows

**A1**:
中身はほとんど同じだけど、中身を表示させたときの挙動が違うよ。通常のdata.frameは、全データを単純に全部出力しようとします。一方、tibbleの場合は行数や列数が長すぎるばあいは途中で省略し（\#
… with 90 more rows（それとあと90行）,
という表記）、また、冒頭にデータフレームの構造（\# A tibble: 100x2
(100列２行のtibble形式の行列、それぞれの列の「型」がなんであるか（<int>(整数)
<dbl>(連続数)など）の情報を出力します。

あと、リストを作成する **lst (←→ list)**
も便利だということを最近知りました。

**Q2**: lstとlistの違いを比較してみよう！

    a <- 1:10
    b <- c("char1", "char2", "char3")
    list_list <- list(a, b)
    list_lst <- tibble::lst(a, b)
    list_list

    [[1]]
     [1]  1  2  3  4  5  6  7  8  9 10

    [[2]]
    [1] "char1" "char2" "char3"

    list_lst

    $a
     [1]  1  2  3  4  5  6  7  8  9 10

    $b
    [1] "char1" "char2" "char3"

**A2**:
オブジェクトをまとめてリストにしたとき、lstは自動的にもともとのオブジェクトの名前がつくよ。関数の返り値をリスト形式にして返すとき、地味に便利です。

### パッケージ：readr

Rのパッケージの名前って、「機能」を示す単語＋r（R）というふうにつけるのが今風らしいです。なので、readrも、read
+
rと考えると、何かを「読む」ためのパッケージだということがわかります。同様に、stringrはstring
+
rなので、文字列を処理するための関数とわかります。（frasyrもそれにならって、FRA
+ SY (sustainable yield) + rということでつけてみました。）

readrはデータの入出力のための **read\_csv( ←→ read.csv)**, **write\_csv
(←→ write.csv)** などを提供しています。

**Q3**: read\_csvとread.csvの違いを比較してみよう！

    write_csv(tibble(a = 1:10, b = rnorm(10)), path = "test.csv")  # 読み込むためのデータを出力しておく
    dat_read1 <- read.csv("test.csv")
    dat_read2 <- readr::read_csv("test.csv")

**A3**:
read\_csvを使ってデータの読み込みに成功すると、読み込んだデータの概要を表示してくれます。また、読み込まれたデータは自動的にtibble形式になります。

その他パッケージ
----------------

その他パッケージは追い追い紹介していきますが、いちおう以下に一覧表を作ってみました。

### 表：tidyverseが提供するパッケージと代表的な機能

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr class="header">
<th>パッケージ名</th>
<th>説明</th>
<th>代表的な関数(括弧内は、tidyverseを使わない場合のRの関数)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>tibble</td>
<td>より柔軟な形式のデータフレーム<code>tibble</code>等を提供</td>
<td>データフレームを作成:<code>tibble</code> (<code>data.frame</code>), データをtibble形式に変換:<code>as_tibble</code>, リストを作成: <code>lst</code>(list)</td>
</tr>
<tr class="even">
<td>tidyr</td>
<td>データを横長データから縦長データへ、縦長データから横長データへ変換する</td>
<td>横長データを縦長データに変換する: <code>gather</code> (<code>as.data.frame.table</code>), 縦長データを横長データに変換する: <code>spread</code> (<code>tapply</code>)</td>
</tr>
<tr class="odd">
<td>dplyr</td>
<td>データを整形したり、変換したりする</td>
<td>列を抽出: <code>select</code>, 行を抽出: <code>filter</code>(<code>subset</code>), 列を追加または変更: <code>mutate</code>, データの並び替え: <code>arrange</code> (<code>sort</code>,<code>order</code>), パイプ演算子: <code>%&gt;%</code>(詳細はBOX)</td>
</tr>
<tr class="even">
<td>readr</td>
<td>データの読み込みと出力。読んだデータはtibble形式となる。</td>
<td><code>read_table</code>(<code>read.table</code>), <code>read_csv</code>(<code>read.csv</code>), <code>write_table</code>(<code>write.table</code>), <code>write_csv</code>(<code>write.csv</code>)</td>
</tr>
<tr class="odd">
<td>stringr</td>
<td>文字列操作</td>
<td><code>str_</code>で始まる関数群。文字列結合: <code>str_c</code>(<code>paste</code>)</td>
</tr>
<tr class="even">
<td>forcats</td>
<td>カテゴリカル変数を取扱う</td>
<td><code>fct_</code>で始まる関数群。他の変数を使ってカテゴリカル要素の順番(level)を付け替える: <code>fct_reorder</code>, 登場頻度が多い（少ない）順にlevelを付け替える: <code>fct_infreq</code></td>
</tr>
<tr class="odd">
<td>purrr</td>
<td>ループ処理やapply系関数</td>
<td><code>map_</code>で始まる関数群。各要素に関数を適用: <code>map</code>(<code>lapply</code>), mapしたのちにdata.frameとして結合: <code>map_dfr</code> (<code>sapply</code>)</td>
</tr>
<tr class="even">
<td>ggplot2</td>
<td>グラフ作成（詳細は後日)</td>
<td><code>ggplot</code>, <code>geom_</code>で始まる関数群</td>
</tr>
</tbody>
</table>

次回は、上の表にある「縦長データ」「横長データ」について解説します。

### どうでもいい？雑談： . と \_ の違いについて

裸のRの関数名は単語と単語の間を.で区切るものが多いです。一方、今風のRの関数は\_で区切るものが主流です。.の関数名はちょっと古い関数（または、メソッドという特別な機能が付与された関数）で、\_の関数名は新しめの関数と覚えておくと便利です（frasyrでもそうです）。

なぜ、.から.へのシフトがおこったんでしょう？

20年前からRを使っていた人は、オブジェクトや関数の名前に絶対\_を入れませんでした。というのは、昔、Rでは
\_ が「代入記号」として利用されていたんです。なんで、うっかり `dat_1`
なんて書いてしまうと、`dat <- 1`
または`dat = 1`と同じ動きをして、プログラムがめちゃくちゃになったのです、、。

しかし、この仕様はかなり昔に変更され、あと、プログラミングの
[命名規約](https://ja.wikipedia.org/wiki/%E5%91%BD%E5%90%8D%E8%A6%8F%E5%89%87_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0))
では\_を使うほうが良いということもあり、私もようやく最近\_をオブジェクト名に使えるようになりました。

ということで、frasyrでも、関数名やオブジェクト名に.と\_が混在していますが、.で区切られた名前の関数はたいてい古く、\_で区切られた名前の関数や関数の引数は比較的最近作られた関数だと区別していただけると幸いです（上に紹介したread.csvとread\_csvの違いのように）。frasyrでは、たとえば、plot.futuresとplot\_futuresという関数がありますが、前者は裸のRで書かれた古い（素朴な）関数で、後者はggplotで書かれた新しい関数です。

（.で区切られた関数名は「メソッド」という新しい別の機能が付与されるため、けっこう使いづらく、最近ではむしろ避けられています。）

４日目：tidy dataとは？
-----------------------

５日目：tidy data と dplyr
--------------------------

6日目：データの縦横変換
-----------------------

7日目：ggplot2とtidyverse (1)
-----------------------------

8日目：ggplot2とtidyverse (2)
-----------------------------
