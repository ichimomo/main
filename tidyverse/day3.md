3日目：tidyverseの中身と代表的な便利関数
----------------------------------------

いよいよtidyverseをインストールしました。3回目は、tidyverseで呼び出される複数のパッケージの中で、従来関数と互換性のある、でも、tidyverseのほうがちょっと便利な関数を紹介します。既存のR関数との比較はRをすでにけっこう知っている人向けの解説なので、R初修者はわからなくても気にしなくてもいいです。

### パッケージ：tibble

データを格納する形式である **tibble (←→ data.frame)** を提供。

**Q1**: tibbleとdata.frameの違いを比較してみよう！

    dat_f <- data.frame(a = 1:100, b = rnorm(100))  # 100行2列のデータフレームを作る
    dat_t <- tibble::tibble(a = 1:100, b = rnorm(100))  # 上と同じ操作
    dat_f

        a            b
    1   1 -1.528585118
    2   2 -0.028523499
    3   3  2.206266881
    4   4 -0.422555063
    5   5  1.151577270
    6   6  1.790286039
    7   7  0.794319870
    8   8 -0.488337838
    9   9  3.308197254
    10 10  0.894252584
    11 11  1.709600375
    12 12 -1.009990368
    13 13 -1.430300005
    14 14  0.473778813
    15 15  0.128039184
    16 16 -1.465024494
    17 17 -0.340616245
    18 18 -0.782958038
    19 19  0.668534645
    20 20 -0.469560151
    21 21  0.371053816
    22 22  0.325549725
    23 23 -0.306637668
    24 24 -0.754134030
    25 25 -0.433352375
    26 26 -0.628792635
    27 27  0.694517382
    28 28 -0.002651191
    29 29  0.873518080
    30 30 -0.680338376
    31 31  1.308500208
    32 32 -0.343558093
    33 33 -1.530673112
    34 34  0.800112071
    35 35  0.367570763
    36 36 -1.579761705
    37 37  0.863388003
     [ reached 'max' / getOption("max.print") -- omitted 63 rows ]

    dat_t

    # A tibble: 100 x 2
           a        b
       <int>    <dbl>
     1     1  1.28   
     2     2  0.00783
     3     3 -0.0674
     4     4  0.874  
     5     5 -0.798  
     6     6  0.0954
     7     7  0.633  
     8     8 -1.83   
     9     9 -2.11   
    10    10  0.543  
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

なぜ、.から\_へのシフトがおこったんでしょう？

20年前からRを使っていた人は、オブジェクトや関数の名前に絶対\_を入れませんでした。というのは、昔、Rでは
\_ が「代入記号」として利用されていたんです。なんで、うっかり `dat_1`
なんて書いてしまうと、`dat <- 1`
または`dat = 1`と同じ動きをして、プログラムがめちゃくちゃになったのです、、。

しかし、 この仕様はかなり昔に変更され、あと、プログラミングの
[命名規約](https://ja.wikipedia.org/wiki/%E5%91%BD%E5%90%8D%E8%A6%8F%E5%89%87_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0))
では\_を使うほうが良いということもあり、私もようやく最近オブジェクト名に
\_を使えるようになりました。

ということで、frasyrでも、関数名やオブジェクト名に.と\_が混在していますが、.で区切られた名前の関数はたいてい古く、\_で区切られた名前の関数や関数の引数は比較的最近作られた関数だと区別してください（上に紹介したread.csvとread\_csvの違いのように）。たとえば、frasyrにはplot.futuresとplot\_futuresという関数がありますが、前者は裸のRで書かれた古い（素朴な）関数で、後者はggplot2で書かれた新しい関数です。

（.で区切られた関数名は「メソッド」という新しい別の機能が付与されるため、けっこう使いづらく、最近ではむしろ避けられています。）
