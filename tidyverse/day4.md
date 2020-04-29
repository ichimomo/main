４日目：tidy data (整然データ) とは？
-------------------------------------

４日目はとうとうtidyverseの根幹である「整然データ(Wickham 2014)
」について解説します。tidyverseは、「ちょと便利な」機能を提供するためだけのものではなく、「整然データ」と呼ばれる形式のデータを解析するためのものなんです。では「整然データ」ってなんでしょうか？

非整然データと整然データ
------------------------

たとえば、A地点とB地点で、とある種のサンプリングをおこない、その個体群が減っているか、増えているかを調べる調査を実施したとします。
サンプリングでは網を一定時間ひいて、そこで採取された魚の数をカウントしました。そして、毎年の調査から以下のような結果が得られたとします。

### 表1. 非整然データ（横長データ）の例

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">2010</th>
<th style="text-align: right;">2011</th>
<th style="text-align: right;">2012</th>
<th style="text-align: right;">2013</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">地点A</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">720</td>
<td style="text-align: right;">420</td>
<td style="text-align: right;">100</td>
</tr>
<tr class="even">
<td style="text-align: left;">地点B</td>
<td style="text-align: right;">120</td>
<td style="text-align: right;">50</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">10</td>
</tr>
</tbody>
</table>

2010年から2013年までの毎年のサンプリングの結果、地点Aでは1000個体から100個体に、地点Bでは120個体から10個体に個体数が減少してます。どうやら、A地点でもB地点でも個体数は約10分の1に減っているようです。

このような形式の表は地点A,
B間で、年ごとの個体数の違いを比較するのには適していますが、調査データそのものをこのような形式で記録していこうとすると、データの記録や解析する際にはいろいろと不都合が生じます。例えば、

1.  それぞれの年・地点での調査は1回だけでなく複数回実施した
2.  採取された魚の数に影響する潜在的な要因（調査時の季節や天気など）も同時に記録し、それらの要因がCPUEに影響するかを見たい

ような場合です。見せるための表でなく、記録したり解析したりするためには、どのようにデータを記録していけばよいでしょう？

整然データ (tidy data､Wickham 2014)
は、このような問題を解決するために提唱されているデータの形式のことです。Wickham
(2014)は整然データの構造を

1.  個々の変数が1つの列をなす
2.  個々の観測が1つの行をなす
3.  個々の観測の構成単位の類型が1つの表をなす
4.  個々の値が1つのセルをなす

のように定義しています。特に重要な点は、整然データではひとつの列に個々の変数が、行に個々の観測が記録されるということです。
それに従って表1を整然データに直してみましょう。それは表2みたいになります。

### 表2. 整然データ（縦長データ）の例

<table>
<thead>
<tr class="header">
<th style="text-align: left;">area</th>
<th style="text-align: right;">year</th>
<th style="text-align: right;">cpue</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">地点A</td>
<td style="text-align: right;">2010</td>
<td style="text-align: right;">1000</td>
</tr>
<tr class="even">
<td style="text-align: left;">地点A</td>
<td style="text-align: right;">2011</td>
<td style="text-align: right;">720</td>
</tr>
<tr class="odd">
<td style="text-align: left;">地点A</td>
<td style="text-align: right;">2012</td>
<td style="text-align: right;">420</td>
</tr>
<tr class="even">
<td style="text-align: left;">地点A</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">100</td>
</tr>
<tr class="odd">
<td style="text-align: left;">地点B</td>
<td style="text-align: right;">2010</td>
<td style="text-align: right;">120</td>
</tr>
<tr class="even">
<td style="text-align: left;">地点B</td>
<td style="text-align: right;">2011</td>
<td style="text-align: right;">50</td>
</tr>
<tr class="odd">
<td style="text-align: left;">地点B</td>
<td style="text-align: right;">2012</td>
<td style="text-align: right;">20</td>
</tr>
<tr class="even">
<td style="text-align: left;">地点B</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">10</td>
</tr>
</tbody>
</table>

このような形式でデータを記録していれば、ある年・ある地点で観測を追加で行った場合でも行（＝観測）を追加するだけで簡単にデータを追加できます。また、それぞれの観測において不随する情報（＝変数）を追加で記録したい場合、列を増やせば簡単に記録できます。整然データであれば、行・列を増やすことでいくらでもデータも変数も追加できるのです。

整然データはデータ数が増えるほど表が縦に長くなるため、縦長データ（long-format）と呼ばれることもあります。それに対して、列と行がそれぞれ変数で構成された表１のようなデータは横長データ（wide-format）と呼ばれます。縦長データは、データ数が非常に多い場合にはメモリを消費してしまう欠点もあります。たとえば1千地点×100年の10万件のデータの場合、横長データではデータの保存におおよそ10万バイト（100KB）しか必要としないですが、横長データでは10万件のデータにそれぞれ地点のラベルと年のラベルをつけるため30万バイト(300KB)必要になります。

まとめ
------

**tidy data = 整然データ = 縦長データ**

**non-tidy data = 非整然データ =? 横長データ**

自分の手元にあるデータが整然データになっているか非整然データになっているか、確認してみましょう。もし自分のデータが非整然データである場合に、非整然データを整然データに変換する作業がデータ解析の第一歩になります。次回は、表１で示したような形式の横長データを縦長データに成型するための方法などを紹介します。Teamsで話題になった`dplyr::pivot_longer`を使っていくことになります。（古い関数としては`dplyr::gather`に対応します）。

frasyrにおけるtidy dataの取り扱い
---------------------------------

frasyrでは、VPAへのインプットデータ、また、関数を適用したあとの出力データはtidy形式ではありません。ただ、出力データをtidy形式に変換するための関数は実装しています（以下、参照。名前の統一感がないのが課題ですが、、）。ggplotを使った描画関数は、これらのtidy形式に変換したデータを使って描画するようにする方式に変えようとしているので、将来的には、独自の関数を使った出力結果でも、同じtidy形式で揃えれば、frasyrの描画関数が適用できるようにしたいと思っています。
