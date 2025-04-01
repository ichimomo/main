- [このページのHP版](https://ichimomo.github.io/main/)
- [昔のホームページ(更新停止中、古い情報を含みます)](https://ichimomo.github.io/main/home-page/index.html)

### [frasyr](https://github.com/ichimomo/frasyr)
   - 1系資源のVPA計算・管理基準値計算・将来予測シミュレーションを行うための関数を集めたRパッケージ
   - ブランチ構成
      - dev: 最新版です。節目節目でバージョン番号をつけますが、不定期なので、パッケージのバージョンを引用したい場合はコミット番号をご利用ください。
   - インストール方法などは、トップページの[README](https://github.com/ichimomo/frasyr/blob/master/README.md)を見るか、以下のfrasryr_toolで配布されているスクリプトを使ってインストールしてください。

### [fraysr_tool](https://github.com/ichimomo/frasyr_tool)
   - frasyrを動かすためのRスクリプトを集めたファイル集（パッケージではありません）。水研内部のみのプライベートレポジトリになります。利用したい場合はgithubのIDを取得し、IDを市野川までご連絡ください。
   - 使い方はトップページの[README](https://github.com/ichimomo/frasyr_tool/blob/master/README.md)をご参照ください

### 各種情報ローカル
 - VPA
   - [資源量推定の方法](https://ichimomo.github.io/frasyr/articles/vpa.html)
   - [モデル診断の方法](https://ichimomo.github.io/frasyr/articles/Diagnostics-for-VPA.html)
- 再生産関係
  - [推定方法](https://ichimomo.github.io/frasyr/articles/fittingSR.html)
  - [モデル診断方法](https://github.com/ichimomo/frasyr_tool/tree/YR2024/SRcheck) ※(プライベートリポジトリです)
- [将来予測](https://github.com/ichimomo/frasyr/wiki/future_new) 
- [開発ワークフロー](https://github.com/ichimomo/frasyr/wiki/Development-Policy)

- [frasyr_toolのWiki](https://github.com/ichimomo/frasyr_tool/wiki)には以下の情報があります
   - [fraysr_toolのスクリプトがうまく動かない場合の相談のしかた](https://github.com/ichimomo/frasyr_tool/wiki/%E3%83%90%E3%82%B0%E5%A0%B1%E5%91%8A%E3%83%BB%E3%82%A8%E3%83%A9%E3%83%BC%E7%9B%B8%E8%AB%87%E3%81%AE%E3%82%84%E3%82%8A%E3%81%8B%E3%81%9F)。問題あったかたはまずこちらをご一読し，その後，issueかチームスかどちらかに質問を投げてください．
   - [fraysr_toolの途中で出力される再生産関係の比較の表の見方](https://github.com/ichimomo/frasyr_tool/wiki/%E5%86%8D%E7%94%9F%E7%94%A3%E9%96%A2%E4%BF%82%E3%81%AE%E8%A1%A8%E3%81%AE%E8%A6%8B%E6%96%B9)


## その他のレポジトリ

### [future-text](https://github.com/ichimomo/future-text)
   - fraysrの計算方法を数式で解説しています
   - 年度ごとにバージョンが異なります

### [frasyr23](https://github.com/ichimomo/frasyr23)
   - 2・3系資源をHCRでABC計算するときの関数を提供します

### その他
- [tidyverse連載](https://github.com/ichimomo/main/blob/master/tidyverse)　Rを使うときに欠かせないtidyverseをちょっとづつ解説しています
- [Rで使えるデバッグ技まとめ](https://github.com/ichimomo/main/blob/master/R/debugging_method/README.md)

#### frasyrとfrasyr_toolの関係
<img src="fit1.png" width=50%>
