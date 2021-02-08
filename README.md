# alqo

Yet another algo web app written.

## REST API

### 概要

- 入力・出力は全て JSON。
- プレイヤーは 1 以上 4 以下のインデックスで表す。
- カードは 0 以上 23 以下の値で表す。
  - 2 で割った商がカードに書かれている数、余りが色。
- ゲームに参加している人はプレイヤー。それ以外は観客。

### エンドポイント

- GET `/`
  - 多分ここで静的ファイルを配信する
- POST `/room`
  - 新しい room を作る。
  - parameters
    - `nplayers` プレイヤーの数（2〜4）
      - メモ：そのうち`num_players`に変更するかも
  - responses
    - 201 Created
      - header の location に新しくできた room の URL が格納される
        - e.g., `/room/af4b58eac16c4b128064771633ab233d`
        - 以下では `/room/:roomid` と書く
- GET `/room/:roomid`
  - room の状況を返す
  - parameters
    - なし
  - responses
    - 200 OK
      - ゲームがまだ始まっていない場合
        - JSON map
          - `status: "not_started"`
          - `registered: (いま登録しているプレイヤーの数)`
          - `nplayers: (/room で作ったときに指定したプレイヤーの数)`
          - `your_status: (registerしていれば1, そうでなければ0)}`
      - ゲームが始まっている時
        - JSON map
          - `status: "playing"`
          - `board: (encoded board)`
- POST `/room/:roomid/register`
  - room にプレイヤーとして登録する。
  - parameters
    - なし
  - responses
    - 201 Created
      - 成功
- POST `/room/:roomid/attack`
  - アタックを行う
  - parameters
    - `target_player: (アタックするプレイヤーのインデックス)`
    - `target_index: (アタックするカードのインデックス。そのプレイヤーが持っているカードで何番目に小さいか。)`
    - `guess: (予想した値)`
  - responses
    - 200 OK
      - `result: (成功すればtrue, 失敗すればfalse)`
- POST `/room/:roomid/stay`
  - ステイを行う
  - parameters
    - なし
  - responses
    - 200 OK
      - `result: true`
        - メモ：この結果は必要ないので、そのうち無くすかも。

## WebSocket API

### 概要

- 出力は全て JSON で`["(イベントの名前)", (詳細情報)]`の形。
  - クライアントがサーバーに WebSocket 経由で何かを送ることはない。
- エンドポイントは`/room/:roomid/ws`

### イベント

- `["player_registered", (登録したプレイヤーの合計人数)]`
  - プレイヤーが`POST /register`を叩いて登録した。
- `["game_started", (encoded board)]`
  - 必要な人数のプレイヤーが登録したため、ゲームが始まった。
  - 詳細情報には、観客から見たゲームの状態が載る。
- `["your_hand", (自分の手札のカードの数のリスト)]`
  - ゲームが始まり、自分の手札が分かった。
- `["your_turn", (attacker cardの数)]`
  - 自分のターンが始まった。
- `["attacked", (後述のmap)]`
  - アタックが行われた。
  - 入力は以下の map
    - `board: (encoded board)`
    - `target_player: (攻撃されたプレイヤーのインデックス)`
    - `target_hand_index: (攻撃された手札のインデックス)`
      - メモ：`/attack`の`target_index`と一貫性がないので直したい。
    - `guess: (予測した値)`
    - `result: (アタックの結果。成功していればtrue, 失敗していればfalse)`
- `["stayed", (encoded board)]`
  - ステイが行われた。
- `["game_finished", (勝利したプレイヤーのインデックス)]`
  - ゲームが終了し、勝者が決まった。

## encoded board

ゲームの状態を表す以下の map.

- `can_stay: (stayできるならtrue, できないならfalse)`
- `current_turn: (現在ターンを持っているプレイヤーのインデックス（1〜4）)`
- `next_turn: (次にターンが渡るプレイヤーのインデックス)`
- `num_players: (プレイヤーの人数)`
- `winner: (ゲームが終了している場合は、勝利したプレイヤーのインデックス。そうでない場合はnull)`
- `hands: プレイヤーが持っているカードの、他人からの見た目`
  - `hands[i][j][0]: プレイヤーiのj番目に小さいカードの数（ただし裏向きの場合はそのmod 2）`
  - `hands[i][j][1]: プレイヤーiのj番目に小さいカードが裏向きならtrue, 表向きならfalse`
- `deck_top: 山札の一番上のカードの数 mod 2`
- `attacker_card: 現在攻撃に使用しているカード`
  - 存在しない（選ぶ必要がある）場合: `null`
    - 現在選ぶエンドポイントは存在しない
  - 山札からとったカードの場合: `[1, (そのカードの数 mod 2)]`
  - 手札からとったカードの場合: `[2, (そのカードの手札における添字（何番目に小さいか）)]`

プレイヤーの場合（ログインしている場合）は、下記が追加される。

- `your_player_index: (自分のプレイヤーインデックス)`
- `your_hand: (自分の手札の数のリスト)`
  - `your_hand[i]: 自分が持っている手札のうち、i番目に小さいカードの数`
- `your_attacker_card_from_deck: attacker cardが山札からとったものの場合、そのカードの数。そうでなければnull`
