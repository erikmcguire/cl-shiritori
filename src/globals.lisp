(in-package :shiritori)

(defvar *h* (make-instance 'easy-acceptor :port 5067))

(defparameter *user-opt* nil) ; Allow/forbid kanji word prompts.
(defparameter *pos* nil) ; Allow/forbid -ru endings.
(defparameter *word* nil) ; Word prompt: User supplies response.
(defparameter *rhead* nil) ; For computer chaining.
(defparameter *dict* nil) ; Level-specific hash table.
(defparameter *corr-resp* nil) ; Correct responses.
(defparameter *wrong-resp* nil) ; Wrong response.
(defparameter *missed-words* nil) ; Missed prompts.
(defparameter *export* nil) ; Toggle export missed prompts.
(defparameter *seen* nil) ; Avoid repeated prompts.

(defparameter *n-all* "/jlpt-all.txt")
(defparameter *dict-all* (make-hash-table :test 'equal))
(defparameter *dicth* (make-hash-table :test 'equal)) ; letter(s)->hiragana
(defparameter *dictk* (make-hash-table :test 'equal)) ; letter(s)->katakana
(defparameter *all-kana* nil)
(defparameter *all-kanji* nil)

(defparameter youon '("ゃ" "ゅ" "ょ" "ャ" "ュ" "ョ"))

(defparameter *hiragana-map* '(("je" "じぇ") ("cche" "っちぇ") ("che" "ちぇ") ("-" "ー") ("dyi" "でぃ") ("n" "ん") ("wo" "を") ("we" "ゑ") ("wi" "ゐ")
 ("wa" "わ") ("xwa" "ゎ") ("ro" "ろ") ("re" "れ") ("ru" "る") ("ryo" "りょ") ("ryu" "りゅ") ("rya" "りゃ") ("ri" "り")
 ("ra" "ら") ("yo" "よ") ("xyo" "ょ") ("yu" "ゆ") ("xyu" "ゅ") ("ya" "や") ("xya" "ゃ") ("mo" "も") ("me" "め")
 ("mu" "む") ("myo" "みょ") ("myu" "みゅ") ("mya" "みゃ") ("mi" "み") ("ma" "ま") ("po" "ぽ") ("bo" "ぼ") ("ho" "ほ")
 ("pe" "ぺ") ("be" "べ") ("he" "へ") ("pu" "ぷ") ("bu" "ぶ") ("fo" "ふぉ") ("fe" "ふぇ") ("fi" "ふぃ") ("fa" "ふぁ")
 ("fu" "ふ") ("pyo" "ぴょ") ("pyu" "ぴゅ") ("pya" "ぴゃ") ("pi" "ぴ") ("byo" "びょ") ("byu" "びゅ") ("bya" "びゃ")
 ("bi" "び") ("hyo" "ひょ") ("hyu" "ひゅ") ("hya" "ひゃ") ("hi" "ひ") ("pa" "ぱ") ("ba" "ば") ("ha" "は") ("no" "の")
 ("ne" "ね") ("nu" "ぬ") ("nyo" "にょ") ("nyu" "にゅ") ("nya" "にゃ") ("ni" "に") ("na" "な") ("do" "ど") ("to" "と")
 ("de" "で") ("te" "て") ("du" "づ") ("tsu" "つ") ("rro" "っろ") ("rre" "っれ") ("rru" "っる") ("rryo" "っりょ")
 ("rryu" "っりゅ") ("rrya" "っりゃ") ("rri" "っり") ("rra" "っら") ("yyo" "っよ") ("yyu" "っゆ") ("yya" "っや")
 ("ppo" "っぽ") ("bbo" "っぼ") ("hho" "っほ") ("ppe" "っぺ") ("bbe" "っべ") ("hhe" "っへ") ("ppu" "っぷ")
 ("bbu" "っぶ") ("ffo" "っふぉ") ("ffe" "っふぇ") ("ffi" "っふぃ") ("ffa" "っふぁ") ("ffu" "っふ") ("ppyo" "っぴょ")
 ("ppyu" "っぴゅ") ("ppya" "っぴゃ") ("ppi" "っぴ") ("bbyo" "っびょ") ("bbyu" "っびゅ") ("bbya" "っびゃ")
 ("bbi" "っび") ("hhyo" "っひょ") ("hhyu" "っひゅ") ("hhya" "っひゃ") ("hhi" "っひ") ("ppa" "っぱ") ("bba" "っば")
 ("hha" "っは") ("ddo" "っど") ("tto" "っと") ("dde" "っで") ("tte" "って") ("ddu" "っづ") ("ttsu" "っつ")
 ("ddyo" "っぢょ") ("ddyu" "っぢゅ") ("ddya" "っぢゃ") ("ddi" "っぢ") ("ccho" "っちょ") ("cchu" "っちゅ")
 ("ccha" "っちゃ") ("cchi" "っち") ("dda" "っだ") ("tta" "った") ("zzo" "っぞ") ("sso" "っそ") ("zze" "っぜ")
 ("sse" "っせ") ("zzu" "っず") ("ssu" "っす") ("jjo" "っじょ") ("jju" "っじゅ") ("jja" "っじゃ") ("jji" "っじ")
 ("ssho" "っしょ") ("sshu" "っしゅ") ("ssha" "っしゃ") ("sshi" "っし") ("zza" "っざ") ("ssa" "っさ") ("ggo" "っご")
 ("kko" "っこ") ("gge" "っげ") ("kke" "っけ") ("ggu" "っぐ") ("kku" "っく") ("ggyo" "っぎょ") ("ggyu" "っぎゅ")
 ("ggya" "っぎゃ") ("ggi" "っぎ") ("kkyo" "っきょ") ("kkyu" "っきゅ") ("kkya" "っきゃ") ("kki" "っき") ("gga" "っが")
 ("kka" "っか") ("vvo" "っう゛ぉ") ("vve" "っう゛ぇ") ("vvi" "っう゛ぃ") ("vva" "っう゛ぁ") ("vvu" "っう゛")
 ("xtsu" "っ") ("dyo" "ぢょ") ("dyu" "ぢゅ") ("dya" "ぢゃ") ("di" "ぢ") ("cho" "ちょ") ("chu" "ちゅ") ("cha" "ちゃ")
 ("chi" "ち") ("da" "だ") ("ta" "た") ("zo" "ぞ") ("so" "そ") ("ze" "ぜ") ("se" "せ") ("zu" "ず") ("su" "す")
 ("jo" "じょ") ("ju" "じゅ") ("ja" "じゃ") ("ji" "じ") ("sho" "しょ") ("shu" "しゅ") ("sha" "しゃ") ("shi" "し")
 ("za" "ざ") ("sa" "さ") ("go" "ご") ("ko" "こ") ("ge" "げ") ("ke" "け") ("gu" "ぐ") ("ku" "く") ("gyo" "ぎょ")
 ("gyu" "ぎゅ") ("gya" "ぎゃ") ("gi" "ぎ") ("kyo" "きょ") ("kyu" "きゅ") ("kya" "きゃ") ("ki" "き") ("ga" "が")
 ("ka" "か") ("o" "お") ("xo" "ぉ") ("e" "え") ("xe" "ぇ") ("vo" "う゛ぉ") ("ve" "う゛ぇ") ("vi" "う゛ぃ") ("va" "う゛ぁ")
 ("vu" "う゛") ("u" "う") ("xu" "ぅ") ("i" "い") ("xi" "ぃ") ("a" "あ") ("xa" "ぁ")))

 (defparameter *katakana-map* '(("je" "ジェ") ("cche" "ッチェ") ("che" "チェ") ("-" "ー") ("di" "ディ") ("n" "ン") ("wo" "ウォ") ("wo" "ヲ") ("we" "ウェ")
 ("we" "ヱ") ("wi" "ヰ") ("wi" "ウィ") ("wa" "ワ") ("xwa" "ヮ") ("ro" "ロ") ("re" "レ") ("ru" "ル") ("ryo" "リョ")
 ("ryu" "リュ") ("rya" "リャ") ("ri" "リ") ("ra" "ラ") ("yo" "ヨ") ("xyo" "ョ") ("yu" "ユ") ("xyu" "ュ") ("ya" "ヤ")
 ("xya" "ャ") ("mo" "モ") ("me" "メ") ("mu" "ム") ("myo" "ミョ") ("myu" "ミュ") ("mya" "ミャ") ("mi" "ミ") ("ma" "マ")
 ("po" "ポ") ("bo" "ボ") ("ho" "ホ") ("pe" "ペ") ("be" "ベ") ("he" "ヘ") ("pu" "プ") ("bu" "ブ") ("fu" "フュ")
 ("fo" "フォ") ("fe" "フェ") ("fi" "フィ") ("fa" "ファ") ("fu" "フ") ("pyo" "ピョ") ("pyu" "ピュ") ("pya" "ピャ")
 ("pi" "ピ") ("byo" "ビョ") ("byu" "ビュ") ("bya" "ビャ") ("bi" "ビ") ("hyo" "ヒョ") ("hyu" "ヒュ") ("hya" "ヒャ")
 ("hi" "ヒ") ("pa" "パ") ("ba" "バ") ("ha" "ハ") ("no" "ノ") ("ne" "ネ") ("nu" "ヌ") ("nyo" "ニョ") ("nyu" "ニュ")
 ("nya" "ニャ") ("ni" "ニ") ("na" "ナ") ("du" "ドゥ") ("do" "ド") ("to" "ト") ("de" "デ") ("te" "テ") ("du" "ヅ")
 ("tsu" "ツ") ("rro" "ッロ") ("rre" "ッレ") ("rru" "ッル") ("rryo" "ッリョ") ("rryu" "ッリュ") ("rrya" "ッリャ")
 ("rri" "ッリ") ("rra" "ッラ") ("yyo" "ッヨ") ("yyu" "ッユ") ("yya" "ッヤ") ("ppo" "ッポ") ("bbo" "ッボ")
 ("hho" "ッホ") ("ppe" "ッペ") ("bbe" "ッベ") ("hhe" "ッヘ") ("ppu" "ップ") ("bbu" "ッブ") ("ffo" "ッフォ")
 ("ffe" "ッフェ") ("ffi" "ッフィ") ("ffa" "ッファ") ("ffu" "ッフュ") ("ffu" "ッフ") ("ppyo" "ッピョ") ("ppyu" "ッピュ")
 ("ppya" "ッピャ") ("ppi" "ッピ") ("bbyo" "ッビョ") ("bbyu" "ッビュ") ("bbya" "ッビャ") ("bbi" "ッビ")
 ("hhyo" "ッヒョ") ("hhyu" "ッヒュ") ("hhya" "ッヒャ") ("hhi" "ッヒ") ("ppa" "ッパ") ("bba" "ッバ") ("hha" "ッハ")
 ("ddu" "ッドゥ") ("ddo" "ッド") ("tto" "ット") ("dde" "ッデ") ("tte" "ッテ") ("ddu" "ッヅ") ("ttsu" "ッツ")
 ("ddyo" "ッヂョ") ("ddyu" "ッヂュ") ("ddya" "ッヂャ") ("ddi" "ッヂ") ("ccho" "ッチョ") ("cchu" "ッチュ")
 ("ccha" "ッチャ") ("tti" "ッティ") ("cchi" "ッチ") ("dda" "ッダ") ("tta" "ッタ") ("zzo" "ッゾ") ("sso" "ッソ")
 ("zze" "ッゼ") ("sse" "ッセ") ("zzu" "ッズ") ("ssu" "ッス") ("jjo" "ッジョ") ("jju" "ッジュ") ("jja" "ッジャ")
 ("jji" "ッジ") ("sshe" "ッシェ") ("ssho" "ッショ") ("sshu" "ッシュ") ("ssha" "ッシャ") ("sshi" "ッシ")
 ("zza" "ッザ") ("ssa" "ッサ") ("ggo" "ッゴ") ("kko" "ッコ") ("gge" "ッゲ") ("kke" "ッケ") ("ggu" "ッグ")
 ("kku" "ック") ("ggyo" "ッギョ") ("ggyu" "ッギュ") ("ggya" "ッギャ") ("ggi" "ッギ") ("kkyo" "ッキョ")
 ("kkyu" "ッキュ") ("kkya" "ッキャ") ("kki" "ッキ") ("gga" "ッガ") ("kka" "ッカ") ("vvo" "ッヴォ") ("vve" "ッヴェ")
 ("vvi" "ッヴィ") ("vva" "ッヴァ") ("vvu" "ッヴ") ("xtsu" "ッ") ("ti" "ティ") ("dyo" "ヂョ") ("dyu" "ヂュ")
 ("dya" "ヂャ") ("di" "ヂ") ("cho" "チョ") ("chu" "チュ") ("cha" "チャ") ("chi" "チ") ("da" "ダ") ("ta" "タ")
 ("zo" "ゾ") ("so" "ソ") ("ze" "ゼ") ("se" "セ") ("zu" "ズ") ("su" "ス") ("jo" "ジョ") ("ju" "ジュ") ("ja" "ジャ")
 ("ji" "ジ") ("she" "シェ") ("sho" "ショ") ("shu" "シュ") ("sha" "シャ") ("shi" "シ") ("za" "ザ") ("sa" "サ")
 ("go" "ゴ") ("ko" "コ") ("ge" "ゲ") ("ke" "ケ") ("gu" "グ") ("ku" "ク") ("gyo" "ギョ") ("gyu" "ギュ") ("gya" "ギャ")
 ("gi" "ギ") ("kyo" "キョ") ("kyu" "キュ") ("kya" "キャ") ("ki" "キ") ("ga" "ガ") ("ka" "カ") ("o" "オ") ("xo" "ォ")
 ("e" "エ") ("xe" "ェ") ("vo" "ヴォ") ("ve" "ヴェ") ("vi" "ヴィ") ("va" "ヴァ") ("vu" "ヴ") ("u" "ウ") ("xu" "ゥ") ("i" "イ")
 ("xi" "ィ") ("a" "ア") ("xa" "ァ")))
