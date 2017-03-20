{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Text.Zenhan
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Map as M

newtype Hkana = Hkana T.Text
newtype Zkana = Zkana T.Text
newtype HAsciiDigit = HAsciiDigit T.Text
newtype ZAsciiDigit = ZAsciiDigit T.Text
newtype AllLetter = AllLetter T.Text
newtype Ignore = Ignore T.Text

instance Show Hkana where
  show (Hkana t) = toString t

instance Show Zkana where
  show (Zkana t) = toString t

instance Show HAsciiDigit where
  show (HAsciiDigit t) = toString t

instance Show ZAsciiDigit where
  show (ZAsciiDigit t) = toString t

instance Show Ignore where
  show (Ignore t) = toString t

instance Arbitrary Hkana where
  arbitrary = randomHkana

instance Arbitrary Zkana where
  arbitrary = randomZkana

instance Arbitrary HAsciiDigit where
  arbitrary = randomHAsciiDigit

instance Arbitrary ZAsciiDigit where
  arbitrary = randomZAsciiDigit

instance Arbitrary Ignore where
  arbitrary = randomIgnore

randomHkana :: Gen Hkana
randomHkana = Hkana `liftM` T.concat `liftM` listOf (elements h_kana)

randomZkana :: Gen Zkana
randomZkana = Zkana `liftM` T.concat `liftM` listOf (elements z_kana)

randomHAsciiDigit :: Gen HAsciiDigit
randomHAsciiDigit = HAsciiDigit `liftM` T.concat `liftM` listOf (elements h_asciidigit)

randomZAsciiDigit :: Gen ZAsciiDigit
randomZAsciiDigit = ZAsciiDigit `liftM` T.concat `liftM` listOf (elements z_asciidigit)

randomIgnore :: Gen Ignore
randomIgnore = Ignore `liftM` T.concat `liftM` listOf (elements ignore)

ignore :: [T.Text]
ignore = h_kana ++ z_kana ++ h_asciidigit ++ z_asciidigit

h_kana :: [T.Text]
h_kana = [ "ｱ", "ｲ", "ｳ", "ｴ", "ｵ",
           "ｶ", "ｷ", "ｸ", "ｹ", "ｺ",
           "ｻ", "ｼ", "ｽ", "ｾ", "ｿ",
           "ﾀ", "ﾁ", "ﾂ", "ﾃ", "ﾄ",
           "ﾅ", "ﾆ", "ﾇ", "ﾈ", "ﾉ",
           "ﾊ", "ﾋ", "ﾌ", "ﾍ", "ﾎ",
           "ﾏ", "ﾐ", "ﾑ", "ﾒ", "ﾓ",
           "ﾔ", "ﾕ", "ﾖ",
           "ﾗ", "ﾘ", "ﾙ", "ﾚ", "ﾛ",
           "ﾜ", "ｦ", "ﾝ",
           "ｧ", "ｨ", "ｩ", "ｪ", "ｫ",
           "ｯ", "ｬ", "ｭ", "ｮ" ,
           "ｳﾞ",
           "ｶﾞ", "ｷﾞ", "ｸﾞ", "ｹﾞ", "ｺﾞ",
           "ｻﾞ", "ｼﾞ", "ｽﾞ", "ｾﾞ", "ｿﾞ",
           "ﾀﾞ", "ﾁﾞ", "ﾂﾞ", "ﾃﾞ", "ﾄﾞ",
           "ﾊﾞ", "ﾋﾞ", "ﾌﾞ", "ﾍﾞ", "ﾎﾞ",
           "ﾊﾟ", "ﾋﾟ", "ﾌﾟ", "ﾍﾟ", "ﾎﾟ",
           "｡", "､", "･", "ﾞ", "ﾟ", "｢", "｣", "ｰ" ]

z_kana :: [T.Text]
z_kana = [ "ア", "イ", "ウ", "エ", "オ",
           "カ", "キ", "ク", "ケ", "コ",
           "サ", "シ", "ス", "セ", "ソ",
           "タ", "チ", "ツ", "テ", "ト",
           "ナ", "ニ", "ヌ", "ネ", "ノ",
           "ハ", "ヒ", "フ", "ヘ", "ホ",
           "マ", "ミ", "ム", "メ", "モ",
           "ヤ", "ユ", "ヨ",
           "ラ", "リ", "ル", "レ", "ロ",
           "ワ", "ヲ", "ン",
           "ァ", "ィ", "ゥ", "ェ", "ォ",
           "ッ", "ャ", "ュ", "ョ",
           "ヴ",
           "ガ", "ギ", "グ", "ゲ", "ゴ",
           "ザ", "ジ", "ズ", "ゼ", "ゾ",
           "ダ", "ヂ", "ヅ", "デ", "ド",
           "バ", "ビ", "ブ", "ベ", "ボ",
           "パ", "ピ", "プ", "ペ", "ポ",
           "。", "、", "・", "゛", "゜", "「", "」", "ー" ]

h_asciidigit :: [T.Text]
h_asciidigit = h_ascii ++ h_digit

h_ascii :: [T.Text]
h_ascii = [ "a", "b", "c", "d", "e", "f", "g", "h", "i",
            "j", "k", "l", "m", "n", "o", "p", "q", "r",
            "s", "t", "u", "v", "w", "x", "y", "z",
            "A", "B", "C", "D", "E", "F", "G", "H", "I",
            "J", "K", "L", "M", "N", "O", "P", "Q", "R",
            "S", "T", "U", "V", "W", "X", "Y", "Z",
            "!", "\"", "#", "$", "%", "&", "'", "(", ")",
            "*", "+", ",", "-", ".", "/", ":", ";", "<",
            "=", ">", "?", "@", "[", "\\", "]", "^", "_",
            "`", "{", "|", "}", "~", " " ]

h_digit :: [T.Text]
h_digit = [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
  
z_asciidigit :: [T.Text]
z_asciidigit = z_ascii ++ z_digit

z_ascii :: [T.Text]
z_ascii = [ "ａ", "ｂ", "ｃ", "ｄ", "ｅ", "ｆ", "ｇ", "ｈ", "ｉ",
            "ｊ", "ｋ", "ｌ", "ｍ", "ｎ", "ｏ", "ｐ", "ｑ", "ｒ",
            "ｓ", "ｔ", "ｕ", "ｖ", "ｗ", "ｘ", "ｙ", "ｚ",
            "Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ", "Ｆ", "Ｇ", "Ｈ", "Ｉ",
            "Ｊ", "Ｋ", "Ｌ", "Ｍ", "Ｎ", "Ｏ", "Ｐ", "Ｑ", "Ｒ",
            "Ｓ", "Ｔ", "Ｕ", "Ｖ", "Ｗ", "Ｘ", "Ｙ", "Ｚ",
            "！", "”", "＃", "＄", "％", "＆", "’", "（", "）",
            "＊", "＋", "，", "−", "．", "／", "：", "；", "＜",
            "＝", "＞", "？", "＠", "［", "￥", "］", "＾", "＿",
            "‘", "｛", "｜", "｝", "〜", "　" ]

z_digit :: [T.Text]
z_digit = [ "０", "１", "２", "３", "４", "５", "６", "７", "８", "９" ]

h_kana_d :: M.Map T.Text T.Text
h_kana_d = M.fromList [ ("ウ゛", "ヴ"),
             ("カ゛", "ガ"), ("キ゛", "ギ"), ("ク゛", "グ"), ("ケ゛", "ゲ"), ("コ゛", "ゴ"),
             ("サ゛", "ザ"), ("シ゛", "ジ"), ("ス゛", "ズ"), ("セ゛", "ゼ"), ("ソ゛", "ゾ"),
             ("タ゛", "ダ"), ("チ゛", "ヂ"), ("ツ゛", "ヅ"), ("テ゛", "デ"), ("ト゛", "ド"),
             ("ハ゛", "バ"), ("ヒ゛", "ビ"), ("フ゛", "ブ"), ("ヘ゛", "ベ"), ("ホ゛", "ボ"),
             ("ハ゜", "パ"), ("ヒ゜", "ピ"), ("フ゜", "プ"), ("ヘ゜", "ペ"), ("ホ゜", "ポ") ]

main :: IO()
main = do
  verboseCheck (prop_idempontent_hkana)
  verboseCheck (prop_idempontent_zkana)
  verboseCheck (prop_idempontent_hasciidigit)
  verboseCheck (prop_idempontent_zasciidigit)
  putStrLn $ toString $ z2h [Kana, Digit, Ascii] "Ａ＆ｍ　５パス" "ｗＨ％Ｏ＄ｇＵ　ＣｙＹ７＝タラバェハパマスヅビゼーピダヨメ」オツフタュプ｜＆ｄＨＴＹＡｆＤＥＵ￥ｘ￥Ｎ￥ｊＢｍｊｚ％％＊Ｐｊｑ４ｗＡｐ］ｑ　Ｘ｜ｑ５ＷＲｇス１ｇｉ＞マＷＪ　：？＾＾Ｗ‘］ｂｆ「ツディゴパッテエプイバヴロリセォヴテｐＺＣｐＶｖ．ｚ"

prop_idempontent_hkana :: Hkana -> Bool
prop_idempontent_hkana (Hkana t) = z2h [Kana, Digit, Ascii] "" (h2z [Kana, Digit, Ascii] "" t) == t

prop_idempontent_zkana :: Zkana -> Bool
prop_idempontent_zkana (Zkana t) = h2z [Kana, Digit, Ascii] "" (z2h [Kana, Digit, Ascii] "" t') == t'
  where t' = zconcat (T.transpose (T.words t))

prop_idempontent_hasciidigit :: HAsciiDigit -> Bool
prop_idempontent_hasciidigit (HAsciiDigit t) = z2h [Kana, Digit, Ascii] "" (h2z [Kana, Digit, Ascii] "" t) == t

prop_idempontent_zasciidigit :: ZAsciiDigit -> Bool
prop_idempontent_zasciidigit (ZAsciiDigit t) = h2z [Kana, Digit, Ascii] "" (z2h [Kana, Digit, Ascii] "" t) == t

zconcat :: [T.Text] -> T.Text
zconcat = T.concat . zconcat'
  where
    zconcat' (x:y:xs) =case M.lookup (T.concat [x, y]) h_kana_d of
                         Just v -> v : zconcat' xs
                         Nothing -> x : zconcat' (y:xs)
    zconcat' (x:xs) = x : zconcat' xs
    zconcat' [] = []

