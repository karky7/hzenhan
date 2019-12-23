{- |
Module      :  Text.Zenhan
Description :  Converter between Full-width Japanese and Half-width Japanese
Copyright   :  2017 karky7 Calimakvonia
License     :  BSD3

Maintainer  :  cantimerny.g@gmail.com
Stability   :  unstable
Portability :  portable

This module ported Python's zenhan library, similar to the
"zenhan" library found in pypi:

<https://pypi.python.org/pypi/zenhan/>

Let's see an example.

@
{-# LANGUAGE OverloadedStrings #-}

import Text.Zenhan
import Data.Text (pack, unpack)

main :: IO ()
main = do
  let h = h2z [Kana, Digit, Ascii] \"A\" \"ABCd\\\\｢｣ｱｲｳｴｵ123\"
      z = z2h [Kana, Digit, Ascii] \"Ｂエ\" h
  putStrLn $ toString h
  putStrLn $ toString z
@

This library is still a work-in-progress, and contributions are welcome for
missing pieces and to fix bugs. Please see the Github page to contribute with
code or bug reports:

<https://github.com/karky7/hzenhan>
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Zenhan
  (
    z2h
  , h2z
  , isAllZenKana
  , isAllHanKana
  , toString
  , Mode(..)
  ) where

import Data.Tuple (swap)
import qualified Data.Text as T
import qualified Data.Map as M

-- | Type for character type to be converted
data Mode = Kana | Digit | Ascii deriving(Eq, Show)

-- | Convert from Full-width Japanese character to Half-width Japanese character
z2h ::
     [Mode] -- ^ Type of character to be converted
  -> T.Text -- ^ Conversion exclusion character
  -> T.Text -- ^ Characters to be converted
  -> T.Text -- ^ Result
z2h mode ignore text = T.concat $ map (zhTrans mode (convAry ignore)) (convAry text)

-- | Convert from Half-width Japanese character to Full-width Japanese character
h2z ::
     [Mode] -- ^ Type of character to be converted
  -> T.Text -- ^ Conversion exclusion character
  -> T.Text -- ^ Characters to be converted
  -> T.Text -- ^ Result
h2z mode ignore text = T.concat $ map (hzTrans mode (hconvAry ignore)) (hconvAry text)

-- | Check Full-width Japanese character zen kana, return True if all text is Zen Kana
isAllZenKana ::
     T.Text -- ^ The text to be checked
  -> Bool   -- ^ Result
isAllZenKana = T.all (flip elem (T.unpack $ T.concat $ z_kana))

-- | Check Full-width Japanese character zen kana, return True if all text is Hen Kana
isAllHanKana ::
     T.Text -- ^ The text to be checked
  -> Bool   -- ^ Result
isAllHanKana = T.all (flip elem (T.unpack $ T.concat $ h_kana))

-- | Convert to String
toString :: T.Text -> String
toString = T.unpack

zhTrans :: [Mode] -> [T.Text] -> T.Text -> T.Text
zhTrans mode ign t = case M.lookup t (z2hMap mode) of
  Just v -> choice (elem t ign) t v
  Nothing -> t

hzTrans :: [Mode] -> [T.Text] -> T.Text -> T.Text
hzTrans mode ign t = case M.lookup t (h2zMap mode) of
  Just v -> choice (elem t ign) t v
  Nothing -> t

choice :: Bool -> a -> a -> a
choice True t _ = t
choice False _ v = v

z2hMap :: [Mode] -> M.Map T.Text T.Text
z2hMap [] = M.empty
z2hMap (x:xs) = M.unions ([zhMap x, z2hMap xs])

zhMap :: Mode -> M.Map T.Text T.Text
zhMap Kana  = zh_kana
zhMap Digit = zh_digit
zhMap Ascii = zh_ascii

h2zMap :: [Mode] -> M.Map T.Text T.Text
h2zMap [] = M.empty
h2zMap (x:xs) = M.unions ([hzMap x, h2zMap xs])

hzMap :: Mode -> M.Map T.Text T.Text
hzMap Kana  = hz_kana
hzMap Digit = hz_digit
hzMap Ascii = hz_ascii

convAry :: T.Text -> [T.Text]
convAry xs = T.transpose [xs]

hconvAry :: T.Text -> [T.Text]
hconvAry = mergeWord . convAry

mergeWord :: [T.Text] -> [T.Text]
mergeWord (x:y:xs) = choice (elem (T.concat [x, y]) (h_kana_d ++ h_kana_p))
                         (T.concat [x, y] : mergeWord xs) (x : mergeWord (y:xs))
mergeWord (x:xs) = x : mergeWord xs
mergeWord [] = []

zh_ascii :: M.Map T.Text T.Text
zh_ascii = M.fromList $ zip z_ascii h_ascii

hz_ascii :: M.Map T.Text T.Text
hz_ascii = transposeMap zh_ascii

zh_digit :: M.Map T.Text T.Text
zh_digit = M.fromList $ zip z_digit h_digit

hz_digit :: M.Map T.Text T.Text
hz_digit = transposeMap zh_digit

zh_kana :: M.Map T.Text T.Text
zh_kana = M.fromList $ zip z_kana h_kana

hz_kana :: M.Map T.Text T.Text
hz_kana = transposeMap zh_kana

transposeMap :: M.Map T.Text T.Text -> M.Map T.Text T.Text
transposeMap = M.fromList . map swap . M.toList

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
           "ｯ", "ｬ", "ｭ", "ｮ" ] ++
           h_kana_d ++
           h_kana_p ++
           [ "｡", "､", "･", "ﾞ", "ﾟ", "｢", "｣", "ｰ" ]

h_kana_d :: [T.Text]
h_kana_d = [ "ｳﾞ",
             "ｶﾞ", "ｷﾞ", "ｸﾞ", "ｹﾞ", "ｺﾞ",
             "ｻﾞ", "ｼﾞ", "ｽﾞ", "ｾﾞ", "ｿﾞ",
             "ﾀﾞ", "ﾁﾞ", "ﾂﾞ", "ﾃﾞ", "ﾄﾞ",
             "ﾊﾞ", "ﾋﾞ", "ﾌﾞ", "ﾍﾞ", "ﾎﾞ" ]

h_kana_p :: [T.Text]
h_kana_p = [ "ﾊﾟ", "ﾋﾟ", "ﾌﾟ", "ﾍﾟ", "ﾎﾟ" ]
