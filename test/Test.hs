{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Zenhan
import Text.Table.Chars

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

main :: IO()
main = do
  verboseCheck (prop_idempontent_hkana)
  verboseCheck (prop_idempontent_zkana)
  verboseCheck (prop_idempontent_hasciidigit)
  verboseCheck (prop_idempontent_zasciidigit)
  verboseCheck (prop_idempontent_isallzenkana)
  verboseCheck (prop_idempontent_isallhankana)
  verboseCheck (prop_idempontent_isallzen)
  verboseCheck (prop_idempontent_isallzen')
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

prop_idempontent_isallzenkana :: Zkana -> Bool
prop_idempontent_isallzenkana (Zkana t) = isAllZenKana t == True

prop_idempontent_isallhankana :: Hkana -> Bool
prop_idempontent_isallhankana (Hkana t) = isAllHanKana t == True

prop_idempontent_isallzen :: Bool
prop_idempontent_isallzen = isAllZen "本日は晴天なり" == True

prop_idempontent_isallzen' :: Bool
prop_idempontent_isallzen' = isAllZen "本日ﾊ晴天なり" == False

zconcat :: [T.Text] -> T.Text
zconcat = T.concat . zconcat'
  where
    zconcat' (x:y:xs) =case M.lookup (T.concat [x, y]) h_kana_d_map of
                         Just v -> v : zconcat' xs
                         Nothing -> x : zconcat' (y:xs)
    zconcat' (x:xs) = x : zconcat' xs
    zconcat' [] = []
