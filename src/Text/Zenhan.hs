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
  , isAllZen
  , toString
  , Mode(..)
  ) where

import Data.Tuple (swap)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Table.Chars

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

-- | Check Full-width Japanese character zen kana, returns True if the text all Zen Kana
isAllZen ::
     T.Text -- ^ The text to be checked
  -> Bool   -- ^ Result
isAllZen = not . T.any (flip elem (T.unpack $ T.concat $ (h_asciidigit ++ h_kana)))

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
