# hzenhan

Converter between Full-width Japanese and Half-width Japanese  
  
This module ported Python's zenhan library, similar to the  
"zenhan" library found in pypi:

[<https://pypi.python.org/pypi/zenhan/>](<https://pypi.python.org/pypi/zenhan/> "zenhan")

# Install

Installing from Hackage with stack is straightforward:

    > stack install

or gentoo emerge from gentoo-haskell overlay

    > layman -a haskell
    > emerge dev-haskell/hzenhan

# Usage

Let's see an example.

    > {-# LANGUAGE OverloadedStrings #-}
    >
    > import Text.Zenhan
    > import Data.Text (pack, unpack)
    >
    > main :: IO ()
    > main = do
    >   let h = h2z [Kana, Digit, Ascii] \"A\" \"ABCd\\\\｢｣ｱｲｳｴｵ123\"
    >       z = z2h [Kana, Digit, Ascii] \"Ｂエ\" h
    >       r = isAllZenKana "アイウエオ"
    >   putStrLn $ toString h
    >   putStrLn $ toString z
