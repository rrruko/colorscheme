{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Int
import Control.Applicative
import Data.Semigroup
import Numeric
import Data.Foldable
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe

type Color = (Int, Int, Int)
type Colorscheme = Vector (Color, Color, Color)

type Parser = AT.Parser

colorFromHex :: Parser Color
colorFromHex = (,,) <$> hexByte <*> hexByte <*> hexByte
    where hexByte = liftA2 (\hi lo -> 16 * hi + lo) hexDigit hexDigit
          hexDigit = do 
              n <- AT.satisfy  (\c -> isJust $ T.findIndex (==c) "0123456789abcdef")
              pure . fromIntegral . fromJust $ T.findIndex (==n) "0123456789abcdef"

hexShow :: Color -> Text
hexShow (r, g, b) = 
    let hex n = T.justifyRight 2 '0' . T.pack $ showHex n ""
    in  T.concat [hex r, hex g, hex b]

hashShow :: Color -> Text
hashShow rgb = '#' `T.cons` hexShow rgb

toXresources :: Colorscheme -> Text
toXresources = fold . V.toList . V.imap (\ix (reg, bri, dim) -> 
    fold [
        "! ", colorNames !! ix, "\n",
        colorLine ix reg,
        colorLine (ix + 8) bri, 
        colorLine (ix + 16) dim,
        "\n"
    ])
    where colorNames = T.words "Black Red Green Yellow Blue Magenta Cyan White"
          colorLine ix col = 
              T.justifyLeft 16 ' ' (fold ["*.color", T.pack $ show ix, ": "])
              <> hashShow col <> "\n"

xresourcesLine :: Parser (Int, Color)
xresourcesLine = (,)
    <$> ("*.color" *> AT.decimal)
    <*> (":" *> AT.skipSpace *> "#" *> colorFromHex <* AT.skipSpace)

fromXresources :: Parser Colorscheme
fromXresources = do
    colordefs <- many xresourcesLine
    pure . V.fromList
        . map asColorVariants
        . transpose
        . chunksOf 8
        . map snd
        $ sort colordefs
    where asColorVariants xs =
              case xs of
                  [reg, bri, dim] -> (reg, bri, dim)
                  [reg, bri]      -> (reg, bri, (0,0,0))
                  [reg]           -> (reg, (0,0,0), (0,0,0))
                  []              -> ((0,0,0),(0,0,0),(0,0,0))

fromAlacrittyYml :: Parser Colorscheme
fromAlacrittyYml = do
    reg <- "normal:" *> AT.skipSpace *> alacrittyColors
    bri <- "bright:" *> AT.skipSpace *> alacrittyColors
    dim <- "dim:" *> AT.skipSpace *> alacrittyColors
    pure $ V.zip3 reg bri dim
    where alacrittyColors  = do
              kvs <- AT.count 8 alacrittyColorKV
              pure . V.fromList $ map snd kvs
          alacrittyColorKV = (,) 
              <$> (many AT.letter <* ":" <* AT.skipSpace)
              <*> ("'0x" *> colorFromHex <* "'" <* AT.skipSpace)

alphaNum :: Parser Text
alphaNum = T.pack <$> many (AT.letter <|> AT.digit)

main :: IO ()
main = do
    xresources <- readFile "data/.Xresources"
    alacritty <- readFile "data/alacritty.yml"
    print $ AT.parseOnly xresourcesLine "*.color0: #fffefd"
    print $ AT.parseOnly xresourcesLine "*.color0: #fffefd\n"
    print $ AT.parseOnly (many xresourcesLine) "*.color0: #fffefd\n*.color1: #fcfbfa\n"
    print . AT.parseOnly fromAlacrittyYml $ T.pack alacritty
    putStrLn $ case AT.parseOnly fromXresources $ T.pack xresources of
                   Right r -> show r
                   Left e -> "Failed with " <> e <> " left"
