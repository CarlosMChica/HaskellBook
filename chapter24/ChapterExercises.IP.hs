{-# OPTIONS_GHC -fwarn-missing-signatures -fno-warn-name-shadowing -fwarn-incomplete-patterns #-}
module ChapterExercises.IP where

import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Word
import           Test.Hspec
import           Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Show)
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Show)

parseIPv4 :: Parser IPAddress
parseIPv4 = IPAddress <$> liftM4 (from8To32)
    parseV4Group'
    parseV4Group'
    parseV4Group'
    parseV4Group
    where parseV4Group' = parseV4Group <* skipDot
          skipDot = skip '.'

parseIPv6 :: Parser IPAddress6
parseIPv6 = liftM2 IPAddress6 w1 w2
  where w1 = liftM4 from16To64 parseV6Group' parseV6Group' parseV6Group' parseV6Group'
        w2 = liftM4 from16To64 parseV6Group' parseV6Group' parseV6Group' parseV6Group
        parseV6Group' = parseV6Group <* skipColon
        skipColon = skip ':'

parseV4Group :: Parser Word8
parseV4Group = read <$> some digit

parseV6Group :: Parser Word16
parseV6Group = hexToDec <$> (some $ oneOf (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']))
  where hexToDec :: String -> Word16
        hexToDec = fromIntegral . foldl (\acc hex -> (acc * 16) + digitToInt hex) 0

skip :: Char -> Parser ()
skip c = char c >> return ()

from8To32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
from8To32 x y z t = foldl' accum 0 $ [x, y, z, t]
  where accum acc word = (acc `shiftL` 8) .|. fromIntegral word

from16To64 :: Word16 -> Word16 -> Word16 -> Word16 -> Word64
from16To64 x y z t = foldl' accum 0 $ [x, y, z, t]
  where accum acc word = (acc `shiftL` 16) .|. fromIntegral word

toDecimal :: IPAddress6 -> Integer
toDecimal (IPAddress6 high low) = sumHighLow high low
  where sumHighLow :: Word64 -> Word64 -> Integer
        sumHighLow w1 w2 = (shift (toInteger w1) 64) + (toInteger w2)

main :: IO ()
main = hspec $ do
  it "can parse ip v4" $ do
    let ip1 = maybeIPAddressV4 "172.16.254.1"
        ip2 = maybeIPAddressV4 "204.120.0.15"
    ip1 `shouldBe` (Just . IPAddress $ 2886794753)
    ip2 `shouldBe` (Just . IPAddress $ 3430416399)

  it "can parse ip v6" $ do
    let ip1 = toDecimal <$> maybeIPAddressV6 "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
        ip2 = toDecimal <$> maybeIPAddressV6 "2001:DB8:0000:0000:8:800:200C:417A"
-- Can't handle :: yet
--        ip3 = toDecimal <$> maybeIPAddressV6 "2001:DB8::8:0800:200C:417A"
        ip4 = toDecimal <$> maybeIPAddressV6 "0:0:0:0:0:ffff:ac10:fe01"
        ip5 = toDecimal <$> maybeIPAddressV6 "0:0:0:0:0:ffff:cc78:f"
    ip1 `shouldBe` (Just $ 338288524927261089654163772891438416681)
    ip2 `shouldBe` (Just $ 42540766411282592856906245548098208122)
--    ip3 `shouldBe` (Just $ 42540766411282592856906245548098208122)
    ip4 `shouldBe` (Just $ 281473568538113)
    ip5 `shouldBe` (Just $ 281474112159759)

maybeIPAddressV4 :: String -> Maybe IPAddress
maybeIPAddressV4 = maybeSuccess . parseString parseIPv4 mempty

maybeIPAddressV6 :: String -> Maybe IPAddress6
maybeIPAddressV6 = maybeSuccess . parseString parseIPv6 mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing
