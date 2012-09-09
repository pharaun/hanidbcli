module Crypto.Hash.Ed2k
    ( init
    , update
    , finalize

    , hash
--    , hashlazy
    ) where

import Data.Functor ((<$>))
import Prelude hiding (init)
import qualified Crypto.Hash.MD4 as MD4
import qualified Data.ByteString as B
import qualified Data.List as L

-- 9.27 MiB Blocks
blockSize :: Int
blockSize = 9500 * 1024

data Ctx = Ctx [(MD4.Ctx, Int)]

init :: Ctx
init = Ctx [(MD4.init, 0)]

-- Steps
-- 1. grab last element in list, see what its length is
-- 2. if less than blockSize, append more data up to blockSize
--  a. split the bytestring, feed the remaintant into hash
--  b. if there's more left, call update again and repeat
-- 3. if exactly blockSize, create a new tuple, call update
update :: Ctx -> B.ByteString -> Ctx
update (Ctx xs) a
    | len xs < blockSize    = uncurry (updateHash xs) (split xs a)
    | otherwise             = update (Ctx (xs ++ [(MD4.init, 0)])) a
    where
        len xs = snd $ L.last xs
        split xs = B.splitAt (blockSize - len xs)
        newHash a xs = MD4.update (fst $ L.last xs) a
        newLength a xs = len xs + B.length a

        updateHash xs a1 a2
            | B.length a2 == 0  = Ctx (L.init xs ++ [(newHash a1 xs, newLength a1 xs)])
            | otherwise         = update (Ctx (L.init xs ++ [(newHash a1 xs, newLength a1 xs)])) a2

-- Steps
-- 1. if only one hash in list, return it
-- 2. otherwise roll up all of the hash into a master hash and return it
finalize :: Ctx -> B.ByteString
finalize (Ctx xs)
    | L.length xs == 1  = MD4.finalize (fst $ L.head xs)
    | otherwise         = MD4.finalize (foldl MD4.update MD4.init ((MD4.finalize . fst) <$> xs))


hash :: B.ByteString -> B.ByteString
hash = finalize . update init
