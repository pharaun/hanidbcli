module Ed2k
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
update (Ctx xs) a =
    if len xs < blockSize
    then (do
            let (a1, a2) = split xs a
                newHash = MD4.update (fst $ L.last xs) a1
                newLength = len xs + B.length a1

            if B.length a2 > 0
            then update (Ctx (L.init xs ++ [(newHash, newLength)])) a2
            else Ctx (L.init xs ++ [(newHash, newLength)])
         )
    else update (Ctx (xs ++ [(MD4.init, 0)])) a
    where
        len xs = snd $ L.last xs
        split xs = B.splitAt (blockSize - len xs)

-- Steps
-- 1. if only one hash in list, return it
-- 2. otherwise roll up all of the hash into a master hash and return it
finalize :: Ctx -> B.ByteString
finalize (Ctx xs) =
    if L.length xs == 1
    then MD4.finalize (fst $ L.head xs)
    else MD4.finalize (foldl MD4.update MD4.init ((MD4.finalize . fst) <$> xs))

hash :: B.ByteString -> B.ByteString
hash = finalize . update init
