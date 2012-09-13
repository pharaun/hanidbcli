{-# LANGUAGE MultiParamTypeClasses #-}
module Crypto.Hash.Ed2k
    ( Ctx(..)
    , ED2K

    , initEd2k
    , updateEd2k
    , finalizeEd2k

    , hash
    , hashlazy

    -- Crypto-api
    , C.Hash(..)
    ) where

import Data.Functor ((<$>))
import Prelude hiding (init)
import qualified Crypto.Hash.MD4 as MD4
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L

-- CryptoAPI
import Control.Monad (liftM)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getByteString)
import Data.Serialize.Put (putByteString)
import Data.Tagged (Tagged(..))
import qualified Crypto.Classes as C (Hash(..))

-- Strictness
--import Control.Seq
import Control.DeepSeq

instance C.Hash Ctx ED2K where
    outputLength    = Tagged (digestSize * 8)
    blockLength     = Tagged (blockSize * 8) -- 9.27 MiB Blocks
    initialCtx      = initEd2k
    updateCtx       = updateEd2k
    finalize ctx bs = Digest . finalizeEd2k $ updateEd2k ctx bs

instance Serialize ED2K where
    get            = liftM Digest (getByteString digestSize)
    put (Digest d) = putByteString d

-- 9.27 MiB Blocks
blockSize :: Int
blockSize = 9500 * 1024

digestSize :: Int
digestSize = 16

data Ctx = Ctx ![(MD4.Ctx, Int)]
data ED2K = Digest !B.ByteString
    deriving (Eq, Ord, Show)

instance NFData B.ByteString where
    rnf a = a `seq` ()

instance NFData MD4.Ctx where
    rnf a = a `seq` ()

initEd2k :: Ctx
initEd2k = Ctx [(MD4.init, 0)]

-- Steps
-- 1. grab last element in list, see what its length is
-- 2. if less than blockSize, append more data up to blockSize
--  a. split the bytestring, feed the remaintant into hash
--  b. if there's more left, call update again and repeat
-- 3. if exactly blockSize, create a new tuple, call update
updateEd2k :: Ctx -> B.ByteString -> Ctx
updateEd2k (Ctx xs) a
    | lengthFrom xs < blockSize = uncurry (updateHash xs) (split xs a)
    | otherwise                 = updateEd2k (Ctx (xs ++ [(MD4.init, 0)])) a
    where
        lengthFrom xs = snd $ L.last xs
        split xs = B.splitAt (blockSize - lengthFrom xs)
        newHash a xs = MD4.update (fst $ L.last xs) a
        newLength a xs = lengthFrom xs + B.length a

        -- A deepseq worked here because it reduced each element in the list
        -- via a seq for the MD4.Ctx to rnf, now we don't hold on to the
        -- bytestring here
        newCtx a xs = Ctx $!! (L.init xs ++ [(newHash a xs, newLength a xs)])

        updateHash xs a1 a2
            | B.length a2 == 0  = newCtx a1 xs
            | otherwise         = updateEd2k (newCtx a1 xs) a2

-- Steps
-- 1. if only one hash in list, return it
-- 2. otherwise roll up all of the hash into a master hash and return it
finalizeEd2k :: Ctx -> B.ByteString
finalizeEd2k (Ctx xs)
    | L.length xs == 1  = MD4.finalize (fst $ L.head xs)
    | otherwise         = MD4.finalize (foldl MD4.update MD4.init ((MD4.finalize . fst) <$> xs)) -- TODO: also make this strict i think (foldl is non-strict, maybe foldl')

hash :: B.ByteString -> B.ByteString
hash = finalizeEd2k . updateEd2k initEd2k

hashlazy :: BL.ByteString -> B.ByteString
hashlazy l = finalizeEd2k $ foldl updateEd2k initEd2k (BL.toChunks l) -- TODO: probably make the foldl into foldl' to be strict on the hashing/evalulation
