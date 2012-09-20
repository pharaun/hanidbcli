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
import Control.DeepSeq

instance C.Hash Ctx ED2K where
    outputLength    = Tagged (digestSize * 8)
    -- 9.27 MiB Blocks - (Implodes performance to abysmal level)
--  blockLength     = Tagged (blockSize * 8)
    -- Best performance with (4*1024) block sourceFile conduit
--  blockLength     = Tagged ((40 * 1024) * 8)
    -- Best performance with (1024*1024) block sourceFile conduit
    blockLength     = Tagged ((1024 * 1024) * 8)
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

-- Master md4, total, child MD4, consumed to date
data Ctx = Ctx !MD4.Ctx !Int !MD4.Ctx !Int
data ED2K = Digest !B.ByteString
    deriving (Eq, Ord, Show)

-- Needed for deepseq and strictness
instance NFData B.ByteString where
    rnf a = a `seq` ()

instance NFData MD4.Ctx where
    rnf a = a `seq` ()

instance NFData Ctx where
    rnf a = a `seq` ()

initEd2k :: Ctx
initEd2k = Ctx MD4.init 0 MD4.init 0

-- Steps
-- 1. See how much data the child has consumed
-- 2. if less than blockSize, append more data up to blockSize
--  a. split the bytestring, feed the remaintant into hash
--  b. if there's more left, call update again and repeat
-- 3. if exactly blockSize, roll child into master, update master #, reset child to 0 and re-init
updateEd2k :: Ctx -> B.ByteString -> Ctx
updateEd2k xs@(Ctx _ _ _ cl) a
    | cl < blockSize = uncurry (updateHash xs) (split xs a)
    | otherwise      = updateEd2k (updateMasterCtx xs) a

    where
        split :: Ctx -> B.ByteString -> (B.ByteString, B.ByteString)
        split (Ctx _ _  _ cl) = B.splitAt (blockSize - cl)

        updateMasterCtx :: Ctx -> Ctx
        updateMasterCtx (Ctx m ml c cl) = Ctx (MD4.update m (MD4.finalize c)) (ml + cl) MD4.init 0

        updateChildCtx :: Ctx -> B.ByteString -> Ctx
        updateChildCtx (Ctx m ml c cl) a = Ctx m ml (MD4.update c a) (cl + B.length a)

        updateHash :: Ctx -> B.ByteString -> B.ByteString -> Ctx
        updateHash xs a1 a2
            | B.length a2 == 0  = updateChildCtx xs a1
            | otherwise         = updateEd2k (updateChildCtx xs a1) a2

-- Steps
-- 1. if master length is 0 finalize and return child
-- 2. otherwise roll up the child into master and finalize and return master
finalizeEd2k :: Ctx -> B.ByteString
finalizeEd2k (Ctx m ml c _)
    | ml == 0   = MD4.finalize c
    | otherwise = MD4.finalize (MD4.update m (MD4.finalize c))

hash :: B.ByteString -> B.ByteString
hash = finalizeEd2k . updateEd2k initEd2k

hashlazy :: BL.ByteString -> B.ByteString
hashlazy l = finalizeEd2k $ L.foldl' updateEd2k initEd2k (BL.toChunks l)
