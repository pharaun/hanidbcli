import qualified Crypto.Hash.MD4 as MD4
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.Ed2k as E
import Test.HUnit

main = runTestTT tests

blockSize :: Int
blockSize = 9500 * 1024

tests = TestList [
    "Empty"
        ~: (MD4.hash empty) ~=? (E.hash empty)
    , "Half Block"
        ~: (MD4.hash half1) ~=? (E.hash half1)
    , "Full Block"
        ~: (MD4.hash full1) ~=? (E.hash full1)
    , "2x Full Block"
        ~:  (MD4.hash (MD4.hash full1 `B.append` MD4.hash full2))
        ~=? (E.finalize (E.update (E.update (E.init) full1) full2))
    , "Double Full Block"
        ~:  (MD4.hash (MD4.hash full1 `B.append` MD4.hash full2))
        ~=? (E.finalize (E.update E.init (full1 `B.append` full2)))
    , "Full Block and Half Block"
        ~:  (MD4.hash (MD4.hash full1 `B.append` MD4.hash half1))
        ~=? (E.finalize (E.update E.init (full1 `B.append` half1)))
    , "2x Quarter Block"
        ~:  (MD4.hash (quarter1 `B.append` quarter2))
        ~=? (E.finalize (E.update (E.update (E.init) quarter1) quarter2))
    , "2x Half Block"
        ~:  (MD4.hash (half1 `B.append` half2))
        ~=? (E.finalize (E.update (E.update (E.init) half1) half2))
    , "3x Half Block"
        ~:  (MD4.hash ((MD4.hash (half1 `B.append` half2)) `B.append` (MD4.hash half3)))
        ~=? (E.finalize (E.update (E.update (E.update (E.init) half1) half2) half3))
    , "4x Half Block"
        ~:  (MD4.hash ((MD4.hash (half1 `B.append` half2)) `B.append` (MD4.hash (half3 `B.append` half4))))
        ~=? (E.finalize (E.update (E.update (E.update (E.update (E.init) half1) half2) half3) half4))
    , "Half Block and Full Block"
        ~:  (MD4.hash ((MD4.hash (half1 `B.append` half2)) `B.append` (MD4.hash half3)))
        ~=? (E.finalize (E.update (E.update (E.init) half1) (half2 `B.append` half3)))
    , "Half Block and Full Block and Half Block"
        ~:  (MD4.hash ((MD4.hash (half1 `B.append` half2)) `B.append` (MD4.hash (half3 `B.append` half4))))
        ~=? (E.finalize (E.update (E.update (E.update (E.init) half1) (half2 `B.append` half3)) half4))
    ]
    where
        empty = C.pack ""

        quarter1 = C.replicate (blockSize `div` 4) 'a'
        quarter2 = C.replicate (blockSize `div` 4) 'b'

        half1 = C.replicate (blockSize `div` 2) 'c'
        half2 = C.replicate (blockSize `div` 2) 'd'
        half3 = C.replicate (blockSize `div` 2) 'e'
        half4 = C.replicate (blockSize `div` 2) 'f'

        full1 = C.replicate blockSize 'g'
        full2 = C.replicate blockSize 'h'


-- Anidb reply parser test
--testString :: String
--testString = "dadsf 201 JasdSf 10.0.1.123:233 LOGIN ACCEPTED - NEW VERSION AVAILABLE\nanidb.imgserver.com|testdata|dadf\n"
--
--testEncode :: String -> L.ByteString
--testEncode = L.pack . U.encode
--
--testComp :: String -> L.ByteString
--testComp input = L.pack [0,0] `L.append` Z.compress (testEncode input)
