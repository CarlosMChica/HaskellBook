{-# LANGUAGE BangPatterns #-}
module ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

--doesntEval
--doesntEval =
--  \_ -> I# 1#
--manualSeq
--manualSeq =
--  \b_a1ia ->
--    case b_a1ia of _
--                     { __DEFAULT -> I# 1# }
--banging
--banging =
--  \b_a1ib ->
--    case b_a1ib of _
--                     { __DEFAULT -> I# 1# }} -> undefined} -> undefined -> undefined
