{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Main where

import Lekser
import Control.Monad
import Data.Maybe
import Tensor
import Prelude as P
import System.IO
import System.Directory
import System.Exit
import Control.Applicative(Applicative(..))
import Control.Monad (ap)
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict as M
import Codec.Compression.GZip
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn t4 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Map String (Tensor Double) -> IO ( Map String (Tensor Double) )) -> (HappyAbsSyn t4)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> IO ( Map String (Tensor Double) ))
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (TIndex) -> (HappyAbsSyn t4)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4) -> (TIndex)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Map String (Tensor Double) -> Maybe (Tensor Double)) -> (HappyAbsSyn t4)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t4) -> (Map String (Tensor Double) -> Maybe (Tensor Double))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x01\x00\x00\x00\x16\x00\x00\x00\x00\x00\x13\x00\x00\x00\x11\x00\x00\x00\x00\x00\x05\x00\x16\x00\x10\x00\x0e\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x76\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x0a\x00\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x0c\x00\x08\x00\xff\xff\x1f\x00\x31\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xfc\xff\xf2\xff\xfd\xff\x03\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x53\x00\x5d\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x8f\x00\x8d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x09\x00\x85\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\xf0\xff\xe3\xff\xfc\xff\xee\xff\xeb\xff\xea\xff\xe8\xff\xe5\xff\x00\x00\x00\x00\x00\x00\xf5\xff\xf4\xff\xf3\xff\xef\xff\xf2\xff\x00\x00\x00\x00\x00\x00\xfb\xff\xfa\xff\xf9\xff\x00\x00\xf8\xff\xe6\xff\xef\xff\xe7\xff\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\xed\xff\xe9\xff\xe4\xff\xe2\xff\x00\x00\x00\x00\xfd\xff\x00\x00\x00\x00\xf7\xff\xf6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x06\x00\x03\x00\x03\x00\x05\x00\x04\x00\x16\x00\x0d\x00\x3d\x00\x0d\x00\x03\x00\x04\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0a\x00\x0c\x00\x16\x00\x15\x00\x01\x00\x0c\x00\x03\x00\x10\x00\x05\x00\x23\x00\x18\x00\x1d\x00\x11\x00\x01\x00\x11\x00\x21\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x17\x00\x16\x00\xff\xff\x15\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x01\x00\x1d\x00\x15\x00\xff\xff\xff\xff\x21\x00\xff\xff\xff\xff\xff\xff\x3a\x00\x3b\x00\x3c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x02\x00\x03\x00\x04\x00\x15\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\xff\xff\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x03\x00\x04\x00\xff\xff\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0d\x00\x0e\x00\x0f\x00\xff\xff\x11\x00\x03\x00\x04\x00\xff\xff\x15\x00\xff\xff\x08\x00\x09\x00\x0a\x00\x03\x00\x04\x00\x03\x00\x04\x00\xff\xff\xff\xff\x09\x00\x0a\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2c\x00\x0d\x00\x33\x00\x0e\x00\x1f\x00\x0f\x00\x32\x00\x27\x00\x30\x00\xff\xff\x31\x00\x04\x00\x05\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x2a\x00\x2d\x00\x27\x00\x15\x00\x0d\x00\x2e\x00\x0e\x00\x1c\x00\x0f\x00\x21\x00\x25\x00\x16\x00\x22\x00\x0d\x00\x23\x00\x17\x00\x10\x00\x11\x00\x12\x00\x1e\x00\x14\x00\x26\x00\x27\x00\x00\x00\x15\x00\x10\x00\x11\x00\x12\x00\x1e\x00\x14\x00\x00\x00\x0d\x00\x16\x00\x15\x00\x00\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x18\x00\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x1e\x00\x14\x00\x03\x00\x04\x00\x05\x00\x15\x00\x28\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x17\x00\x1a\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x2e\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x23\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x00\x00\x27\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x10\x00\x11\x00\x12\x00\x00\x00\x14\x00\x04\x00\x05\x00\x00\x00\x15\x00\x00\x00\x29\x00\x0a\x00\x0b\x00\x04\x00\x05\x00\x04\x00\x05\x00\x00\x00\x00\x00\x1c\x00\x0b\x00\x1e\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 29) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29)
	]

happy_n_terms = 62 :: Int
happy_n_nonterms = 11 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (happy_var_1
	)}

happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (Name happy_var_1) -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (-- Assign tensor to variable
			\ts ->  do
				if isJust (happy_var_3 ts) 
				then do
					let t = fromJust (happy_var_3 ts)
					when (Tensor.elems t > 100000) (putStrLn "Generating tensor...")
					let ts' = M.insert happy_var_1 t ts
					when (Tensor.elems t > 100000) (putStrLn "Generated! Saving session...")
					B.writeFile "~neurol.tmp" $ compress $ encode ts'
					putStrLn (happy_var_1 P.++ " saved!")
					return ts'
				else do
					putStrLn "Variable unknown!"
					return ts
	)}}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (-- just print a tensor expression value
			\ts -> do
				if isJust (happy_var_1 ts)
				then
					print (fromJust (happy_var_1 ts))
				else
					putStrLn "Variable unknown!"
				return ts
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  happyIn5
		 (\ts -> exitSuccess
	)

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn5
		 (\ts -> 	do
				print $ keys ts
				return ts
	)

happyReduce_6 = happySpecReduce_1  1# happyReduction_6
happyReduction_6 happy_x_1
	 =  happyIn5
		 (\ts -> do
				let ts' = M.empty :: Map String (Tensor Double)
				B.writeFile "~neurol.tmp" $ compress $ encode ts'
				putStrLn "Variable bindings cleared!"
				return ts'
	)

happyReduce_7 = happySpecReduce_2  1# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (Name happy_var_2) -> 
	happyIn5
		 (\ts -> do
				let ts' = happy_var_2 `M.delete` ts :: Map String (Tensor Double)
				B.writeFile "~neurol.tmp" $ compress $ encode ts'
				putStrLn $ "Variable " P.++ happy_var_2 P.++ " deleted!"
				return ts'
	)}

happyReduce_8 = happyReduce 5# 2# happyReduction_8
happyReduction_8 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (Index happy_var_2) -> 
	case happyOutTok happy_x_4 of { (Natural happy_var_4) -> 
	happyIn6
		 (-- index may be contravariant
				Contravariant happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

happyReduce_9 = happyReduce 5# 2# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (Index happy_var_2) -> 
	case happyOutTok happy_x_4 of { (Natural happy_var_4) -> 
	happyIn6
		 (-- or covariant
				Covariant happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

happyReduce_10 = happySpecReduce_1  3# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Natural happy_var_1) -> 
	happyIn7
		 (-- natural number
				\ts -> Just $ Scalar (fromIntegral happy_var_1 :: Double)
	)}

happyReduce_11 = happySpecReduce_1  3# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Integer happy_var_1) -> 
	happyIn7
		 (-- integer
				\ts -> Just $ Scalar (fromIntegral happy_var_1 :: Double)
	)}

happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Real happy_var_1) -> 
	happyIn7
		 (--real number
				\ts -> Just $ Scalar (happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  3# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Index happy_var_1) -> 
	happyIn7
		 (-- index value
				\ts -> happy_var_1 `M.lookup` ts
	)}

happyReduce_14 = happySpecReduce_2  3# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (\ts -> do
					t <- (happy_var_2 ts)
					return $ (-1) * t
	)}

happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (-- simple tensor may be scalar
					happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  4# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Name happy_var_1) -> 
	happyIn8
		 (-- or a tensor already saved as variable
					\ts -> happy_var_1 `M.lookup` ts
	)}

happyReduce_17 = happySpecReduce_1  5# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (happy_var_1
	)}

happyReduce_18 = happySpecReduce_3  5# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (InfiksAddNum happy_var_2) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (\ts -> case happy_var_2 of
					"+" -> do
						t1 <- (happy_var_1 ts)
						t3 <- (happy_var_3 ts)
						return $ t1 + t3
					"-" -> do
						t1 <- (happy_var_1 ts)
						t3 <- (happy_var_3 ts)
						return $ t1 - t3
	)}}}

happyReduce_19 = happySpecReduce_2  6# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (-- as a function of indices
			\ts -> Just $ generate happy_var_1 (
				\i -> fromJust (happy_var_2 ( M.insert (indexName happy_var_1) (Scalar $ fromIntegral i) ts )) 
				)
	)}}

happyReduce_20 = happySpecReduce_1  6# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (-- as a simple factor
				happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_22 = happySpecReduce_3  7# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (InfiksMultNum happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (\ts -> case happy_var_2 of
					"*" -> do
						t1 <- (happy_var_1 ts)
						t3 <- (happy_var_3 ts)
						return $ t1 * t3
					"/" -> do
						t1 <- (happy_var_1 ts)
						t3 <- (happy_var_3 ts)
						return $ t1 / t3
	)}}}

happyReduce_23 = happySpecReduce_1  8# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_2  8# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (PrefiksUnary happy_var_1) -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (\ts -> case happy_var_1 of
					"exp"   -> (happy_var_2 ts) >>= (\t -> return $ exp t)
					"log"   -> (happy_var_2 ts) >>= (\t -> return $ log t)
					"sin"   -> (happy_var_2 ts) >>= (\t -> return $ sin t)
					"cos"   -> (happy_var_2 ts) >>= (\t -> return $ cos t)
					"asin"  -> (happy_var_2 ts) >>= (\t -> return $ asin t)
					"acos"  -> (happy_var_2 ts) >>= (\t -> return $ acos t)
					"atan"  -> (happy_var_2 ts) >>= (\t -> return $ atan t)
					"sinh"  -> (happy_var_2 ts) >>= (\t -> return $ sinh t)
					"cosh"  -> (happy_var_2 ts) >>= (\t -> return $ cosh t)
					"asinh" -> (happy_var_2 ts) >>= (\t -> return $ asinh t)
					"acosh" -> (happy_var_2 ts) >>= (\t -> return $ acosh t)
					"atanh" -> (happy_var_2 ts) >>= (\t -> return $ atanh t)
	)}}

happyReduce_25 = happySpecReduce_2  8# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TensUnary happy_var_1) -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (\ts -> case happy_var_1 of
					"transpose" -> (happy_var_2 ts) >>= (\t -> return $ transpose t)
	)}}

happyReduce_26 = happySpecReduce_1  9# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_27 = happySpecReduce_3  9# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (InfiksPowNum happy_var_2) -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (\ts -> case happy_var_2 of 
					"^" -> do
						t1 <- (happy_var_1 ts)
						t3 <- (happy_var_3 ts)
						return $ t1 ** t3
	)}}}

happyReduce_28 = happySpecReduce_1  10# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (happy_var_1
	)}

happyReduce_29 = happySpecReduce_3  10# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 61# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	OpeningBracket -> cont 1#;
	ClosingBracket -> cont 2#;
	ContravariantOpen -> cont 3#;
	ContravariantClose -> cont 4#;
	CovariantOpen -> cont 5#;
	CovariantClose -> cont 6#;
	OpenBlock -> cont 7#;
	CloseBlock -> cont 8#;
	Semicolon -> cont 9#;
	Dot -> cont 10#;
	Comma -> cont 11#;
	Colon -> cont 12#;
	Natural happy_dollar_dollar -> cont 13#;
	Integer happy_dollar_dollar -> cont 14#;
	Real happy_dollar_dollar -> cont 15#;
	Name happy_dollar_dollar -> cont 16#;
	Index happy_dollar_dollar -> cont 17#;
	Time -> cont 18#;
	Boolean happy_dollar_dollar -> cont 19#;
	FilePath happy_dollar_dollar -> cont 20#;
	Negate -> cont 21#;
	InfiksAddNum happy_dollar_dollar -> cont 22#;
	InfiksMultNum happy_dollar_dollar -> cont 23#;
	InfiksPowNum happy_dollar_dollar -> cont 24#;
	InfiksAddBool happy_dollar_dollar -> cont 25#;
	InfiksMultBool happy_dollar_dollar -> cont 26#;
	TensorProduct -> cont 27#;
	PrefiksBoolNeg -> cont 28#;
	PrefiksUnary happy_dollar_dollar -> cont 29#;
	PrefiksBinary happy_dollar_dollar -> cont 30#;
	NormUnary happy_dollar_dollar -> cont 31#;
	NormBinary happy_dollar_dollar -> cont 32#;
	TensUnary happy_dollar_dollar -> cont 33#;
	InfiksComp happy_dollar_dollar -> cont 34#;
	AssignOperator -> cont 35#;
	ConnectOperator -> cont 36#;
	Quantifier happy_dollar_dollar -> cont 37#;
	If -> cont 38#;
	Then -> cont 39#;
	Else -> cont 40#;
	Import -> cont 41#;
	Interpolation happy_dollar_dollar -> cont 42#;
	Get -> cont 43#;
	From -> cont 44#;
	Guard -> cont 45#;
	All -> cont 46#;
	First -> cont 47#;
	Maxtime -> cont 48#;
	Module -> cont 49#;
	As -> cont 50#;
	Learning -> cont 51#;
	LearningType happy_dollar_dollar -> cont 52#;
	Learn -> cont 53#;
	Input -> cont 54#;
	Layer -> cont 55#;
	Network -> cont 56#;
	Experiment -> cont 57#;
	Exit -> cont 58#;
	Binds -> cont 59#;
	Clear -> cont 60#;
	_ -> happyError' (tk:tks)
	}

happyError_ 61# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => IO a -> (a -> IO b) -> IO b
happyThen = (>>=)
happyReturn :: () => a -> IO a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> IO a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> IO a
happyError' = parseError

neurol tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


parseError :: [Token] -> IO a
parseError tok = do
	putStrLn $ "Command error at " P.++ show tok P.++ "!"
	s <- decodeFile "~neurol.tmp"
	repl s

-- procedure of REPL interpreter
repl :: Map String (Tensor Double) -> IO a
repl ts = do
	putStr ">> " 		-- Show prompt
	hFlush stdout
	s <- getLine 		-- read command
	resF <- neurol (alexScanTokens s) -- parse command
	ts' <- resF ts -- execute command and create new vars bindings
	repl ts' -- loop

main :: IO a
main = do
	putStrLn "Welcome to Glorious NeuroL Interpreter 0.1!"
	hFlush stdout
	isSession <- doesFileExist "~neurol.tmp"
	if isSession
	then do
		putStrLn "Reading saved session..."
		hFlush stdout
		bs <- B.readFile "~neurol.tmp"
		let decoded = decodeOrFail (decompress (seq (B.length bs) bs))
		either 
			(\(_,_,msg) -> do
				putStrLn $ msg P.++ " Starting from clear session.\nPlease type a command now..."
				repl M.empty
			)
			(\(_,_,ts) -> do
				putStrLn "Session read!\nPlease type a command now..."
				repl ts)
			decoded
	else repl M.empty
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc16932_0\\ghc_2.h" #-}






















































































































































{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
