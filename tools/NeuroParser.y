-- NeuroL Parser in Happy
-- Author: Artur M. Brodzki, December 2016
-- Proudly powered by Haskell


--- Code header

{-
	TODOS:
		File saving locked after opening program - lazy IO / conduit
		Handling errors when decompress
		Boolean expressions - expecially accessors to tensor elements
		Neuronal components of language
-}

{
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

}

%name neurol
%monad { IO }
%tokentype { Token }
%error { parseError }

%token
	'('        		{ OpeningBracket }
	')'        		{ ClosingBracket }
	'<'        		{ ContravariantOpen }
	'>'        		{ ContravariantClose }
	'['        		{ CovariantOpen }
	']'        		{ CovariantClose }
	'{'        		{ OpenBlock }
	'}'        		{ CloseBlock }
	';'        		{ Semicolon }
	'.'        		{ Dot }
	','        		{ Comma } 
	':'        		{ Colon }
	natural    		{ Natural $$ }
	int        		{ Integer $$ }
	real       		{ Real $$ }
	name       		{ Name $$ }
	index      		{ Index $$ }
	t          		{ Time }
	boolean    		{ Boolean $$ }
	path       		{ FilePath $$ }
	
	negate          { Negate }
	infiksAddNum  	{ InfiksAddNum $$ }
	infiksMultNum   { InfiksMultNum $$ }
	infiksPowNum    { InfiksPowNum $$ }
	infiksAddBool 	{ InfiksAddBool $$ }
	infiksMultBool  { InfiksMultBool $$ }
	tensProd        { TensorProduct }
	prefiksBool		{ PrefiksBoolNeg }
	prefiksUn  		{ PrefiksUnary $$ }
	prefiksBin 		{ PrefiksBinary $$ }
	normUn     		{ NormUnary $$ }
	normBin    		{ NormBinary $$ }
	tensUn     		{ TensUnary $$ }
	infiksComp 		{ InfiksComp $$ }
	assign     		{ AssignOperator }
	conn       		{ ConnectOperator }
	
	quantifier 		{ Quantifier $$ }
	if         		{ If } 
	then       		{ Then }
	else       		{ Else }
	import     		{ Import }
	interpol   		{ Interpolation $$ }
	get        		{ Get }
	from       		{ From }
	guard      		{ Guard }
	all        		{ All } 
	first      		{ First }
	maxtime    		{ Maxtime } 
	module     		{ Module }
	as         		{ As } 
	learning   		{ Learning }
	learnType  		{ LearningType $$ }
	learn      		{ Learn }
	input      		{ Input } 
	layer      		{ Layer }
	network    		{ Network } 
	experiment 		{ Experiment }
	
	exit            { Exit }
	binds           { Binds }
	clear           { Clear }
	
%%

-- GRAMMAR RULES SECTION

-- Start symbol
S			: Instr {
				$1
			}

-- Instruction type
Instr :: { Map String (Tensor Double) -> IO ( Map String (Tensor Double) ) }
Instr: 	name assign TensorExpr { -- Assign tensor to variable
			\ts ->  do
				if isJust ($3 ts) 
				then do
					let t = fromJust ($3 ts)
					when (Tensor.elems t > 100000) (putStrLn "Generating tensor...")
					let ts' = M.insert $1 t ts
					when (Tensor.elems t > 100000) (putStrLn "Generated! Saving session...")
					B.writeFile "~neurol.tmp" $ compress $ encode ts'
					putStrLn ($1 P.++ " saved!")
					return ts'
				else do
					putStrLn "Variable unknown!"
					return ts
		}
		| TensorExpr { -- just print a tensor expression value
			\ts -> do
				if isJust ($1 ts)
				then
					print (fromJust ($1 ts))
				else
					putStrLn "Variable unknown!"
				return ts
		}
		| exit {
			\ts -> exitSuccess
		}
		| binds {
			\ts -> 	do
				print $ keys ts
				return ts
		}
		| clear {
			\ts -> do
				let ts' = M.empty :: Map String (Tensor Double)
				B.writeFile "~neurol.tmp" $ compress $ encode ts'
				putStrLn "Variable bindings cleared!"
				return ts'
		}
		| clear name {
			\ts -> do
				let ts' = $2 `M.delete` ts :: Map String (Tensor Double)
				B.writeFile "~neurol.tmp" $ compress $ encode ts'
				putStrLn $ "Variable " P.++ $2 P.++ " deleted!"
				return ts'
			}
	
-- declaration of tensor index
IndexDecl :: { TIndex }
IndexDecl:  '<' index ':' natural '>' { -- index may be contravariant
				Contravariant $4 $2
			}
			| '[' index ':' natural ']' { -- or covariant
				Covariant $4 $2
			}

-- Scalar is just a number
Scalar :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
Scalar		: natural { -- natural number
				\ts -> Just $ Scalar (fromIntegral $1 :: Double)
			}
			| int { -- integer
				\ts -> Just $ Scalar (fromIntegral $1 :: Double)
			}
			| real { --real number
				\ts -> Just $ Scalar ($1)
			}
			| index { -- index value
				\ts -> $1 `M.lookup` ts
			}
			| negate Scalar {
				\ts -> do
					t <- ($2 ts)
					return $ (-1) * t
			}
			

-- Simple tensor is a tensor which value doesn't have to be computed
-- represented as function of current variables bindings
SimpleTensor :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
SimpleTensor 	: Scalar { -- simple tensor may be scalar
					$1
				}
				| name { -- or a tensor already saved as variable
					\ts -> $1 `M.lookup` ts
				}
	
-- Expression returning tensor
-- Expression is a sum of tensor generators
-- Is a function of current variables bindings
TensorExpr :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorExpr: TensorGen { 
				$1
			}
			| TensorExpr infiksAddNum TensorGen {
				\ts -> case $2 of
					"+" -> do
						t1 <- ($1 ts)
						t3 <- ($3 ts)
						return $ t1 + t3
					"-" -> do
						t1 <- ($1 ts)
						t3 <- ($3 ts)
						return $ t1 - t3
			}

-- Tensor generator
TensorGen :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorGen: IndexDecl TensorGen { -- as a function of indices
			\ts -> Just $ generate $1 (
				\i -> fromJust ($2 ( M.insert (indexName $1) (Scalar $ fromIntegral i) ts )) 
				)
		}
		| TensorFactor { -- as a simple factor
				$1
		}
			
-- Tensor factor is a product of tensors functional expressions
TensorFactor :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorFactor: TensorFunc {
				$1
			}
			| TensorFactor infiksMultNum TensorFunc {
				\ts -> case $2 of
					"*" -> do
						t1 <- ($1 ts)
						t3 <- ($3 ts)
						return $ t1 * t3
					"/" -> do
						t1 <- ($1 ts)
						t3 <- ($3 ts)
						return $ t1 / t3
			}

-- Tensor functional expression
TensorFunc :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorFunc: TensorPower {
				$1
			}
			| prefiksUn TensorPower {
				\ts -> case $1 of
					"exp"   -> ($2 ts) >>= (\t -> return $ exp t)
					"log"   -> ($2 ts) >>= (\t -> return $ log t)
					"sin"   -> ($2 ts) >>= (\t -> return $ sin t)
					"cos"   -> ($2 ts) >>= (\t -> return $ cos t)
					"asin"  -> ($2 ts) >>= (\t -> return $ asin t)
					"acos"  -> ($2 ts) >>= (\t -> return $ acos t)
					"atan"  -> ($2 ts) >>= (\t -> return $ atan t)
					"sinh"  -> ($2 ts) >>= (\t -> return $ sinh t)
					"cosh"  -> ($2 ts) >>= (\t -> return $ cosh t)
					"asinh" -> ($2 ts) >>= (\t -> return $ asinh t)
					"acosh" -> ($2 ts) >>= (\t -> return $ acosh t)
					"atanh" -> ($2 ts) >>= (\t -> return $ atanh t)
			}
			| tensUn TensorPower {
				\ts -> case $1 of
					"transpose" -> ($2 ts) >>= (\t -> return $ transpose t)
			}

-- Tensor power has the highest priority in operators precedence
TensorPower :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorPower : TensorTerm { 
				$1
			}
			| TensorTerm infiksPowNum TensorTerm {
				\ts -> case $2 of 
					"^" -> do
						t1 <- ($1 ts)
						t3 <- ($3 ts)
						return $ t1 ** t3
			}

-- Tensor term is a simple tensor...
-- ...or tensor expression in brackets, which must be computed first
TensorTerm :: { Map String (Tensor Double) -> Maybe (Tensor Double) }
TensorTerm: SimpleTensor {
				$1
			}
			| '(' TensorExpr ')' {
				$2
			}			
	
-- FINAL CODE SECTION			
{

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
	putStrLn "Welcome to Glorious NeuroL Interpreter 0.1!\nPlease type a command now..."
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
				putStrLn $ msg P.++ " Starting from clear session.\n"
				repl M.empty
			)
			(\(_,_,ts) -> do
				putStrLn "Session read!"
				repl ts)
			decoded
	else repl M.empty
	
}








	