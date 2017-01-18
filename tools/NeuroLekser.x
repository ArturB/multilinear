-- NeuroL Lexer in Alex
-- Author: Artur M. Brodzki, December 2016
-- Proudly powered by Haskell


--- Code header
{

module Lekser (
	alexScanTokens, 
	Token(..) 
) where

}

%wrapper "basic"

--MAKRA

$dig = 0-9		
$posDig = 1-9	                                                 -- digits
$alpha = [a-zA-Z]		                                         -- alphabetic characters
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]              -- znaki specjalne 
$keyboard = [a-zA-Z0-9\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\\]  -- standardowe znaki z klawiatury

--DEFINICJE TOKENÓW

tokens :-

  -- SŁOWA KLUCZOWE (mają największy priorytet)
  
  for|any                           { \s -> Quantifier s }              -- kwantyfikatory
  if|If|IF                          { \s -> If }                        -- if
  then|Then|THEN                    { \s -> Then }                      -- then
  else|Else|ELSE                    { \s -> Else }                      -- else
  import|Import|IMPORT              { \s -> Import }                    -- import
  flat|Flat|FLAT                    { \s -> Interpolation s }           -- interpolacja płaska
  linear|Linear|LINEAR              { \s -> Interpolation s }           -- interpolacja liniowa
  quadratic|Quadratic|QUADRATIC     { \s -> Interpolation s }           -- interpolacja kwadratowa
  get|Get|GET                       { \s -> Get }                       -- get keyword
  all|All|ALL                       { \s -> All }                       -- get all keyword
  first|First|FIRST                 { \s -> First }                     -- get any keyword
  from|From|FROM                    { \s -> From }                      -- from keyword
  maxtime|Maxtime|MAXTIME           { \s -> Maxtime }                   -- maxtime keyword
  \||where|Where|WHERE              { \s -> Guard }                     -- strażnik
  module|Module|MODULE              { \s -> Module }                    -- module keyword
  as|As|AS                          { \s -> As }                        -- as keyword
  learning|Learning|LEARNING        { \s -> Learning }                  -- learning keyword
  synch|Synch|SYNCH                 { \s -> LearningType s }            -- uczenie synchroniczne
  cyclic|Cyclic|CYCLIC              { \s -> LearningType s }            -- uczenie cykliczne
  asynch|Asynch|ASYNCH              { \s -> LearningType s }            -- uczenie asynchroniczne
  learn|Learn|LEARN                 { \s -> Learn }                     -- wskazanie funkcji uczącej
  input|Input|INPUT                 { \s -> Input }                     -- input keyword
  layer|Layer|LAYER                 { \s -> Layer }                     -- layer keyword
  network|Network|NETWORK           { \s -> Network }                   -- network keyword
  experiment|Experiment|EXPERIMENT  { \s -> Experiment }                -- experiment keyword
  
  -- LITERAŁY (większy priorytet niż operatory)
  \/\/.*\n			             	;                                   -- komentarz jednolinijkowy
  \/\*.*\*\/                        ;                                   -- komentarz wielonijkowy
  $white+			            	;                                   -- usuwamy białe znaki
  ","                               { \s -> Comma }                     -- przecinek
  ":"                               { \s -> Colon }                     -- dwukropek
  ";"                               { \s -> Semicolon }                 -- średnik
  "."                               { \s -> Dot }                       -- kropka
  \(                                { \s -> OpeningBracket }            -- nawias otwierający
  \)                                { \s -> ClosingBracket }            -- nawias zamykający
  \<                                { \s -> ContravariantOpen }         -- otwarcie wyrażenia kontrawariantnego
  \>                                { \s -> ContravariantClose }        -- zamknięcie wyrażenia kontrawariatnego
  \[                                { \s -> CovariantOpen }             -- otwarcie wyrażenia kowariantnego
  \]                                { \s -> CovariantClose }            -- zamknięcie wyrażenia kowariantnego
  \{                                { \s -> OpenBlock }                 -- nawias klamrowy - otwarcie bloku
  \}                                { \s -> CloseBlock }                -- nawias klamrowy - zamknięcie bloku
  t                                 { \s -> Time }                      -- zmienna czasu 
  0|$posDig$dig*   	                { \s -> Natural (read s) }          -- liczba naturalna
  0|$posDig$dig*                    { \s -> Integer (read s) }          -- liczba całkowita
  pi                                { \s -> Real 3.14159 }              -- liczba rzeczywista pi
  e                                 { \s -> Real 2.71828 }              -- liczba rzeczywista e
  0|0\.$dig+|$posDig$dig*\.$dig+    { \s -> Real (read s) }             -- dowolna liczba rzeczywista
  [A-Z][a-zA-Z0-9]*                 { \s -> Name s }                    -- nazwa tensora
  [a-z][0-9]?                       { \s -> Index s }                   -- nazwa indeksu          
  true|TRUE|True                    { \s -> Boolean True }              -- literał logiczny true
  false|FALSE|False                 { \s -> Boolean False }             -- literał logiczny false
  \"$keyboard+\"                    { \s -> FilePath s }                -- ścieżka do pliku
  
  -- OPERATORY
  \-                                { \s -> Negate }                    -- operator liczby ujemnej                             
  \-\-|" - "                        { \s -> InfiksAddNum "-" }          -- operator odejmowania - musi być rozróżnialny od liczby ujemnej!
  \+                                { \s -> InfiksAddNum s }            -- operatory infiksowe liczbowe addytywne
  \*|\/|mod|\%                      { \s -> InfiksMultNum s }           -- operatory infiksowe liczbowe multiplikatywne
  \^                                { \s -> InfiksPowNum s }            -- operatory infiksowe liczbowe potęgowe
  or|\|\||xor                       { \s -> InfiksAddBool s }           -- operatory infiksowe logiczne addytywne
  and|&&                            { \s -> InfiksMultBool s }          -- operatory infiksowe logiczne multipikatywne
  not|\~                            { \s -> PrefiksBoolNeg }            -- operatory prefiksowe logiczne 
  "!*"                              { \s -> TensorProduct }             -- iloczyn tensorowy
  exp|log|sqrt|sgn|thr              { \s -> PrefiksUnary s }            -- podstawowe funkcje liczbowe
  sin|cos|tan|asin|acos|atan        { \s -> PrefiksUnary s }            -- funkcje trygonometryczne i odwrotne
  sinh|cosh|tanh|asinh|acosh|atanh  { \s -> PrefiksUnary s }            -- funkcje hiperboliczne i area
  ceil|floor|round|abs              { \s -> PrefiksUnary s }            -- zaokrąglanie
  uniform|gauss|poisson|binomial    { \s -> PrefiksBinary s }           -- rozkłady prawdopodobieństwa
  taxi|euclidean|max                { \s -> NormUnary s }               -- metryki jednoargumentowe
  p\-norm                           { \s -> NormBinary s }              -- metryki dwuargumentowe
  inverse|transpose                 { \s -> TensUnary s }               -- operatory tensorowe - odwrotność i transpozycja
  "=="|"!="|">="|"<="               { \s -> InfiksComp s }              -- operatory porównawcze infiksowe
  ":="                              { \s -> AssignOperator }            -- operator przypisania
  \-+\>                             { \s -> ConnectOperator }           -- operator łączenia warstw sieci
  
  -- POLECENIA INTERPRETERA
  exit|quit                         { \s -> Exit }                      -- wyłączn intepreter
  binds                             { \s -> Binds }                     -- show currently binded variables
  clear                             { \s -> Clear }
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	OpeningBracket       |
	ClosingBracket       |
	ContravariantOpen    |
	ContravariantClose   |
	CovariantOpen        |
	CovariantClose       |
	OpenBlock            |
	CloseBlock           |
	Semicolon            |
	Dot | Comma | Colon  |
	Natural Int          | 
	Integer Int          |
	Real Double          |
	Name String          |
	Index String         |
	Time                 |
	Boolean Bool         |
	FilePath String      |
	
	Negate               |
	InfiksAddNum String  |
	InfiksMultNum String |
	InfiksPowNum String  |
	InfiksMultBool String|
	InfiksAddBool String |
	PrefiksBoolNeg       |
	TensorProduct        |
	PrefiksUnary String  | 
	PrefiksBinary String |
	NormUnary String     |
	NormBinary String    |
	TensUnary String     |
	InfiksComp String    |
	AssignOperator       |
	ConnectOperator      |
	
	Quantifier String    |
	If | Then | Else     |
	Import               |
	Interpolation String |
	Get | From | Guard   |
	All | First          |
	Maxtime | Module     |
	As | Learning        |
	LearningType String  |
	Learn                |
	Input | Layer        |
	Network | Experiment |
	
	Exit | Binds | Clear
	
	deriving (Eq,Show)

-- Lexer entry point

{-
main = do
  s <- getContents
  print (alexScanTokens s) 
-}

} 

