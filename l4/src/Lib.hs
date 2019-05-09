{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( evalMath
    , mkFSFD
    , runScenario
    , goals
    , nlgstyles
    , loadTestFile
    , Verbosity(VNeutral,VConcrete,VAbstract,VAlgebra)
    , SomeFact(FS)
    , Environment(Env)
    , variables_dict
    , RatesRebateWorld
    ) where

-- import qualified Data.Yaml as Y
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.Maybe
import Text.Printf
import Control.Monad.Reader

variables_dict :: NLdict
variables_dict = Map.fromList [("en",
                                Map.fromList [
    ("rates_total",              NLAttr UserVar "the total rates for the property")
  , ("combined_income",          NLAttr UserVar "the ratepayer's income for the preceding tax year")
  , ("dependants",               NLAttr UserVar "the number of Persons classified as dependant for the purposes of Rates Rebates")
  , ("initial_contribution",     NLAttr LegislativeParam "the initial contribution by ratepayer")
  , ("additional_per_dependant", NLAttr LegislativeParam "the additional allowable income per dependant")
  , ("income_threshold",         NLAttr LegislativeParam "the income threshold")
  , ("maximum_allowable",        NLAttr LegislativeParam "the maximum rebate allowed")
  , ("excess_rates_amount",      NLAttr LegislativeParam "the excess rates amount")
  , ("income_taper_amount",      NLAttr LegislativeParam "the income taper amount")
  , ("income_taper_trigger",     NLAttr LegislativeParam "the income taper trigger")
  ])]

type Goals = Map.Map String (MathExpr Double)
goals :: Goals
goals = Map.fromList [("math1", (Mul (Div (Lit 1.0) (Lit 2.0)) (Lit 8.0)))
                     ,("math2", (Mul (Div (Lit 1.0) (Lit 2.0)) (Descr "week_numDays")))
                     ,("rr-br3nda-clip", (Min
                                    (Max (Lit 0)
                                     (Sub
                                      (Labeled "excess_rates_amount"
                                      (Mul
                                       (Div (Lit 2.0) (Lit 3.0))
                                       (Max (Lit 0)
                                       (Sub
                                        (Descr "rates_total")
                                        (Descr "initial_contribution")))))
                                      (Max (Lit 0)
                                      (Labeled "income_taper_amount"
                                       (Div
                                        (Labeled "the income component adjusted for dependants"
                                        (Sub
                                         (Descr "combined_income")
                                          (Labeled "income_taper_trigger"
                                           (Sum
                                            (Descr "income_threshold")
                                            (Mul
                                             (Descr "additional_per_dependant")
                                             (Descr "dependants"))))))
                                        (Lit 8.0))))))
                                     (Descr "maximum_allowable")))
                     ,("rr-br3nda-noclip", (Min
                                    (Max (Lit 0)
                                     (Sub
                                      (Labeled "excess_rates_amount"
                                      (Mul
                                       (Div (Lit 2.0) (Lit 3.0))
                                       (Max (Lit 0)
                                       (Sub
                                        (Descr "rates_total")
                                        (Descr "initial_contribution")))))
                                      (Labeled "income_taper_amount"
                                       (Div
                                        (Labeled "the income component adjusted for dependants"
                                        (Sub
                                         (Descr "combined_income")
                                          (Labeled "income_taper_trigger"
                                           (Sum
                                            (Descr "income_threshold")
                                            (Mul
                                             (Descr "additional_per_dependant")
                                             (Descr "dependants"))))))
                                        (Lit 8.0)))))
                                     (Descr "maximum_allowable")))
                     ]



type NatLang = String -- locale string? "en-us"

-- let's try to model the rates rebate act 1973!
data SomeFact = FS String
              | FD Double
              | FB Bool
              | FM (MathExpr Double) -- should we allow FM or should we keep MathExprs separate from Facts?
              deriving (Ord, Eq, Show)
type RatesRebateWorld = Map.Map SomeFact SomeFact

data Environment = Env { verbosity :: Maybe Verbosity
                       , facts :: RatesRebateWorld
                       , nldict :: NLdict
                       }
                   deriving Show

data NLExpr = FLS NatLang String -- later we can make this more complicated with gender and other cases

type NLDict = Map.Map SomeFact NLExpr

-- TODO next: pass a dictionary of english expressions alongside, upgrade everythin to a monad reader, jam both the fact world and the dictionary together in the same environment

-- we can explain the rule in the abstract (intensionally, by giving constitutive definitions)
-- we can explain the rule concretely (extensionally, filling in known values wherever possible)
data Verbosity = VAbstract | VNeutral | VConcrete | VAlgebra
  deriving (Ord, Eq, Show)

mkFSFD x y = (FS x, FD y)

rates_rebate__1 :: RatesRebateWorld -> Double
rates_rebate__1 world = 0.0

type MathVarName = String

data MathExpr a = Lit a
                | Descr MathVarName
                | IdiomaticNum String a
                | Sum (MathExpr a) (MathExpr a) -- x + y
                | Sub (MathExpr a) (MathExpr a) -- x - y
                | Mul (MathExpr a) (MathExpr a) -- x * y
                | Div (MathExpr a) (MathExpr a) -- x / y
                | Min (MathExpr a) (MathExpr a) -- min x y
                | Max (MathExpr a) (MathExpr a) -- max x y
                | Labeled MathVarName (MathExpr a) -- let varFive = (2+3)  or perhaps   varFive@(2+3)
                deriving (Ord, Eq, Show)

toAlgebra :: Environment -> MathExpr Double -> String
toAlgebra env m = let v = verbosity env
                      sho = toAlgebra env
                  in case m of
  (Sum x y) -> "(" ++ sho x ++ "+" ++ sho y ++ ")"
  (Sub x y) -> "(" ++ sho x ++ "-" ++ sho y ++ ")"
  (Mul x y) -> "(" ++ sho x ++ "*" ++ sho y ++ ")"
  (Div x y) -> "(" ++ sho x ++ "/" ++ sho y ++ ")"
  (Max x y) -> "max(" ++ sho x ++ ", " ++ sho y ++ ")"
  (Min x y) -> "min(" ++ sho x ++ ", " ++ sho y ++ ")"
  (Lit x)   -> showdouble x
  (Labeled s x) -> sho x
  (Descr x) -> equalsFact x (facts env)

notGiven x w = case Map.lookup (FS x) w of
                 Just (FD y) -> showdouble y
                 _           -> "not given -- you'll be wanting to define " ++ x ++ ", please."

equalsFact x w = case Map.lookup (FS x) w of
                   Just (FD y) -> "(" ++ x ++ "=" ++ showdouble y ++ ")"
                   _           -> x

showdouble = printf "%.2f"

-- extract variable definitions into RatesRebateWorld

evalMath :: RatesRebateWorld -> MathExpr Double -> Double
evalMath w (Sum x y) = evalMath w x + evalMath w y
evalMath w (Sub x y) = evalMath w x - evalMath w y
evalMath w (Mul x y) = evalMath w x * evalMath w y
evalMath w (Div x y) = evalMath w x / evalMath w y
evalMath w (Min x y) = min (evalMath w x) (evalMath w y)
evalMath w (Max x y) = max (evalMath w x) (evalMath w y)
evalMath w (IdiomaticNum s x) = x
evalMath w (Labeled s x) = evalMath w x
evalMath w (Lit x)   = x
evalMath w (Descr x) = case (Map.lookup (FS x) w) of
                         Just (FD y) -> y
                         Nothing     -> error $ "in math expression, description " ++ x ++ " was not found in the RatesRebateWorld"
                         Just _      -> error $ "in math expression, description " ++ x ++ " needs to resolve to a Double, but is some other type. :-("

doublespace = "| "
space x = concat $ replicate x doublespace

-- todo: express IdiomaticNum / Labeled in terms of each other, using a transfer function

is_idiomatic x = x /= transfer_idiomatic x

transfer_idiomatic :: MathExpr Double -> MathExpr Double
transfer_idiomatic indiv@(Div (Lit x) (Lit y)) = case (x,y) of
                                                   (1.0, 2.0) -> IdiomaticNum "one-half"       (x/y)
                                                   (2.0, 3.0) -> IdiomaticNum "two-thirds"     (x/y)
                                                   (3.0, 4.0) -> IdiomaticNum "three-quarters" (x/y)
                                                   (1.0, 4.0) -> IdiomaticNum "one-quarter"    (x/y)
                                                   _ -> indiv
transfer_idiomatic x = x

-- todo: upgrade this to a Reader monad, and stuff (v,w) inside it
-- for extra credit, stuff this inside a RWST monad transformer to increment depth

sayMath :: Int -> Environment -> MathExpr Double -> String
sayMath depth env m = let space0 = space depth
                          space1 = space $ depth + 1
                          space_1 = space $ depth - 1
                          v = verbosity env
                          appropriate_space = if v==Just VConcrete then space0 else space0
                          indent1 (line1,part2) = (space0 ++ line1 ++                      part2)
                          indent2 (line1,part2) = (space0 ++ line1 ++ appropriate_space ++ part2)
                          indent0 = (++) space0
                          sho = sayWithAnswer (depth+1) env
                       in case m of
  (Sum x y) -> indent2 ("the sum of\n" ++ sho x ++ "\n",          "with\n" ++ sho y)
  (Sub x y) -> indent2 ("the amount by which\n" ++ sho x ++ "\n", "exceeds\n" ++ sho y)
  (Mul x y) -> if is_idiomatic x
    then let (IdiomaticNum idiomatic_string idiomatic_value) = transfer_idiomatic x
         in indent1 (idiomatic_string ++ "\n", sho y)
    else indent2 ("the product of\n" ++ sho x ++ "\n",           "multiplied by\n" ++ sho y)
  (Div x y) -> indent2("the quotient given by\n" ++ sho x ++ "\n", "divided by\n" ++ sho y)
  (Min x y) -> indent2("the lesser of\n" ++ sho x ++ "\n", "and\n" ++ sho y)
  (Max x y) -> indent2("the greater of\n" ++ sho x ++ "\n", "and\n" ++ sho y)
  (Lit x)       -> space0 ++ showdouble x
  (Labeled s x) -> let nlsyntax__ = do
                         nlattr <- enlookup s $ nldict env
                         return $ nlsyntax nlattr
                       nlsyntax_ = Data.Maybe.fromMaybe s nlsyntax__
                   in indent1(nlsyntax_ ++", which is" ++ "\n", sayMath depth env x)
  (Descr x) -> indent0 $ case verbosity env of
    Nothing          -> ""
    Just VConcrete   -> x ++ " (which has the value " ++ (showdouble $ evalMath (facts env) m) ++ ")"
    Just VAbstract   -> (Data.Maybe.fromMaybe x (nlsyntax <$> (enlookup x $ nldict env)))
    Just VNeutral    -> Data.Maybe.fromMaybe x $ do
      nlattr <- (enlookup x $ nldict env)
      let nlsyntax_ = nlsyntax nlattr
          nlporv_   = nlporv   nlattr
      return $ case nlporv_ of
                 LegislativeParam -> nlsyntax_ ++ ", which is " ++ notGiven x (facts env)
                 UserVar          -> nlsyntax_
                 BitOfBoth        -> x ++ " (" ++ nlsyntax_ ++ " is " ++ notGiven x (facts env) ++ ")"
      

sayWithAnswer :: Int -> Environment -> MathExpr Double -> String
sayWithAnswer depth env m = let saidMath = sayMath depth env m in
  case m of
    (Descr x) -> saidMath
    (Lit x)   -> saidMath
    _         -> answer ++ saidMath
  where answer = (if (verbosity env) == Just VConcrete
                  then (space $ depth-1) ++ (showdouble $ evalMath (facts env) m) ++ " -- which is\n"
                  else "")

-- some examples of things we can say

nlgstyles :: Map.Map String (Verbosity)
nlgstyles = Map.fromList [("abstract", VAbstract)
                         ,("neutral",  VNeutral)
                         ,("concrete", VConcrete)
                         ,("algebra",  VAlgebra)
                         ]

                      
runScenario env goal
  | verbosity env == Just VAlgebra = toAlgebra env goal
  | verbosity env == Nothing = showdouble $ evalMath (facts env) goal
  | otherwise = case verbosity env of
      Just VAbstract -> "the goal is defined as\n"
      Just VNeutral  -> "the goal you're looking for is\n"
      Just VConcrete -> "showing how we obtain the answer\n"
    ++ (sayWithAnswer 1 env goal)

type LangID = String   -- "en-us", "en-gb"
type NLKey = String    -- "potato" -- language-independent
type NLSyntax = String -- in future this will be some kind of GF structure handling parts of speech, pluralization, gender, etc.
type NLdict = Map.Map LangID (Map.Map NLKey NLAttr)

data NLAttr = NLAttr { nlporv   :: LegislativeParamOrUserVar
                     , nlsyntax :: NLSyntax
                     }
              deriving (Show, Ord, Eq)

data LegislativeParamOrUserVar = LegislativeParam | UserVar | BitOfBoth
              deriving (Show, Ord, Eq)

enlookup :: String -> NLdict -> Maybe NLAttr
enlookup s md = do
  en_dict <- Map.lookup "en" md
  dict_val <- Map.lookup s en_dict
  return dict_val

loadTestFile :: String -> IO [String]
loadTestFile filename = do
  file <- readFile filename
  return (lines file)
