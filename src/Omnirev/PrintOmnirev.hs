{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintOmnirev.
--   Generated by the BNF converter.

module Omnirev.PrintOmnirev where

import Omnirev.AbsOmnirev
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Ident where
  prt _ (Ident i) = doc (showString i)

instance Print Program where
  prt i e = case e of
    Prog defs -> prPrec i 0 (concatD [prt 0 defs])

instance Print Def where
  prt i e = case e of
    DType id type_ -> prPrec i 0 (concatD [doc (showString "type"), prt 0 id, doc (showString "="), prt 0 type_])
    DFunc id type_1 type_2 func -> prPrec i 0 (concatD [doc (showString "func"), prt 0 id, doc (showString ":"), prt 0 type_1, doc (showString "<->"), prt 0 type_2, doc (showString "="), prt 0 func])
    DExpr id type_ expr -> prPrec i 0 (concatD [doc (showString "expr"), prt 0 id, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Def] where
  prt = prtList

instance Print Expr where
  prt i e = case e of
    EUnit -> prPrec i 3 (concatD [doc (showString "()")])
    ETensor expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "*"), prt 3 expr2])
    ESum expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString ","), prt 2 expr2])
    EStar expr -> prPrec i 3 (concatD [doc (showString "~"), prt 3 expr])
    EVar id -> prPrec i 3 (concatD [prt 0 id])
    EApp func expr -> prPrec i 3 (concatD [prt 0 func, prt 3 expr])
    EProj expr -> prPrec i 3 (concatD [doc (showString "measure"), prt 3 expr])

instance Print Type where
  prt i e = case e of
    TUnit -> prPrec i 3 (concatD [doc (showString "unit")])
    TTensor type_1 type_2 -> prPrec i 2 (concatD [prt 2 type_1, doc (showString "*"), prt 3 type_2])
    TSum type_1 type_2 -> prPrec i 1 (concatD [prt 1 type_1, doc (showString "+"), prt 2 type_2])
    TStar type_ -> prPrec i 3 (concatD [doc (showString "~"), prt 3 type_])
    TVar id -> prPrec i 3 (concatD [prt 0 id])

instance Print Func where
  prt i e = case e of
    FId -> prPrec i 4 (concatD [doc (showString "id")])
    FComp func1 func2 -> prPrec i 1 (concatD [prt 1 func1, doc (showString ";"), prt 2 func2])
    FTensor func1 func2 -> prPrec i 3 (concatD [prt 3 func1, doc (showString "*"), prt 4 func2])
    FTensUnit -> prPrec i 4 (concatD [doc (showString "unit_*")])
    FTensAssoc -> prPrec i 4 (concatD [doc (showString "assoc_*")])
    FTensSym -> prPrec i 4 (concatD [doc (showString "sym_*")])
    FSum func1 func2 -> prPrec i 2 (concatD [prt 2 func1, doc (showString "+"), prt 3 func2])
    FSumAssoc -> prPrec i 4 (concatD [doc (showString "assoc_+")])
    FSumSym -> prPrec i 4 (concatD [doc (showString "sym_+")])
    FDistrib -> prPrec i 4 (concatD [doc (showString "distrib")])
    FEval type_ -> prPrec i 4 (concatD [doc (showString "eval"), prt 0 type_])
    FDagger func -> prPrec i 4 (concatD [doc (showString "^"), prt 4 func])
    FVar id -> prPrec i 4 (concatD [prt 0 id])
    FShift n -> prPrec i 4 (concatD [doc (showString "shift"), prt 0 n])

