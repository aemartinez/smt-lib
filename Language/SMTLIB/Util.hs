module Language.SMTLIB.Util 
(
    mapS
  , smtAND
  , smtNOT
  , smtTrue
  , smtEq
  , smtPlus
  , smtConstName
) where

import Language.SMTLIB

-- MapSymbol class for renaming symbols in SMTLIB Script
  
class MapSymbol a where
    mapS :: (Symbol -> Symbol) -> a -> a

instance MapSymbol Script where
    mapS f (Script cmds) = Script (map (mapS f) cmds)

instance MapSymbol Command where
    mapS f cmd =
        case cmd of Set_logic sym -> Set_logic (f sym)
                    Set_option opt -> Set_option (mapS f opt)
                    Set_info attr -> Set_info (mapS f attr)
                    Declare_sort sym n -> Declare_sort (f sym) n
                    Define_sort sym syms sort -> Define_sort (f sym) (map f syms) (mapS f sort)
                    Declare_fun sym sorts sort -> Declare_fun (f sym) (map (mapS f) sorts) (mapS f sort)
                    Declare_const sym sort -> Declare_const (f sym) (mapS f sort)
                    Define_fun sym sVars sort term -> Define_fun (f sym) (map (mapS f) sVars) (mapS f sort) (mapS f term)
                    Push n -> Push n
                    Pop n -> Pop n
                    Assert term -> Assert (mapS f term)
                    Check_sat -> Check_sat
                    Get_assertions -> Get_assertions
                    Get_proof -> Get_proof
                    Get_unsat_core -> Get_unsat_core
                    Get_value terms -> Get_value (map (mapS f) terms)
                    Get_assignment -> Get_assignment
                    Get_option k -> Get_option k
                    Get_info flag -> Get_info flag
                    Exit -> Exit

instance MapSymbol Term where
    mapS f term = 
        case term of Term_spec_constant sConst -> Term_spec_constant sConst
                     Term_qual_identifier qid -> Term_qual_identifier (mapS f qid)
                     Term_qual_identifier_ qid terms -> Term_qual_identifier_ (mapS f qid) (map (mapS f) terms)
                     Term_distinct term terms -> Term_distinct (mapS f term) (map (mapS f) terms)
                     Term_let vBs term -> Term_let (map (mapS f) vBs) (mapS f term)
                     Term_forall sVars term -> Term_forall (map (mapS f) sVars) (mapS f term)
                     Term_exists sVars term -> Term_exists (map (mapS f) sVars) (mapS f term)
                     Term_attributes term attrs -> Term_attributes (mapS f term) (map (mapS f) attrs)

instance MapSymbol Qual_identifier where
    mapS f qid =
        case qid of Qual_identifier id -> Qual_identifier (mapS f id)
                    Qual_identifier_sort id sort -> Qual_identifier_sort (mapS f id) (mapS f sort)

instance MapSymbol Identifier where
    mapS f id =
        case id of Identifier sym -> Identifier (f sym)
                   Identifier_ sym ns -> Identifier_ (f sym) ns

instance MapSymbol Attribute where
    mapS f attr =
        case attr of Attribute k -> Attribute k
                     Attribute_s_expr k sExpr -> Attribute_s_expr k (mapS f sExpr)

instance MapSymbol Attribute_value where
    mapS f attrVal =
        case attrVal of Attribute_value_spec_constant sConst -> Attribute_value_spec_constant sConst
                        Attribute_value_symbol sym           -> Attribute_value_symbol (f sym)   
                        Attribute_value_s_expr sExprs        -> Attribute_value_s_expr (map (mapS f) sExprs)   

instance MapSymbol S_expr where
    mapS f sExpr =
        case sExpr of S_expr_constant sConst -> S_expr_constant sConst
                      S_expr_symbol sym      -> S_expr_symbol (f sym)
                      S_expr_keyword k       -> S_expr_keyword k
                      S_exprs sExprs         -> S_exprs (map (mapS f) sExprs)

instance MapSymbol Var_binding where
    mapS f (Var_binding sym term) = Var_binding (f sym) (mapS f term)

instance MapSymbol Sorted_var where
    mapS f (Sorted_var sym sort) = Sorted_var (f sym) (mapS f sort)

instance MapSymbol Sort where
    mapS f sort =
        case sort of Sort_bool                  -> Sort_bool    
                     Sort_identifier id         -> Sort_identifier (mapS f id)
                     Sort_identifiers id sorts  -> Sort_identifiers (mapS f id) (map (mapS f) sorts)

instance MapSymbol Option where
    mapS f opt =
        case opt of Option_attribute attr -> Option_attribute (mapS f attr)
                    _ -> opt

-- High level operations for manipulating terms

smtAND :: Term -> Term -> Term
smtAND t1 t2 = Term_qual_identifier_ andIdf [t1, t2]
    where andIdf = Qual_identifier (Identifier "and")

smtNOT :: Term -> Term
smtNOT t1 = Term_qual_identifier_ notIdf [t1]
    where notIdf = Qual_identifier (Identifier "not")

smtTrue :: Term
smtTrue = (Term_qual_identifier (Qual_identifier (Identifier "true")))

smtEq :: Term -> Term -> Term
smtEq t1 t2 = Term_qual_identifier_ eqId [t1, t2]
    where eqId = Qual_identifier (Identifier "=")

smtPlus :: Term -> Term -> Term
smtPlus t1 t2 = Term_qual_identifier_ plusId [t1, t2]
    where plusId = Qual_identifier (Identifier "+")

smtConstName :: String -> Term
smtConstName s = (Term_qual_identifier (Qual_identifier (Identifier s)))