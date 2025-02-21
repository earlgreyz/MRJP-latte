module Latte.AbsLatte where

import Data.List

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program a = Program a [TopDef a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        Program a topdefs -> Program (f a) (map (fmap f) topdefs)
data TopDef a
    = FnDef a (FunDef a)
    | ClDef a Ident [Field a]
    | ClExtDef a Ident Ident [Field a]
  deriving (Eq, Ord, Show, Read)

instance Functor TopDef where
    fmap f x = case x of
        FnDef a fundef -> FnDef (f a) (fmap f fundef)
        ClDef a ident fields -> ClDef (f a) ident (map (fmap f) fields)
        ClExtDef a ident1 ident2 fields -> ClExtDef (f a) ident1 ident2 (map (fmap f) fields)
data Arg a = Arg a (Type a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Arg where
    fmap f x = case x of
        Arg a type_ ident -> Arg (f a) (fmap f type_) ident
data Field a = Attr a (Type a) Ident | Method a (FunDef a)
  deriving (Eq, Ord, Show, Read)

instance Functor Field where
    fmap f x = case x of
        Attr a type_ ident -> Attr (f a) (fmap f type_) ident
        Method a fundef -> Method (f a) (fmap f fundef)
data FunDef a = FunDef a (Type a) Ident [Arg a] (Block a)
  deriving (Eq, Ord, Show, Read)

instance Functor FunDef where
    fmap f x = case x of
        FunDef a type_ ident args block -> FunDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
data Block a = Block a [Stmt a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
    fmap f x = case x of
        Block a stmts -> Block (f a) (map (fmap f) stmts)
data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a (LValue a) (Expr a)
    | Incr a (LValue a)
    | Decr a (LValue a)
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | ForEach a (Type a) Ident (Expr a) (Stmt a)
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Decl a type_ items -> Decl (f a) (fmap f type_) (map (fmap f) items)
        Ass a lvalue expr -> Ass (f a) (fmap f lvalue) (fmap f expr)
        Incr a lvalue -> Incr (f a) (fmap f lvalue)
        Decr a lvalue -> Decr (f a) (fmap f lvalue)
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        ForEach a type_ ident expr stmt -> ForEach (f a) (fmap f type_) ident (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)
data Item a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Item where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)
data LValue a
    = LVar a Ident | LAt a (Expr a) (Expr a) | LAttr a (Expr a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor LValue where
    fmap f x = case x of
        LVar a ident -> LVar (f a) ident
        LAt a expr1 expr2 -> LAt (f a) (fmap f expr1) (fmap f expr2)
        LAttr a expr ident -> LAttr (f a) (fmap f expr) ident
data Type a
    = Int a
    | Str a
    | Bool a
    | Void a
    | Array a (Type a)
    | Class a Ident
    | Fun a (Type a) [Type a]
  deriving (Ord, Read)

instance Eq (Type a) where
  (Int _) == (Int _) = True
  (Str _) == (Str _) = True
  (Bool _) == (Bool _) = True
  (Void _) == (Void _) = True
  (Array _ t) == (Array _ tt) = t == tt
  (Class _ c) == (Class _ cc) = c == cc
  (Fun _ r t) == (Fun _ rr tt) = (r == rr) && (t == tt)
  _ == _ = False

instance Show (Type a) where
  show (Int _) = "int"
  show (Str _) = "str"
  show (Bool _) = "bool"
  show (Void _) = "void"
  show (Array _ t) = "[" ++ (show t) ++ "]"
  show (Class _ (Ident cls)) = cls
  show (Fun _ r t) = (show r) ++ "(" ++ (intercalate ", " $ map show t) ++ ")"

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Str a -> Str (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Array a type_ -> Array (f a) (fmap f type_)
        Class a ident -> Class (f a) ident
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)
data Expr a
    = EVar a (LValue a)
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EString a String
    | EApp a Ident [Expr a]
    | ENewArr a (Type a) (Expr a)
    | ENewObj a Ident
    | EAttrFun a (Expr a) Ident [Expr a]
    | ENullCast a Ident
    | ECast a Ident (Expr a)
    | Neg a (Expr a)
    | Not a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
    fmap f x = case x of
        EVar a lvalue -> EVar (f a) (fmap f lvalue)
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EString a string -> EString (f a) string
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        ENewArr a type_ expr -> ENewArr (f a) (fmap f type_) (fmap f expr)
        ENewObj a ident -> ENewObj (f a) ident
        EAttrFun a expr ident exprs -> EAttrFun (f a) (fmap f expr) ident (map (fmap f) exprs)
        ENullCast a ident -> ENullCast (f a) ident
        ECast a ident expr -> ECast (f a) ident (fmap f expr)
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)
data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)
data MulOp a = Times a | Div a | Mod a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)
data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)
