module FunctorStuff where

instance Functor Program where
    fmap f x = case x of
                Program a topdefs -> Program (f a) (map (fmap f) topdefs)
instance Functor TopDef where
    fmap f x = case x of
        FnDef a type_ ident args block -> FnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
        ClassDef a classheader -> ClassDef (f a) (fmap f classheader)
