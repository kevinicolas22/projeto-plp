module TypeClasses where

class Entity a where
    entityCpf :: a -> Int

class Person a where
    personSSN :: a -> String

class Stringfy a where
    toString :: a -> String