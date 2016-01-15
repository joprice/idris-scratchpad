module Main

%language TypeProviders

implicit intString : Int -> String
intString = show

f : Int
f = the Int 2

main : IO ()
main = do args <- getArgs
          case args of
               -- one arg followed by another arg, followed by one or many args
               (_ :: arg :: _) => putStrLn arg
               -- exactly one arg:
               -- [_, arg, _] => putStrLn arg
               _ => putStrLn $ show $ f -- "need some args"

-- type providers

strToType : String -> Type
strToType "Int" = Int
strToType _ = Nat

fromFile : String -> IO (Provider Type)
fromFile fname = do str <- readFile fname
                    case str of
                         Right value =>
                            return (Provide (strToType (trim value)))
                         _ => return (Error "fail")

%provide (T1 : Type) with fromFile "theType"

foo : T1
foo = 1

-- hlists

namespace HList 
  data HList : List Type -> Type where
       Nil : HList []
       (::) : a -> HList xs -> HList (a :: xs)

t: Integer -> Integer
t x = x * 2

hhead: HList(t :: ts) -> t --Type -> Type -- HList
hhead (l :: _) = l

htail: HList(t :: ts) -> HList ts --Type -> Type -- HList
htail (_ :: tail) = tail

hmap: {F: Type -> Type} -> ({a: Type} -> a -> F a) -> HList ts -> HList(map F ts)
hmap f [] = []
hmap f (x :: xs) = f x :: (hmap f xs)

