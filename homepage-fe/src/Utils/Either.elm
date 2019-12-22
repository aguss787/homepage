module Utils.Either exposing (..)

type Either a b = Left a
                | Right b

mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f x = case x of
    Left a -> Left <| f a
    Right a -> Right a

mapRight : (b -> c) -> Either a b -> Either a c
mapRight f x = case x of
    Left a -> Left a
    Right a -> Right <| f a

unify : Either a a -> a
unify x = case x of
    Left a -> a
    Right a -> a
