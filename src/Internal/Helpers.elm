module Internal.Helpers exposing (on)


on : (a -> b) -> (b -> b -> c) -> a -> a -> c
on f op x y =
    op (f x) (f y)
