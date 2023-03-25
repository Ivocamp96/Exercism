module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA (x:xs) = fmap (:) (toRNA_ x) <*> toRNA xs

toRNA_ :: Char -> Either Char Char
toRNA_ c = case c of
    'G' -> Right 'C'
    'C' -> Right 'G'
    'T' -> Right 'A'
    'A' -> Right 'U'
    _   -> Left c
