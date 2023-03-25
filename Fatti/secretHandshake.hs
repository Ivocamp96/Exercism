module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = filter (/= "")
            $ (if isReverse list then reverse else id)
            $ sequence [isWink, isDoubleBlink, isCloseYourEyes, isJump] list
  where list = revIntToBinary n 5
        isWink (n:_) = if n == 1
                       then "wink"
                       else ""
        isDoubleBlink (_:m:_) = if m == 1
                                then "double blink"
                                else ""
        isCloseYourEyes (_:_:l:_) = if l == 1
                                    then "close your eyes"
                                    else ""
        isJump (_:_:_:k:_) = if k == 1
                             then "jump"
                             else ""
        isReverse list = last list == 1

revIntToBinary :: Int -> Int -> [Int]
revIntToBinary x 0   = []
revIntToBinary x e
  | x > 2^e || x < 0 = error "Valori non validi"
  | otherwise        = (rem x 2^e) : revIntToBinary (div x 2) (e - 1)


