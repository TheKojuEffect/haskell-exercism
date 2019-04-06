module SecretHandshake
  ( handshake
  ) where

handshake :: Int -> [String]
handshake 0 = []
handshake n = reverse $ check moves n []
  where
    moves = [("wink" :), ("double blink" :), ("close your eyes" :), ("jump" :), reverse]
    check [] _ = id
    check (m:ms) n' =
      check ms (quot n' 2) .
      (if rem n' 2 == 1
         then m
         else id)
