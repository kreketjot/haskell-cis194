{-# OPTIONS_GHC -Wall #-}

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disks fromPeg toPeg tempPeg =
  hanoi (disks - 1) fromPeg tempPeg toPeg
    ++ ( (fromPeg, toPeg) : hanoi (disks - 1) tempPeg toPeg fromPeg
       )
