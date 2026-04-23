!(import! &self lib/lib_spaces)

(friend a b)
(friend b c)

!(test (collapse (if (find &self (friend $a $b))
                     (if (find &self (friend $b $c))
                         (FoundChain $a $b $c)
                         (MissedSecondPiece))
                     (MissedAllPieces)))
       ((FoundChain a b c) (MissedSecondPiece)))
