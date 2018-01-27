data FormattedChar = Code Char | Strike Char | Bold Char | Italic Char | Plain Char deriving(Eq, Show)
theCharOf :: FormattedChar -> Char
theCharOf (Code y) = y
theCharOf (Strike y) = y
theCharOf (Bold y) = y
theCharOf (Italic y) = y
theCharOf (Plain y) = y

whichChar :: FormattedChar -> FormattedChar -> String
whichChar (Plain x) (Plain y) = x:[]

