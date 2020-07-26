data OperatingSystem = GnuLinux | OpenBSD | Mac | Windows deriving (Eq, Show, Enum)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show, Enum)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

allProgrammers' = [Programmer os lang | os <- enumFromTo GnuLinux Windows, lang <- enumFromTo Haskell PureScript]
