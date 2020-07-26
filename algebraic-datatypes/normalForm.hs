data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

-- type AuthorName = String

-- data Author = Author (AuthorName, BookType)



-- APPLY DISTRIBUTIVE PROPERTY OF TYPES

type AuthorName = String

data Author = Fiction AuthorName | Nonfiction AuthorName deriving (Eq, Show)

-- Another example of sum of product types
data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr
          | Divide Expr Expr


-- Exercises

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- Normal form of Garden

data NormalGarden = Gardenia Gardener
                  | Daisy Gardener
                  | Rose Gardener
                  | Lilac Gardener
                  deriving Show
