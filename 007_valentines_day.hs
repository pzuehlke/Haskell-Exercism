data Approval
    = Yes
    | No
    | Maybe

data Cuisine
    = Korean
    | Turkish

data Genre
    = Crime
    | Horror
    | Romance
    | Thriller
    deriving Eq

data Activity
    = BoardGame
    | Chill
    | Movie Genre
    | Restaurant Cuisine
    | Walk Int

rateActivity :: Activity -> Approval
rateActivity  BoardGame             = No
rateActivity  Chill                 = No
rateActivity  (Movie genre)         = goodGenre genre
rateActivity  (Restaurant cuis)     = goodCuisine cuis
rateActivity  (Walk miles)          = goodMiles miles

goodMiles :: Int -> Approval
goodMiles n | n < 3              = Yes
            | 3 <= n && n <= 5   = Maybe
            | otherwise          = No

goodCuisine :: Cuisine -> Approval
goodCuisine Korean        = Yes
goodCuisine Turkish       = Maybe

goodGenre :: Genre -> Approval
goodGenre genre | genre == Romance     = Yes
                | otherwise            = No
