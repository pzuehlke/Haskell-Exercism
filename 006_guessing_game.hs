answer :: Int
answer = 42

reply :: Int -> String
reply guess | guess == answer               = "Correct"
            | abs (guess - answer) == 1     = "So close"
            | guess > answer                = "Too high"
            | guess < answer                = "Too low"
