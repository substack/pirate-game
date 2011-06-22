{-
    Here is a little-known mathematical puzzle which appeared in a 1999 edition
    of Scientific American which has some counter-intuitive solutions.
    
    A band of pirates need to 
    
    Despite their lack of trustworthiness, pirates always obey the pirate code.
    Pirates are rational egoists. For the purposes of this puzzle, this means:
        * Pirates always think things through completely and make the best
          decision for their self-interest.
        * Pirates are not trustworthy, so they are incapable of keeping promises
          or striking deals.
-}

import Control.Monad (replicateM)
import Data.List (find)

-- A proposal is just an allotment of the gold for each pirate, so a list of
-- integers will do.
type Proposal = [Int]

{-
    An outcome has three components as far as each pirate is concerned. In order
    of importance:
        * whether or not the pirate lives
        * how much gold the pirate gets
        * how many of his shipmates he can throw overboard (the more the merrier)
-}

type Outcome = Maybe (Int,Int)

-- all possible allotments of the gold
proposals :: Int -> Int -> [Proposal]
proposals gold pirates =
    filter ((== gold) . sum) $ replicateM pirates $ reverse [ 0 .. gold ]

outcome :: Int -> Int -> [Outcome]
outcome gold pirates = outcome' gold pirates 0

outcome' :: Int -> Int -> Int -> [Outcome]
outcome' gold pirates overboard =
    case find (passes gold pirates overboard) $ proposals gold pirates of
        -- no proposal where the captain doesn't walk the plank
        Nothing -> Nothing : outcome' gold (pirates - 1) (overboard + 1)
        -- proposal is accepted by the crew
        Just proposal -> [ Just (p,overboard) | p <- proposal ]

-- whether the proposal passes or fails
passes :: Int -> Int -> Int -> Proposal -> Bool
{-
    When there are two pirates left, the first pirate can use his tie-breaking
    vote to give himself all the gold. This means that the last two pirates will
    never walk the plank.
-}
passes _ _ _ proposal | length proposal <= 2 = True
passes gold pirates overboard proposal = election >= 0
    where
        election = sum [ 2 * n - 1 | n <- map fromEnum votes ]
        -- captain always votes for their own proposal
        votes = True : map (vote proposal) [ 1 .. length proposal - 1 ]
        
        vote :: Proposal -> Int -> Bool
        vote proposal n = passOutcome > failOutcome
            where
                failOutcome = outcome' gold (pirates - 1) (overboard + 1)
                    !! (n - 1)
                passOutcome = Just (proposal !! n, overboard)
