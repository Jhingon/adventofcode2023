module Wait () where


a = 1

winning :: Int -> Int -> Int ->  Bool
winning totalTime record chargeTime= (chargeTime * (totalTime - chargeTime)) > record

numWinning :: Int -> Int -> Int
numWinning totalTime record = length (filter (winning totalTime record) [0..totalTime])

f xs = product (map (uncurry numWinning) xs)

-- part 2 is a quadratic equation; find the difference between the roots
-- 
