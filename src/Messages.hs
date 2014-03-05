{- |
Module : Messages
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Messages about project status 
-}
module Messages where


-- | Switch to load project page 
errorNoPom :: FilePath -> String   
errorNoPom src = "This is not maven project.\n" ++
                 "To convert this project to maven execute command:\n" ++
                 "mavex convert " ++ src 