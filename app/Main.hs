{-
Simple CAS utilising some features defined in Functions.hs,
namely expansion, simplification and differentiation of functions and taylor and fourier expansions.
-}

module Main where
    
import Parser
import Text.Parsec (ParseError, parse)
import Functions

load::Maybe (Expr)->IO()
load f = do 
    input <- words <$> getLine
    if null input then print "Invalid input." >> load f
    else
        case head input of
            "quit"->return ()
            --Evaluate the given expression at the given value
            "eval"->
                let vals = reads.concat $ tail input :: [(Double, String)] in
                    if null vals then print "Invalid input for evaluation." >> load f
                    else 
                        if f == Nothing then print "No expression is loaded." >> load f
                        else do 
                            print.show $ eval ((\(Just  e)->e) f) (fst $ head vals)
                            load f
            --load a new expression into memory
            "load"->
                case parse expr "" (concat.tail $ input) of
                    Left err -> print err >> load f
                    Right newf -> load $ Just newf
            --simplify the loaded expression
            "simplify"-> if f == Nothing then print "No expression is loaded." >> load f
                        else 
                            do 
                                print $ simplify ((\(Just  e)->e) f)
                                load f
            --expand the loaded expression
            "expand"-> if f == Nothing then print "No expression is loaded." >> load f
                        else 
                            do 
                                print $ c_expand $ ((\(Just  e)->e) f)
                                load f
            --differentiate the loaded expression
            "diff"-> if f == Nothing then print "No expression is loaded." >> load f
                        else 
                            do 
                                print $ clean_diff ((\(Just  e)->e) f)
                                load f
            --set the derivative as the new function
            "setdiff"->if f == Nothing then print "No expression is loaded." >> load f
                        else
                            do
                                print $ clean_diff ((\(Just  e)->e) f)
                                load $ Just $ clean_diff ((\(Just  e)->e) f)
            --display the current expression
            "show"->if f == Nothing then print "No expression is loaded." >> load f
                    else
                        do 
                            print $ (\(Just  e)->e) f
                            load f
            --numerical tools
            --Taylor expansion
            "taylor"->if f == Nothing then print "No expression is loaded." >> load f
                      else
                        if length (tail input) < 2 then print "Incorrect number of inputs for: taylor degree a" >> load f
                        else
                            do
                                print $ taylor_exp (read(input !! 1)::Integer) (read(input !! 2)::Double)  ((\(Just  e)->e) f)
                                load f
            "fourier"->if f == Nothing then print "No expression is loaded." >> load f
                      else
                        if length (tail input) < 3 then print "Incorrect number of inputs for: fourier degree (period/2) a" >> load f
                        else
                            do
                                print $ fourier_exp (read(input !! 1)::Integer) (read(input !! 2)::Double) (read(input !! 3)::Double)  ((\(Just  e)->e) f)
                                load f
            otherwise->print "Invalid command.">>load f
                    

main::IO ()
main = load Nothing