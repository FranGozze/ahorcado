import System.IO

ahorcado = do 
                putStrLn "Cuantos jugadores seran (1 o 2)"
                cant_p <- sgetLine
                putStrLn "Piense una Palabra"
                palabra <- sgetLine
                putStrLn "Intente Adivinar"
                adivinar 5 palabra (guion palabra) 

sgetLine = do hSetEcho stdin False
              palabra <- sgetLine'
              hSetEcho stdin True
              return palabra

sgetLine' = do x <- getChar
               if x == '\n' then do putChar x
                                    return []
                            else do putChar '_'
                                    xs <- sgetLine'
                                    return (x:xs)






adivinar :: (Show t, Num t, Eq t) => t -> String -> [Char] -> IO ()
adivinar contador palabra aciertos =  if contador == 0 
                then  putStrLn "Haha! Perdiste!"
                else do putStrLn ("Intentos Restantes: " ++ (show contador))
                        putStrLn aciertos
                        putStr "> "
                        xs <- getLine
                        if length(xs) == 1
                                then
                                        if busqueda xs palabra
                                                then        adivinar contador palabra (union (diff palabra xs) aciertos) 
                                                else        adivinar (contador-1) palabra aciertos    
                                
                                else    
                                        if xs == palabra
                                                then    putStrLn "Esa es la palabra"
                                                else    putStrLn "Haha! Perdiste!Esa no era la palabra correcta"
                                                

guion [] = []
guion (x:xs) =  '_' : guion xs 

union [] [] = []
union (x:xs)(y:ys) = if x /= '_' then x : union xs ys
                        else if y /= '_' then y : union xs ys
                                else '_' : union xs ys

diff :: [Char] -> [Char] -> [Char]
diff xs ys = [if elem x ys then x else '_' | x <- xs]
main = ahorcado



busqueda :: String -> String -> Bool
busqueda (x:xs) []= False
busqueda (x:xs) (y:ys)= if x == y  then    True
                        else    busqueda (x:xs) ys 
--adivinar contador palabra = do putStr "> "
--                               xs <- getLine
--                               if xs == palabra then putStrLn "Esa es la palabra!"
--                                                else do putStrLn (diff palabra xs)
--                                                        if contador == 0 then  putStrLn "Haha! Perdiste!"
--                                                                         else adivinar (contador - 1) palabra

