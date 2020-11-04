import System.IO
data Player = Jugador{    
                nombre :: String,
                contador :: Int,
                palabra_en_juego :: Int,
                palabra1 :: String,
                acierto1 ::String,
                palabra2 :: String,
                acierto2 ::String,
                puntos :: Int
}       deriving Show
ahorcado = do 
                putStrLn "Cuantos jugadores seran (1 o 2)"
                cant_p <- getLine
                if cant_p == "1"
                        then do
                                putStrLn "Piense una Palabra"
                                palabra <- sgetLine
                                putStrLn "Intente Adivinar"
                                adivinar1 5 palabra (guion palabra) 
                        else    if cant_p == "2"
                                then    do      putStrLn "ingrese el nombre del jugador 1"
                                                nombre <- getLine
                                                putStrLn "Piense una Palabra"
                                                palabra <- sgetLine
                                                putStrLn "Piense una Palabra"
                                                palabra2 <- sgetLine
                                                
                                                putStrLn "ingrese el nombre del jugador 2"
                                                nombre2 <- getLine
                                                putStrLn "Piense una Palabra"
                                                palabra3 <- sgetLine
                                                putStrLn "Piense una Palabra"
                                                palabra4 <- sgetLine
                                                adivinar_2 0 (Jugador nombre 8 0 palabra3 (guion palabra3) palabra4 (guion palabra4) 0) (Jugador nombre2 8 0 palabra (guion palabra) palabra2 (guion palabra2) 0)
                                else    do      putStrLn "La cantidad ingresada es incorrecta."
                                                ahorcado

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



adivinar_2 turno j1 j2= 
        if (palabra_en_juego j1) == 2 && (palabra_en_juego j2) == 2
                then    if (puntos j1) > (puntos j2)
                        then    putStrLn ( (show (nombre j1)) ++" ha ganado")
                        else    if (puntos j1) < (puntos j2)
                                then    putStrLn ( (show (nombre j2)) ++" ha ganado")
                                else    putStrLn ( (show (nombre j1)) ++ " " ++ (show (nombre j2)) ++ " han empatado")
                else
                        if mod turno 2 == 0
                                then    if (palabra_en_juego j1) ==0
                                                then    
                                                        if (contador j1) == 0
                                                        then    adivinar_2 turno (Jugador (nombre j1) ((contador j1) -1) ((palabra_en_juego j1) +1) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) -3) ) j2
                                                        else
                                                        do      putStrLn ("Jugador: " ++ (show (nombre j1)))
                                                                putStrLn ("Intentos restantes: " ++ (show (contador j1)))
                                                                putStrLn (acierto1 j1)
                                                                putStr "> "
                                                                xs <- getLine
                                                                if length(xs) == 1
                                                                then
                                                                        if busqueda xs (palabra1 j1)
                                                                        then    do              putStrLn "Esta esa letra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) (contador j1) (palabra_en_juego j1) (palabra1 j1) ((union (diff (palabra1 j1) xs) (acierto1 j1))) (palabra2 j1) (acierto2 j1) ((puntos j1) +1) ) j2
                                                                        else    do              putStrLn "No esta esa letra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) ((contador j1) -1) (palabra_en_juego j1)   (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) -3) ) j2    
                                                                else
                                                                        if xs == (palabra1 j1)
                                                                                then    do      putStrLn "Esa es la primera palabra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) 8 ((palabra_en_juego j1) +1 ) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) +50) ) j2
                                                                                else    do      putStrLn "Haha! Esa no era la primera palabra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) 8 ((palabra_en_juego j1) +1 ) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) -25) ) j2


                                                else if (palabra_en_juego j1) ==1
                                                        then  do        putStrLn ("Jugador: " ++ (show (nombre j1)))
                                                                        putStrLn ("Intentos restantes: " ++ (show (contador j1)))
                                                                        putStrLn (acierto2 j1)
                                                                        putStr "> "
                                                                        xs <- getLine
                                                                        if length(xs) == 1
                                                                        then
                                                                                if busqueda xs (palabra2 j1)
                                                                                then    do      putStrLn " Esta esa letra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) (contador j1) (palabra_en_juego j1) (palabra1 j1) (acierto1 j1) (palabra2 j1) ((union (diff (palabra2 j1) xs) (acierto2 j1))) ((puntos j1) +1) ) j2
                                                                                else    do      putStrLn "No esta esa letra"
                                                                                                adivinar_2 (turno+1) (Jugador (nombre j1) ((contador j1) -1) (palabra_en_juego j1) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) -3) ) j2    
                                                                        else
                                                                                if xs == (palabra2 j1)
                                                                                        then    do      putStrLn "Esa es la segunda palabra"
                                                                                                        adivinar_2 (turno+1) (Jugador (nombre j1) 8 ((palabra_en_juego j1) +1 ) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) +50) ) j2
                                                                                        else    do      putStrLn "Haha! Esa no era la segunda palabra "
                                                                                                        adivinar_2 (turno+1) (Jugador (nombre j1) 8 ((palabra_en_juego j1) +1 ) (palabra1 j1) (acierto1 j1) (palabra2 j1) (acierto2 j1) ((puntos j1) -25) ) j2

                                                        else
                                                                adivinar_2 (turno +1) j1 j2
                                
                                
                                
                                else    if (palabra_en_juego j2) ==0
                                                then    do      putStrLn ("Jugador: " ++ (show (nombre j2)))
                                                                putStrLn ("Intentos restantes: " ++ (show (contador j2)))
                                                                putStrLn (acierto1 j2)
                                                                putStr "> "
                                                                xs <- getLine
                                                                if length(xs) == 1
                                                                then
                                                                        if busqueda xs (palabra1 j2)
                                                                        then            do      putStrLn "Esta esa letra"
                                                                                                adivinar_2 (turno+1) j1 (Jugador (nombre j2) (contador j2) (palabra_en_juego j2) (palabra1 j2) ((union (diff (palabra1 j2) xs) (acierto1 j2))) (palabra2 j2) (acierto2 j2) ((puntos j2) +1) ) 
                                                                        else            do      putStrLn "No esta esa letra"
                                                                                                adivinar_2 (turno+1) j1 (Jugador (nombre j2) ((contador j2) -1) (palabra_en_juego j2) (palabra1 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) -3) )     
                                                                else
                                                                        if xs == (palabra1 j2)
                                                                                then    do      putStrLn "Esa es la primera palabra"
                                                                                                adivinar_2 (turno+1) j1 (Jugador (nombre j2) 8 ((palabra_en_juego j2) +1 ) (palabra1 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) +50) ) 
                                                                                else    do      putStrLn "Haha! Esa no era la primera palabra "
                                                                                                adivinar_2 (turno+1) j1 (Jugador (nombre j2) 8 ((palabra_en_juego j2) +1 ) (palabra1 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) -25) )


                                                else if (palabra_en_juego j2) ==1
                                                        then  do        putStrLn ("Jugador: " ++ (show (nombre j2)))
                                                                        putStrLn ("Intentos restantes: " ++ (show (contador j2)))
                                                                        putStrLn (acierto2 j2)
                                                                        putStr "> "
                                                                        xs <- getLine
                                                                        if length(xs) == 1
                                                                        then
                                                                                if busqueda xs (palabra2 j2)
                                                                                then            do      putStrLn "Esta esa letra"
                                                                                                        adivinar_2 (turno+1) j1 (Jugador (nombre j2) (contador j2) (palabra_en_juego j2) (palabra1 j2) (acierto1 j2) (palabra2 j2) ((union (diff (palabra2 j2) xs) (acierto2 j2))) ((puntos j2) +1) ) 
                                                                                else            do      putStrLn "No esta esa letra"
                                                                                                        adivinar_2 (turno+1) j1 (Jugador (nombre j2) ((contador j2) -1) (palabra_en_juego j2) (palabra1 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) -3) )     
                                                                        else
                                                                                if xs == (palabra2 j2)
                                                                                        then    do      putStrLn "Esa es la segunda palabra"
                                                                                                        adivinar_2 (turno+1) j1 (Jugador (nombre j2) 8 ((palabra_en_juego j2) +1 ) (palabra2 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) +50) ) 
                                                                                        else    do      putStrLn "Haha! Esa no era la segunda palabra "
                                                                                                        adivinar_2 (turno+1) j1 (Jugador (nombre j2) 8 ((palabra_en_juego j2) +1 ) (palabra1 j2) (acierto1 j2) (palabra2 j2) (acierto2 j2) ((puntos j2) -25) ) 

                                                        else
                                                                adivinar_2 (turno +1) j1 j2
                                
                        
                        
                        

adivinar1 :: (Show t, Num t, Eq t) => t -> String -> [Char] -> IO ()
adivinar1 contador palabra aciertos =  if contador == 0 
                then  putStrLn "Haha! Perdiste!"
                else do putStrLn ("Intentos Restantes: " ++ (show contador))
                        putStrLn aciertos
                        putStr "> "
                        xs <- getLine
                        if length(xs) == 1
                                then
                                        if busqueda xs palabra
                                                then        adivinar1 contador palabra (union (diff palabra xs) aciertos) 
                                                else        adivinar1 (contador-1) palabra aciertos    
                                
                                else    
                                        if xs == palabra
                                                then    putStrLn "Esa es la palabra"
                                                else    putStrLn "Haha! Perdiste!Esa no era la palabra correcta"
                                                

guion :: [a] -> [Char]
guion [] = []
guion (x:xs) =  '_' : guion xs 

union [] [] = []
union (x:xs)(y:ys) = if x /= '_' then x : union xs ys
                        else if y /= '_' then y : union xs ys
                                else '_' : union xs ys

diff :: [Char] -> [Char] -> [Char]
diff xs ys = [if elem x ys then x else '_' | x <- xs]




busqueda :: String -> String -> Bool
busqueda (x:xs) []= False
busqueda (x:xs) (y:ys)= if x == y  then    True
                        else    busqueda (x:xs) ys 

main = ahorcado

