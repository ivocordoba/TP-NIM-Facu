type Posicion = [Int]
type Jugada = (Int, Int)

jugarAux :: Posicion -> Jugada -> Int -> Posicion
jugarAux (x:xs) (i, v) k | k == i && (x - v) == 0 = xs
                         | k == i = (x - v) : xs
                         | otherwise = x : jugarAux xs (i, v) (k+1)

jugar :: Posicion -> Jugada -> Posicion
jugar xs (i, v) = jugarAux xs (i, v) 1

posiblesJugadasAux :: Int -> Int -> [Jugada]
posiblesJugadasAux i v | v == 0 = []
                       | otherwise = (i, v) : posiblesJugadasAux i (v-1)          

posiblesJugadasAux2 :: Posicion -> Int -> [Jugada]
posiblesJugadasAux2 [] _ = []
posiblesJugadasAux2 (x:xs) i = posiblesJugadasAux2 xs (i+1) ++ posiblesJugadasAux i x 

darVueltaLista :: [Jugada] -> [Jugada]
darVueltaLista [] = []
darVueltaLista (x:xs) = darVueltaLista xs ++ [x]

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas (x:xs) = darVueltaLista (posiblesJugadasAux2 (x:xs) 1)

esPosicionGanadoraAux :: Posicion -> [Jugada] -> Bool
esPosicionGanadoraAux _ [] = False
esPosicionGanadoraAux xs ((i, v):ys) | esPosicionGanadoraAux k j == False = True
                                     | otherwise = esPosicionGanadoraAux xs ys
                                      where k = jugar xs (i, v)
                                            j = posiblesJugadas (jugar xs (i, v))

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora xs = esPosicionGanadoraAux xs (posiblesJugadas xs)

jugadaGanadoraAux :: Posicion -> [Jugada] -> Jugada
jugadaGanadoraAux xs ((i, v):ys) | esPosicionGanadoraAux k j == False = (i, v)
                                 | otherwise = jugadaGanadoraAux xs ys
                                  where k = jugar xs (i, v)
                                        j = posiblesJugadas (jugar xs (i, v))

jugadaGanadora :: Posicion -> Jugada
jugadaGanadora xs = jugadaGanadoraAux xs (posiblesJugadas xs)

numeroDeJugadasGanadorasAux :: Posicion -> [Jugada] -> Int
numeroDeJugadasGanadorasAux _ [] = 0
numeroDeJugadasGanadorasAux xs ((i, v):ys) | esPosicionGanadoraAux k j == False = 1 + numeroDeJugadasGanadorasAux xs ys
                                           | otherwise = numeroDeJugadasGanadorasAux xs ys
                                            where k = jugar xs (i, v)
                                                  j = posiblesJugadas (jugar xs (i, v))

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras xs = numeroDeJugadasGanadorasAux xs (posiblesJugadas xs)