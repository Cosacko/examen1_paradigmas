module Pred where
import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = foldDib (\x -> if p x then f x else basica x) -- Si cumple el predicado aplica f a basica
              rotar espejar
              rotar45 apilar
              juntar encimar

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib
            p            -- Chequeamos el predicado
            id
            id
            id
            (\_ _ a b -> a || b) -- no nos interesan los float, se trata casi como un id.
            (\_ _ a b -> a || b)
            (||)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib
            p                     -- Chequeamos el predicado
            id
            id
            id
            (\_ _ a b -> a && b)
            (\_ _ a b -> a && b)
            (&&)

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 = snd . go
  where
    go = foldDib
      (\_ -> (0, False))                                 -- Basica
      (\(n, b) -> let n' = n + 1 in (n', b || n' == 4))  -- rotar: Paso clave, chequea cuatro rotar
      (\_ -> (0, False))                                 -- espejar
      (\_ -> (0, False))                                 -- rot45
      (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))          -- apilar: Distribuye la busqueda
      (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))          -- juntar: Distribuye la busqueda
      (\(n1, b1) (n2, b2) -> (0, b1 || b2))              -- encimar: Distribuye la busqueda


-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 = snd . go
  where
    go = foldDib
      (\_ -> (0, False))                                -- basica
      (\_ -> (0, False))                                -- rotar
      (\_ -> (0, False))                                -- rot45
      (\(n, b) -> let n' = n + 1 in (n', b || n' == 2)) -- espejar: Paso clave, chequea doble espejar seguidos
      (\_ _ (_, b1) (_, b2) -> (0, b1 || b2))           -- apilar: Distribuye la busqueda
      (\_ _ (_, b1) (_, b2) -> (0, b1 || b2))           -- juntar: Distribuye la busqueda
      (\(_, b1) (_, b2) -> (0, b1 || b2))               -- encimar: Distribuye la busqueda


data Superfluo = RotacionSuperflua | FlipSuperfluo

-- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion d
  | esRot360 d = [RotacionSuperflua]
  | otherwise  = []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip d
  | esFlip2 d = [FlipSuperfluo]
  | otherwise = []

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo d =
  let errores = errorRotacion d ++ errorFlip d
  in if null errores
     then Right d
     else Left errores

profundidad :: Dibujo a -> Int
profundidad = foldDib
    (\_ -> 0)       -- Basica: profundidad 0
    (\d -> 1 + d)   -- Rotar: aumenta la profundidad
    (\d -> 1 + d)   -- Rotar45: igual que Rotar
    (\d -> 1 + d)   -- Espejar: igual
    (\_ _ d1 d2 -> max d1 d2)  -- Apilar: toma el máximo de los subdibujos
    (\_ _ d1 d2 -> max d1 d2)  -- Juntar: igual
    (\d1 d2 -> max d1 d2)      -- Encimar: igual