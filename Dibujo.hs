module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a 
               |Rotar (Dibujo a)
               |Rotar45 (Dibujo a)
               |Espejar (Dibujo a)
               |Apilar Float Float (Dibujo a) (Dibujo a)
               |Juntar Float Float (Dibujo a) (Dibujo a)
               |Encimar (Dibujo a) (Dibujo a)
               deriving (Eq,Show)

data Figura = Triangulo | Rectangulo | Circulo deriving(Eq,Show)               
-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 a = a
comp f n b = f (comp f (n-1) b)

basica :: a -> Dibujo a
basica = Basica

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

rotar45 :: Dibujo a -> Dibujo a
rotar45 = Rotar45

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = comp rotar 2

r270 :: Dibujo a -> Dibujo a
r270 = comp rotar 3


-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
a .-. b = Apilar 1 1 a b 

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
a /// b = Juntar 1 1 a b

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
a ^^^ b = Encimar a b

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (a .-. b) /// (c .-. d)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 a = a .-. (rotar a) .-. (r180 a) .-. (r270 a)

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto a (rotar a) (r180 a) (r270 a) 

-- -- Transfomar un valor de tipo a como una Basica.
-- pureDib :: a -> Dibujo a
-- pureDib a = basica

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = Basica (f a)
mapDib f (Rotar a) =  Rotar (mapDib f a)
mapDib f (Rotar45 a) = Rotar45 (mapDib f a)
mapDib f (Espejar a)        = Espejar (mapDib f a)
mapDib f (Juntar x y a b)   = Juntar x y (mapDib f a) (mapDib f b)
mapDib f (Encimar a b)      = Encimar (mapDib f a) (mapDib f b)
mapDib f (Apilar v1 v2 a b) = Apilar v1 v2 (mapDib f a) (mapDib f b) 

-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib bas rot rot45 esp ap jun enc   = go where
                                         go (Basica x) = bas x
                                         go (Rotar d) = rot (go d)
                                         go (Rotar45 d) = rot45 (go d)
                                         go (Espejar d) = esp (go d)
                                         go (Apilar x y a b) = ap x y (go a) (go b)
                                         go (Juntar x y a b) = jun x y (go a) (go b)
                                         go (Encimar a b) = enc (go a) (go b) 

type Pred a = a -> Bool

cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = foldDib 
              (\x -> if p x then f x else basica x)
              rotar
              rotar45
              espejar
              (\x y d1 d2 -> Apilar x y d1 d2)   -- Usa la función `apilar`
              (\x y d1 d2 -> Juntar x y d1 d2)   -- Usa la función `juntar`
              (\d1 d2 -> Encimar d1 d2)  


contarBasicas :: Dibujo a -> Int
contarBasicas = foldDib (\_ -> 1) id id id (\_ _ a b -> a + b) (\_ _ a b -> a + b) (\a b -> a + b)

rotarTriangulos :: Dibujo Figura -> Dibujo Figura
rotarTriangulos = cambiar (==Triangulo) (rotar.basica)

anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib
            p            -- Chequeamos el predicado
            id
            id
            id
            (\_ _ a b -> a || b) -- no nos interesan los float, se trata casi como un id.
            (\_ _ a b -> a || b)
            (||)

contieneFigura :: Eq a => a -> Dibujo a -> Bool
contieneFigura a = anyDib (==a)    