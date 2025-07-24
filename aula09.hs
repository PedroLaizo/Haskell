-- 1. Como você colocaria os tipos Int e Pares a como instâncias da classe Visible?
data Pares a = Par a a

class Visible t where
    toString :: t -> String
    size :: t -> Int

--instance Visible Pares a where
--    toString par = [par]
--    size _ = 1

instance Visible Int where
    toString x = show x
    size _ = 1

-- 2. Crie um tipo algébrico Ponto com as coordenadas X e Y. Modifique o tipo Forma levando em
-- consideração o tipo Ponto. Modifique também a função surface.
data Ponto = 
    Coordenadas Float Float
    deriving (Show)

data Forma =
    Circulo Ponto Float |
    Retangulo Ponto Ponto
    deriving (Show)

surface :: Forma -> Float
surface (Circulo _ r) = pi * r ^ 2
surface (Retangulo (Coordenadas x1 y1) (Coordenadas x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 3. Crie uma função que recebe dois pontos e retorna um retângulo.
fazret :: Ponto -> Ponto -> Forma
fazret p1 p2 = Retangulo p1 p2


-- 4. Crie uma função que recebe um ponto e uma forma e retorna Verdadeiro se o ponto se encontra
-- dentro da forma ou Falso caso não se encontre. Use o tipo Booleano com o retorno.
data Booleano = Falso | Verdadeiro

distancia :: Ponto -> Ponto -> Float
distancia (Coordenadas x1 y1) (Coordenadas x2 y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

encontra :: Ponto -> Forma -> Bool
encontra pt (Circulo c r) = distancia c pt <= r
encontra (Coordenadas x y) (Retangulo (Coordenadas x1 y1) (Coordenadas x2 y2)) = 
    x1 <= x1 && x <= x2 && y2 <= y && y <= y1


-- 5. Crie uma função desloca que recebe uma Forma e dois valores Float que correspondem ao deslocamento
-- nos eixos X e Y e retorna a Forma com os eixos dos seus pontos reajustados.
