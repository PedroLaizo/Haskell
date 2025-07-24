import Text.Printf (printf)

-- 1

type Nome = String
type Valor = Float
type Quantidade = Int
type Produto = (Nome, Valor)
type Item = (Produto, Quantidade)

produtos :: [Produto]
produtos = [("AMENDOIM", 3.99),
            ("REFRIGERANTE", 10.00),
            ("AGUA MINERAL", 2.50),
            ("PAO KG", 4.78),
            ("BISCOITO", 7.25),
            ("SUCO", 8.90),
            ("HEINEKEN 5L", 64.99),
            ("CHOCOLATE", 3.99),
            ("LEITE", 2.99),
            ("CAFE", 30.99)]

itensExemplo :: [Item]
itensExemplo =
    [ (("AMENDOIM", 3.99), 5),
      (("REFRIGERANTE", 10.00), 10),
      (("AGUA MINERAL", 2.50), 15)]

quantidadesExemplo :: [Quantidade]
quantidadesExemplo = [4, 1, 6, 5, 3, 2]

-- Gerar os itens com base nas quantidades
itensGerados :: [Item]
itensGerados = proditemx quantidadesExemplo

-- 2
repete :: a -> Int -> [a]
repete _ 0 = []
repete a 1 = [a]
repete a x = a : repete a (x-1)

index :: Eq a => a -> [(a, b)] -> Maybe Int
index _ [] = Nothing
index b ((h, _):t) = if b == h
                        then Just 0
                        else fmap (+1) (index b t)

elemento :: [a] -> Int -> Maybe a
elemento [] _ = Nothing
elemento (h:t) 0 = Just h
elemento (h:t) n
    | n < 0     = Nothing
    | otherwise = elemento t (n - 1)

-- 3

addProduto :: [Produto] -> Produto -> [Produto]
addProduto lista produto = lista ++ [produto]

remProduto :: [Produto] -> Nome -> [Produto]
remProduto [] _ = [] 
remProduto ((n, v):ps) nome 
    | n == nome = ps  
    | otherwise = (n, v) : remProduto ps nome 

buscaProduto :: [Produto] -> Nome -> Maybe Produto
buscaProduto [] _ = Nothing 
buscaProduto ((n, v):ps) nome
    | n == nome = Just (n, v)
    | otherwise = buscaProduto ps nome 

-- 4

alinhaEsq :: String -> Char -> Int -> String
alinhaEsq str c x
    | length str >= x = take x str
    | otherwise       = alinhaEsq (str ++ [c]) c x

alinhaDir :: String -> Char -> Int -> String
alinhaDir str c 0 = str
alinhaDir str c x = repete c (x - length str) ++ str 

-- 5

infix 5 $$
( $$ ) :: Valor -> Int -> String
valor $$ casas = printf ("%." ++ show casas ++ "f") valor 

dinheiro :: Valor -> String 
dinheiro valor = "$" ++ (valor $$ 2)

-- 6

formataItem :: Item -> String
formataItem ((nome, valor), 0) = []
formataItem ((nome, valor), quantidade) =
    let
        subtotal = valor * fromIntegral quantidade
        nomeFormatado = alinhaEsq nome '.' 45
        valorQuantidadeFormatado = take 25 (" " ++ dinheiro valor ++ " x " ++ show quantidade ++ " =" ++ repete ' ' 25)
        subtotalFormatado = alinhaDir (dinheiro subtotal) ' ' 10
    in
        nomeFormatado ++ valorQuantidadeFormatado ++ subtotalFormatado        

-- 7
total :: [Item] -> String
total itens =
    let
        valorTotal = sum [valor * fromIntegral quantidade | ((_, valor), quantidade) <- itens]
    in
        dinheiro valorTotal

-- 8
notafiscal :: [Item] -> String
notafiscal itens = 
    let
        linhaestrela = repete '*' 80 ++ "\n"
        linhanota = repete ' ' 34 ++ "NOTA FISCAL " ++ repete ' ' 34 ++ "\n"
        itensFormatados = unlines (map formataItem itens) ++ "\n"
        totalNota = alinhaDir ("TOTAL: " ++ total itens ++ "\n") ' ' 81
    in
        linhaestrela ++ linhanota ++ linhaestrela ++ itensFormatados ++ linhaestrela ++ totalNota ++ linhaestrela

-- 9
proditem :: [Item]
proditem = aux produtos 0
  where
    aux [] _ = []
    aux (h:t) x = (h, x) : aux t (x + 1)

proditemx :: [Quantidade] -> [Item]
proditemx quantidades = aux produtos quantidades
  where
    aux _ [] = []
    aux [] _ = []
    aux (h:t) (q:quantidades) = (h, q) : aux t quantidades

-- 10
itensn :: [(Nome, Quantidade)] -> [Item]
itensn [] = []
itensn ((nome, qtd):xs) =
    case buscaProduto produtos nome of
        Just produto -> (produto, qtd) : itensn xs
        Nothing      -> itensn xs  -- ignora caso nome não encontrado

itensi :: [(Int, Quantidade)] -> [Item]
itensi [] = []
itensi ((i, qtd):xs) =
    case elemento produtos i of
        Just produto -> (produto, qtd) : itensi xs
        Nothing      -> itensi xs  -- ignora índices inválidos

-- 11
venda :: [Item] -> IO ()
venda itens = putStrLn (notafiscal itens)