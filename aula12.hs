-- 1. Modifique a implementação da função show de Stack para que não apareça o símbolo # quando 
-- uma pilha com um ou mais elementos é impressa. Ex.: show (Stk 5 (Stk 2EmptyStk))deveresultar em 5 | 2

module Stack (Stack, push, pop, top, stackEmpty) where
push :: t -> Stack t -> Stack t --coloca um item no topo da pilha
pop :: Stack t -> Stack t --retira o item do topo da pilha
top :: Stack t -> t --obtém o item do topo da pilha
stackEmpty :: Stack t -> Bool --verifica se a pilha está vazia
data Stack t = EmptyStk | Stk t (Stack t)

instance (Show t) => Show (Stack t) where
    show (EmptyStk) = ""
    show (Stk x s) = (show x) ++ " | " ++ (show s)

push x s = Stk x s

pop EmptyStk = error "retirada em uma pilha vazia"
pop (Stk _ s) = s

top EmptyStk = error "topo de uma pilha vazia"
top (Stk x _) = x

stackEmpty EmptyStk = True
stackEmpty _ = False


-- 2. Reimplemente a TAD Stack de modo que uma pilha contenha uma lista de valores: data Stack t = Stk [t]
data Pilha t = Plh [t]

instance (Show t) => Show (Pilha t) where
    show (Plh []) = "|"
    show (Plh (h:t)) = "|" ++ (show h) ++ (show (Plh t))

pinserir :: t -> Pilha t -> Pilha t
pinserir v (Plh lst) = Plh (v:lst)

premover :: Pilha t -> Pilha t
premover (Plh []) = (Plh [])
premover (Plh (_:t)) = (Plh t)

ptopo :: Pilha t -> Maybe t
ptopo (Plh []) = Nothing
ptopo (Plh (h:_)) = Just h

pbase :: Pilha t -> Maybe t
pbase (Plh []) = Nothing
pbase (Plh [h]) = Just h
pbase (Plh (_:t)) = pbase (Plh t)

pvazia :: Pilha t -> Bool
pvazia (Plh []) = True
pvazia _ = False

pilha = Plh [1,2,3,4,5]


data Fila t = Vazia | Fl t (Fila t)

instance (Show t) => Show (Fila t) where
    show Vazia = "<"
    show (Fl x f) = "<" ++ (show x) ++ (show f)

finserir :: t -> Fila t -> Fila t
finserir x Vazia = Fl x Vazia
finserir x (Fl h f) = Fl h (finserir x f)

fremover :: Fila t -> Fila t
fremover Vazia = Vazia
fremover (Fl x f) = f

fprimeiro :: Fila t -> Maybe t
fprimeiro Vazia = Nothing
fprimeiro (Fl x _) = Just x

fultimo :: Fila t -> Maybe t
fultimo Vazia = Nothing
fultimo (Fl x Vazia) = Just x
fultimo (Fl _ f) = fultimo f

fvazia :: Fila t -> Bool
fvazia Vazia = True
fvazia _ = False

fila = Fl 1 (Fl 2 (Fl 3 (Fl 4 (Fl 5 Vazia))))


-- 3. Implemente uma TAD Lista com as seguintes operações:
    --1. show, que imprime igual a do Haskell (usando colchetes e vírgula);
    --2. read, que transforma de string para lista;
    --3. add, adiciona um elemento e na posição i;
    --4. remove, retira o elemento da posição i;
    --5. len, calcula o tamanho da lista;
    --6. vazia, indica se a lista está vazia ou não;
    --7. concatena, junta duas listas em uma só. 


-- 4. Implemente uma TAD Árvore de busca binária, que pode ser vazia ou ser um nó com duas subárvores.
-- Implemente as funções show, add, remove, tamanho, altura e fromList
-- (criaárvoreapartir de uma lista do Haskell).