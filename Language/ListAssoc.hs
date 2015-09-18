module Language.ListAssoc where

data ListAssoc a b = Empty
                   | Node a b (ListAssoc a b)
                   
la_long :: Integral c => ListAssoc a b -> c
la_long = undefined

la_aListaDePares :: ListAssoc a b -> [(a,b)]
la_aListaDePares = undefined

la_existe :: Eq a => ListAssoc a b -> a -> Bool
la_existe = undefined

la_buscar :: Eq a => ListAssoc a b -> a -> Maybe b
la_buscar = undefined

la_agregar :: Eq a => a -> b -> ListAssoc a b -> ListAssoc a b
la_agregar = undefined

la_borrar :: Eq a => a -> ListAssoc a b -> ListAssoc a b
la_borrar = undefined






