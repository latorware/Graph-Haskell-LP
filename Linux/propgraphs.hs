import Data.List

--custom types per fer mes clara la definició de les funcions
type V = String
type E = String
type Lab = String
type Prop = String
type Tvalue = String

--Datatype que representa el valor d'una propietat definida. Com es pot observar, aquest valor pot ser de diversos tipus (tal com diu l'enunciat)
data Val = ValInt {vInt :: Int } | ValDouble {vDouble :: Double } | ValString {vString :: String } | ValBool {vBool :: Bool } | ValDate {vDate :: String}

--Datatype que representa un vertex: te un nom (v), una etiqueta (vlab, que pot estar o no definida),
-- i uns valors per a aquelles propietats que te definides
data Vertex = Vertex {v :: V,    vlab :: [Lab],    vvalues :: [(Prop, Val)]}

--Datatype que representa una aresta: te un nom (e), una etiqueta (elab, que pot estar o no definida), el nom del vertex des d'on
--comença aquesta (v1), el nom del vertex on acaba aquesta (v2), i uns valors per a aquelles propietats que te definides
data Edge = Edge {e :: E,    elab :: [Lab],    v1 :: V,    v2 :: V,    evalues :: [(Prop, Val)]}

--Datatype que representa el graf: te vertexs (vertexs),  arestes (edges),
-- i totes aquelles propietats que poden estar definides en els vertexs i arestes
data PG = PG {vertexs :: [Vertex],    edges :: [Edge],    properties :: [(Prop, Tvalue)]}



--funcio basica per comprovar si un either es left o right
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False


--comprovem si un vertex esta a un graf pel nom d'aquest
checkv :: PG -> V -> Bool
checkv (PG vertexs edges properties) vertex =
    any (\(Vertex x y z) -> x == vertex) vertexs

--el mateix pero quan tenim com a parametre un either
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
checkv_either :: Either String PG -> V -> Bool
checkv_either (Right (PG vertexs edges properties)) vertex =
    any (\(Vertex x y z) -> x == vertex) vertexs


--comprovem si una aresta esta a un graf pel nom de aquesta
checke :: PG -> E -> Bool
checke (PG vertexs edges properties) edge =
    any (\(Edge x y z w q) -> x == edge) edges


--el mateix pero quan tenim com a parametre un either
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
checke_either :: Either String PG -> E -> Bool
checke_either (Right(PG vertexs edges properties)) edge =
    any (\(Edge x y z w q) -> x == edge) edges


--comprovem si una propietat esta definida en el graf (es a dir, si hem sabem el tipus del valor per aquesta)
--aquestes propietats son totes les que hi ha al fitxer propFile (o un altre, segons quin introdueixi lusuari)
checkp :: PG -> Prop -> Bool
checkp (PG vertexs edges properties) prop =
    any (\(x, y) -> x == prop) properties


--comprovem si una propietat esta definida en un vertex
checkpInV :: Vertex -> Prop -> Bool
checkpInV (Vertex v vlab vvalues) p =
    any (\(x, y) -> x == p) vvalues


--comprovem si una propietat esta definida en una aresta
checkpInE :: Edge -> Prop -> Bool
checkpInE (Edge e elab v1 v2 evalues) p =
    any (\(x, y) -> x == p) evalues


--Comprovem si un vertex no té label (és a dir, no té etiqueta)
checkVemptyLab :: Vertex -> Bool
checkVemptyLab (Vertex v vlab vvalues)
    | vlab == [] = True
    | otherwise = False


--Comprovem si un a aresta no té label (és a dir, no té etiqueta)
checkEemptyLab :: Edge -> Bool
checkEemptyLab (Edge e elab v1 v2 evalues)
    | elab == [] = True
    | otherwise = False


--Ens diu el tipus del que ha de ser un valor per a una propietat
ptype :: PG -> Prop -> Tvalue
ptype (PG vertexs edges properties) prop = valor
    where
        v = (filter (\(x, y) -> x == prop) properties)
        valor --en cas que al fitxer prop (ex: propFile.pg) no s'hagi especificat el tipus, es declararà per defecte com un String
            --Aixo es necessari ja que als fitxers del test1 hi ha un error: birthday /= birthDay
            | v == [] = "String"
            |otherwise = snd (head v)

--passa un string a int
toInt :: String -> Int
toInt = read

--passa una data a string (es a dir, no fa res ja que una data es un string en aquest programa)
toDate :: String -> String
toDate x = x

--passa un string a double
toDouble :: String -> Double
toDouble = read

--passa un string a bool
toBool :: String -> Bool
toBool = read


--afegeix un vertex en un graf
addVertex :: PG -> V -> PG
addVertex (PG vertexs edges properties) vertex = PG ( Vertex vertex [] [] : vertexs) edges properties


--afegeix una aresta en un graf
--PUNT 2 del primer apartat de l'enunciat
addEdge :: PG -> E -> V -> V -> Either String PG
addEdge (PG vertexs edges properties) nouE startV endV
    | not (checkv (PG vertexs edges properties) startV) =
        addEdge (addVertex (PG vertexs edges properties) startV) nouE startV endV
    | not (checkv (PG vertexs edges properties) endV) =
        addEdge (addVertex (PG vertexs edges properties) endV) nouE startV endV
    | not (checke (PG vertexs edges properties) nouE) =
        Right (PG vertexs (Edge nouE [] startV endV [] : edges) properties)
    | otherwise = Left ("ERROR: aresta" ++ show nouE ++  "ja definida.")


--passa una string (que representa un valor) al tipus corresponent, segons la propietat al que aquest pertanyi
stoValue :: PG -> Prop -> String -> Val
stoValue (PG vertexs edges properties) p value
  --  | not (checkp (PG vertexs edges properties) p) = Left "ERROR: El tipus d'aquesta propietat no ha estat definida a l'arxiu de propietats del graf, i per tant no es pot saber"
    | t == "Int" = ValInt (toInt value)
    | t == "String" = ValString value
    | t == "Double" = ValDouble (toDouble value)
    | t == "Bool" = ValBool (toBool value)
    | t == "Date" = ValDate (toDate value)
    where
        t = ptype (PG vertexs edges properties) p


--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
-- el mateix que la anterior funcio
stoValue_either :: Either String PG -> Prop -> String -> Val
stoValue_either (Right(PG vertexs edges properties)) p value
  --  | not (checkp (PG vertexs edges properties) p) = Left "ERROR: El tipus d'aquesta propietat no ha estat definida a l'arxiu de propietats del graf, i per tant no es pot saber"
    | t == "Int" = ValInt (toInt value)
    | t == "String" = ValString value
    | t == "Double" = ValDouble (toDouble value)
    | t == "Bool" = ValBool (toBool value)
    | t == "Date" = ValDate (toDate value)
    where
        t = ptype (PG vertexs edges properties) p



-- PUNT 3 APARTAT 1 de l'enunciat
defVprop :: PG -> V -> (Prop, Val) -> PG
defVprop (PG vertexs edges properties) vertex p
    | not (checkv (PG vertexs edges properties) vertex) = PG vertexs edges properties
    | not (checkpInV (Vertex o u i) (fst p)) = pg1 --afegir a Vdata el prop i value
    | otherwise = pg2
    where
        pg1 = PG (Vertex o u (p:i):allMinusV) edges properties
        pg2 = PG (Vertex o u (p : allPminusCurrent):allMinusV) edges properties
        (Vertex o u i) = head(filter (\(Vertex x y z) -> x == vertex) vertexs)
        allPminusCurrent = filter (\(j, h) -> j /= fst p) i
        allMinusV = filter (\(Vertex x y z) -> x /= vertex) vertexs


-- PUNT 4 APARTAT 1 de l'enunciat
defEprop :: PG -> E -> (Prop, Val) -> PG
defEprop (PG vertexs edges properties) edge p
    | not (checke (PG vertexs edges properties) edge) = PG vertexs edges properties
    | not (checkpInE (Edge o u c b i) (fst p)) = pg1
    | otherwise = pg2
    where
        pg1 = PG vertexs (Edge o u c b (p:i):allMinusE) properties
        pg2 = PG vertexs (Edge o u c b (p:allPMinusCurrent):allMinusE) properties
        (Edge o u c b i) = head (filter (\(Edge x y z w q) -> x == edge) edges)
        allPMinusCurrent = filter (\(j, h) -> j /= fst p) i
        allMinusE = filter (\(Edge x y z w q) -> x /= edge) edges



--PUNT 5 APARTAT 1 de l'enunciat
defVlabel :: PG -> V -> Lab -> Either String PG
defVlabel (PG vertexs edges properties) vertex label
    | not (checkv (PG vertexs edges properties) vertex) = Left "El vertex no existeix"
    | not (checkVemptyLab (Vertex o u i)) = Left "ERROR: Label ja definida per a aquest vertex"
    | otherwise = Right (PG (Vertex o [label] i:allMinusV) edges properties)
    where
        (Vertex o u i) = head (filter (\(Vertex x y z) -> x == vertex) vertexs )
        allMinusV = filter (\(Vertex x y z) -> x /= vertex) vertexs


-- PUNT 6 APARTAT 1 de l'enunciat
defElabel :: PG -> E -> Lab -> Either String PG
defElabel (PG vertexs edges properties) edge label
    | not (checke (PG vertexs edges properties) edge) = Left "La aresta no existeix"
    | not (checkEemptyLab (Edge o u i q s)) = Left "ERROR: Label ja definida per a auqesta aresta."
    | otherwise = Right (PG vertexs (Edge o [label] i q s:allMinusE) properties)
    where
        (Edge o u i q s) = head (filter (\(Edge x y z w r) -> x == edge) edges)
        allMinusE = filter (\(Edge x y z w r) -> x /= edge) edges


--PUNT 1 APARTAT 2 de l'enunciat
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
getpvalue :: Either String PG -> Either V E -> Either String [(Prop, Val)]
getpvalue (Right (PG vertexs edges properties)) (Left vertex)
    | checkv (PG vertexs edges properties) vertex = Right i
    | otherwise = Left "ERROR: Aquest vertex no existeix"
    where
        (Vertex o u i) = head (filter (\(Vertex x y z) -> x == vertex) vertexs )
getpvalue (Right(PG vertexs edges properties)) (Right edge)
    | checke (PG vertexs edges properties) edge = Right b
    | otherwise = Left "ERROR: Aquesta aresta no existeix"
    where
        (Edge j k l m b) = head (filter (\(Edge x y z q w) -> x == edge) edges)


--PUNT 2 APARTAT 2 de enunicat
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
propV :: Either String PG -> Int -> Prop -> [(Lab, Val)]
propV (Right(PG vertexs edges properties)) k p
     = final
     where
        allpv = filter (\(Vertex x y z) -> checkpInV (Vertex x y z) p) vertexs
        candidates = take k allpv
        final = map (\(Vertex r t e) -> (head t, snd (head (filter (\(l, d) -> l == p) e)))) candidates


--util per a imprimir una llista de etiquetes-valor
labValString :: [(Lab, Val)] -> [(Lab, String)]
labValString x = map (\(x, y) -> (x, showVal y )) x

--el mateix que la anterior pero tambe amb el nom del vertex
vlabValString :: [(V, Lab, Val)] -> [(V, Lab, String)]
vlabValString x = map (\(q, x, y) -> (q, x, showVal y )) x


--PUNT 3 APARTAT 2 de enunciat
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
propE :: Either String PG -> Int -> Prop -> [(Lab, Val)]
propE (Right(PG vertexs edges properties)) k p
    = final
    where
        allpe = filter (\(Edge z x c v b) -> checkpInE (Edge z x c v b) p) edges
        candidates = take k allpe
        final = map (\(Edge q w e r t) -> (head w, snd (head (filter (\(l, d) -> l == p) t)))) candidates



--funcio  auxiliar de la seguent
reachableAux :: PG -> V -> V -> Lab -> [V] -> Bool
reachableAux (PG vertexs edges properties) vstart vend lab visited
    | vstart == vend = True
    | otherwise = any (\x -> reachableAux (PG vertexs edges properties) x vend lab (vstart:visited)) candidates4
    where
        candidates1 = filter (\(Edge q w e r t) -> (e == vstart) && not (any (== r) visited)) edges
        candidates2 = filter (\(Edge qq ww ee rr tt) -> ww /= []) candidates1
        candidates3 = filter (\(Edge qqq www eee rrr ttt) -> head www == lab) candidates2
        candidates4 = map (\(Edge qqqq wwww eeee rrrr tttt) -> rrrr) candidates3

--PUNT 5 APARTAT 2 de enunciat
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
reachable :: Either String PG -> V -> V -> Lab -> Either String Bool
reachable (Right pg) vstart vend lab
    | not (checkv pg vstart) = Left "ERROR: El primer vertex introduit no existeix al graf"
    | not (checkv pg vend) = Left "ERROR: El segon vertex introduit no existeix al graf"
    | otherwise = Right (reachableAux pg vstart vend lab [])



--diu si entre un vertex i un altre vertex hi ha un k-path
kpath :: PG -> Int ->  V -> V -> [V] -> Bool
kpath (PG vertexs edges properties) nat vstart vend visited
    | ((0 == nat) && (vstart == vend)) = True
    | ((0 /= nat) && (vstart == vend)) = False
    | otherwise = any (\x -> kpath (PG vertexs edges properties) (nat-1) x vend (vstart:visited)) candidates2
    where
        candidates1 = filter (\(Edge q w e r t) -> (e == vstart) && not (any (== r) visited)) edges
        candidates2 = map (\(Edge qqqq wwww eeee rrrr tttt) -> rrrr) candidates1


--PUNT 4 APARTAT 2 de enunciat
--either necessari ja que el graf al main serà declarat com un either String PG (tot i que aqui mai pasara que sigui String)
kHops :: Either String PG -> Int -> V -> Prop -> (Val -> Val -> Bool) -> Val -> Either String [(V, Lab, Val)]
kHops (Right(PG vertexs edges properties)) nat vstart p function v
    | not (checkv (PG vertexs edges properties) vstart) = Left "ERROR: El vertex introduit no existeix en el graf"
    | otherwise = Right result3
    where
        result1 = filter (\(Vertex x y z) -> (kpath (PG vertexs edges properties) nat x vstart []) && (checkpInV (Vertex x y z) p )) vertexs
        result2 = filter (\(Vertex q w e) -> function v (snd(head(filter (\(l, j) -> l == p)e)))    ) result1
        result3 = map (\(Vertex q w e) ->  (q, head(w), (snd(head(filter (\(l, j) -> l == p)e)))  )) result2

--Compara dos valors, i retorna true si son iguals, false sinó
eqVal :: Val -> Val -> Bool 
eqVal (ValInt x) (ValInt y) 
    |x == y = True 
    |otherwise = False 
eqVal (ValBool x) (ValBool y) 
    |x == y = True 
    |otherwise = False 
eqVal (ValString x) (ValString y) 
    |x == y = True 
    |otherwise = False 
eqVal (ValDouble x) (ValDouble y) 
    |x == y = True 
    |otherwise = False
eqVal (ValDate x) (ValDate y) 
    |x == y = True 
    |otherwise = False 
eqVal _ _ = False  



--funcio auxiliar de populate. popula graf amb l'arxiu Rho (o el que s'hagi especificat)
populateRho :: [String] -> Either String PG -> Either String PG
populateRho _ (Left message) = Left message
populateRho [] (Right pg ) = Right pg
populateRho (edge:vstart:vend:xs) (Right pg)
    = populateRho xs newpg
    where
        newpg = addEdge pg edge vstart vend


--afegeix una propietat a un graf
addPropG :: Prop -> String -> PG -> PG
addPropG p t (PG vertexs edges properties) = PG vertexs edges ((p,t):properties)




--funcio auxiliar de populate. popula graf amb l'arxiu prop (o el que s'hagi especificat)
populateProp :: [String] -> Either String PG -> Either String PG
populateProp _ (Left message) = Left message
populateProp [] (Right pg) = Right pg
populateProp (p:t:xs) (Right pg)
    = populateProp xs (Right newpg)
    where
        newpg = addPropG p t pg


--funcio auxiliar de populate. popula graf amb l'arxiu lambda (o el que s'hagi especificat)
populateLambda :: [String] -> Either String PG -> Either String PG
populateLambda _ (Left message) = Left message
populateLambda [] (Right pg) = Right pg
populateLambda (ve:label:xs) (Right pg)
    | checkv pg ve = populateLambda xs newpg1
    | otherwise = populateLambda xs newpg2
    where
        newpg1 = defVlabel pg ve label
        newpg2 = defElabel pg ve label

--funcio auxiliar de populate. popula graf amb l'arxiu sigma (o el que s'hagi especificat)
populateSigma :: [String] -> Either String PG -> Either String PG
populateSigma _ (Left message) = Left message
populateSigma [] (Right pg) = Right pg
populateSigma (ve:p:v:xs) (Right pg)
    | checkv pg ve = populateSigma xs (Right newpg1)
    | otherwise = populateSigma xs (Right newpg2)
    where
        value = stoValue pg p v
        newpg1 = defVprop pg ve (p,value)
        newpg2 = defEprop pg ve (p,value)

--PUNT 1 APARTAT 1 de l'enunciat
populate :: [String] -> [String] -> [String] -> [String] -> Either String PG
populate rho lambda sigma prop
    = pg4
    where
        pg1 = populateRho rho (Right (PG [] [] []))
        pg2 = populateProp prop pg1
        pg3 = populateLambda lambda pg2
        pg4 = populateSigma sigma pg3


--passa una llista de arestes a string tal com es volen imprimir
showLE :: [Edge] -> String
showLE [] = "\n"
showLE x
    = unlines lstring
    where
        lstring = map (\y -> showE y) x

--passa una aresta a String tal com es vol imprimir
showE :: Edge -> String
showE (Edge e elab v1 v2 evalues)
    | elab == []= "(" ++ v1 ++ ")-" ++ e ++ "[" ++ " " ++ "]-> (" ++ v2 ++ "){" ++ showPvalueS evalues ++ "}"
    |otherwise = "(" ++ v1 ++ ")-" ++ e ++ "[" ++ head(elab) ++ "]-> (" ++ v2 ++ "){" ++ showPvalueS evalues ++ "}"


--passa una llista de vertexs a string tal com es vol imprimir a aquests
showLV :: [Vertex] -> String
showLV [] = "\n"
showLV x
    = unlines lstring
    where
        lstring = map (\y -> showV y) x


--passa un vertex a String tal com es vol imprimir
showV :: Vertex -> String
showV (Vertex v vlab vvalues)
    | vlab == [] = v ++ "[" ++ " " ++ "]" ++ "{" ++ showPvalueS vvalues ++ "}"
    | otherwise= v ++ "[" ++ head(vlab) ++ "]" ++ "{" ++ showPvalueS vvalues ++ "}"


--passa una llista propietats-val a String tal com es vol imprimir
showPvalueS :: [(Prop, Val)] -> String
showPvalueS [] = []
showPvalueS x = intercalate ", " (showPvalueSAux x)

--passa una llista propietats-val a string tal com simprimiran aquests 
showPvalueSAux :: [(Prop, Val)] -> [String]
showPvalueSAux xs = map (\ x -> "(" ++ showPvalue x ++ ")") xs


--passa un valor a string tal com simprimira
showVal :: Val -> String
showVal (ValInt x) = show x
showVal (ValBool y) = show y
showVal (ValDate z) = z
showVal (ValDouble q) = show q
showVal (ValString s) = s

--passa un tupla prop-val a string tal com simprimira
showPvalue :: (Prop, Val) -> String
showPvalue (x, y) = intercalate ", " (x:[showVal y])



--PUNT 7 APARTAT 1 de el enunciat
showGraph :: PG -> String
showGraph (PG vertexs edges properties)
    = showLV vertexs ++ "\n" ++ showLE edges




main = do
    putStrLn "Introdueix el nom del fitxer 'rho'  (on hi ha les arestes (junt amb els vertexs d'aquestes))"
    rhopath <- getLine
    rhoFile <- readFile rhopath

    putStrLn "Introdueix el nom del fitxer 'lambda'  (on hi ha les etiquetes (labels) de les arestes i vertexs)"
    lambdapath <- getLine
    lambdaFile <- readFile lambdapath

    putStrLn "Introdueix el nom del fitxer 'sigma'  (on hi ha les propietats (i valors) definits principalment per als vertexs i arestes)"
    sigmapath <- getLine
    sigmaFile <- readFile sigmapath

    putStrLn "Introdueix el nom del fitxer 'prop'  (on s'informa dels tipus que són els valors de cada propietat)"
    proppath <- getLine
    propFile <- readFile proppath

    let rhoInWords = words rhoFile
    let lambdaInWords = words lambdaFile
    let sigmaInWords = words sigmaFile
    let propInWords = words propFile

    let pgOFICIAL = populate rhoInWords lambdaInWords sigmaInWords propInWords

    case pgOFICIAL of
        Left s -> print s
        Right z -> putStr (showGraph z)

    let script = do
        putStrLn "Tria una de les opcions disponibles de updates, i queries: "
        putStrLn ""
        putStrLn "1: addEdge (afegeix una aresta nova, Si els vertexs d'aquesta no estan al graf, també s'afegiran)"
        putStrLn "2: defVprop (defineix una propietat per a un vertex que està  al graf)"
        putStrLn "3: defEprop (defineix una propiertat per a una aresta que està  al graf)"
        putStrLn "4: defVlabel (defineix l'etiqueta (label) de un vertex que està al graf )"
        putStrLn "5: defElabel (defineix l'etiqueta (label) per una aresta que està al graf)"
        putStrLn "6: showGraph (torna a imprimir graf)"
        putStrLn "7: rho_prima (mostra les propietats junt amb els valors d'aquestes, per a aquelles propietats definides d'un vertex o aresta)"
        putStrLn "8: propV (definició llarga, per tant per consultar-la mirar a l'enunciat))"
        putStrLn "9: propE (definició llarga, per tant per consultar-la mirar a l'enunciat))"
        putStrLn "10: kHops (definició llarga, per tant per consultar-la mirar a l'enunciat))"
        putStrLn "11: reachable (definició llarga, per tant per consultar-la mirar a l'enunciat))"
        putStrLn "12: STOP SCRIPT"

        opcio <- getLine
        let opcio_int = toInt opcio

        if opcio_int == 1 then
            do
                putStrLn "Introdueix, primer el nom de la aresta, després el del primer vertex (amb el que comença l'aresta), i després el de l'altre vertex."
                putStrLn "TOTS EN UNA LINIA "
                aresta_line <- getLine
                let aresta = words aresta_line
                let pg1 = populateRho aresta pgOFICIAL  --populateRho crida automaticament a addEdge
                case pg1 of
                    Left s -> print s
                    Right z -> putStr (showGraph z)

                script

        else if opcio_int == 2 then
            do
                putStrLn "Introdueix, primer el nom del vertex al que se li afegirà la propietat"
                putStrLn "A continuació, el nom de la propietat, i finalment el valor de la propietat que tindrà el  vertex"
                putStrLn "TOT AIXO EN UNA SOLA LINIA"
                vePV_line <- getLine
                let vePV = words vePV_line
                let pg2 = populateSigma vePV pgOFICIAL --populateSigma crida automaticament a defVprop
                case pg2 of
                    Left s -> print s
                    Right z -> putStr (showGraph z)

                script

        else if opcio_int == 3 then
            do
                putStrLn "Introdueix, primer el nom de la aresta al que se li afegirà la propietat"
                putStrLn "A continuació, el nom de la propietat, i finalment el valor de la propietat que tindrà la aresta "
                putStrLn "TOT AIXO EN UNA SOLA LINIA"
                vePV_line2 <- getLine
                let vePV2 = words vePV_line2
                let pg3 = populateSigma vePV2 pgOFICIAL --populateSigma crida automaticament a defEprop
                case pg3 of
                    Left s -> print s
                    Right z -> putStr (showGraph z)
                
                script

        else if opcio_int == 4 then
            do
                putStrLn "Introdueix, primer el nom del vertex al que se li posarà la etiqueta  "
                putStrLn "I a continuació el nom de la etiqueta"
                putStrLn "TOT AIXO EN UNA SOLA LINIA"
                vLabel_line <- getLine
                let vLabel = words vLabel_line
                let pg4 = populateLambda vLabel pgOFICIAL --populateLambda crida automaticament a defEprop
                case pg4 of
                    Left s -> print s
                    Right z -> putStr (showGraph z)

                script


        else if opcio_int == 5 then
            do
                putStrLn "Introdueix, primer el nom de la aresta al que se li posarà la etiqueta  "
                putStrLn "I a continuació el nom de la etiqueta"
                putStrLn "TOT AIXO EN UNA SOLA LINIA"
                eLabel_line <- getLine
                let eLabel = words eLabel_line
                let pg5 = populateLambda eLabel pgOFICIAL --populateLambda crida automaticament a defEprop
                case pg5 of
                    Left s -> print s
                    Right z -> putStr (showGraph z)
                
                script


        else if opcio_int == 6 then
            do
                putStrLn "Impresio del graf: "
                case pgOFICIAL of
                    Left s -> print s
                    Right z -> putStr (showGraph z)

                script

        
        else if opcio_int == 7 then
            do
                putStrLn "Introdueix el nom de la aresta o graf"
                ve_line <- getLine 
                let ve = words ve_line
                let ve2 = head(ve)
                let is_v = (checkv_either pgOFICIAL ve2)
                let is_e = (checke_either pgOFICIAL ve2)

                if is_v then
                    do
                        let result = getpvalue pgOFICIAL (Left ve2)
                        case result of
                            Left s -> print s
                            Right z -> putStrLn (showPvalueS z)

                    else if is_e then
                    do
                        let result = getpvalue pgOFICIAL (Right ve2)
                        case result of
                            Left s -> print s
                            Right z -> putStrLn (showPvalueS z)


                else 
                    do
                        putStrLn "El vertex o aresta no es troba al graf"
                
                script



        else if opcio_int == 8 then
            do
                putStrLn "Introdueix el paràmetre k"
                putStrLn "A continuació introdueix la propietat"
                putStrLn "TOT EN UNA SOLA LINIA"
                k_prop_line <- getLine 
                let k_prop = words k_prop_line
                let k = head(k_prop)
                let prop = last(k_prop)
                let kInt = toInt k
                let result = propV pgOFICIAL kInt prop
                print (labValString result)
                script


        else if opcio_int == 9 then
            do
                putStrLn "Introdueix el paràmetre k"
                putStrLn "A continuació introdueix la propietat"
                putStrLn "TOT EN UNA SOLA LINIA"
                k_prop_line <- getLine 
                let k_prop = words k_prop_line
                let k = head(k_prop)
                let prop = last(k_prop)
                let kInt = toInt k
                let result = propE pgOFICIAL kInt prop
                print (labValString result)
                script

        else if opcio_int == 10 then
            do
                putStrLn "Introdueix el paràmetre k"
                putStrLn "A continuació, el vertex (el nom)"
                putStrLn "A continuació, la propietat"
                putStrLn "A continuació, el valor que es passa com a parametre a la funcio"
                putStrLn "TOT EN UNA SOLA LINIA"
                putStrLn "S'utilitzara la propietat per defecte (igualtat). Aquesta propietat es pot canviar en el codi del programa"
                input_line <- getLine 
                let input = words input_line
                let k = head(input)
                let kInt = toInt k
                let v = input!!1
                let prop = input!!2
                let value = last(input)
                let valueInType = stoValue_either pgOFICIAL prop value
                let result = kHops pgOFICIAL kInt v prop eqVal valueInType
                case result of
                    Left x -> print x
                    Right y -> print (vlabValString y)
                
                script


        else if opcio_int == 11 then
            do
                putStrLn "Introdueix el valor del primer vertex (el nom)"
                putStrLn "A continuació, el valor del segon vertex (el nom)"
                putStrLn "A continuació, el valor de la etiqueta"
                putStrLn "TOT EN UNA SOLA LINIA"
                input_line <- getLine 
                let input = words input_line
                let v1 = head(input)
                let v2 = input!!1
                let lab = last(input)
                let result = reachable pgOFICIAL v1 v2 lab
                case result of
                    Left x -> print x
                    Right y -> print y

                script

        else
            return()


    script


    return ()
