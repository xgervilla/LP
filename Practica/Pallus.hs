-- Xavier Gervilla Machado
-- Cuatrimestre de primavera
-- Curso 2020/2021
-- Asignatura LP


{-# LANGUAGE RecordWildCards #-}    -- pels fields

import Data.Char(isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Maybe (mapMaybe, fromMaybe)
-- nub (lista) -> quita los repetidos (nub [1,2,2] -> [1,2])

data Term = Var String | Sym String     deriving (Eq,Show)

data Atom = Atom {_nomPredicat::String, _termes::[Term]}   deriving (Eq,Show)

data Regla = Regla {_cap::Atom,_cos::[Atom]}      deriving (Eq,Show)

type Programa = [Regla]

type Sustitucio = [(Term, Term)]    --[(variable, constante), (variable, constante),..]

type BaseConeixement = [Atom]


-- MAIN -- 

main :: IO()
main = do
    entrada <- getContents -- cogemos todo el input
    let pr = readBasicProgram entrada   -- cogemos la parte de hechos y ground atoms del programa (hasta el primer end.)
    let bc = creaBaseConeixement pr [] []
    
    let queries = readQueries entrada -- cogemos la parte de queries del programa (hasta el segundo end.)
    -- avaluamos las queries para obtener una respuesta
    avaluaQueries queries bc
    return()




--LECTURA DEL INPUT--

-- separamos en dos grandes strings (hasta el primer end. y a partir de el), para la parte basica nos quedamos con la primera (head) y para la parte de queries hay que quitar la primer (tail) y quedarnos con esa (head (tail))

-- hasta el primer end (hechos e implicaciones)
readBasicProgram :: String -> Programa
readBasicProgram s = readParte $ init $ splitOn "." $ strip $ head $ splitOn "end." s

-- hasta el segundo end (queries)
readQueries :: String -> Programa
readQueries s = readParte $ init $ splitOn "." $ strip $ head $ tail $ splitOn "end." s




-- CREACION DE LA BASE DE CONOCIMIENTO --

-- dado un programa y dos bases de conocimiento (una previa y una actual) generamos la nueva base de conocimiento que se puede generar como consecuencia de la actual
creaBaseConeixement :: Programa -> BaseConeixement -> BaseConeixement -> BaseConeixement
creaBaseConeixement pr [] [] = creaBaseConeixement pr [] (consecuencia pr []) --inicialmente ambas estan vacias
creaBaseConeixement pr bPrev bAct = if(bAct /= bPrev) then creaBaseConeixement pr bAct (consecuencia pr bAct) else bAct

-- dado un programa y una base de conocimiento, se genera la base de conocimiento con las diferentes reglas disponibles
consecuencia :: Programa -> BaseConeixement -> BaseConeixement
consecuencia [] _ = []   -- si no quedan mas reglas que evaluar, devolvemos la base
consecuencia (r:pr) dbIni = nub(dbIni ++ dbEval ++ (consecuencia pr dbIni))
    where dbEval = (avaluaRegla dbIni r) -- evaluamos la regla y hacemos recursion con el resto de reglas y la base de datos




-- AVALUACIONES PARA LA BASE DE CONOCIMIENTO --

-- avaluacion una regla para expandir la base de coneixement
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla bc r = if(esHechoRegla r)
    then
        if(not (elem (getCabeza r) bc))
            then bc ++ [(getCabeza r)] -- si la regla es un hecho Y no pertenece ya a la base de conocimiento la anadimos
            else bc -- si la regla es un hecho (ground atom) Y existe ya en la bc, no la anadimos
    else avaluaImplicaciones cabezaR sustituciones -- si la regla no es un hecho la avaluamos como implicacion (=>)
    where
        cabezaR = getCabeza r
        sustituciones = avaluaSustituciones bc r (obtenSustituciones bc r)

-- avaluacion de las reglas que son implicaciones ( => ) -> para el atomo que hay que buscar (cabeza de la regla implicacion) 
avaluaImplicaciones :: Atom -> [Sustitucio] -> BaseConeixement
avaluaImplicaciones a sust = map sustitucion sust where sustitucion = sustitueix a

-- avalua las diferentes sustituciones que se pueden obtener de una regla
avaluaSustituciones :: BaseConeixement -> Regla -> [Sustitucio] -> [Sustitucio]
avaluaSustituciones bc r sust = if(length cuerpo == 1)
    then avaluaAtom bc (head cuerpo) sust -- si no se puede "conseguir" nada mas avaluamos el atomo del cuerpo directamente
    else avaluaSustituciones bc rNext sustNext -- si el cuerpo tiene mas de un atomo avaluamos las sustituciones que se pueden obtener con la regla generada sin el primer atomo del cuerpo Y las sustituciones generadas con ese atomo
    where
        cuerpo = (getCuerpo r)
        cabeza = (getCabeza r)
        rNext = turnIntoRegla cabeza (tail cuerpo)
        sustNext = avaluaAtom bc (head cuerpo) sust


-- dada una base de conocimiento, un atomo (con variables) y una lista de sustituciones, si una sustitucion se puede unificar con el atomo entonces anadimos la sustitucion (junto con los valores de unificar) a la lista de sustituciones validas (quitando las repeticiones)
avaluaAtom :: BaseConeixement -> Atom -> [Sustitucio] -> [Sustitucio]
avaluaAtom bc atom [] = []
avaluaAtom bc atom (s:sust) = quitaRepetidos (map ((++) s) (mapMaybe (unifica sustitucion) bc)) ++ (avaluaAtom bc atom sust) where sustitucion = (sustitueix atom s)




-- COMPLEMENTOS PARA LAS AVALUACIONES

-- dado un atomo y una sustitucion, genera el atomo con la sustitucion
sustitueix :: Atom -> Sustitucio -> Atom
sustitueix a1 [] = a1
sustitueix a1 sust = Atom (getPredicado a1) (ponCuerpo (getTerms a1) sust)

-- asigna los valores disponibles a las variables
ponCuerpo :: [Term] -> Sustitucio -> [Term]
ponCuerpo terms [] = terms
ponCuerpo [] _ = []
ponCuerpo (t:terms) (s:sust) = if(esSym t)
    then [t]++ (ponCuerpo terms (s:sust))   -- si es simbolo O no existe una sustitucion en la lista, se pone directamente
    else if((fst s) == t)
        then [snd s] ++ (ponCuerpo terms sust) -- si es variable Y coincide con la de la sustitucion, se pone el valor de la sustitucion
        else if(existeVarTerm t sust)
            then (ponCuerpo (t:terms) (sust ++[s])) -- si es variable Y no coincide con la actual PERO existe, seguimos buscando con la lista poniendo el elemnto al final
            else [t] ++ (ponCuerpo terms (s:sust)) -- si es variable Y no coincide con la actual PERO no existe, lo ponemos directamente y seguimos con la siguiente

-- unifica el atomo 1 y el 2 -> genera la sustitucion para que atomo 1 sea atomo 2 O Just [] si ya son iguales O Nothing si no es posible
unifica :: Atom -> Atom -> Maybe Sustitucio
unifica a1 a2 = 
    --si hay variables repetidas en el primer termino O los terms son diferentes, Nothing
    if(variableRepetida termsA1 || (diferentesTerms termsA1 termsA2)) then Nothing
        else    -- si tienen diferente predicado O no se han podido generar sustituciones con el mismo predicado pero diferentes terms: Nothing
            if (predA1 /= predA2 || (length sust) == 0 && predA1 == predA2 && termsA1 /= termsA2) then Nothing
            else Just sust  -- si tienen mismo predicado y sust /= [] (habia variables) O sust = [] pero tienen los mismos terms (no habia variables): Just sust
    where
        predA1 = getPredicado a1
        predA2 = getPredicado a2
        termsA1 = getTerms a1
        termsA2 = getTerms a2
        sust = [(first,second) | x<- (zip termsA1 termsA2), let first = fst x,let second = snd x, not (esSym first), not (elem second termsA1) ] -- generamos los pares (variable, valor) que cumplen la sustitucion (si los hay)

-- dada una lista de sustituciones, quita aquellas repetidas
quitaRepetidos :: [Sustitucio] -> [Sustitucio]
quitaRepetidos [] = []
quitaRepetidos (x:xs) = if(quitados == []) then quitaRepetidos xs else [quitados] ++ (quitaRepetidos xs) where quitados = symRepetido x

-- dada una base de conocimiento y una regla genera la lista de sustituciones del cuerpo de la regla y los diferentes atomos de una base de conocimiento
obtenSustituciones :: BaseConeixement -> Regla -> [Sustitucio]
obtenSustituciones bc a = mapMaybe (unifica (head cuerpo)) bc where cuerpo = (getCuerpo a)




-- TRANSFORMACIONES Y CONVERSIONES --
-- de un tipo de dato a otro

-- convierte el conjunto de strings en reglas -> cada string es una linea y dentro de cada linea hay que volver a separar para quitar los espacios y tener las diferentes subStrings de una linea 
readParte :: [String] -> [Regla] --de lista de strings (lineas) a lista de reglas
readParte [] = [] 
readParte (s1:s)
    | esHechoStrings subS = [(turnIntoRegla (becomeAtom sAtom) [])] ++ readParte s
    | otherwise = [turnIntoRegla (becomeAtom sAtom) (becomeAnts sAnts)] ++ readParte s
    where
        subS = splitOn "=>" $ strip s1
        sAtom = splitOn " " $ strip $ last subS
        sAnts = splitOn "&" $ strip $ head subS

turnIntoTerm :: String -> Term --de string a term
turnIntoTerm s = if(isUpper (s!!0)) then (Var s) else (Sym s)

becomeTerms :: [String] -> [Term] --de lista de strings a lista de terms
becomeTerms [] = []
becomeTerms (t1:terms) = (turnIntoTerm t1):(becomeTerms terms)

turnIntoAtom :: String -> [Term] -> Atom -- de string y lista de terms a atomo
turnIntoAtom s term = Atom s term

becomeAtom :: [String] -> Atom --de conjunto de strings a atomo
becomeAtom (predicado:terms) = turnIntoAtom predicado (becomeTerms terms) 

becomeAnts :: [String] -> [Atom] --antecedentes de una implicacion
becomeAnts [] = []
becomeAnts (atom:ants) = [becomeAtom at1] ++ becomeAnts ants where at1 = splitOn " " $ strip atom

turnIntoRegla :: Atom -> [Atom] -> Regla --de atomo y lista de atomos a una regla
turnIntoRegla a1 a2 = Regla a1 a2




-- BUSQUEDA DE RESPUESTAS --
-- dado el programa base y una query encuentra la respuesta

-- dado un conjunto de queries (formato Programa), busca para cada una de ellas la respuesta y la imprime por pantalla
avaluaQueries :: Programa -> BaseConeixement -> IO()
avaluaQueries [] bc = return()
avaluaQueries (q:queries) bc = do
    avaluaQuery q bc
    avaluaQueries queries bc

-- dada una query (formato Regla) comprueba busca la respuesta segun su tipo
avaluaQuery :: Regla -> BaseConeixement -> IO()
avaluaQuery q bc =
    if( (getTerms (getCabeza q)) == [])
        then print (checkQuery (getCuerpo q) bc) -- si no tiene terms en la cabeza es del tipo query. Hay que ver si es true o false
        else print (getSoluciones (avaluaRegla bc q) (getCabeza q)) -- si tiene variables hay que ver que valores hacen que se cumplan

-- dado un conjunto de atomos (cuerpo de una query) indica si existen todos ellos en la base de conocimiento (es cierto)
checkQuery :: [Atom] -> BaseConeixement -> Bool
checkQuery [] bc = True
checkQuery (q:quers) bc = (elem q bc) && (checkQuery quers bc)

-- dada una base de conocimiento y un atomo a comprobar, cogemos los pares (Variable,valor) que cumplen el atomo
getSoluciones :: BaseConeixement -> Atom -> [Sustitucio]
getSoluciones [] _ = []
getSoluciones (r:res) a = [varsAct] ++ (getSoluciones res a)
    where varsAct = [(var,valor) | x <- zip (getTerms r) (getTerms a), let var = snd x, let valor = fst x]




-- GETTERS --
-- de las diferentes variables que puede tener un tipo de datos

getCuerpo :: Regla -> [Atom] --cuerpo de una regla
getCuerpo r = (_cos r)

getCabeza :: Regla -> Atom --cabeza de una regla
getCabeza r = (_cap r)

getPredicado :: Atom -> String --predicado de un atomo
getPredicado a = (_nomPredicat a)

getTerms :: Atom -> [Term] --terms de un atomo
getTerms a = (_termes a)

getTermsSus :: Sustitucio -> [Term] -- terms de una sustitucion (valores)
getTermsSus [] = []
getTermsSus (x:xs) = [second] ++ getTermsSus xs where second = snd x





-- FUNCIONES AUXILIARES --

-- constante de sustitucion vacia
sustitucioBuida :: Sustitucio
sustitucioBuida = []

-- indica si un conjunto de strings no tiene implicacion (no tiene segunda "parte", =>), por lo que al dividir la regla en => no tiene nada a la derecha (el "cuerpo" es [])
esHechoStrings :: [String] -> Bool
esHechoStrings (x:[]) = True
esHechoStrings (_:s) = False

-- indica si una regla es un hecho (no tiene cuerpo)
esHechoRegla :: Regla -> Bool
esHechoRegla r = (cuerpo == []) where cuerpo = getCuerpo r

-- indica si un atom es un hecho sin variables
esHechoAtom :: Atom -> Bool
esHechoAtom a = not (hayVariables (getTerms a))

-- indica si el termino es sym (True) o var (False)
esSym :: Term -> Bool
esSym (Sym t) = True
esSym (Var t) = False

-- indica si en los terms de un atom hay variables o son todo constantes
hayVariables :: [Term] -> Bool
hayVariables [] = False
hayVariables (t:terms) = (not (esSym t)) || (hayVariables terms)

-- indica si los terms son simbolos y son diferentes
diferentesTerms :: [Term] -> [Term] -> Bool
diferentesTerms [] [] = False
diferentesTerms (x:xs) (y:ys) = ((esSym x) && (esSym y) && x /= y) || diferentesTerms xs ys

-- indica si existe una sustitutcion con el term como variable
existeVarTerm :: Term -> Sustitucio -> Bool
existeVarTerm t [] = False
existeVarTerm t (s:sust) = (t == (fst s)) || (existeVarTerm t sust)

-- indica si en los terms hay alguna variable repetida
variableRepetida :: [Term] -> Bool
variableRepetida [] = False
variableRepetida (t:terms) = (elem t terms) || (variableRepetida terms)

-- indica si hay algun simbolo repetido, si lo hay la sustitucion sera vacia, sino sera la original
symRepetido :: Sustitucio -> Sustitucio 
symRepetido sus = if(length1 == length2) then sus else sustitucioBuida
    where
        length1 = length (nub (getTermsSus sus))
        length2 = length sus
