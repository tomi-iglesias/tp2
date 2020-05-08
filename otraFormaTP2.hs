import Text.Show.Functions()
import Data.List (genericLength)

main :: IO ()
main = return ()

--data!
-- Sin Record Syntax!
data Libro' = UnLibro' String String Int deriving (Show, Eq)

autor' :: Libro' -> String
autor' (UnLibro' _ unAutor _) = unAutor

-- Record Syntax!
data Libro = UnLibro {
  titulo :: String,
  autor :: String,
  cantidadDePaginas :: Int
} deriving (Show, Eq)

type Autor = String

{-
Nos crea:
- Un tipo: Libro
- Una función constructora: UnLibro :: String -> Autor -> Int -> Libro
- Un Patrón: (UnLibro unTitulo unAutor cantidadDePaginas) = .....
- (Si usamos record Syntax) funciones para accessors: titulo unLibro
-}

elVisitante :: Libro
elVisitante = UnLibro "El Visitante" "Stephen King" 592

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = UnLibro "Shingeki no Kyojin 1" "Hajime Isayama" 40

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = UnLibro "Shingeki no Kyojin 3" "Hajime Isayama" 40

shingekiNoKyojin27 :: Libro
shingekiNoKyojin27 = UnLibro "Shingeki no Kyojin 27" "Hajime Isayama" 40

fundacion :: Libro
fundacion = UnLibro "Fundacion" "Isaac Asimov" 230

sandman5 :: Libro
sandman5 = UnLibro "sandman5" "Neil Gaiman" 35

sandman10 :: Libro
sandman10 = UnLibro "sandman10" "Neil Gaiman" 35

sandman12 :: Libro
sandman12 = UnLibro "sandman12" "Neil Gaiman" 35

eragon :: Libro
eragon = UnLibro "eragon" "Christopher Paolini" 544

eldest :: Libro
eldest = UnLibro "eldest" "Christopher Paolini" 704

brisignr :: Libro
brisignr = UnLibro "brisignr" "Christopher Paolini" 700

legado :: Libro
legado = UnLibro "legado" "Christopher Paolini" 811

type Saga = [Libro]

sagaDeEragon :: Saga
sagaDeEragon = [eragon, eldest, brisignr, legado]

type Biblioteca = [Libro]

biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin3, shingekiNoKyojin27, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]

promedioDeHojas :: Biblioteca -> Float
promedioDeHojas unaBiblioteca = fromIntegral (cantidadDeHojas unaBiblioteca) / genericLength unaBiblioteca

cantidadDeHojas :: Biblioteca -> Int
cantidadDeHojas unaBiblioteca = (sum . map cantidadDePaginas) unaBiblioteca

esLecturaObligatoria :: Libro -> Bool
esLecturaObligatoria unLibro = esDe "Stephen King" unLibro || perteneceASagaEragon unLibro || esFundacion unLibro

esDe :: Autor -> Libro -> Bool
esDe unAutor unLibro = ((== unAutor) . autor) unLibro

perteneceASagaEragon :: Libro -> Bool
perteneceASagaEragon unLibro = elem unLibro sagaDeEragon

esFundacion :: Libro -> Bool
esFundacion (UnLibro "fundacion" "Isaac Asimov" 230) = True
esFundacion _                                        = False

esFundacion' :: Libro -> Bool
esFundacion' unLibro = unLibro == fundacion

esLecturaObligatoria' :: Libro -> Bool
esLecturaObligatoria' (UnLibro _ "Stephen King" _)             = True
esLecturaObligatoria' (UnLibro "fundacion" "Isaac Asimov" 230) = True
esLecturaObligatoria' unLibro                                  = perteneceASagaEragon unLibro

esFantasiosa :: Biblioteca -> Bool
esFantasiosa unaBiblioteca = any esLibroFantasioso unaBiblioteca

esFantasiosa' :: Biblioteca -> Bool
esFantasiosa' unaBiblioteca = any (esDe "Christopher Paolini") unaBiblioteca || any (esDe "Neil Gaiman") unaBiblioteca

esLibroFantasioso :: Libro -> Bool
esLibroFantasioso (UnLibro _ unAutor _) = elem unAutor ["Cristopher Paolini", "Neil Gaiman"]

esLibroFantasioso' :: Libro -> Bool
esLibroFantasioso' unLibro = esDe "Christopher Paolini" unLibro || esDe  "Neil Gaiman" unLibro

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca unaBiblioteca = (sacarVocales . nombreDeLaBibliotecaConVocales) unaBiblioteca

sacarVocales :: String -> String
sacarVocales unTitulo = filter esConsonante unTitulo

esConsonante :: Char -> Bool
esConsonante unCaracter = (not . elem unCaracter) "aeiouAEIOU"

nombreDeLaBibliotecaConVocales :: Biblioteca -> String
nombreDeLaBibliotecaConVocales unaBiblioteca = concatMap titulo unaBiblioteca

esBibliotecaLigera :: Biblioteca -> Bool
esBibliotecaLigera unaBiblioteca = all esLibroLigero unaBiblioteca

esLibroLigero :: Libro -> Bool
esLibroLigero unLibro = ((<= 40) . cantidadDePaginas) unLibro

genero :: Libro -> String
genero unLibro
  | ((< 50) . cantidadDePaginas) unLibro = "Comic"
  | esDe "Stephen King" unLibro          = "Terror"
  | (esJapones . autor) unLibro          = "Manga"
  | otherwise                            = "No clasificado"

esJapones :: Autor -> Bool
esJapones "Hajime Isayama" = True
esJapones _                = False

agregarPaginas :: Libro -> Int -> Libro
agregarPaginas (UnLibro unTitulo unAutor unasPaginas) paginasAAgregar = UnLibro unTitulo unAutor (unasPaginas + paginasAAgregar)

agregarPaginas' :: Libro -> Int -> Libro
agregarPaginas' algunLibro paginasAAgregar = cambiarCantidadDePaginas (+ paginasAAgregar) algunLibro

escritoPorElLider :: Libro -> Libro
escritoPorElLider unLibro = (cambiarTitulo (++ " escrito por el lider") . cambiarAutor (const "El Lider")) unLibro

sacarSecuela :: Libro -> Libro
sacarSecuela unLibro = (cambiarCantidadDePaginas (const 400) . cambiarTitulo (++ " 2")) unLibro

cambiarTitulo :: (String -> String) -> Libro -> Libro
cambiarTitulo unaFuncion unLibro = unLibro { titulo = unaFuncion (titulo unLibro) }

cambiarAutor :: (String -> String) -> Libro -> Libro
cambiarAutor unaFuncion unLibro = unLibro { autor = unaFuncion (autor unLibro) }

cambiarCantidadDePaginas :: (Int -> Int) -> Libro -> Libro
cambiarCantidadDePaginas unaFuncion unLibro = unLibro { cantidadDePaginas = unaFuncion (cantidadDePaginas unLibro) }


type Persona = (String, [Libro -> Bool])

julian :: Persona
julian = ("Julian", [esLibroLigero, esLibroFantasioso, esDe "Stephen King"])

gustos :: Persona -> [Libro -> Bool]
gustos unaPersona = snd unaPersona

{-
- Necesitamos una funcion
- No tenemos un (buen) nombre para la funcion
- No la queremos usar de nuevo
-}

-- Expresiones Lambda!

-- Sintaxis tradicional
leGusta :: Libro -> Persona -> Bool
leGusta unLibro unaPersona = any (leGustaSegun unLibro) (gustos unaPersona)

leGustaSegun ::  Libro -> (Libro -> Bool) ->         Bool
leGustaSegun    unLibro       unGusto     =     unGusto unLibro

-- con lambdas
leGusta' :: Libro -> Persona -> Bool
leGusta' unLibro unaPersona = any (\unGusto -> unGusto unLibro) (gustos unaPersona)

-- (\unParametro -> algo que quiero hacer con ese parametro )
-- (\unNumero otroNumero -> (unNumero + otroNumero) * (unNumero - otroNumero))