import Data.List

main :: IO ()
main = return ()

thd :: (String,String,Float) -> Float
thd (_,_,hojas) = hojas

promedioDeHojas :: [(String, String, Float)] -> Float
promedioDeHojas listaLibros = ((/ genericLength listaLibros).sum.(map thd)) listaLibros

lecturaObligatoria :: (String, String, Float) -> Bool
lecturaObligatoria (_,"Stephen King",_) = True
lecturaObligatoria ("Fundacion","Isaac Asimov",230.0) = True
lecturaObligatoria ("Eragon","Cristopher Paolini",_) = True
lecturaObligatoria ("Eldest","Cristopher Paolini",_) = True
lecturaObligatoria ("Brisignr","Cristopher Paolini",_) = True
lecturaObligatoria ("Legado","Cristopher Paolini",_) = True
lecturaObligatoria (_, _, _) = False

esFantasiosa :: [(String,String,Float)] -> Bool
esFantasiosa listaLibros  = (any esDePaoliniOGaiman) listaLibros

esDePaoliniOGaiman :: (String,String,Float) -> Bool
esDePaoliniOGaiman (_, autor, _) = autor == "Cristopher Paolini" || autor == "Neil Gaiman"

first :: (String, String, Float) -> String
first (libro,_,_) = libro

nombreDeLaBiblioteca :: [(String, String, Float)] -> String
nombreDeLaBiblioteca listaLibros = (concat.map (sacarVocales.(first))) listaLibros

sacarVocales :: String -> String
sacarVocales libro = filter (noEsVocal) libro

noEsVocal :: Char -> Bool
noEsVocal letra =  letra /= 'a' || letra /= 'e' || letra /= 'i' || letra /= 'o' || letra /= 'u' 

bibliotecaLigera :: [(String, String, Float)] -> Bool
bibliotecaLigera listaLibros = ((all (40>=)).(map thd)) listaLibros

