module Lib where
import Text.Show.Functions

laVerdad = True

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

--1)

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f a b = f a > f b

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f a  = not . mayor f a 

ejemplo1 = ordenarSegun (mayor length) ["hola", "com", "andas","?"]
ejemplo2 = ordenarSegun (menor length) ["hola", "com", "andas","?"]

--2)

barrios = ["Palermo", "Caballito", "Recoleta", "Boedo"]

departamento1 =  Depto 1 80 7500 "Palermo"
departamento2 = Depto 1 80 7500 "Boedo"
departamento3 = Depto 2 80 5000 "Caballito"
departamento4 =  Depto 2 80 5000 "Recoleta"

departamentos = [departamento1, departamento2, departamento3, departamento4]

ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barrios depto = elem (barrio depto) barrios 

cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Requisito
cumpleRango f a b  = between a b . f   

--3)

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (cumpleRequisito depto)

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar requisitos condicion = ordenarSegun condicion . filter (flip cumpleBusqueda requisitos)

ejemploBusqueda = buscar [ubicadoEn ["Recoleta", "Palermo"] , cumpleRango ambientes 1 2, cumpleRango precio 0 6000] (mayor superficie) departamentos

--4)

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = map mail . filter (cumpleBusquedaPersona depto) 

cumpleBusquedaPersona :: Depto -> Persona -> Bool
cumpleBusquedaPersona depto persona = any (cumpleBusqueda depto) (busquedas persona)