module Library where
import PdePreludat

--Escuelita de Thanos
-- Primera parte
-- Los enanos de Nidavellir nos han pedido modelar los guanteletes que ellos producen en su herrería.
-- Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. 

-- También se sabe de los personajes que tienen una edad, una energía, una serie de habilidades 
-- (como por ejemplo “usar espada”, “controlar la mente”, etc), su nombre y en qué planeta viven. 

-- Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- 
-- y su material es “uru”, se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes 
-- y reducir a la mitad la cantidad de dichos personajes. Por ejemplo si tenemos un universo en el cual existen ironMan, 
-- drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. Si además de los 4 personajes 
-- estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.
-- Punto 1: (2 puntos) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.
data Guantalete = Guantalete {
  material :: String,
  gemas :: [Gema]
} deriving (Show, Eq, Ord)

data Personaje = Personaje {
  edad :: Number,
  energia :: Number,
  habilidades :: [String],
  nombre :: String,
  planeta :: String
} deriving (Show, Eq, Ord)

data Universo = Universo{
  personajes :: [Personaje]
} deriving (Show, Eq, Ord)

chasquido :: Universo -> Guantalete -> [Personaje]
chasquido universo guantalete 
  | guantaleteCompleto guantalete = take (div (cantidadPersonajesUniverso universo) 2) (personajes universo)   
  | otherwise                     = personajes universo

guantaleteCompleto :: Guantalete -> Bool
guantaleteCompleto guantalete = (cantidadGemas guantalete == 6) && (material guantalete == "uru")

cantidadGemas :: Guantalete -> Number
cantidadGemas = length.gemas

cantidadPersonajesUniverso :: Universo -> Number
cantidadPersonajesUniverso = length.personajes

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
aptoPendex :: Universo -> Bool
aptoPendex = any ((<45).edad).personajes

-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso = sum.energiaPorPersonajes

energiaPorPersonajes :: Universo -> [Number]
energiaPorPersonajes universo = map (energia) (integrantesConMasDeUnaHabilidad (personajes universo))

integrantesConMasDeUnaHabilidad :: [Personaje] -> [Personaje]
integrantesConMasDeUnaHabilidad = filter ((> 1).length.habilidades) 

-- Segunda parte
-- A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, el poseedor puede utilizar el poder del mismo contra un enemigo,
-- es decir que puede aplicar el poder de cada gema sobre el enemigo. Las gemas del infinito fueron originalmente parte de la entidad primordial
-- llamada Némesis, un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir como la única conciencia en el universo.

-- Al morir, dio paso al universo actual, y el núcleo de su ser reencarnó en las seis gemas: 
--    * La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
--    * El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía. 
--    * El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.


-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada.
type Gema = Personaje -> Personaje
mente :: Number -> Gema
mente  = restarEnergia

alma :: String -> Gema
alma habilidad personaje = restarEnergia 10 personaje{
  habilidades = filter (/=habilidad) $ habilidades personaje
}

espacio :: String -> Gema
espacio nuevoPlaneta personaje = restarEnergia 20 personaje{
  planeta = nuevoPlaneta
}
--    * El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
poder :: Gema
poder  personaje = restarEnergia (energia personaje) personaje{
  habilidades = quitarHabilidades (habilidades personaje)
}

restarEnergia :: Number -> Gema
restarEnergia energiaARestar personaje = personaje{
  energia = energia personaje - energiaARestar
}

quitarHabilidades :: [String] -> [String]
quitarHabilidades habilidades 
  | length habilidades >= 2  = []
  | otherwise                 = habilidades

--    * El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años.
--       Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, 
--       le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
tiempo :: Gema
tiempo  personaje = restarEnergia (50) personaje{
  edad = min 18 $ div (edad personaje) 2
}

--    * La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.
loca :: Gema -> Gema
loca gema = gema.gema

-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y
-- la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
guantaleteGoma = Guantalete{
  material = "Goma",
  gemas = [tiempo,alma "usar Mjolnir", loca (alma "programación en Haskell")]
}

-- Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta 
-- el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.
utilizar :: [Gema] -> Gema
utilizar gemas personaje = foldr ($) personaje gemas

-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene 
-- la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
gemaMasPoderosa :: Personaje -> Guantalete -> Gema
gemaMasPoderosa personaje guantalete = gemaMasPoderosaDe personaje $ gemas guantalete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe  _ [gema] = gema
gemaMasPoderosaDe personaje (gemaPrimera:gemaSegunda:gemas)
  | (energia.gemaPrimera) personaje > (energia.gemaSegunda) personaje = gemaMasPoderosaDe personaje (gemaPrimera:gemas) 
  | otherwise                                                             = gemaMasPoderosaDe personaje (gemaSegunda:gemas) 

-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantalete
guanteleteDeLocos = Guantalete "vesconite" (infinitasGemas tiempo)

-- Y la función 
usoLasTresPrimerasGemas :: Guantalete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantalete = (utilizar . take 3. gemas) guantalete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher

