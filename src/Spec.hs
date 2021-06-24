module Spec where
import PdePreludat
import Library
import Test.Hspec

guantaleteCon6GemasConUru = Guantalete{
  material = "uru",
  gemas = ["Alma", "Poder", "Mente","Espacio", "Tiempo","Realidad"]
}

guantaleteSin6GemasConUru = Guantalete{
  material = "uru",
  gemas = ["Alma", "Poder", "Mente","Espacio"]
}

guantaleteCon6GemasSinUru = Guantalete{
  material = "lara",
  gemas = ["Alma", "Poder", "Mente","Espacio", "Tiempo","Realidad"]
}

guantaleteSin6GemasSinUru = Guantalete{
  material = "lara",
  gemas = ["Alma", "Poder", "Mente","Espacio"]
}

ironMan = Personaje {
  edad = 34,
  energia = 456,
  habilidades = ["Volar", "Inteligencia superior", "Tirar Rayos lazer"],
  nombre = "Tony Stark",
  planeta = "Tierra"
}

drStrange = Personaje {
  edad = 40,
  energia = 345,
  habilidades = ["Volar", "Magia", "Lucha con poderes"],
  nombre = "Javier Strange",
  planeta = "Tierra"
}

groot = Personaje {
  edad = 121,
  energia = 123,
  habilidades = ["Fuerza", "Recomposicion", "Habla poco"],
  nombre = "Groot",
  planeta = "Otro planeta"
}

wolverine = Personaje {
  edad = 342,
  energia = 556,
  habilidades = ["Recomposicion", "Olfato superior", "Garras de adamantium", "Fuerza superior"],
  nombre = "Logan",
  planeta = "Tierra"
}

viudaNegra = Personaje {
  edad = 28,
  energia = 98,
  habilidades = ["Astucia", "Experta en artes marciales", "Agilidad"],
  nombre = "Natasha Romanoff",
  planeta = "Tierra"
}

universoMarvel = Universo{
  personajes = [ironMan,drStrange,groot,wolverine,viudaNegra]
}

universoMarvelMitad = Universo{
  personajes = [ironMan,drStrange]
}

universoMarvelNoPendex = Universo{
  personajes = [wolverine,groot]
}

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Thanos - Punto 1" $ do
    it "Si el guantalete con 6 gemas y es de uru entonces debe devolver la mitad de personajes del universo" $ do
      chasquido universoMarvel guantaleteCon6GemasConUru `shouldBe` personajes universoMarvelMitad
    it "Si el guantalete con 6 gemas y no es de uru entonces debe devolver los mismos personajes del universo" $ do
      chasquido universoMarvel guantaleteCon6GemasSinUru `shouldBe` personajes universoMarvel
    it "Si el guantalete sin 6 gemas y es de uru entonces debe devolver los mismos personajes del universo" $ do
      chasquido universoMarvel guantaleteSin6GemasConUru `shouldBe` personajes universoMarvel
    it "Si el guantalete sin 6 gemas y no es de uru entonces debe devolver los mismos personajes del universo" $ do
      chasquido universoMarvel guantaleteSin6GemasSinUru `shouldBe` personajes universoMarvel
  describe "Test de Thanos - Punto 2" $ do
    it "Como ironMan tiene menos de 45 años satiface la funcion aptoPendex" $ do
      universoMarvel `shouldSatisfy` aptoPendex
    it "Como no hay nadie con menos de 45 años no satiface la funcion aptoPendex" $ do
      universoMarvelNoPendex `shouldNotSatisfy` aptoPendex 
