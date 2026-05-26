module Spec where
import PdePreludat
import Library
import Test.Hspec
import Text.Printf (formatChar)
    
correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2


---------------------------------------------- Test Practica 1 ---------------------------------

-- Punto 1. 

  describe "esMultiploDeTres" $ do 
    it "9 es multiplo de 3?" $ do 
      esMultiploDeTres 9 `shouldBe` True

-- Punto 2. 

  describe "esMultiploDe" $ do
    it " 12 es multiplo de 3?" $ do 
      esMultiploDe 3 12 `shouldBe` True


-- Punto 3. 

  describe "cubo" $ do
    it " el cubo de 5 es 125" $ do 
      cubo 5 `shouldBe` 125

-- Punto 4. 

  describe "area" $ do 
    it "el area de un rectanngulo de base 5 y altura 20 es 50 " $ do 
      area 5 20 `shouldBe`50

-- Punto 5. 

  describe "esBisiesto" $ do 
    it "El anio 2024 fue bisiesto" $ do 
      esBisiesto 2024 `shouldBe` True

-- Punto 6. 

  describe "celsiusToFahr" $ do 
    it "35 grados celsius equivalen a 95 grados fahrenheit" $ do 
      celsiusToFahr 35 `shouldBe` 95

-- Punto 7. 

  describe "fahrToCelsius" $ do 
    it "32 grados fahrenheit equivalen a 0 grados celsius" $ do 
      fahrToCelsius 32 `shouldBe` 0

-- Punto 8. 

  describe "haceFrio" $ do 
    it "hace frio porque hacen 7 grados celsius" $ do 
      haceFrio 44.6 `shouldBe` True

-- Punto 9. 

  describe "mcm" $ do 
    it "el minimo comun multiplo entre 8 y 12 es 24 " $ do 
      mcm 8 12 `shouldBe` 24

-- Punto 10. 
-- a
  describe "dispersion" $ do 
    it " la dispersion de los dias: 1 (322cm), 2 (283cm) y 3 (294cm) es 39"$ do 
      dispersion 322 283 294 `shouldBe` 39

-- b. 
  describe "diasParejos" $ do 
    it " Son dias parejos?" $ do 
      diasParejos 322 283 294 `shouldBe` False 

  describe "diasLocos" $ do 
    it "son dias Locos? " $ do 
      diasLocos 322 283 294 `shouldBe` False 
  
  describe "diasNormales" $ do 
    it " son dias Normales? " $ do 
      diasNormales 322 283 294 `shouldBe` True