module Spec where
import PdePreludat
import Library
import Test.Hspec
import Text.Printf (formatChar)
import GHC.RTS.Flags (ProfFlags(automaticEraIncrement))
    
correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2


-- & -------------------------------------------- Test Practica 1 ---------------------------------
{-

 -- * Inicio  comentario practica 1 

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

-- * Fin comentario practica  1  
-}

-- & -------------------------------------------- Test Practica 2 ---------------------------------

-- 1. 

  describe "siguiente" $ do 
    it "el numero siguiente a 4 es 5" $ do 
      siguiente 4 `shouldBe` 5 

-- 2. 

  describe "mitad" $ do 
    it "la mitad de 6 es 3" $ do 
      mitad 6 `shouldBe` 3

-- 3.
  
  describe "inversa" $ do 
    it " la inversa de 4 es 0.25" $ do 
      inversa 4 `shouldBe` 0.25

-- 4. 
  
  describe "tripe" $ do 
    it " el triple de 3 es 9 " $ do
      triple 3 `shouldBe` 9

-- 5. 

  describe "esNumeroPositivo" $ do
    it "el numero -5 no es positivo" $ do
      esNumeroPositivo (-5) `shouldBe` False

-- 6. 

  describe "esMultiploDeVersion2" $ do
    it " 12 es multiplo de 3?" $ do 
      esMultiploDeVersion2 3 12 `shouldBe` True

-- 7. 

  describe "esBisiestoVersion2" $ do 
    it "el anio 2024 fue bisiesto" $ do
      esBisiestoVersion2 2024 `shouldBe` True

-- 8.

  describe "inversaRaizCuadrada" $ do 
    it "la inversa de la raiz cuadrada de 4 es 0.5" $ do
      inversaRaizCuadrada 4 `shouldBe` 0.5

-- 9.

  describe "incrementMCuadradoN" $ do
    it " si incremento 2 al cuadrado de 5 el resultado es 27" $ do
      incrementMCuadradoN  2 5 `shouldBe` 27

-- 10.

  describe "esResultadoPar" $ do
    it "4 elevado al cubo es par" $ do 
      esResultadoPar 4 3 `shouldBe` True
