module Library where
import PdePreludat
import GHC.Num (Num)
import GHC.RTS.Flags (MiscFlags(disableDelayedOsMemoryReturn))
import GHC.Float (RealFloat(isNegativeZero), Floating (sqrt))

-- Funcion incial del test (vino por defecto)

doble :: Number -> Number 
doble numero = 2 * numero 

---------------------------------------------------------------------------------------------------------------------

-- &&  -------------------------------------- Practica 1: Primeros ejercicios ---------------------------------------------
{- -- * Inicio  comentario practica 1  


-- 1. Definir la función esMultiploDeTres, que devuelve True si un número es múltiplo de 3 

esMultiploDeTres :: Number -> Bool  
esMultiploDeTres numero = mod numero 3 == 0

-- Punto 2:Definir la función esMultiploDe, que devuelve True si el segundo es múltiplo del primero.

esMultiploDe ::Number -> Number -> Bool 
esMultiploDe numero1 numero2 = mod numero2 numero1 == 0 

-- Punto 3. Definir la función cubo, devuelve el cubo de un número.

cubo :: Number -> Number 
cubo numero = numero * numero * numero 

-- Punto 4. Definir la función area, devuelve el área de un rectángulo a partir de su base y su altura.

area :: Number -> Number -> Number 
area base altura = (base * altura)/2

-- Punto 5. Definir la funcion esBisiesto, indica si un anio es bisiesto. 

esBisiesto :: Number -> Bool 
esBisiesto anio
    | esMultiploDe 400 anio = True
    | esMultiploDe 4 anio && not (esMultiploDe 100 anio) = True
    | otherwise = False

-- Punto 6. Definir la función celsiusToFahr, pasa una temperatura en grados Celsius a grados Fahrenheit

celsiusToFahr :: Number -> Number 
celsiusToFahr gradosCelsius = (gradosCelsius * 1.8) + 32

-- Punto 7. Definir la función fahrToCelsius, la inversa de la anterior.

fahrToCelsius :: Number -> Number 
fahrToCelsius gradosFahr = (gradosFahr - 32) * 0.55556

-- Punto 8. Definir la función haceFrio , indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius.  

haceFrio :: Number -> Bool 
haceFrio gradosFahr = fahrToCelsius gradosFahr < 8 


-- Punto 9. Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números. 
-- ? Nota: la funcion rem es del tipo: Number -> Number -> Number. Calcula el resto de la division entre dos numeros 
mcd :: Number -> Number -> Number 
mcd numero1 0 = numero1 
mcd numero1 numero2 = mcd numero2 (numero1 `rem` numero2)


mcm :: Number -> Number -> Number 
mcm numero1 numero2 = (numero1 * numero2)/ (mcd numero1 numero2)


-- Punto 10. 

-- a. 

dispersion :: Number -> Number -> Number -> Number 
dispersion dia1 dia2 dia3 = max (max dia1 dia2) dia3 - min (min dia1 dia2) dia3

-- b. 

diasParejos :: Number -> Number-> Number -> Bool 
diasParejos temperaturaDia1 temperaturaDia2 temperaturaDia3 = dispersion temperaturaDia1 temperaturaDia2 temperaturaDia3  < 30

diasLocos :: Number -> Number-> Number -> Bool 
diasLocos temperaturaDia1 temperaturaDia2 temperaturaDia3 = dispersion temperaturaDia1 temperaturaDia2 temperaturaDia3  > 100

diasNormales :: Number -> Number-> Number -> Bool 
diasNormales temperaturaDia1 temperaturaDia2 temperaturaDia3 | diasParejos temperaturaDia1 temperaturaDia2 temperaturaDia3 = False 
                                                             | diasLocos temperaturaDia1 temperaturaDia2 temperaturaDia3 = False 
                                                             | otherwise = True
-- * Fin comentario practica  1                                                            
-} 

-- & ------------------------------------- Practica 2: Aplicacion Parcial y Composicion ---------------------------------------------

-- 1. Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1.

siguiente :: Number -> Number 
siguiente = (+1)

-- 2. Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número. 

mitad :: Number -> Number 
mitad = (/2)

-- 3. Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa.  

inversa :: Number -> Number
inversa = (1/)

-- 4. Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo. 

triple :: Number -> Number
triple = (3*)

-- 5. Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario.

esNumeroPositivo :: Number -> Bool
esNumeroPositivo = (>0)

-- 6. Resolver la función del ejercicio 2 de la guía anterior (Practica 1: Primeros Ejercicios) esMultiploDe, utilizando  aplicación parcial y composición. 

esMultiploDeVersion2 :: Number -> Number -> Bool 
esMultiploDeVersion2 numero1 numero2 = (mod numero2)numero1  == 0

-- 7. Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición. 


esBisiestoVersion2 :: Number -> Bool 
--esBisiestoVersion2 anio =  (esMultiploDeVersion2 400) anio ||((esMultiploDeVersion2 4) anio  && not((esMultiploDeVersion2 100) anio) ) 
-- No me gusta mucho, voy a intentar mejorarla:   
esBisiestoVersion2 anio = (esMultiploDeVersion2 400) anio || ((esMultiploDeVersion2 4) anio && (not . esMultiploDeVersion2 100)anio)

-- 8. Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada.  

inversaRaizCuadrada :: Number -> Number 
inversaRaizCuadrada  = (inversa . PdePreludat.sqrt ) 

-- 9. Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n

incrementMCuadradoN :: Number -> Number -> Number 
incrementMCuadradoN incremento numeroaelevar = ((incremento +). (^2))numeroaelevar

-- 10. . Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par.

esResultadoPar :: Number -> Number -> Bool 
esResultadoPar base exponente = (even . (^ exponente) )base