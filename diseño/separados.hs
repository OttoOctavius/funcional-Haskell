{-# LANGUAGE FlexibleContexts #-} 
--No existe extension que soporte este tipo de definiciones
--las unicas formas de mostrar las definiciones de funciones son de sus tipos
-- y son :info, :browse en los interactivos.
data Figura = Rectangulo Float Float | Circunferencia Float

--sus metodos heredados e implementados
perimetro self@(Rectangulo a b) = 2*a+2*b 
area self@(Rectangulo a b) = a*b 
second self@(Rectangulo a b) = b


perimetro self@(Circunferencia radio) = radio * 2 *pi
first self@(Circunferencia radio) = radio
area self@(Circunferencia radio) = radio * radio*pi


first _ = error "Sin implementar"
second _ = error "Sin implementar"