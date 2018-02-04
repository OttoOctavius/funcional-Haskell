{-
class abstract Figura{
 public float area();
 public float perimetro();
}

class Rectangulo : Figura{
 private float a,b;
 public Rectangulo(float a,float b){
  this.a = a;
  this.b = b;
 }
 public float area(){ return this.a * this.b;}
 public float perimetro(){ return this.a * this.b;}
 public float second(){ return b;}
}

class Circunferencia: Figura{
 private float radio;
 public Rectangulo(float radio){
  this.radio = radio;
 }
 public float area(){ return pi*this.radio; * this.radio;;}
 public float perimetro(){ return this.radio * 2 *pi;}
 public float first(){ return radio;}
}
-}

--Clase abstracta no se instancia
data Figura = Rectangulo Float Float | Circunferencia Float

--sus metodos heredados e implementados
perimetro self@(Rectangulo a b) = 2*a+2*b 
perimetro self@(Circunferencia radio) = radio * 2 *pi

area self@(Rectangulo a b) = a*b 
area self@(Circunferencia radio) = radio * radio*pi

second self@(Rectangulo a b) = b
second _ = error "Sin implementar"

first self@(Circunferencia radio) = radio
first _ = error "Sin implementar"
--