type wielomian

val oblicz : wielomian -> float -> float 
(** [oblicz w x] oblicza wartość wielomianu [w] w punkcie [x] *) 
 
val suma : wielomian -> wielomian -> wielomian 
(** Suma wielomianów *) 
 
val iloczyn : wielomian -> wielomian -> wielomian 
(** Iloczyn wielomianów *) 
 
val pochodna : wielomian -> wielomian 
(** Pochodna wielomianu *) 
 
val stopien : wielomian -> int 
(** Zwraca stopień wielomianu (czyli najwyższą potęgę argumentu, przy której wykładnik jest różny od 0.) *) 
 
val calka : wielomian -> wielomian 
(** Calka wielomianu. [calka w] ma wyraz wolny równy 0. *) 
