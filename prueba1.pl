masgrande(elefante,caballo).
masgrande(caballo,perro).

%A es mucho mas grande que C si A es mas grande que B y B mas grande que C.
muchomasgrande(A,C):-
    masgrande(A,B),
    masgrande(B,C).

persona(Nombre, Apellido, Gusto, Edad, [Nombre, Apellido, Gusto, Edad]):-
    string(Nombre),
    string(Apellido),
    string(Gusto),
    integer(Edad),
    Edad>0.

segundoElemento([_,S|_], S).

potencia(Base, 1, Base) :-
    number(Base).

potencia(Base, 0, 1):-
    number(Base).

potencia(Base, Exp, R) :-
    number(Base),
    number(Exp),
    Exp1 is Exp-1, potencia(Base, Exp1, R2),
    R is R2 * Base.