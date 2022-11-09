%1. pertenece

%Cuando llega al caso base donde la cabeza es el elemento buscado, hace un corte.
pertenece([H|_], H):-
	!.
%Por el contrario, si no está en la cabeza sigue buscando en el resto de la lista.
pertenece([_|T], H):-
	pertenece(T, H).

%2. agregar: Toma un elemento y lo agrega a la lista.
%Toma el elemento y lo junta con la lista.
%Recordar que una lista siempre se compone de:
%lista = null | elemento X lista.
agregar(E, Lista, [E|Lista]).

%3. eliminar: Elimina los elementos de una lista dado un elemento.
%Caso base: Sea el elemento que sea, si se topa con una lista vacía,
%devuelve una lista vacía.
eliminar(_, [], []).
%Primer caso: Ocurre que el primer elemento es el elemento que se quiere borrar,
%por lo que se salta la cabeza de la lista y sigue verificando el resto.
eliminar(E, [E|T], Res):-
	eliminar(E, T, Res).
%Resto de casos: Ocurre cuando la cabeza de la lista no es el elemento que
%se quiere borrar, por lo tanto lo acopla al resto de solucion.
eliminar(E, [H|T], [H|Res2]):-
	eliminar(E, T, Res2).

%4. cons: Igual que agregar elemento.
cons([], L, L).
cons(E, Lista, [E|Lista]).

%5. conc: Acopla dos listas.
%Caso base: Acoplar una lista vacía y una lista cualquiera es la misma lista.
conc([], L, L).
conc(L, [], L).
%Resto de casos: Se agrega el primer elemento de la lista con el resto de la lista modificada.
conc([H|T], [H2|T2], L):-
	conc(T, [H2|T2], L2),
	append([H], L2, L).

%6. filtrar
%Caso base: Sea la condición que sea, si hay una lista vacía, devuelve lista vacía.
filtrar(_, [], []).
%Si la condición se cumple con la cabeza de la lista, 
%se incluye en la solución y se explora el resto de la lista.
filtrar(Cond, [H|T], [H|Res2]):-
	call(Cond, H),
	filtrar(Cond, T, Res2).
%Resto de casos: Si no se cumple la condición con la cabeza de la lista,
%se salta y se sigue con el resto de la lista.
filtrar(Cond, [_|T], Res):-
	filtrar(Cond, T, Res).

%7. inversa: Revierte una lista.
%Caso base: La inversa de una lista vacía es lista vacía.
inversa([], []).

%Resto de casos: Se concatena el resto de la lista invertida con el primer elemento de la lista ingresada.
inversa([H|T], L2):-
	inversa(T, L3),
	append(L3, [H], L2).

%8. primer
primer([H|_], H).

%9. resto
resto([_|T], T).

%10. maximo: Devuelve el elemento mayor de una lista.
max(X, Y, X):-
	X >= Y.
max(X, Y, Y):-
	Y >= X.

%Caso base: El máximo de una lista con un elemento es el mismo elemento.
maximo([E], E).
%Resto de casos: Se saca el máximo de la cabeza de la lista ingresada y el resto
%de la lista.
maximo([H|T], Z):-
	maximo(T, Y),
	max(H, Y, Z).

%11. minimo: Devuelve el minimo de una lista.
min(X, Y, X):-
	X =< Y.
min(X, Y, Y):-
	Y =< X.

%Caso base: El mínimo de una lista de un elemento es sí mismo.
minimo([E], E).
%Se saca el mínimo entre la cabeza y el resto de la lista.
minimo([H|T], M):-
	minimo(T, Y),
	min(H, Y, M).

%12. promedio
%Caso base: Al llegar a una lista vacía, su suma es 0.
suma([], 0).
%Demás casos: Se suma la cabeza de la lista con la suma del resto de la lista.
suma([H|T], N):-
	suma(T, N2),
	N is H + N2.

%Se suman los elementos de la lista y se divide en su longitud (cantidad de elementos), así sacando
%el promedio.
promedio(Lista, Prom):-
	suma(Lista, N),
	longitud(Lista, L),
	Prom is N/L.

%13. eliminarRepetidos: Elimina los repetidos de una lista.
%Caso base: No hay repetidos en una lista vacía.
eliminarRepetidos([], []).
%Resto de casos: Se eliminan los elementos que sean iguales a la cabeza de la lista,
%y se acopla la cabeza de la lista con el resto de la lista modificada.
eliminarRepetidos([H|T], L3):-
	eliminar(H, T, Res),
	eliminarRepetidos(Res, L2),
	append([H], L2, L3).

%14. agregarFinal: Agrega un elemento al final de la lista.
%Caso base: Al agregar un elemento a una lista vacía, devolverá una lista
%de un solo elemento.
agregarFinal(E, [], [E]).
%Resto de casos: Mientras la lista no sea vacía, seguirá recorriendo y acoplando a la cabeza de la lista.
agregarFinal(E, [H|T], [H|Res2]):-
	agregarFinal(E, T, Res2).

%15. longitud: Determina la longitud de una lista.
%Caso base: La longitud de una lista vacía es 0.
longitud([], 0).
%Resto de casos: La longitud de una lista será la longitud del resto de la lista + 1.
longitud([_|T], L):-
	longitud(T, L2),
	L is L2 + 1.

%16. contarRepetidos: Dado un número se devuelve la cantidad de veces que se repite
%el número en la lista.
%Caso base: Sea cual sea el número, si la lista es vacía, el número tiene 0 repeticiones.
contarRepetidos(_, [], 0).
%Caso principal: El elemento es igual a la cabeza de la lista, por lo que se sigue recorriendo
%el resto de la lista y se determina que la cantidad de repeticiones es la cantidad de veces
%que se repite en el resto de la lista + 1 (porque no se había considerado la cabeza).
contarRepetidos(E, [E|T], N):-
	contarRepetidos(E, T, N2),
	N is N2 + 1.
%Resto de casos: Donde el elemento no es igual a la cabeza de la lista, por lo que sigue
%avanzando con el resto de la lista y el contador sigue igual al no haber repetición.
contarRepetidos(E, [_|T], N):-
	contarRepetidos(E, T, N).

%17. repetidosLista: Devuelve una lista de una lista compuesta de un elemento y la cantidad
%de repeticiones.
%Se eliminan los repetidos y se llama a repetidosAux.
repetidosLista(L1, ListaFinal):-
	eliminarRepetidos(L1, LSR),
	repetidosAux(L1, LSR, ListaFinal).
	
%Caso base
repetidosAux(_, [], []).
%Resto de casos: Se agrega [Número, Repeticiones] a la lista general y va desplazandose
%a lo largo de la lista sin repeticiones de la lista ingresada. Una vez haya terminado con
%la lista sin repeticiones, se devuelve la lista general.
repetidosAux(L1, [H2|T2], Res):-
	contarRepetidos(H2, L1, X),
	repetidosAux(L1, T2, Res2),
	append([[H2, X]], Res2, Res).

%18. reemplazar: Dado un elemento, elemento nuevo y una lista, reemplaza los elementos
%de la lista donde el elemento sea igual al elemento ingresado y lo cambia por el
%elemento nuevo.

%Caso base: Dado cualquier elemento y elemento nuevo, si hay una lista vacía, se devuelve
%una lista vacía.
reemplazar(_, _, [], []).
%Si el elemento es igual a la cabeza de la lista, se cambia por el nuevo elemento
%y se sigue con el resto.
reemplazar(E, New, [E|T], [New|Res]):-
	reemplazar(E, New, T, Res).
%Si el elemento es distinto a la cabeza de la lista, se toma la cabeza y se cambia
%el resto de la lista.
reemplazar(E, New, [H|T], [H|Res]):-
	reemplazar(E, New, T, Res).

%19. subconjunto: Determina si un conjunto C1 es subconjunto de C2.

%Caso base: Vacío es subconjunto de C2.
subconjunto([], _).
%Resto de casos: Si el primer elemento de C1 es miembro de C2, se avanza
%con el resto de la primera lista y se vuelve a analizar su pertenencia.
subconjunto([H|T], [H2|T2]):-
	pertenece([H2|T2], H),
	subconjunto(T, [H2|T2]).

%20. disjunto: Determina si C2 no tiene ningún elemento de C1.
%Caso base: Vacío es disjunto de C2.
disjunto([], _).
%Resto de casos: La cabeza de C1 no debe pertenecer a C2, y sigue con el resto de C1.
disjunto([H|T], [H2|T2]):-
	not(pertenece([H2|T2], H)),
	disjunto(T, [H2|T2]).

%21. diferencia: C1 - C2
%Ejemplo: [1, 2, 3] - [1, 4] = [2, 3]

%Predicado que elimina solamente un elemento de la lista.
eliminarUno(_, [], []).
eliminarUno(E, [E|T], T):-
	!.
eliminarUno(E, [H|T], [H|Res]):-
	eliminarUno(E, T, Res).

%Casos base: 
diferencia([], L, L).
diferencia(L, [], L).
%Elimina el primer elemento de L2 en L1, actualiza L1 y vuelve a seguir con ambas listas.
diferencia([H|T], [H2|T2], Res3):-
	eliminarUno(H2, [H|T], Res2),
	diferencia(Res2, T2, Res3).

%22. union: Une dos listas (lo mismo que conc).
myunion([], L, L).
myunion([H|T], [H2|T2], L):-
	myunion(T, [H2|T2], L2),
	append([H], L2, L).

%23. interseccion: Devuelve los elementos en común entre dos conjuntos.
%Casos base: La intersección de cualquier cosa con vacío es vacío.
interseccion([], [], []).
interseccion(_, [], []).
interseccion([], _, []).
%Resto de casos: Si la cabeza de L1 pertenece en L2,
%se borra un elemento de L2 y se vuelve a hacer interseccion
%entre el resto de L1 y la lista resultante L2.
%Luego, acopla la cabeza de L1 y el resultado de las intersecciones.
interseccion([H|T], [H2|T2], Res):-
	pertenece([H2|T2], H),
	eliminarUno(H, [H2|T2], L),
	interseccion(T, L, L1),
	append([H], L1, Res).

%Si la cabeza no está en L2, se sigue haciendo intersección hasta llegar a casos base.
interseccion([_|T], [H2|T2], Res):-
	interseccion(T, [H2|T2], Res).

%24. union sin repeticion: Une dos listas sin repeticiones.
%Une dos listas y luego borra los repetidos.
unionSR(L1, L2, Res):-
	myunion(L1, L2, L3),
	eliminarRepetidos(L3, Res).

%25. unionOrdenada: Une dos conjuntos, elimina los repetidos y devuelve la lista ordenada de menor a mayor.
unionOrdenada(L1, L2, Res):-
	unionSR(L1, L2, L3),
	unionOrdenadaAux(L3, Res).

%Caso base: Una lista de un elemento ya está ordenada.
unionOrdenadaAux([H], [H]).
%Resto: El mínimo de la lista se guarda en X, se elimina un X de la lista (siendo esto igual a Res2), 
%se hace recursión con Res2 y su resultado se almacena en Res3.
%Se acopla el mínimo (X) con el resto de la lista ordenada.
unionOrdenadaAux([H|T], Res):-
	minimo([H|T], X),
	eliminarUno(X, [H|T], Res2),
	unionOrdenadaAux(Res2, Res3),
	append([X], Res3, Res).
	
meta(estado(d, d, d, d)).
direccion(i, d).
direccion(d, i).

/*  Movimientos permitidos */
%Granjero puede ir de izq a der o viceversa.
movimiento(estado(M, CB, LB, CL), estado(M1, CB, LB, CL)):- 
	direccion(M, M1).

%Granjero puede ir de izq a der con lobo.
movimiento(estado(M, CB, M, CL), estado(M1, CB, M1, CL)):- 
	direccion(M, M1).

%Granjero puede ir de izq a der con cabra.
movimiento(estado(M, M, LB, CL), estado(M1, M1, LB, CL)):- 
	direccion(M, M1).

%Granjero puede ir de izq a der con col.
movimiento(estado(M, CB, LB, M), estado(M1, CB, LB, M1)):- 
	direccion(M, M1).

/*  Movimientos prohibidos */

%La cabra y el lobo no puede estar juntos.
prohibido(estado(M, M1, M1, _)):- 
	direccion(M, M1).

%La cabra y la col no pueden estar juntos.
prohibido(estado(M, M1, _, M1)):- 
	direccion(M, M1).

/*  Verifica si un elemento pertenece a una lista */
miembro(X, [X|_]):- 
	!.
miembro(X, [_|L]):- 
	miembro(X, L).

/*  Solución */ 
%Caso base: Ambas listas son iguales y se cumple la meta de que todos estén a la derecha.
solucion([E | L], [E | L]) :- 
	meta(E).
solucion([E | L], LS):- 
	%Se busca un posible movimiento
	movimiento(E, EP), 
	%El movimiento no debe estar prohibido.
	not(prohibido(EP)), 
	%El movimiento no debe estar en L.
	not(miembro(EP, L)), 
	%Se agrega el movimiento a la lista de solución.
	solucion([EP, E | L], LS).