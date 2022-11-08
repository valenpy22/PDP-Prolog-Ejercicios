%1.
%pertenece(Elemento, Lista)

%Una vez encuentra el elemento, realiza una operación de corte
%ya que solo es necesario saber si hay al menos un elemento.
pertenece(E, [E|_]):-
	!.
pertenece(E, [_|T]):-
	pertenece(E, T).

%2. agregarInicio(Elemento, Lista, ListaResultante)
agregarInicio(E, L, [E|L]).

%3. eliminarElemento(Elemento, Lista, ListaResultante)


%4. cons(Elemento, Lista, ListaResultante)
cons(E, [], [E]).
cons(E, L, [E|L]).

%5. conc(Lista1, Lista2, ListaResultante)
%Concatena el resto de la primera lista con la segunda lista,
%para luego unir la cabeza de la primera lista y lo de arriba.
%Caso base: Cuando se llegue a vacío.
conc([], L, L).
conc([H1|T1], [H2|T2], Res):-
	conc(T1, [H2|T2], R2),
	cons(H1, R2, Res).

%6. filtrar(Condicion, Lista, ListaResultante)

%Caso base: Vacío.
filtrar(_, [], []).
%Caso donde no se cumple la condición, sigue con el resto de la lista.
filtrar(Condicion, [H|T], L):-
	not(call(Condicion, H)),
	filtrar(Condicion, T, L).
%Caso donde sí se cumple la condición. Toma el primer elemento de la lista
%y se acopla a los elementos que también cumplan la condición al verificar
%el resto de la lista.
filtrar(Condicion, [H|T], L):-
	call(Condicion, H),
	filtrar(Condicion, T, L2),
	cons(H, L2, L).

%7. inversa(Lista, ListaInversa)
inversa([], []).
inversa([H|T], Y):-
	inversa(T, Y2),
	append(Y2, [H], Y).

%8. primerElemento(Lista, Elemento).
primerElemento([H|_], H).

%9. restoLista(Lista, Resto).
restoLista([_|T], T).

%10. maximo(Lista, Max)

max(A, B, A):-
	A >= B.
max(A, B, B):-
	A < B.

%Caso base: El máximo de un vector de 1 elemento, es el mismo elemento.
maximo([X], X).
maximo([H|T], M):-
	maximo(T, M1),
	max(M1, H, M).

%11. minimo(Lista, Min)

min(A, B, A):-
	A =< B.
min(A, B, B):-
	A > B.

minimo([X], X).
minimo([H|T], Min):-
	minimo(T, M1),
	min(M1, H, Min).

%12. promedio(Lista, Promedio)

%Dividir en subproblemas.
suma([], 0).
suma([H|T], Suma):-
	suma(T, Suma2),
	Suma is Suma2 + H.

promedio([H|T], Prom):-
	suma([H|T], Suma),
	longitud([H|T], Longitud),
	Prom is Suma/Longitud.

%13. eliminarRepetidos(Lista, ListaResultante)

%14. agregarFinal(Elemento, Lista, ListaResultante)
agregarFinal(E, [], [E]).
agregarFinal(E, [H|T], ListaResultante):-
	agregarFinal(E, T, Lista2),
	append([H], Lista2, ListaResultante).

%15. longitud(Lista, Long).

longitud([], 0).
longitud([_|T], Largo):-
	longitud(T, Largo2),
	Largo is Largo2+1.

%16. contarRepetidos(Elemento, Lista, Repeticiones)

%17. reemplazar(Elemento, ElemNuevo, Lista, ListaResultante)