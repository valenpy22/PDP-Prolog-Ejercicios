%1. Considerar la base de conocimiento constituida en este caso solo por hechos:
padre(carlos, julio).
padre(elizabeth, julio).
padre(karina,julio).
padre(carlitos,carlos).
padre(daniel,carlos).
padre(mariela,carlos).
padre(sofia, elizabeth).
padre(diego, karina).
padre(jose, karina).

%a. Implementar una regla en Prolog para determinar si dos personas son hermanos.
hermano(X, Y):-
    padre(X, A),
    padre(Y, A),
    X \== Y.

%b. Implementar una regla en Prolog para determinar si dos personas son primos.
primo(X, Y):-
    padre(X, A),
    padre(Y, B),
    hermano(A, B),
    X \== Y.

%2. Considerar el siguiente código en Prolog:

y(0, 0).
y(N, OUT):-
    N1 is N-1,
    y(N1, OUT1),
    OUT is N + OUT1.

%3. ¿Qué aseveraciones son correctas con respecto a los hechos en prolog?
%Están formados por una relación y uno o más constantes.
%Son un caso especial de cláusula de Horn.
%Son siempre verdaderos.

%4. Indique la alternativa que no es correcta con respecto a la siguiente línea de código:
/*
esArtefactoElectrico(Articulo, TIPO, [20, 30, 45])
El primer y segundo parámetro son variables al iniciar con una mayúscula, el tercer parámetro es una lista.
*/

%5. 

buscador(ask_com).
buscador(duckDuckGo).
buscador(google).
buscador(yahoo).
buscador(yandex).
buscador(trovator).

metabuscador(brainBoot).
metabuscador(gogpile).
metabuscador(ixquick).
metabuscador(webcrawler).

subbuscador(startpage).
subbuscador(lycos).
subbuscador(a9_com).

ocupa(gogpile, ask_com).
ocupa(gogpile, bing).
ocupa(gogpile, google).
ocupa(gogpile, yahoo).
ocupa(ixquick, yahoo).
ocupa(webcrawler, ask_com).
ocupa(webcrawler, bing).
ocupa(webcrawler, google).
ocupa(webcrawler, yahoo).
ocupa(startpage, google).
ocupa(lycos, ask_com).
ocupa(a9_com, bing).

subMeta(Subbuscador, Metabuscador):-
    ocupa(Subbuscador, X),
    ocupa(Metabuscador, X),
    subbuscador(Subbuscador),
    metabuscador(Metabuscador).

/*6. Un AVL es un árbol binario ordenado balanceado, es decir, es un árbol que tiene dos hijos, los cuales 
los menores se encuentran a la izquierda de la raíz y los mayores a la derecha de la raíz. Además, es un árbol
balanceado, en donde la altura (es el camino más largo de un nodo a una hoja) de los árboles del hijo izquierdo 
con el derecho no es mayor a 1. Para la siguiente figura realizar las siguientes preguntas:

a. Los hechos del árbol dado a partir del predicado padre(Padre, Hijo).
*/

padreArbol(18, 10).
padreArbol(18, 30).
padreArbol(10, 5).
padreArbol(10, 16).
padreArbol(16, 15).
padreArbol(16, 17).
padreArbol(30, 23).
padreArbol(30, 32).
padreArbol(23, 20).
padreArbol(23, 25).
padreArbol(32, 33).

%b. Una regla que permita determinar si dos nodos X e Y son hermanos.

hermanoArbol(X, Y):-
    padreArbol(A, X),
    padreArbol(A, Y),
    X \== Y.

%c. Una regla que permita saber si un nodo se encuentra a la derecha y una para saber
%si un nodo está a la izquierda de otro, ambas reglas se deben implementar a partir
%de los predicados derecha e izquierda respectivamente.

derecha(X, Y):-
    X >= Y.

izquierda(X, Y):-
    X =< Y.

%d. Dado el valor de un nodo, saber cuales son todos los antecesores de este. antecesor(X, Y)

antecesor(X, Y):-
    padreArbol(A, X),
    antecesor(A, Y1),
    append([A], Y1, Y).
antecesor(_, []).

%e. Se necesita responder la siguiente pregunta a partir del predicado descendiente (X, Y): 
%Dado el valor de un nodo, saber cuales son todos los descendientes de este.

hijoIzquierdo(Padre, Hijo):-
    padreArbol(Padre, Hijo),
    Hijo < Padre.

hijoDerecho(Padre, Hijo):-
    padreArbol(Padre, Hijo),
    Hijo >= Padre.

descendiente(X, Y):-
    hijoIzquierdo(X, A),
    hijoDerecho(X, B),
    descendiente(A, A1),
    descendiente(B, B1),
    append([A], [B], C),
    append(A1, B1, C1),
    append(C, C1, Y).

descendiente(X, Y):-
    hijoDerecho(X, B),
    descendiente(B, B1),
    append([B], B1, Y).

descendiente(X, Y):-
    hijoIzquierdo(X, B),
    descendiente(B, B1),
    append([B], B1, Y).

descendiente(_, []).

%7. Una máquina generadora de números aleatorios tiene la particularidad de que no son tan aleatorios como se cree,
%sino que más bien, los genera con una cierta secuencia. Se ha estudiado por años esta máquina y se ha podido constatar
%que los números que genera están definidos por un diagrama. Este se muestra en la figura 1 y se han listado los hechos.

contiguo(4, 2).
contiguo(2, 3).
contiguo(2, 7).
contiguo(3, 0).
contiguo(3, 7).
contiguo(1, 0).
contiguo(7, 6).
contiguo(6, 5).
contiguo(5, 1).

%a. 
%   i. Regla para determinar si existe un número con una determinada serie de dos dígitos contiguos.
%   ii. Puede haber un número aleatorio con dos números no necesariamente contiguos.
%   iii. Es posible terminar con un determinado número?

%8. El predicado da vuelta una lista, dando falso.
predicado1([], Z, Z).
predicado1([H|T], Z, Acc) :-
    predicado1(T, Z, [H|Acc]).

pred2(A, B):-
    predicado1(A, B, []).
/*
[a, b, c, d], X, []
[b, c, d], X, [a]
[c, d], X, [b, a]
[d], X, [c, b, a]
[], X, [d, c, b, a]
Caso base
X = [d, c, b, a]

*/

%9. 
/*
Al encontrarse con un predicado verdadero, el not lo vuelve falso y devuelve esto.
*/

producto(notebook, samsung, "Np-300").
producto(televisor, lg, "43UK2600").
producto(refrigerador, lg, "LT29BPPX").
producto(refrigerador, fersa, "progress 3200").
producto(smartphone, motorola, "Moto G5").
producto(horno, thomas, "TH-25N01").

sinPantalla(Modelo):- 
    producto(_, _, Modelo),
    not(producto(televisor, _, Modelo)), 
    not(producto(notebook, _, Modelo)), 
    not(producto(smartphone, _, Modelo)).


%Dominio: String.
%Predicado: sinPantalla2
%Metas: sinPantalla2

sinPantalla2(Modelo):-
    producto(horno, _, Modelo);
    producto(refrigerador, _, Modelo).

%10.
%a.
miembro(_, []):-
    false.
miembro(Elem, [Elem|_]).
miembro(Elem, [_|T]):-
    miembro(Elem, T).

%b. 
cardinalidad([], 0).
cardinalidad([_|T], Largo) :-
    cardinalidad(T, Largo2),
    Largo is Largo2 + 1.

%c. 
subconjunto([], [_|_]).
subconjunto([H1|T1], [H2|T2]):-
    miembro(H1, [H2|T2]),
    subconjunto(T1, [H2|T2]).

%d.
disjunto([], [_|_]).
disjunto([H1|T1], [H2|T2]):-
    not(miembro(H1, [H2|T2])),
    disjunto(T1, [H2|T2]).
    
%e.




%11.

estado(n).
estado(e).
estado(r).
estado(b).
estado(f).

atraves(n, a, e).
atraves(e, d, r).
atraves(r, t, e).
atraves(r, e1, b).
atraves(r, l, f).
atraves(b, e2, e).

verificarLog(Estado, [A, B]):-
    atraves(Estado, A, B).
verificarLog(Estado, [A, B|Resto]):-
    atraves(Estado, A, B),
    verificarLog(A, [B|Resto]).