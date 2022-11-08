ir(a, b, 50).
ir(a, c, 75).
ir(b, c, 15).
ir(b, x, 35).
ir(b, y, 60).
ir(c, y, 85).
ir(y, x, 25).
ir(y, z, 10).
ir(x, z, 40).

verificar(O, O, 0).
verificar(O, D, C):-
	ir(O, A, C1),
	C2 is C - C1,
	verificar(A, D, C2).

camino(O, O, I, 0).
camino(O, D, O, CT):-
  ir(O, X1, C1), 
  camino(X1, D, O, C2), 
  CT is C1 + C2.
camino(O, D, I, CT):-
  ir(O, X1, C1), 
  camino(X1, D, I, C2), 
  CT is C1 + C2.

estados(O, D, []):-
	ir(O, D, _).
estados(O, D, L):-
	ir(O, A, _),
	estados(A, D, L1),
	\+(member(L1, A)),
	append([A], L1, L).

%producto(Codigo, Nombre, Tipo, Azucar, Grasa, Sodio, Calorias, Precio, Stock).

producto(1, "Helado", postre, 10, 20, 30, 40, 50, 60).
producto(2, "Yogurth", lacteo, 70, 80, 90, 100, 110, 120).
producto(3, "Queso", lacteo, 110, 100, 90, 80, 70, 60).
producto(4, "Donut", confite, 50, 40, 30, 20, 10, 0).

%total(ListaP, TotalC, TotalG, TotalA, TotalS, TotalP).

total([], 0, 0, 0, 0, 0).
total([H|T], TotalC, TotalG, TotalA, TotalS, TotalP):-
	producto(H, _, _, A, G, S, C, P, _),
	total(T, C2, G2, A2, S2, P2),
	TotalC is C + C2,
	TotalG is G + G2,
	TotalA is A + A2,
	TotalS is S + S2,
	TotalP is P + P2.

	