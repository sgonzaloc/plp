% Auxiliares

desde(X, Y) :- nonvar(Y), Y > X.
desde(X, Y) :- var(Y), N is X + 1, desde(N, Y).

tablero(ej5x5, T) :-
      tablero(5, 5, T),
      ocupar(pos(1, 1), T),
      ocupar(pos(1, 2), T).

tablero(libre20, T) :- tablero(20, 20, T).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(1,C,[L]) :- C > 0, length(L, C).
tablero(F,C,T) :- F > 0, tablero(1, C, Unario), Anterior is F - 1,
                  tablero(Anterior, C, S), append(S, Unario, T).

% TODO: Si saco el append y lo mando a la definicion,
% se cuelga cuando uso ocupar


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(X, Y), T) :- nth0(X, T, L), nth0(Y, L, ocupada).

% TODO: Esto no es necesario hacerlo, pues no tenemos que dimensionar todos los tableros

%ocupar(pos(X, Y), tablero(Q, W, E)) :- var(tablero(Q, W, E)), desde(2, Limite),
%  CotaF is Limite - 1, between(1, CotaF, F),
%  F > X, C is Limite - F, C > Y,
%  tablero(F, C, T),
%  ocupar(pos(X, Y), T).



%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(_,_,_).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(_,_,_,_).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(_,_,_,_).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(_,_,_,_).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_,_,_,_,_).
