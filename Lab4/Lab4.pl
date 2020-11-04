:- use_module(library(clpfd)).

neighbor(X, Y, List) :- nextto(X, Y, List);nextto(Y, X, List).

einstein :-
    Houses = [_,_,_,_,_],
    nth1(1, Houses, [norwegian,_,_,_,_]),
    member([englishman,_,_,_,red], Houses),
    nextto([_,_,_,_,green], [_,_,_,_,white], Houses),
    member([dane,_,_,tea,_], Houses),
    neighbor([_,_,rothmans,_,_], [_,cat,_,_,_], Houses),
    member([_,_,dunhill,_,yellow], Houses),
    member([german,_,marlboro,_,_], Houses),
    nth1(3, Houses, [_,_,_,milk,_]),
    neighbor([_,_,rothmans,_,_], [_,_,_,water,_], Houses),
    member([_,bird,pallmall,_,_], Houses),
    member([swede,dog,_,_,_], Houses),
    neighbor([norwegian,_,_,_,_], [_,_,_,_,blue], Houses),
    member([_,horse,_,_,blue], Houses),
    member([_,_,phillipmorris,beer,_], Houses),
    member([_,_,_,coffee,green], Houses),
    member([Owner,fish,_,_,_], Houses),
    print('Owner of the fish: '), print(Owner), nl,
    print('Full Solution: '), print(Houses), nl.

create(0, _, []).
create(N0, N, [H|T]) :-
	N0 > 0,
	N1 is N0 - 1,
	length(H, N),
	create(N1, N, T).

sum_row([], _).
sum_row([Row|Matrix], Sum) :-
	sum(Row, #=, Sum),
	sum_row(Matrix, Sum).

diag([], _, _, []).
diag([Row|Matrix], Idx, P, [X|ListeDiag]) :-
	nth1(Idx, Row, X),
	Idx1 is Idx+P,
	diag(Matrix, Idx1, P, ListeDiag).

magic_square(N, Matrix) :-
	Nmax is N * N,
	SumDim is N * (N * N + 1) / 2,
	create(N, N, Matrix),
	flatten(Matrix, Vars),
	Vars ins 1..Nmax,
        sum_row(Matrix, SumDim),
	transpose(Matrix, TransMat),
	sum_row(TransMat, SumDim),
	diag(Matrix, N, -1, D1),
	sum(D1, #=, SumDim),
	diag(Matrix, 1, +1, D2),
	sum(D2, #=, SumDim),
	all_different(Vars),
        label_row(Matrix).

label_row([]).
label_row([Row|Matrix]):-label(Row),label_row(Matrix).

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,2,4, _,_,_, _,5,_],
            [7,_,_, 2,_,9, _,_,8],
            [_,_,_, 6,_,_, _,1,2],

            [6,_,_, _,_,2, _,_,_],
            [_,3,2, _,7,_, 8,4,_],
            [_,_,_, 8,_,_, _,_,1],

            [8,7,_, _,_,1, _,_,_],
            [2,_,_, 5,_,7, _,_,3],
            [_,5,_, _,_,_, 9,6,_]]).
