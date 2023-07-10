:- use_module(library(clpfd)).

% Leemos la tabla de Sudoku de 9x9 en el formato solicitado
read_sp(P) :-
    read_gl(9, P).

read_gl(0, []).
read_gl(T, [R|Rs]) :-
    T > 0,
    read_line_to_string(user_input, LI),
    split_string(LI, " ", "", LInput),
    maplist(convert, LInput, R),
    T1 is T - 1,
    read_gl(T1, Rs).

convert(".", _).
convert(NS, N) :-
    number_string(N, NS).

% Imprime tabla 9x9 en el formato solicitado
print_sp([]).
print_sp([R|Rs]) :-
    print_r(R),
    nl,
    print_sp(Rs).

print_r([]).
print_r([N|Ns]) :-
    (   var(N)
    ->  write('.')
    ;   write(N) ),
    write(' '),
    print_r(Ns).

% Reglas para resolver Sudoku
solve_sp(G) :-
        length(G, 9),
        maplist(same_length(G), G),
        append(G, E), E ins 1..9,
        maplist(all_distinct, G),
        transpose(G, C),
        maplist(all_distinct, C),
        G = [RA,RB,RC,RD,RE,RF,RG,RH,RI],
        check_b(RA, RB, RC),
        check_b(RD, RE, RF),
        check_b(RG, RH, RI).

check_b([], [], []).
check_b([X1,X2,X3|Xs1], [Y1,Y2,Y3|Ys2], [Z1,Z2,Z3|Zs3]) :-
        all_distinct([X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3]),
        check_b(Xs1, Ys2, Zs3).

solve_sudoku :-
    read_sp(Grid),
    solve_sp(Grid),
    append(Grid, E),
    findall(_, (labeling([ff], E), print_sp(Grid), nl), _).