tower(N, T, C) :- 
	C = counts(Top, Bottom, Left, Right),
	length(T, N),
	goodset(N, T),
	transpose(T, TTran),
	goodset(N, TTran),
	checkcounts(Left, T),
	checkcountsreversed(Right, T),
	checkcounts(Top, TTran),
	checkcountsreversed(Bottom, TTran),
	label(T).
 
goodset(N, []).

goodset(N, [H|T]) :- 
	length(H, N),
	fd_all_different(H),
	fd_domain(H, 1, N),
	goodset(N, T).

label([]).

label([X|T]) :-
	fd_labeling(X),
	label(T).

goodcount(1, [T], Ht) :-
	T #> Ht.

goodcount(0, [T], Height) :-
	T #< Height.

goodcount(Cts, [H|T], Height) :-
	H #> Height, 
	NCts #= Cts-1,
	goodcount(NCts, T, H).

goodcount(Cts, [H|T], Height) :-
	H #< Height, 
	goodcount(Cts, T, Height).

checkcounts([], []).
checkcounts([Count|Rest], [List|T]) :-
	goodcount(Count, List, 0),
	checkcounts(Rest, T).

checkcountsreversed([], []).
checkcountsreversed([Count|Rest], [List|T]) :-
	reverse(List, Listrev),
	goodcount(Count, Listrev, 0),
	checkcountsreversed(Rest, T).

%%%%%%%%%%%%%%%
%
%
% TRANSPOSE
%
%
% Taken from stack-overflow
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
%
%
%%%%%%%%%%%%%%%%%%

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%
%
%
%
% PLAIN tower
%
%
%
%%%%%%%%%%%%%%%%


plain_tower(N, T, C) :- 
	C = counts(Top, Bottom, Left, Right),
	length(T, N),
	plain_matrix(N, T, Left, Right),
	transpose(T, TTran),
	plain_matrix(N, TTran, Top, Bottom),
	length(Left, N),
	length(Right, N),
	length(Top, N),
	length(Bottom, N).

plain_matrix(N, [], _, _).

	% make_123_row creates [N, N-1,...,2,1]
	% It should not be tracked back to, since for some reason it gives stack overflow. 
	% But its okay, since we only need it once

	% Member is needed to instatinate Fleft. Unlike goodcount, goodcount2 assumes ALL its params already
	% instatinated, and just checks if the count is correct.

plain_matrix(N, [HPerm|T], [Fleft|Left], [Fright|Right]) :- 
	length(H, N),
	make_123_row(N, H), !,
	permutation(H, HPerm),
	member(Fleft, H),
	goodcount2(Fleft, HPerm, 0),
	reverse(HPerm, Hrev),
	member(Fright, H),
	goodcount2(Fright, Hrev, 0),
	plain_matrix(N, T, Left, Right).

make_123_row(0, []).
make_123_row(N, [A|T]) :-
	NNew is N-1,
	A is N,
	make_123_row(NNew, T).

goodcount2(1, [T], Ht) :-
	T > Ht.

goodcount2(0, [T], Height) :-
	T < Height.

goodcount2(Cts, [H|T], Height) :-
	H > Height, 
	NCts is Cts-1,
	goodcount2(NCts, T, H).

goodcount2(Cts, [H|T], Height) :-
	H < Height, 
	goodcount2(Cts, T, Height).


%%%%%%%%%%%%%%
%
%
% AMBIGUOUS
%
%
%%%%%%%%%%%%%%%

ambiguous(N, C, T1, T2) :- 
	fd_domain(N, 1, 10),
	fd_labeling(N),
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.

%%%%%%%%%%%%%
%
%
% Statistics
%
%
%
%%%%%%%%%%%%%%

ttime(TT) :- 
	statistics(cpu_time, [Start|_]),
	tower(5, T,counts([2,3,4,1,2],[3,2,1,3,2],[2,4,1,2,3],[2,1,3,3,2])),
	statistics(cpu_time, [Stop|_]),
	TT is Stop - Start.


pttime(PTT) :-
	statistics(cpu_time, [Start|_]),
	plain_tower(5, T,counts([2,3,4,1,2],[3,2,1,3,2],[2,4,1,2,3],[2,1,3,3,2])),
	statistics(cpu_time, [Stop|_]),
	PTT is Stop - Start.

speedup(Ratio) :-
	pttime(PTT),
	ttime(TT),
	Ratio is PTT / TT.