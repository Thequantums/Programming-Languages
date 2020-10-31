tower(N, T, C) :-
	% Length of T must be N
	length(T, N),
	% Length of each element of T must be N
	column_length(N, T),
	% Each number in T must be between 1..N 
	restrict_domain(N, T),
	% Each number in the rows must be different
	maplist(fd_all_different, T),
	transpose(T, Cols),
	% Each number in the col must be different
	maplist(fd_all_different, Cols),
	% Give variables values using FD solver
	maplist(fd_labeling, T),
	reverse_all(T, Tr),
	reverse_all(Cols, ColsR),
	% Give each element of counts a name
	counts(U, D, L, R) = C,
	length(U, N),
	length(D, N),
	length(L, N),
	length(R, N),
	check_counts(Cols, U),
	check_counts(ColsR, D),
	check_counts(T, L),
	check_counts(Tr, R).


% Force T to be 2D list
column_length(_, []).
column_length(N, [H | T]) :-
	length(H, N),
	column_length(N, T).


% Restrict the domain recursively for each row of T
restrict_domain(_, []).
restrict_domain(N, [H | T]) :-
	fd_domain(H, 1, N),
	restrict_domain(N, T).


% Transpose the row list to be column list instead
% Implementation adapted from an older version of SWI-PROLOG w/o foldl
transpose([], []).
transpose([L | Ls], Ts) :-
	transpose_(L, [L | Ls], Ts).

transpose_([], _, []).
transpose_([_ | Es], Lists0, [Fs | Fss]) :-
	maplist(list_first_rest, Lists0, Fs, Lists),
	transpose_(Es, Lists, Fss).

list_first_rest([L | Ls], L, Ls).


% Reverse all rows
reverse_all([], []).
reverse_all([T | Ts], [Tr | Trs]) :-
	reverse(T, Tr),
	reverse_all(Ts, Trs).


% Produce the left count for a row
left_count([R | Rs], Co) :-
	left_count(Rs, R, 1, Co).
% Force Co to be equal to C, "returning" the answer
left_count([], _, C, C).
% Two cases: R > Max, or R < Max
left_count([R | Rs], Max, C, Co) :-
	R > Max,
	C1 is C+1,
	% New max is R, and we see one more tower
	left_count(Rs, R, C1, Co).
left_count([R | Rs], Max, C, Co) :-
	R < Max,
	% No need to define new variables
	left_count(Rs, Max, C, Co).


% Ensures that left counts are correct for one side of the tower
check_counts([], []).
check_counts([[R | Rs] | T], [C | Cs]) :-
	left_count([R | Rs], C),
	check_counts(T, Cs).

% ######################################################################## %
plain_tower(N, T, C) :-
	length(T, N),
	column_length(N, T),
	fill(T, N),	
	transpose(T, Cols),
	maplist(different, Cols),
	reverse_all(T, Tr),
	reverse_all(Cols, ColsR),
	% Give each element of counts a name
	counts(U, D, L, R) = C,
	length(U, N),
	length(D, N),
	length(L, N),
	length(R, N),
	check_counts(Cols, U),
	check_counts(ColsR, D),
	check_counts(T, L),
	check_counts(Tr, R).

% Generates a list of between 1 and N
domain(N, D) :- 
	findall(X, between(1, N, X), D).

fill([], _).
fill([H | T], N) :-
	domain(N, D),
	permutation(D, H),
	fill(T, N).

different(L) :-
	sort(L, Sorted),
	length(L, Length1),
	length(Sorted, Length2),
	Length1 == Length2.

speedup(R) :-
	statistics(cpu_time, [_, _]),
	tower(4, T1, C),
	statistics(cpu_time, [_ , E1]),
	plain_tower(4, T2, C),
	statistics(cpu_time, [_ , E2]),
	PTT is 1.0*(E2+1),
	TT is 1.0*(E1+1),
	write(E1), nl, write(E2), nl,
	R is PTT/TT.


% ######################################################################## %
% Finds two towers that satisfy the same N, C
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.