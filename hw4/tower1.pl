% Using FD Solver.
  
% how to transpoe matrix. source: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
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



%Return true if the the Num is greater than all the value in the list
%   If one is fail, then all return fail
% Num is each element in row, [H|T] built up list to test against


greater(_, []).
greater(Num, [H|T]):-
    H #< Num,
    greater(Num, T).	

% first list is row elements
fit_row([], _, Count, Count).
fit_row([H|T], Prev_val, Count, Accum):-
    append(Prev_val, [H], New_Prev_val),
	(greater(H, Prev_val) -> Count1 is Count + 1,
	 fit_row(T, New_Prev_val, Count1, Accum);
	 fit_row(T, New_Prev_val, Count, Accum)).
 
	    	
fit_tower([],[]).		
fit_tower([H|T], [N|NRT]):-
    fit_row(H, [0], 0, Count), N = Count,
	fit_tower(T,NRT).
  
	
fit_all(T, T_tran, Top, Bottom, Left, Right):-
       %from left view to the matrix
	fit_tower(T, Left),
    
	%from right view to the matrix
	maplist(reverse, T, T_right),
	fit_tower(T_right, Right),
	
	% from top view on the matrix
	fit_tower(T_tran, Top),
	
	% from bottom view to the matrix
	maplist(reverse, T_tran, T_bottom),
	fit_tower(T_bottom, Bottom).

% TA discussion slides: - Finite domain solver
ulistfd([],_).
ulistfd([H|T],N) :-
  length(H, N),
  fd_domain(H, 1, N),
  fd_all_different(H),
  ulistfd(T, N). 

% Tower implementation using FD
tower(N, T, C):-
   length(T,N),
   ulistfd(T,N),
   %set_constraints(N,T),
   transpose(T, T_tran),
   %make sure the new list of lists are unique and generate them row by row
   ulistfd(T_tran,N),
   maplist(fd_labeling, T), %(TA slid) to generate solution to the matrix before passing down
   
   C = counts(Top, Bottom, Left, Right),
   length(Top,N), length(Bottom,N), length(Left, N), length(Right, N),
   %fit the top, bottom, left, right with the matrix list to produce a correct result.
   fit_all(T, T_tran, Top, Bottom, Left, Right).    
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%Plain Tower implementation not using FD solver%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


greater_nofd(_, []).
greater_nofd(Num, [H|T]):-
    H < Num,
    greater_nofd(Num, T).	

% first list is row elements
fit_row_nofd([], _, Count, Count).
fit_row_nofd([H|T], Prev_val, Count, Accum):-
    append(Prev_val, [H], New_Prev_val),
	(greater_nofd(H, Prev_val) -> Count1 is Count + 1,
	 fit_row_nofd(T, New_Prev_val, Count1, Accum);
	 fit_row_nofd(T, New_Prev_val, Count, Accum)).
 
	    	
fit_tower_nofd([],[]).		
fit_tower_nofd([H|T], [N|NRT]):-
    fit_row(H, [0], 0, Count), N = Count,
	fit_tower_nofd(T,NRT).
  
	
fit_all_nofd(T, T_tran, Top, Bottom, Left, Right):-
       %from left view to the matrix
	fit_tower_nofd(T, Left),
    
	%from right view to the matrix
	maplist(reverse, T, T_right),
	fit_tower_nofd(T_right, Right),
	
	% from top view on the matrix
	fit_tower_nofd(T_tran, Top),
	
	% from bottom view to the matrix
	maplist(reverse, T_tran, T_bottom),
	fit_tower_nofd(T_bottom, Bottom).




/*
TOO SLOW!!!!!!!!
TA slides for generating a list with Constraints
all_unique([]).
all_unique([H|T]):- member(H,T),!,fail.
all_unique([_|T]):- all_unique(T).

unique_list(Row, N):-
  length(Row,N),
  maplist(between(1,N),Row),
  all_unique(Row).
  
unique_list_nofd([],_).
unique_list_nofd([H|T], N):-
   length(H,N),
   unique_list(H,N),
   unique_list_nofd(T,N).
*/

% TA github guides
% hint of accelerating plain_tower

% a list contain N elements 
% http://www.gprolog.org/manual/html_node/gprolog033.html
% http://www.gprolog.org/manual/gprolog.html#hevea_default674
% Domain is all the enumerated answers of between(1, N, X)
within_domain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _).
fill_2d([Head | Tail], N) :-
    within_domain(N, Domain),
    permutation(Domain, Head),
    fill_2d(Tail, N).
    
make_row([],_).
make_row([H|T],N):-
    length(H,N),
    make_row(T,N).    


create_sequence(0,[]).
create_sequence(N,[H|T]):-
    Num is N-1,
    H is N,
    create_sequence(Num,T).

fit_matrix([],_,_,_).
fit_matrix([H|T],N,[Nleft|Nlr], [Nright|Nrr]):-
    length(H_seq,N),
    create_sequence(N, H_seq),!,
    permutation(H_seq,H),
    fit_row_nofd(H, [0], 0, Count), Nleft = Count,
    reverse(H,Hrev),
    fit_row_nofd(Hrev, [0], 0, Count1), Nright = Count1,
    fit_matrix(T,N,Nlr,Nrr).
    

fit_matrix_bottom([],_,_,_).
fit_matrix_bottom([H|T],N,[Ntop|Ntr], [Nbottom|Nbr]):-
    length(H_seq,N),
    create_sequence(N, H_seq),!,
    permutation(H_seq,H),
    fit_row_nofd(H, [0], 0, Count), Ntop = Count,
    reverse(H,Hrev),
    fit_row_nofd(Hrev, [0], 0, Count1), Nbottom = Count1,
    fit_matrix(T,N,Ntr,Nbr).

plain_tower(N, T, C):-
   C = counts(Top, Bottom, Left, Right),
   length(Top,N), length(Bottom,N), length(Left, N), length(Right, N),
   length(T, N),
   make_row(T,N),
   fit_matrix(T,N,Left, Right),
    %from left view to the matrix
    %fit_tower_nofd(T, Left),
    %from right view to the matrix
    %maplist(reverse, T, T_right),
    %fit_tower_nofd(T_right, Right).
   %fill_2d(T,N),
   
   transpose(T, T_tran),
   fit_matrix_bottom(T_tran,N,Top,Bottom).
   	% from top view on the matrix
	%fit_tower_nofd(T_tran, Top),
	
	% from bottom view to the matrix
	%maplist(reverse, T_tran, T_bottom),
	%fit_tower_nofd(T_bottom, Bottom).
   %check uniqueness of the matrix and transposed matrix   
   
   %fill_2d(T_tran,N),
   %C = counts(Top, Bottom, Left, Right),
   %generate whatever list of N length Top, bottom, Left, Right.
   %length(Top,N), length(Bottom,N), length(Left, N), length(Right, N),
   %fit the top, bottom, left, right with the matrix list to produce a correct result.
   %fit_all_nofd(T, T_tran, Top, Bottom, Left, Right).  
   
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                             
%speedup to measure performance of plain tower vs fd solver tower, get ratio plain/fd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_plain(X):-
    statistics(cpu_time, [Start|_]),
	plain_tower(4, _, counts([1,2,2,3],[3,2,2,1],[1,2,2,4],[3,2,3,1])),
	statistics(cpu_time, [End|_]),
	X is End - Start.

time_fd(X):-
    statistics(cpu_time, [Start|_]),
	tower(4, _, counts([1,2,2,3],[3,2,2,1],[1,2,2,4],[3,2,3,1])),
	statistics(cpu_time, [End|_]),
	X is End - Start.    


speedup(Ratio):-
    time_plain(Time_plain_tower),time_fd(Time_fd_tower), Ratio is Time_plain_tower/Time_fd_tower.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%ambiguous to find ambiguous puzzle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ambiguous(N, C, T1, T2):-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.