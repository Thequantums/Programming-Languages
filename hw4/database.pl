member(X,[X|_]).
member(X,[H|T]):- member(X,T).

append([], L, L).
append([H|T], L, [H|LT]):- append(T,L,LT).

/*delete(X, [], T).
delete(X, [X|T], T).
delete(X, [Y|T], [Y|N]):- delete(X,T,N).*/

/*mydelete(X, [], T).
mydelete(X, [X|T], T):-mydelete(X,T,T).*/

mydelete(X,[], []).
mydelete(X, [X|T],Z):-mydelete(X, T, Z).
mydelete(X, [Y|T], [Y|N]):-mydelete(X,T,N).


elements_between([],_,_).
elements_between([H|T], Min, Max):-
   between(Min, Max, H), elements_between(T, Min, Max).

all_unique([]).
all_unique([H|T]):-member(H,T),!,fail.
all_unique([H|T]):- all_unique(T).

unique_list(List,N):-
  length(List,N), elements_between(List, 1, N), all_unique(List).