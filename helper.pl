choose([X|L],X,L).
choose([H|T],X,[H|L]) :- choose(T,X,L).

choose(X,[X|T],[],T).
choose(X,[H|T],[H|H1],T1) :- choose(X,T,H1,T1).

choose_n(0,L,[],L).
choose_n(N,L,[X|Xs],T) :-
	N > 0, choose(X,L,H1,T1),
	N1 is N-1, choose_n(N1,T1,Xs,T2),
	append(H1,T2,T).
	
choose_any(0,L,[],L).
choose_any(N,L1,L2,T) :- 
	N @> 0,
	choose_n(N,L1,L2,T).
choose_any(N,L1,L2,T) :- 
	N @> 0, 
	N1 is N-1,
	choose_any(N1,L1,L2,T).

choose_any(L1,L2,T) :- 
	length(L1,Len),
	choose_any(Len,L1,L2,T).

choose_any_map(L1,(L2,T)) :-
	choose_any(L1,L2,T).
	
choose_max_map(N,L1,(L2,T)) :-
	choose_any(N,L1,L2,T).

partition_map(Pred,List,(Include,Exclude)) :-
	partition(Pred,List,Include,Exclude).

pair_lists([],[],[]).
pair_lists([X|L1],[Y|L2],[(X,Y)|L]) :-
	pair_lists(L1,L2,L).

pair_lists([],[],[],[]).
pair_lists([X|L1],[Y|L2],[Z|L3],[(X,Y,Z)|L]) :-
	pair_lists(L1,L2,L3,L).

sum(X,Y,Z) :- Z is X + Y.

string_builder(S,[],S).
string_builder(S1,[H|T],S2) :-
	string_concat(S1,H,S3),
	string_builder(S3,T,S2).
string_builder(L,S) :- string_builder("",L,S).

max(X,Y,X) :- X @>= Y.
max(X,Y,Y) :- X @< Y.

min(X,Y,X) :- X @=< Y.
min(X,Y,Y) :- X @> Y.