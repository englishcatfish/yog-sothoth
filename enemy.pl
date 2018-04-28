:- ensure_loaded(card).

enemy_get("Id",    (I,_,_),I).
enemy_get("Skill", (_,S,_),S).
enemy_get("Status",(_,_,S),S).
% Skill
enemy_get("Fight",  E,F) :- enemy_get("Skill",E,(F,_,_,_,_)).
enemy_get("Health", E,H) :- enemy_get("Skill",E,(_,H,_,_,_)).
enemy_get("Agility",E,A) :- enemy_get("Skill",E,(_,_,A,_,_)).
enemy_get("Damage", E,D) :- enemy_get("Skill",E,(_,_,_,D,_)).
enemy_get("Horror", E,H) :- enemy_get("Skill",E,(_,_,_,_,H)).
% Status
enemy_get("Loc",      E,L) :- enemy_get("Status",E,(L,_,_,_,_)).
enemy_get("EngWith",  E,W) :- enemy_get("Status",E,(_,W,_,_,_)).
enemy_get("Damage",   E,D) :- enemy_get("Status",E,(_,_,D,_,_)).
enemy_get("Exhausted",E,D) :- enemy_get("Status",E,(_,_,_,D,_)).
enemy_get("Tokens",   E,T) :- enemy_get("Status",E,(_,_,_,_,T)).

enemy_filter(String,L,E) :- enemy_get(String,E,L).
enemy_filter("Type",Type,E) :- 
	enemy_get("Id",E,(Id,_)),
	card_get("Types",Id,Types),
	choose(Types,Type,_).