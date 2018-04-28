% location : (Id,Active,Locked,Clues,Attached)

loc_get("Id",      (I,_,_,_,_),I).
loc_get("Revealed",(_,A,_,_,_),A).
loc_get("Locked"  ,(_,_,L,_,_),L).
loc_get("Clues",   (_,_,_,C,_),C).
loc_get("Attached",(_,_,_,_,A),A).

loc_get("Shroud",L,S) :-
	loc_get(["Id","Attached"],L,[LId,Attached]),
	location_get("Shroud",LId,Shroud),
	choose(Attached,1168,_),
	S is Shroud + 2.
loc_get("Shroud",L,S) :-
	loc_get(["Id","Attached"],L,[LId,Attached]),
	location_get("Shroud",LId,S),
	\+choose(Attached,1168,_).

loc_get([],_,[]).
loc_get([H|T],I,[X|L]) :- loc_get(H,I,X), loc_get(T,I,L).

loc_get("Connection", LId, Map, DId) :-
	% remove my location from list
	exclude(loc_filter("Id",LId),Map,Map1),
	% remove locked locations
	include(loc_filter("Locked",false),Map1,Map2),
	% find connecting locations
	location_get("Connections",true,LId,LIcons),
	choose(LIcons,Icon,_),
	choose(Map2, DLoc, _),
	loc_get(["Id","Revealed"],DLoc,[DId, DRev]),
	location_get("Icon",DRev,DId,Icon).

loc_set("Revealed",T,L,L1) :- L = (I,_,K,C,A), L1 = (I,T,K,C,A).
loc_set("Locked",  K,L,L1) :- L = (I,T,_,C,A), L1 = (I,T,K,C,A).
loc_set("Clues",   C,L,L1) :- L = (I,T,K,_,A), L1 = (I,T,K,C,A).
loc_set("Attached",A,L,L1) :- L = (I,T,K,C,_), L1 = (I,T,K,C,A).

loc_set([H],[X],L,L1) :- loc_set(H,X,L,L1).
loc_set([H|T],[X|Y],L,L1) :-
	loc_set(H,X,L,L2),
	loc_set(T,Y,L2,L1).


loc_filter(String,I,L) :- loc_get(String,L,I).

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1111,
	Name = "Study",
	BackConn = ("YellowCircle",[]),
	FrontConn = ("YellowCircle",[]),
	Shroud = 2,
	NumClues = 2,
	PerInv = true.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1112,
	Name = "Hallway",
	BackConn = ("RedSquare",["BlueTriangle","MaroonPlus","GreenDiamond"]),
	FrontConn = ("RedSquare",["BlueTriangle","MaroonPlus","GreenDiamond"]),
	Shroud = 1,
	NumClues = 0,
	PerInv = false.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1113,
	Name = "Attic",
	BackConn = ("BlueTriangle",["RedSquare"]),
	FrontConn = ("BlueTriangle",["RedSquare"]),
	Shroud = 1,
	NumClues = 2,
	PerInv = true.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1114,
	Name = "Cellar",
	BackConn = ("MaroonPlus",["RedSquare"]),
	FrontConn = ("MaroonPlus",["RedSquare"]),
	Shroud = 4,
	NumClues = 2,
	PerInv = true.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1115,
	Name = "Parlor",
	BackConn = ("GreenDiamond",["RedSquare"]),
	FrontConn = ("GreenDiamond",["RedSquare"]),
	Shroud = 2,
	NumClues = 0,
	PerInv = false.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1149,
	Name = "Main Path",
	BackConn = ("BrownTilde",["RedSquare","MaroonPlus"]),
	FrontConn = ("BrownTilde",["RedSquare","MaroonPlus"]),
	Shroud = 3,
	NumClues = 1,
	PerInv = true.

location(Id,Name,BackConn,FrontConn,Shroud,NumClues,PerInv) :-
	Id = 1151,
	Name = "Arkham Woods",
	BackConn = ("RedSquare",["BrownTilde"]),
	FrontConn = ("BlueTee",["BrownTilde","GreenDiamond","GreenSlashes"]),
	Shroud = 3,
	NumClues = 1,
	PerInv = true.

location_get("Name",L,N) :- location(L,N,_,_,_,_,_).
location_get("Shroud",L,S) :- location(L,_,_,_,S,_,_).
location_get("Icon",false,L,I) :- location(L,_,(I,_),_,_,_,_).
location_get("Icon",true,L,I) :- location(L,_,_,(I,_),_,_,_).
location_get("Connections",false,L,C) :- location(L,_,(_,C),_,_,_,_).
location_get("Connections",true,L,C) :- location(L,_,_,(_,C),_,_,_).
location_get("Clues",N,L,C) :- location(L,_,_,_,_,N1,true), C is N1 * N.
location_get("Clues",_,L,C) :- location(L,_,_,_,_,C,false).


sample_map(Map) :-
	L1 = (1112,true,false,0,[]),
	L2 = (1113,false,false,0,[]),
	L3 = (1114,false,false,0,[]),
	L4 = (1115,false,true,0,[]),
	Map = [L1,L2,L3,L4].