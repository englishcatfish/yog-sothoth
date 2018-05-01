% TODO: When activating skill bonuses, they go in the Active list under ("SkillBonus",Val).

:- set_prolog_stack(global, limit(100 000 000 000)).
:- discontiguous single_step/3.

:- ensure_loaded(helper).
:- ensure_loaded(game_state).
:- ensure_loaded(investigator).
:- ensure_loaded(location).
:- ensure_loaded(card).
:- ensure_loaded(enemy).
%:- ensure_loaded(tests).

step_n(0,GS,GS,[]).
step_n(N,GS,GSN,Actions) :-
	N @> 0, N1 is N - 1,
	single_step(GS,GS1,A1),
	step_n(N1,GS1,GSN,AN),
	Actions = [A1|AN].

step_n_atom(0,GS,GS,[]).
step_n_atom(N,GS,GSN,Actions) :-
	term_to_atom(GS_Term,GS),
	step_n(N,GS_Term,GSN_Term,Actions),
	term_to_atom(GSN_Term,GSN).

sample_gs_atom(GS) :-
	%init_gs(GS_Term),
	sample_gs("Action",GS_Term),
	term_to_atom(GS_Term,GS).

sample_gs("ST.2", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1039],[((1030,1),false,[]),((1033,1),false,[]),((1073,1),false,[])],[((1007,1),[("Clues",3)])],[1006]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[("SkillBonus",0)]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[1168])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",2,"Intel",2,ChaosBag),("Investigate",("Location",1111),false)],
	GS = (G,Is,Map,Enc,S).

sample_gs("ST.2a", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1039],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[("SkillBonus",0)]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[1168])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",2,"Intel",2,ChaosBag),("Investigate",("Location",1111),false)],
	GS = (G,Is,Map,Enc,S).

sample_gs("Action", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Easy"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([1087,1039],[1087,1039],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = ["Next Player"],
	GS = (G,Is,Map,Enc,S).

sample_gs("Action2", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Easy"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([1006,1007,1020],[1087,1039],[],[],[]),(1112,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1112,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1112,true,false,2,[]),(1113,false,false,0,[]),(1114,false,false,0,[]),(1115,false,true,0,[])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = ["Next Action"],
	GS = (G,Is,Map,Enc,S).

sample_gs("ST.3", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1056],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[])],
	%E1 = ((1160,1),(2,2,2,1,1),(1111,0,0,false,[])),
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",3,"Intel",2,ChaosBag,[1014,1087,1014],[(1001,[1087,1014]),(1002,[1014])]),("Investigate",1111)],
	GS = (G,Is,Map,Enc,S).

sample_gs("InitOne", GS) :-
	ChaosBag = ["+1","+1","0","0","0","-1","-1","-1","-2","-2",
	            "Skull","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	Global = ((1104,"Easy"),"Investigation",[1001],1001,1001,ChaosBag),
	Deck = [ 1006, 1007, 1016, 1017, 1018, 1019, 1020, 1021, 
	         1022, 1023, 1024, 1025, 1030, 1031, 1032, 1033, 
	         1034, 1035, 1036, 1037, 1038, 1039, 1086, 1086, 
	         1087, 1087, 1088, 1088, 1089, 1089, 1092, 1092	],
	I = (1001,(Deck,[],[],[],[]),(1111,3,0,0,5,0,false),(3,3,4,2,9,5),[],[]),
	Map = [(1111,true,false,2,[])],
	EDeck = [ 1118, 1119, 1059, 1059, 1059, 1060, 1060, 1060,
	          1061, 1062, 1062, 1062, 1063, 1063, 1063, 1064,
	          1064, 1065, 1065, 1066, 1066, 1066, 1067, 1067,
	          1068, 1068 ],
	Enc = ([],EDeck,[],[1106,1107],[1109,1110],1105,1108),
	S = ["Start Investigation"],
	GS = (Global,[I],Map,Enc,S).

init_gs(GS) :-
	sample_gs("InitOne", GS_tmp),
	game_state_get("Inv", 1001, GS_tmp, I),
	inv_get("Deck", I, Deck),
	choose_n(5, Deck, Hand, Deck0),
	inv_set(["Deck", "Hand"], [Deck0, Hand], I, I0),
	game_state_set("Inv",I0,GS_tmp,GS).

% TODO: Mythos Phase
% TODO: Enemy Phase
% TODO: Upkeep Phase

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PHASE: INVESTIGATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2.2.1 Investigator takes an action, if able.
%
% During their turn, an investigator is permitted to take three actions. An action can be used
% to do one of the following:
% - Investigate your location.
% - Move to a connecting location.
% - Draw (draw 1 card).
% - Resource (gain 1 resource).
% - Play an asset or event card from your hand.
% - Activate an =>-costed ability on:
%   - an in-play card you control.
%   - an in-play encounter card at your location.
%   - a card in your threat area.
%   - the current act card.
%   - the current agenda card.
% - Fight an enemy at your location.
% - Engage an enemy at your location.
% - Attempt to evade an enemy engaged with you.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(0, GS_in, GS_out, Action, 1) :-
	Pop = "Start Investigation",
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Push = "Next Player",
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Start Investigation"].

%%% Next Action
% Q: Is it always the case that the Stack will be 1 single element at this point?
% No more actions
single_step(2, GS_in, GS_out, Action, 3) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get("Actions", I, 0),
	Push = "End Turn",
	game_state_set("Stack", [Push], GS_in, GS_out),
	Action = ["Turn finished"].

%%% Next Action
% Investigator may choose to end turn early
single_step(2, GS_in, GS_out, Action, 3) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get("Actions", I, N),
	N @> 0,
	inv_set("Actions", 0, I, I1),
	Push = "End Turn",
	game_state_set(["Stack", "Inv"],[[Push], I1], GS_in, GS_out),
	Action = ["End turn early"].

%%% End Turn
% Investigator turn finished
% TODO: process end of turn event limits
single_step(3, GS_in, GS_out, Action, 1) :-
	Pop = "End Turn",
	game_state_get("Stack", GS_in, [Pop]),
	Push = "Next Player",
	game_state_set("Stack", [Push], GS_in, GS_out),
	Action = ["Ending Turn"].

%%% Next Player
% choose next investigator
single_step(1, GS_in, GS_out, Action, 4) :-
	Pop = "Next Player",
	game_state_get(["Stack", "Invs"], GS_in, [[Pop], Is]),
	% choose investigator that still has actions left
	choose(Is, I, _),
	inv_get("Actions", I, N),
	N @> 0,
	inv_get("Id", I, Id),
	Push = "Begin Turn",
	game_state_set(["Stack", "CurrInvId"], [[Push], Id], GS_in, GS_out),
	string_builder(["Selected ", Id, " to go next."], Str),
	Action = [Str].

%%% Begin Turn
% Formalize begining of player turn
single_step(4, GS_in, GS_out, Action, 2) :-
	Pop = "Begin Turn",
	game_state_get("Stack", GS_in, [Pop]),
	Push = "Next Action",
	game_state_set("Stack", [Push], GS_in, GS_out),
	Action = ["Begin turn"].

%%% Next Action
% Investigate your location (Basic)
% condition: at least one clue at your location
% TODO: handle more complex scenarios like locations that use 
%       different skill type for investigation
single_step(2, GS_in, GS_out, Action, 5) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get("Actions", I, N),
	N @> 0,
	inv_get("Loc", I, LocId),
	game_state_get(("Loc", LocId), GS_in, Loc),
	loc_get("Clues", Loc, Clues),
	Clues @> 0,
	N1 is N - 1,
	game_state_get("ChaosBag", GS_in, ChaosBag),
	loc_get("Shroud", Loc, Shroud),
	inv_get("Active", I, Active),
	inv_set(["Actions", "Active"], [N1, [("SkillBonus", 0)|Active]], I, I1),
	Push0 = "End Action",
	Push1 = ("Investigate", ("Location", LocId), false),
	Push2 = ("Skill Test", 2, "Intel", Shroud, ChaosBag),
	game_state_set(["Stack", "Inv"],[[Push2, Push1, Push0], I1], GS_in, GS_out),
	string_builder(["Investigate ", LocId], Str),
	Action = [Str].

%%% Next Action
% Move to a connecting location
% - Choose an unlocked connecting location to move to
% - Will be split into two events: MoveOut, MoveIn
%   Contrary to the rules, which say this is "simulataneous", but it'll behave that way
single_step(2, GS_in, GS_out, Action, 6) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get("Actions", I, N),
	N @> 0,
	game_state_get("Map", GS_in, Map),
	inv_get("Loc", I, LocId),
	loc_get("Connection", LocId, Map, DstId),
	Push0 = "End Action",
	Push1 = ("Move", "MoveOut", LocId, DstId),
	game_state_set("Stack", [Push1, Push0], GS_in, GS_out),
	string_builder(["Move from ", LocId, " to ", DstId],Str),
	Action = [Str].

%%% Move, MoveOut:
% Most places you can simply move out of.
% TODO: handle locations like 1115 that force a test to leave
single_step(6, GS_in, GS_out, Action, 7) :-
	Pop = ("Move", "MoveOut", SrcId, DstId),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Push = ("Move", "MoveIn", SrcId, DstId),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Moving out of ", SrcId], Str),
	Action = [Str].

%%% Move, MoveIn:
% If location is not revealed, then reveal and add clues
single_step(6, GS_in, GS_out, Action, 6) :-
	Pop = ("Move", "MoveIn" , _, DstId),
	game_state_get(["Stack", ("Loc", DstId)], GS_in, [[Pop | _], Loc]),
	loc_get("Revealed", Loc, false),
	game_state_get("NumInv", GS_in, N),
	location_get("Clues", N, DstId, Clues),
	loc_set(["Revealed", "Clues"],[true, Clues], Loc, Loc1),
	game_state_set("Loc", Loc1, GS_in, GS_out),
	string_builder(["Revealing location ", DstId],Str),
	Action = [Str].

% TODO: If there are enemies at the location you move to, they engage you immediately

%%% Move, MoveIn:
% 1113, Attic
% -F->
% After you enter the Attic: Take 1 horror
single_step(7, GS_in, GS_out, Action, 8) :-
	Pop = ("Move", "MoveIn", _, 1113),
	game_state_get(["Stack", ("Loc", 1113)], GS_in,[[Pop | Stack], Loc]),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", 1113, I, I1),
	Push = ("Assign Affliction", 0, 1),
	game_state_set(["Stack", "Inv"],[[Push | Stack], I1], GS_in, GS_out),
	Action = ["Move into 1113"].

%%% Move, MoveIn:
% 1114, Cellar
% -F->
% After you enter the Cellar: Take 1 damage
single_step(7, GS_in, GS_out, Action, 8) :-
	Pop = ("Move", "MoveIn", _, 1114),
	game_state_get(["Stack", ("Loc", 1114)], GS_in, [[Pop | Stack], Loc]),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", 1114, I, I1),
	Push = ("Assign Affliction", 1, 0),
	game_state_set(["Stack", "Inv"],[[Push | Stack], I1], GS_in, GS_out),
	Action = ["Move into 1114"].

%%% Move, MoveIn:
% Most places you can simply move into.
single_step(7, GS_in, GS_out, Action, -1) :-
	Pop = ("Move", "MoveIn", _, DstId),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	\+in(DstId, [1113, 1114]),
	game_state_get(("Loc", DstId), GS_in, Loc),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", DstId, I, I1),
	game_state_set(["Stack", "Inv"],[Stack, I1], GS_in, GS_out),
	string_builder(["Move into ", DstId], Str),
	Action = [Str].

%%% Assign Affliction
% Right now, just going to assign to investigator
% TODO: Choose how to distribute and what amounts to each card
single_step(8, GS_in, GS_out, Action, 9) :-
	Pop = ("Assign Affliction", D, H),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Push = ("Apply Affliction", D, H, "CurrInv"),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Assigned ", D, " damage and ", H, " horror to investigator"], Str),
	Action = [Str].

%%% Apply Affliction
% TODO: What if the investigator faints?
single_step(9, GS_in, GS_out, Action, -1) :-
	Pop = ("Apply Affliction", Dmg, Hrr, "CurrInv"),
	game_state_get(["Stack","CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Health", "Sanity"], I, [Health, Sanity]),
	Health1 is Health - Dmg,
	Sanity1 is Sanity - Hrr,
	inv_set(["Health", "Sanity"], [Health1, Sanity1], I, I1),
	game_state_set(["Stack", "Inv"], [Stack, I1], GS_in, GS_out),
	Action = ["Applying affliction to investigator"].

%%% Next Action
% Draw 1 card
single_step(2, GS_in, GS_out, Action, 10) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get(["Actions", "Deck"], I, [N, Deck]),
	N @> 0,
	N1 is N - 1,
	choose(Deck, Card, Deck1),
	inv_set(["Actions", "Deck"], [N1, Deck1], I, I1),
	Push0 = "End Action",
	Push1 = ("Reveal Card",Card),
	game_state_set(["Stack", "Inv"], [[Push1, Push0], I1], GS_in, GS_out),
	string_builder(["Drew card ", Card], Str),
	Action = [Str].

%%% Reveal Card
% TODO: Process revelation effects
single_step(10, GS_in, GS_out, Action, -1) :-
	Pop = ("Reveal Card",Card),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get("Hand", I, Hand),
	insert_card(Card, Hand, Hand1),
	inv_set("Hand", Hand1, I, I1),
	game_state_set(["Stack", "Inv"], [Stack, I1], GS_in, GS_out),
	Action = ["Adding Card to hand"].

% TODO: Play a card
% TODO: Gain a resource
% TODO: Activate an ==> costed ability
% TODO: Fight enemy at location
% TODO: Evade enemy at location
% TODO: Engaged enemy at location
% TODO: AoE

single_step(2, GS_in, GS_out, Action, 11) :-
	Pop = "Next Action",
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop], I]),
	inv_get("Actions", I, N),
	N @> 0,
	N0 is N - 1,
	inv_set("Actions", N0, I, I0),
	Push0 = "End Action",
	Push1 = ("Gain Resources", 1),
	game_state_set(["Stack", "Inv"], [[Push1, Push0], I0], GS_in, GS_out),
	Action = ["Next Action: Gain a Resource."].

%%% End Action
% TODO: process end action properly
%single_step(GS_in, GS_out, Action) :-
%	Pop = "End Action",
%	game_state_get("Stack", GS_in, [Pop | Stack]),
%	Push = "Next Action",
%	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
%	Action = ["Ending action"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.2: Commit cards from hand to skill test.
% 
% The investigator performing the skill test may commit any number of cards with an appropriate
% skill icon from his or her hand to this test. Each other investigator at the same location as
% the investigator performing the skill test may commit one card with an appropriate skill icon
% to this test. An appropriate skill icon is either one that matches the skill being tested, or
% a wild icon.  The investigator performing this test gets +1 to his or her skill value during
% this test for each appropriate skill icon that is committed to this test. Cards that lack an
% appropriate skill icon may not be committed to a skill test. Do not pay a card's resource cost
% when committing it.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Skill Test, 2
single_step(5, GS_in, GS_out, Action, 12) :-
	Pop = ("Skill Test", 2, Skill, Difficulty, ChaosBag),
	game_state_get(["Stack", "CurrInv", "Invs"], GS_in, [[Pop | Stack], I, Is]),
	% get investigator cards
	inv_get(["Id", "Loc", "Hand"], I, [Id, LocId, Hand]),
	% choose any combination of cards
	choose_any(Hand, Commit, Hand1),
	exclude(can_commit(Skill), Commit, []),
	inv_set("Hand", Hand1, I, I1),
	% get list of lists of appropriate skill/commit cards from other players
	% all players except me, and at my location
	exclude(inv_filter("Id", Id), Is, Is1),
	include(inv_filter("Loc", LocId), Is1, Neighbors),
	maplist(inv_get("Id"), Neighbors, NeighIds),
	maplist(inv_get("Hand"), Neighbors, NeighHands),
	% choose 0 or 1 card(s) from each other player
	maplist(choose_max_map(1), NeighHands, NeighCommitNotCommit),
	pair_lists(NeighCommit, NeighNotCommit, NeighCommitNotCommit),
	% make sure they're all applicable
	maplist(exclude(can_commit(Skill)), NeighCommit, Empty), 
	flatten(Empty, []),
	% update investigator hands
	pair_lists(Neighbors, NeighNotCommit, NeighHands1),
	maplist(inv_set_map("Hand"), NeighHands1, Neighbors1),
	game_state_set(["SubInvs", "Inv"], [Neighbors1, I1], GS_in, GS_tmp),
	% commit chosen
	flatten(NeighCommit, NeighCommitFlat),
	append(NeighCommitFlat, Commit, AllCommit),
	pair_lists(NeighIds, NeighCommit, NeighCommitted),
	PsCommit = [(Id,Commit) | NeighCommitted],
	% Push next step
	Push = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	game_state_set("Stack", [Push | Stack], GS_tmp, GS_out),
	term_string(PsCommit, StrCommit),
	string_builder(["Commit cards to skill test: ", StrCommit], Str),
	Action = [Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.3: Reveal chaos token
%
% The investigator performing the skill test reveals one chaos token at random from the chaos 
% bag.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Skill Test, 3
single_step(12, GS_in, GS_out, Action, 13) :-
	Pop = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	% Reveal Chaos Token
	choose(ChaosBag, Token, ChaosBag1),
	% 2. Push next token
	Push = ("Skill Test", 4, Skill, Difficulty, ChaosBag1, AllCommit, PsCommit, Token),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Drew Token: ", Token], Str),
	Action = [Str].

%%% Skill Test, 3
% 1005, Wendy Adams
% -r->
% When you reveal a chaos token, choose and discard 1 card from your hand:
% Cancel that chaos token and return it to the bag. Reveal a new chaos token.
% (Limit once per test/ability)
single_step(12, GS_in, GS_out, Action, 12) :-
	Pop = ("Skill Test", 3, _, _, ChaosBag, _, _),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop|_], I]),
	% Reveal Chaos Token
	choose(ChaosBag, Token, _),
	% You are Wendy Adams
	inv_get(["Id", "Hand", "Discard", "Limits"], I, [1005, Hand, Discard, Limits]),
	% haven't used this reaction yet this test
	\+in(("Test", 1005), Limits),
	% discard 1 card
	choose(Hand, Card, Hand1),
	inv_set(["Hand", "Discard", "Limits"], [Hand1, [Card | Discard], [("Test", 1005) | Limits]], I, I1),
	game_state_set("Inv", I1, GS_in, GS_out),
	string_builder(["Drew Token: ", Token, ", -r-> Wendy Adams (Discard ", Card, "). Reveal new chaos token"], Str),
	Action = [Str].

% TODO
% 1071, Grotesque Statue
% -r->
% When you would reveal a chaos token, spend 1 charge:
% Reveal 2 chaos tokens instead of 1.  Choose 1 of those tokens to resolve, and ignore the other.

% TODO: Figure out better way to apply Forced effects
% 1009, The Necronomicon: John Dee Translation
% -F->
% Treat each ElderSign you reveal on a chaos token as an AutoFail
% REDCUT
single_step(13, GS_in, GS_out, Action, 13) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get("Threats", I, Threats),
	in((1009,_), Threats),
	!,
	Push = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail"),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["The Necronomicon: ElderSign -F-> AutoFail"].

% TODO: Encode proper fight token
% Technically don't need Pop1 because it 1060 can only be activated for Fight
% 1060, Shrivelling
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this attack, 
% take 1 horror
% REDCUT
single_step(13, GS_in, GS_out, Action, 13) :-
	Pop0 = ("Skill Test", 4, _, _, _, _, _, Token),
	Pop1 = ("Fight"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | Stack], I]),
	in(Token, ["Skull", "Cultist", "Tablet", "ElderThing", "AutoFail"]),
	inv_get(["Active", "Limits"], I, [Active, Limits]),
	in(1060, Active),
	\+in(("Action", 1060), Limits),
	!,
	inv_set("Limits", [("Action", 1060) | Limits], I, I1),
	Push0 = Pop1,
	Push1 = Pop0,
	Push2 = ("Assign Affliction", 0, 1),
	game_state_set(["Stack", "Inv"],[[Push2, Push1, Push0 | Stack], I1], GS_in, GS_out),
	Action = ["Shrivelling -F-> take 1 horror"].

% TODO: Encode proper evade token
% Technically don't need Pop1 because it 1066 can only be activated for Evade
% 1066, Blinding Light
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this evasion 
% attempt, lose 1 action this turn.
% REDCUT
single_step(13, GS_in, GS_out, Action, 13) :-
	Pop0 = ("Skill Test", 4, _, _, _, _, _, Token),
	Pop1 = ("Evade"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | _], I]),
	in(Token, ["Skull", "Cultist", "Tablet", "ElderThing", "AutoFail"]),
	inv_get(["Active", "Limits", "Resources"], I, [Active, Limits, Res]),
	in(1066, Active),
	\+in(("Action", 1066), Limits),
	!,
	Res0 is Res - 1, 
	max(Res0, 0, Res1),
	inv_set(["Limits", "Resources"], [[("Action", 1066) | Limits], Res1], I, I1),
	game_state_set("Inv", I1, GS_in, GS_out),
	Action = ["Blinding Light -F-> lose 1 action"].

% TODO: Proper evade token, even though not necessary
% 1069, Blinding Light
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this evasion 
% attempt, lose 1 action this turn and take 1 horror.
% REDCUT
single_step(13, GS_in, GS_out, Action, 8) :-
	Pop0 = ("Skill Test", 4, _, _, _, _, _, Token),
	Pop1 = ("Evade"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | Stack], I]),
	in(Token, ["Skull", "Cultist", "Tablet", "ElderThing", "AutoFail"]),
	inv_get(["Active", "Limits", "Resources"], I, [Active, Limits, Res]),
	in(1069, Active),
	\+in(("Action", 1069), Limits),
	!,
	Res0 is Res - 1, 
	max(Res0, 0, Res1),
	inv_set(["Limits", "Resources"], [[("Action", 1069) | Limits], Res1], I, I1),
	Push0 = Pop1,
	Push1 = Pop0,
	Push2 = ("Assign Affliction",0,1),
	game_state_set(["Stack", "Inv"], [[Push2, Push1, Push0 | Stack], I1], GS_in, GS_out),
	Action = ["Blinding Light -F-> lose 1 action and take 1 horror"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.4: Apply chaos symbol effect(s)
%
% Apply any effects initiated by the symbol on the revealed chaos token. Each of the following
% symbols indicates that an ability on the scenario reference card must initiate:
% - Skull
% - Cultist
% - Tablet
% - ElderThing
%
% The ElderSign symbol indicates that the ElderSign ability on the investigator card belonging
% to the player performing the test must initiate.
%
% If none of the above symbols are revealed, or if the icon has no corresponding ability, this
% step completes with no effect
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% token drawn has no corresponding ability (i.e, just a numbered token)
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	in(Token, ["+1","0","-1","-2","AutoFail"]),!,
	token_value(Token, TokenVal),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Token: ", Token, " has no additional effect."], Str),
	Action = [Str].

% TODO: handle 1103
% TODO: Make sure eldersign abilities complete correctly

%%% ElderSign
% Roland Banks (1001)
% +1 for each clue on your location
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Id", "Loc"], I, [1001, LocId]),!,
	game_state_get("Loc", LocId, GS_in, Loc),
	loc_get("Clues", Loc, Clues),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", Clues),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Token: ElderSign has the value +", Clues], Str),
	Action = [Str].

%%% ElderSign
% Dasiy Walker (1002)
% +0. If you succeed, draw 1 card for each Tome you control
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Id", "Active"], I, [1002, Active]),!,
	inv_set("Active", [("SkillTest", "ElderSign") | Active], I, I1),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 0),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I1], GS_in, GS_out),
	Action = ["Token: ElderSign has the value +0"].

%%% ElderSign
% "Skids" O'Toole (1003)
% +2. If you succeed, gain 2 resources
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Id", "Active"], I, [1003, Active]),!,
	inv_set("Active", [("SkillTest", "ElderSign") | Active], I, I1),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 2),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I1], GS_in, GS_out),
	Action = ["Token: ElderSign has the value +2"].

%%% ElderSign
% Agnes Baker (1004)
% +1 for each horror on Agnes Baker
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Id", "Horror"], I, [1004, Horror]),!,
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", Horror),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Token: ElderSign has the value ", Horror], Str),
	Action = [Str].

%%% ElderSign
% Wendy Adams (1005)
% +0. If Wendy's Amulet is in play, you automatically succeed instead
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get(["Id", "Assets"], I, [1005, Assets]),!,
	((in((1104,_), Assets),
		Push = ("Skill Test", 5, Skill, 0, ChaosBag, AllCommit, PsCommit, "ElderSign", 0))
	;(\+in((1104,_), Assets),
		Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 0))),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Token: ElderSign has the value +0"].

% Scenario: The Gathering
%   Difficulty: Easy/Standard
%   -   Skull: -X. X is the number of Ghoul enemies at your location.
%   - Cultist: -1. If you fail, take 1 horror.
%   -  Tablet: -2. If there is a Ghoul enemy at your location, take 1 damage
%   Difficulty: Hard/Expert
%   -   Skull: -2. If you fail, after this skill test, search the encounter deck and discard
%                  pile for a Ghoul enemy, and draw it. Shuffle the encounter deck.
%   - Cultist: Reveal another token. If you fail, take 2 horror.
%   -  Tablet: -4. If there is a Ghoul enemy at your location, take 1 damage and 1 horror.

%%% Special Token
% Easy/Standard: Skull 
% -X. X is the number of Ghoul enemies at your location.
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop | Stack], (1104, Diff)]),
	in(Diff, ["Easy","Standard"]),!,	
	% count number of ghoul enemies at my location
	game_state_get(["CurrInv","Enemies"], GS_in, [I, Es]),
	inv_get("Loc", I, LocId),
	include(enemy_filter("Loc", LocId), Es, Es1),
	include(enemy_filter("Type", "Ghoul"), Es1, Es2),
	length(Es2, N),
	X is 0 - N,
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull", X),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder(["Token: Skull has the value: ", X], Str),
	Action = [Str].

%%% Special Token
% Easy/Standard: Cultist 
% -1. If you fail, take 1 horror.
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop | Stack], (1104, Diff)]),
	in(Diff, ["Easy","Standard"]),!,
	game_state_get("CurrInv", GS_in, I),
	inv_get("Active", I, Active),
	inv_set("Active", [("Cultist", (1104, Diff)) | Active], I, I1),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist", -1),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I1], GS_in, GS_out),
	Action = ["Token: Cultist has value -1"].

%%% Special Token
% Easy/Standard: Tablet 
% -2. If there is a Ghoul enemy at your location, take 1 damage
single_step(13, GS_in, GS_out, Action, Next) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop | Stack], (1104, Diff)]),
	in(Diff, ["Easy","Standard"]),!,
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet", -2),
	% count number of ghoul enemies at my location
	game_state_get(["CurrInv", "Enemies"], GS_in, [I, Es]),
	inv_get("Loc", I, LocId),
	include(enemy_filter("Loc", LocId), Es, Es1),
	include(enemy_filter("Type","Ghoul"), Es1, Es2),
	length(Es2, N),
	((N = 0, 
		game_state_set("Stack", [Push | Stack], GS_in, GS_out), 
	    Str = "",
	    Next = 14)
	;(N @> 0,
		Push1 = ("Assign Affliction",1,0),
		game_state_set("Stack", [Push1, Push | Stack], GS_in, GS_out),
		Str = " Also take 1 damage.",
		Next = 8)),
	string_builder(["Token: Tablet has value -2.", Str], Str1),
	Action = [Str1].

%%% Special Token
% Hard/Expert: Skull 
% -2. If you fail, after this skill test, search the encounter deck and discard pile 
% for a Ghoul enemy, and draw it. Shuffle the encounter deck.
single_step(13, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop|Stack], (1104, Diff)]),
	in(Diff, ["Hard", "Expert"]),
	game_state_get("CurInv", GS_in, I),
	inv_get("Active", I, Active),
	inv_set("Active", [("Skull", (1104, Diff)) | Active], I, I1),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull", -2),
	game_state_set(["Stack", "Inv"],[[Push | Stack], I1], GS_in, GS_out),
	Action = ["Token: Skull has value -2"].

%%% Special Token
% Hard/Expert: Cultist
% Reveal another token. If you fail, take 2 horror.
single_step(13, GS_in, GS_out, Action, 12) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop|Stack], (1104, Diff)]),
	in(Diff, ["Hard", "Expert"]),
	game_state_get("CurInv", GS_in, I),
	inv_get("Active", I, Active),
	inv_set("Active", [("Cultist", (1104, Diff)) | Active], I, I1),
	Push = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I1], GS_in, GS_out),
	Action = ["Token: Cultist --> reveal another token"].

%%% Special Token
% Hard/Expert: Tablet
% -4. If there is a Ghoul enemy at your location, take 1 damage and 1 horror.
single_step(13, GS_in, GS_out, Action, Next) :-
	Pop = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet"),
	game_state_get(["Stack", "Scenario"], GS_in, [[Pop|Stack], (1104, Diff)]),
	in(Diff, ["Hard", "Expert"]),
	game_state_get(["CurInv", "Enemies"], GS_in, [I, Es]),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet", -4),
	% count number of ghoul enemies at my location
	inv_get("Loc", I, LocId),
	include(enemy_filter("Loc", LocId), Es, Es1),
	include(enemy_filter("Type", "Ghoul"), Es1, Es2),
	length(Es2, N),
	((N = 0, 
		game_state_set("Stack", [Push | Stack], GS_in, GS_out), 
	    Str = "",
	    Next = 14)
	;(N @> 0,
		Push0 = ("Assign Affliction",1,1),
		game_state_set("Stack", [Push0, Push | Stack], GS_in, GS_out),
		Str = " Also take 1 damage and 1 horror.",
		Next = 8)),
	string_builder(["Token: Tablet has value -4.", Str], Str1),
	Action = [Str1].

% TODO: anytime a card is played, need to check if it is allowed: 1165
% 1056, Sure Gamble
% -p->
% Play after you reveal a chaos token with a negative modifier
% Switch that token's "-" to a "+"
single_step(14, GS_in, GS_out, Action, 14) :-
	Pop = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	TokenVal @< 0,
	game_state_get("CurrInv", GS_in, I),
	inv_get("Hand", I, Hand),
	choose(Hand, 1056, Hand0),
	inv_get("Resources", I, Res),
	Res @>= 2,
	Res0 is Res - 2,
	inv_get("Discard", I, Discard),
	TokenVal0 is TokenVal * -1,
	inv_set(["Resources", "Hand", "Discard"], [Res0, Hand0, [1056 | Discard]], I, I0),
	Push = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal0),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I0], GS_in, GS_out),
	string_builder(["Fast. Play Sure Gamble. Token Value now: ", TokenVal0], Str),
	Action = [Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.5: Determine investigator's modified skill value.
%
% Start with the base skill (of the skill that matches the type of test that is resolving) of 
% the investigator performing this test, and apply all active modifiers, including the
% appropriate icons that have been committed to this test, effects of the chaos token(s)
% revealed, and all active card abilities that are modifying the investigator's skill value.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(14, GS_in, GS_out, Action, 15) :-
	Pop = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Token \= "AutoFail",
	game_state_get("CurrInv", GS_in, I),
	% 1. Base Skill Value
	inv_get(Skill, I, BaseVal),
	% 2. Committed Value
	committed_skill_value(Skill, AllCommit, CommitVal),
	% 3. Active Card Abilities Value
	inv_get("Active", I, Active),
	choose(Active, ("SkillBonus", ActiveVal), Active0),
	inv_set("Active", Active0, I, I0),
	% 4. Passive Card Abilities Value
	% Look for 'external' contributors: 1098, 1117 
	passive_skill_value(Skill, GS_in, PassVal),
	% 5. Sum them up
	ModifiedValue is BaseVal + CommitVal + ActiveVal + PassVal + TokenVal,
	Push = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedValue),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I0], GS_in, GS_out),
	string_builder(["Modified Skill Value: ", ModifiedValue], Str),
	Action = [Str].

single_step(14, GS_in, GS_out, Action, 15) :-
	Pop = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", _),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Push = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", 0),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Modified Skill Value: 0"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.6: Determine success/failure of skill test.
%
% Compare the investigator's modified skill value to the difficulty of the skill test.  If the
% investigator's skill value equals or exceeds the difficulty for this test (as indicated by 
% the card or game mechanic invoking the test), the investigator succeeds at the test.
% - If an investigator automatically succeeds at a test via a card ability, the total difficulty
%   of that test is considered 0
% If the investigator's skill value is less than the difficulty for this test, the investigator
% fails at the test.
% - If an investigator automatically fails at a test via a card ability or revealing the 
%   AutoFail symbol, their total skill value for that test is considered 0.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Skill Test, 6
% Success
single_step(15, GS_in, GS_out, Action, 16) :-
	Pop = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Token \= "AutoFail",
	ModifiedVal @>= Difficulty,
	SumVal is ModifiedVal - Difficulty,
	Push = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Success", SumVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Success"].

%%% Skill Test, 6
% Fail
single_step(15, GS_in, GS_out, Action, 16) :-
	Pop = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	Token \= "AutoFail",
	ModifiedVal @< Difficulty,
	SumVal is ModifiedVal - Difficulty,
	Push = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Fail", SumVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Fail"].

%%% Skill Test, 6
% AutoFail always fails
single_step(15, GS_in, GS_out, Action, 16) :-
	Pop = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", 0),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	SumVal is 0 - Difficulty,
	Push = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, "AutoFail", "Fail", SumVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Fail"].
	
% 1080, Lucky!
% -p->
% Play when you would fail a skill test.  Get +2 to your skill value for that test.
% Note: if AutoFail was drawn, then you don't get +2
%       should this even really be allowed to be played then?

single_step(15, GS_in, GS_out, Action, 15) :-
	Pop = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	ModifiedVal @< Difficulty,
	% play Lucky!
	inv_get("Hand", I, Hand),
	choose(Hand, 1080, Hand0),
	inv_get("Resources", I, Res),
	Res @>= 1, Res0 is Res - 1,
	inv_get("Discard", I, Discard),
	inv_set(["Hand", "Resources", "Discard"], [Hand0, Res0, [1080 | Discard]], I, I1),
	((Token \= "AutoFail",
		ModifiedVal0 is 2 + ModifiedVal)
	;(Token = "AutoFail",
		ModifiedVal0 = ModifiedVal)),
	Push = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal0),
	game_state_set(["Stack", "Inv"], [[Push | Stack], I1], GS_in, GS_out),
	Action = ["Fast. Play Lucky! (1080)"].

% 1084, Lucky!
% -p->
% Play when you would fail a skill test.  
%   Get +2 to your skill value for that test.
%   Draw 1 card.
% Note: if AutoFail was drawn, then you don't get +2

single_step(15, GS_in, GS_out, Action, 17) :-
	Pop = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),	
	ModifiedVal @< Difficulty,
	% play Lucky!
	inv_get("Hand", I, Hand),
	choose(Hand, 1084, Hand0),
	inv_get("Resources", I, Res),
	Res @>= 1, Res0 is Res - 1,
	inv_get("Discard", I, Discard),
	inv_set(["Hand", "Resources", "Discard"], [Hand0, Res0, [1084 | Discard]], I, I1),
	((Token \= "AutoFail",
		ModifiedVal0 is 2 + ModifiedVal)
	;(Token = "AutoFail",
		ModifiedVal0 = ModifiedVal)),
	Push0 = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal0),
	Push1 = "Draw Card",
	game_state_set(["Stack", "Inv"],[[Push1, Push0 | Stack], I1], GS_in, GS_out),
	Action = ["Fast. Play Lucky! (1084)"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.7: Apply skill test results
%
% The card ability or game rule that initiated a skill test usually indicates the consequences
% of success and/or failure for that test. Additionally, some other card abilities may 
% contribute additional consequences, or modify existing consequences, at this time. Resolve
% the appropriate consequences (based on the success or failure established during ST.6) at this
% time.  If there are multiple results to be applied during this step, the investigator 
% performing the test applies those results in the order of his or her choice.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Investigate Location, Success
% Discover clues at your location
single_step(16, GS_in, GS_out, Action, 18) :-
	Pop0 = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Success", SumVal),
	Pop1 = ("Investigate", ("Location", LocId), false),
	game_state_get("Stack", GS_in, [Pop0, Pop1 | Stack]),
	bonus_clues_total(AllCommit, N, AllCommit0),
	NClues is 1 + N,
	Push0 = ("Investigate", ("Location", LocId), true),
	Push1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit0, PsCommit, Token, "Success", SumVal),
	Push2 = ("Discover Clues", LocId, NClues),
	game_state_set("Stack", [Push2, Push1, Push0 | Stack], GS_in, GS_out),
	string_builder(["Discover ", NClues, " clue(s)."], Str),
	Action = [Str].

% Investigate Location, Fail
% Don't discover any clues
single_step(16, GS_in, GS_out, Action, 16) :-
	Pop0 = ("Skill Test", 7, _, _, _, _, _, "Fail", _),
	Pop1 = ("Investigate", ("Location", LocId), false),
	game_state_get("Stack", GS_in, [Pop0, Pop1 | Stack]),
	Push0 = ("Investigate", ("Location", LocId), true),
	Push1 = Pop0,
	game_state_set("Stack", [Push1, Push0 | Stack], GS_in, GS_out),
	Action = ["Didn't discover any clues."].

% Investigate Location
% Finish applying skill test results
single_step(16, GS_in, GS_out, Action, 19) :-
	Pop0 = ("Skill Test", 7, _, _, [], PsCommit, _, _, _),
	Pop1 = ("Investigate", _, true),
	game_state_get("Stack", GS_in, [Pop0, Pop1 | Stack]),
	Push = ("Skill Test", 8, PsCommit),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Done applying results"].

% 1033, Dr. Milan Christopher
% -r->
% After you successfully investigate: Gain 1 resouce
single_step(16, GS_in, GS_out, Action, 11) :-
	Pop0 = ("Skill Test", 7, _, _, _, _, _, "Success", _),
	Pop1 = ("Investigate", _, _),	
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | Stack], I]),
	inv_get("Assets", I, Assets),
	Card = ((1033, UId), false, _),
	in(Card, Assets),
	inv_get("Limits", I, Limits),
	\+in(("Action", (1033, UId)), Limits),
	inv_set("Limits", [("Action", (1033, UId)) | Limits], I, I1),
	Push0 = Pop1,
	Push1 = Pop0,
	Push2 = ("Gain Resources", 1),
	game_state_set(["Stack", "Inv"], [[Push2, Push1, Push0 | Stack], I1], GS_in, GS_out),
	Action  = ["-r-> Dr. Milan Christopher."].

% 1045, Burglary
% Investigate, if you succeed, instead of discovering clues, gain 3 resources.
single_step(16, GS_in, GS_out, Action, 11) :-
	Pop0 = ("Skill Test", 7, _, _, _, _, _, "Success", _),
	Pop1 = ("Investigate", ("Special", 1045), false),	
	game_state_get("Stack", GS_in, [Pop0, Pop1 | Stack]),
	Push0 = ("Investigate", ("Special", 1045), true),
	Push1 = Pop0,
	Push2 = ("Gain Resources", 3),
	game_state_set("Stack", [Push2, Push1, Push0 | Stack], GS_in, GS_out),
	string_builder(["Gain 3 resources."], Str),
	Action = [Str].

% 1073, Scavenging
% -r->
% After you successfully investigate by 2 or more,
% exhaust Scavenging:
% Choose an Item card in your discard pile and add it to your hand
single_step(16, GS_in, GS_out, Action, 16) :-
	Pop0 = ("Skill Test", 7, _, _, _, _, _, "Success", N),
	Pop1 = ("Investigate", _, _),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | _], I]),
	N @>= 2,
	inv_get("Assets", I, Assets),
	Card = ((1073, UId), false, Ts),
	choose(Assets, Card, Assets0),
	inv_get("Discard", I, Discard),
	choose(Discard, Item, Discard0),
	has_type("Item", Item),
	inv_get("Hand", I, Hand),
	% TODO: insert assets in order
	% TODO: insert hand in order
	inv_set(["Assets", "Discard", "Hand"], [[((1073, UId), true, Ts) | Assets0], Discard0, [Item | Hand]], I, I1),
	game_state_set("Inv", I1, GS_in, GS_out),
	card_get("Name", Item, IName),
	string_builder(["-r-> Scavenging, adding ", IName, " to hand."], Str),
	Action = [Str].

% 1168, Obscuring Fog
% -F->
% After attached location is successfully investigated:
% Discard Obscuring Fog
% TODO: How to force events
single_step(16, GS_in, GS_out, Action, 16) :-
	Pop0 = ("Skill Test", 7, _, _, _, _, _, "Success", _),
	Pop1 = ("Investigate", _, _),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop0, Pop1 | _], I]),
	inv_get("Loc", I, LocId),
	game_state_get("Loc", LocId, GS_in, Loc),
	loc_get("Attached", Loc, Attached),
	choose(Attached, 1168, Attached0),
	loc_set("Attached", Attached0, Loc, Loc0),
	game_state_get("EncDiscard", GS_in, EncDiscard),
	game_state_set(["Loc", "EncDiscard"], [Loc0, [1168 | EncDiscard]], GS_in, GS_out),
	Action = ["-F-> Discard Obscuring Fog."].

%%%%%%%% Skill Card effects
% This is only for things like, draw 1 card, and not for bonuses that increase the impact
% of the skill test initiating action.

% TODO: make this one single large step and not multiple small ones
% Skip non-skill cards
single_step(16, GS_in, GS_out, Action, 16) :-
	Pop = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, PassFail, SumVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	choose(AllCommit, Card, AllCommit0),
	\+card_get("Kind", Card, "Skill"),
	Push = ("Skill Test", 7, Skill, ChaosBag, AllCommit0, PsCommit, Token, PassFail, SumVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	string_builder([Card," is not a skill card, continue."], Str),
	Action = [Str].

% 1039, Deduction
% Fail skill test, it has no effect.
single_step(16, GS_in, GS_out, Action, 16) :-
	Pop = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Fail", SumVal),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	choose(AllCommit, 1039, AllCommit0),
	Push = ("Skill Test", 7, Skill, ChaosBag, AllCommit0, PsCommit, Token, "Fail", SumVal),
	game_state_set("Stack", [Push | Stack], GS_in, GS_out),
	Action = ["Failed skill test, 1039 has no effect"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.8 Skill test ends.
%
% This step formalizes the end of this skill test. Discard all cards that were committed to this
% skill test, and return all revealed chaos tokens to the chaos bag.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(19, GS_in, GS_out, Action, -1) :-
	Pop = ("Skill Test", 8, PsCommit),
	game_state_get("Stack", GS_in, [Pop | Stack]),
	foldl(discard_committed, PsCommit, GS_in, GS_tmp),
	game_state_set("Stack", Stack, GS_tmp, GS_out),
	Action = ["Skill test ends."].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Discover Clues %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% If an investigator discovers a clue, they take the clue from the location and place it on
% their investigator card, under their control.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(18, GS_in, GS_out, Action, -1) :-
	Pop = ("Discover Clues", LocId, NClues),
	game_state_get(["Stack", "CurrInv", ("Loc",LocId)], GS_in, [[Pop | Stack], I, Loc]),
	loc_get("Clues", Loc, LocClues),
	inv_get("Clues", I, InvClues),
	min(NClues, LocClues, Clues),
	LocClues0 is LocClues - Clues,
	InvClues0 is InvClues + Clues,
	loc_set("Clues", LocClues0, Loc, Loc0),
	inv_set("Clues", InvClues0, I, I0),
	game_state_set(["Stack", "Inv", "Loc"], [Stack, I0, Loc0], GS_in, GS_out),
	string_builder(["Gained ", Clues, " clue(s)."], Str),
	Action = [Str].

% 1007, Cover Up
% -r->
% When you would discover 1 or more clues at your location:
% Discard that many clues from Cover Up instead.
% Only really need to do this if there are 1 or more clues

single_step(18, GS_in, GS_out, Action, -1) :-
	Pop = ("Discover Clues", LocId, NClues),
	game_state_get(["Stack", "CurrInv", ("Loc",LocId)], GS_in, [[Pop | Stack], I, Loc]),
	loc_get("Clues", Loc, LocClues),
	min(NClues, LocClues, Clues),
	Clues @> 0,
	inv_get("Threats", I, Threats),
	choose(Threats, ((1007,UId),Tokens), Threats0),
	choose(Tokens, ("Clues", CardClues), Tokens0),
	CardClues @> 0,
	min(Clues, CardClues, RemoveClues),
	CardClues0 is CardClues - RemoveClues,
	inv_set("Threats", [((1007, UId), [("Clues", CardClues0) | Tokens0]) | Threats0], I, I0),
	game_state_set(["Stack", "Inv"], [Stack, I0], GS_in, GS_out),
	string_builder(["Discarded ", Clues, " clue(s) from Cover Up."], Str),
	Action = [Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gain Resource %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% An investigator gains a number of resources
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(11, GS_in, GS_out, Action, -1) :-
	Pop = ("Gain Resources", N),
	game_state_get(["Stack", "CurrInv"], GS_in, [[Pop | Stack], I]),
	inv_get("Resources", I, Res),
	Res0 is Res + N,
	inv_set("Resources", Res0, I, I0),
	game_state_set(["Stack", "Inv"], [Stack, I0], GS_in, GS_out),
	string_builder(["Gained ", N, " resource(s)."], Str),
	Action = [Str].

% 1039, Deduction
% If this skill test is successful while investigating a location,
% discover 1 additional clue at that location.
bonus_clues(1039,1).
bonus_clues_sum(C,(N,L),(N1,L)) :-
	bonus_clues(C,M),
	N1 is N + M.
bonus_clues_sum(C,(N,L),(N,[C|L])) :-
	\+bonus_clues(C,_).
bonus_clues_total(L,N,L1) :- foldl(bonus_clues_sum,L,(0,[]),(N,L1)).

run_test(Test,Action) :-
	sample_gs(Test,GS),
	single_step(GS,_,Action).

run_test(N,Test,Actions) :-
	sample_gs(Test,GS),
	step_n(N,GS,_,Actions).

eq(X,X).

token_value("+1",1).
token_value("0",0).
token_value("-1",-1).
token_value("-2",-2).
token_value("AutoFail",0).

passive_skill("Fight",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1018,_),_,_),_).
passive_skill("Will",   GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1027,_),_,_),_).
passive_skill("Fight",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1028,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1030,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1033,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1040,_),_,_),_).
passive_skill("Agility",GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1055,_),_,_),_).
passive_skill("Will",   GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1059,_),_,_),_).
passive_skill("Fight",  GS,1) :- 
	% if an investigator at my location (including me) controls Lita Chantler
	game_state_get(["CurrInv","Invs"],GS,[I,Is]),
	inv_get("Loc",I,LId),
	include(inv_filter("Loc",LId),Is,Is1),
	maplist(inv_get("Assets"),Is1,AllAssets),
	flatten(AllAssets,FlatAssets),
	choose(FlatAssets,((1117,_),_,_),_).

passive_skill(_,GS,-1) :- game_state_get("CurrInv",GS,I), inv_get("Threats",I,Threats), choose(Threats,((1098,_),_),_).
passive_skill(_,_,0).

passive_skill_value(Skill,GS,V) :- aggregate(sum(X), passive_skill(Skill,GS,X), V).

stuck_states(N,State,GS1) :- 
	sample_gs(State,GS), 
	step_n(N,GS,GS1,_), 
	\+game_state_get("Stack",GS1,[]),
	\+step_n(1,GS1,_,_).
stuck_state(State,GS) :-
	sample_gs(State,G),
	step_all(G,GS,_),
	\+game_state_get("Stack",GS,[]).

discard_committed((InvId,Cards),GS,GSN) :-
	game_state_get("Inv",InvId,GS,I),
	inv_get("Discard",I,D),
	append(Cards,D,D1),
	inv_set("Discard",D1,I,I1),
	game_state_set("Inv",I1,GS,GSN).

insert_card(Card,[],[Card]).
insert_card(Card,[X|L],[Card,X|L]) :-
	Card @=< X.
insert_card(Card,[X|L],[X|L1]) :-
	Card @> X,
	insert_card(Card,L,L1).

step_all(_,GS,GS,[]) :- 
	\+single_step(_,GS,_,_,_).
step_all(-1,GS,GSN,[A|L]) :- 
	single_step(_,GS,GS1,A,N),
	step_all(N,GS1,GSN,L).
step_all(M,GS,GSN,[A|L]) :- 
	M \= -1,
	single_step(M,GS,GS1,A,N),
	step_all(N,GS1,GSN,L).

step_action(N,GS,GSN,A) :-
	step_all(N,GS,GS1,A),
	game_state_get("Stack",GS1,["End Action"|Stack]),
	game_state_set("Stack",["Next Action"|Stack],GS1,GSN).
