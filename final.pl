% Modeling the Game Clue Using Answer Set Programming
%
% --------------------------------------------------------------------------
% KRR Methodology:
% This project uses the generate-and-test methodology combined with constraint satisfaction
% to solve a combinatorial deduction problem based on the game Clue (Cluedo).
%
% 1) Generate: Possible card assignments (which players hold which cards, and which cards are in the case file).
% 2) Test/Constraint: Observations are converted into logical constraints or negative facts, which prune
%    impossible models.
% 3) Define/Deduce: Rules are used to propogate knowledge, leading to a set of consistent answer sets.
% --------------------------------------------------------------------------
%
% INPUT PREDICATES AND DESCRIPTIONS:
% player(P): Defines P as a player in the game.
% pos(P,I): Defines the clockwise position I (0 to n-1, clockwise) of player P.
% suspect(S), weapon(W), room(R): Define the possible cards in each category.
% suggestion(E,A,Sus,Wp,Rm): Event E where asker A suggests the combination of Sus, Wp, and Rm.
% responder(E,R): Indicates that player R responded to suggestion E. (Implies R showed *a* card).
% showed_card(E,R,C): The specific card C shown by responder R during event E (if known).
%
% OUTPUT PREDICATES AND DESCRIPTIONS:
% case(C): A card C is in the confidential case file (the solution).
% deduced_case(C,Reason): A card C is logically forced into the case file for the given Reason (e.g., 'no_one_has').
% deduced_not_have(P,C,Reason): It is logically certain that P does not hold card C for the given Reason (e.g., 'from_suggestion(E)').


% --- SETUP & CARD COUNT ---

% Player count N
n(N) :- N = #count{ P : player(P) }.

% Rules defining all cards
card(S) :- suspect(S).
card(W) :- weapon(W).
card(R) :- room(R).

% Calculate total cards in the game (T)
total_cards(T) :- T = #count{ C : card(C) }.

% Calculate the number of cards NOT in the case file (D = T - 3)
distributed_cards(D) :- total_cards(T), D = T - 3.

% Calculate the hand size (S) for each player (assuming equal division)
hand_size(S) :- distributed_cards(D), n(N), N > 0, S = D / N, D \ N = 0.

% Enforce that every player must hold exactly S cards
{ holds(P, C) : card(C) } = S :- player(P), hand_size(S).

% Helper: suggested(E,X) lists the three cards in a suggestion
suggested(E,S) :- suggestion(E,_,S,_,_), suspect(S).
suggested(E,W) :- suggestion(E,_,_,W,_), weapon(W).
suggested(E,Rm) :- suggestion(E,_,_,_,Rm), room(Rm).


% --- CARD DISTRIBUTION GENERATION ---

% Every card must be in the case file or held by exactly one player
{ case(C) ; holds(P,C) : player(P) } = 1 :- card(C).

% A card cannot be both in the case file and held by a player
:- case(C), holds(P,C).

% Exactly one suspect, one weapon, and one room are in the case file
{ case(S) : suspect(S)} = 1.
{ case(W) : weapon(W)} = 1.
{ case(R) : room(R)} = 1.


% --- DISTANCE AND PASSING  ---

% Clockwise distance (0..n-1) from From to To
dist(From, To, D) :- n(N), pos(From, PF), pos(To, PT), Temp = (PT - PF) + N, D = Temp \ N.

% A responder R is observed
% Players P between the asker A and responder R passed
passed(E, P) :- suggestion(E, A, _, _, _), responder(E, R), dist(A, P, Dp), dist(A, R, Dr), Dp > 0, Dp < Dr.

% No responder is observed
% Every player P other than the asker A must have passed
passed(E, P) :- suggestion(E, A, _, _, _), player(P), A != P, not responder(E, _).


% --- OBSERVATION CONSTRAINTS ---

% If a player passed, they cannot hold any of the suggested cards
:- passed(E,P), suggested(E,C), holds(P,C).

% The observed responder R MUST hold at least one suggested card C that is not in the case file
:- responder(E,R), not 1 { holds(R,C) : suggested(E,C), not case(C) }.

% If there is a responder, not all 3 suggested cards can be in the case file
:- responder(E,_), #count{ C : suggested(E,C), case(C) } = 3.

% If no one responded, ALL suggested cards must be in the case file
:- suggested(E,C), not responder(E,_), not case(C).


% --- DEDUCTION AND KNOWLEDGE PROPAGATION ---

% Records the reason why a player does not have a card
deduced_not_have(P,C,reason(passed_event(E))) :- passed(E,P), suggested(E,C), not holds(P,C).

% If a card C is not held by any player in the model, C MUST be in the case file.
no_player_possible(C) :- card(C), not holds(_,C), not case(C).
:- no_player_possible(C), not case(C).


% Records the reason why a card was forced into the case file (for explanation)
deduced_case(C1, reason(no_response_to_suggestion(E))) :- suggestion(E, _, C1, _, _), not responder(E, _), case(C1).
deduced_case(C2, reason(no_response_to_suggestion(E))) :- suggestion(E, _, _, C2, _), not responder(E, _), case(C2).
deduced_case(C3, reason(no_response_to_suggestion(E))) :- suggestion(E, _, _, _, C3), not responder(E, _), case(C3).

% Records the reason why a player does not have a card
deduced_not_have(P,C,reason(passed_event(E))) :- passed(E,P), suggested(E,C), not holds(P,C).


% --- OUTPUT ---
%#show holds/2.
#show case/1.
#show deduced_case/2.
#show deduced_not_have/3.