% -------------------------------------------------
% TEST 2
% This test case is designed to validate that once a suggestion is made
% and no one can respond, that the suggested cards can be correctly deduced
% to be in the case file.
%
% Setup: 4 players, we are p4, 2 cards per hand
% -------------------------------------------------

% Players and Positions
player(p1). player(p2). player(p3). player(p4).
pos(p1,0). pos(p2,1). pos(p3,2). pos(p4,3).

% Cards
suspect(green). suspect(plum). suspect(scarlet). suspect(mustard).
weapon(knife). weapon(revolver). weapon(pipe). weapon(rope).
room(study). room(library). room(lounge).

% Known Hand
holds(p4, revolver). 
holds(p4, library).

% Observations

% Event 1: p2 -> plum / knife / lounge
% No responder (everyone passed)
suggestion(e1, p2, plum, knife, lounge).

