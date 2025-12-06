% -------------------------------------------------
% TEST 1
% This test case is designed to validate that  when our encoding does not have enough
% information to deduce the case file cards when there is insufficient data from suggestions
% it produces multiple possible models.
%
% Setup: 3 players, we are p1, 2 cards per hand
% -------------------------------------------------

% Players and Positions
player(p1). player(p2). player(p3).
pos(p1,0). pos(p2,1). pos(p3,2).

% Cards
suspect(green). suspect(mustard). suspect(peacock).
weapon(knife). weapon(rope). weapon(wrench).
room(kitchen). room(ballroom). room(conservatory).

% Known Hand
holds(p1, green). holds(p1, knife).

% Observations

% Event 1: p2 -> green / rope / kitchen
% p3 responds
suggestion(e1, p1, green, rope, kitchen).
responder(e1, p3).
holds(p3, rope).