% Players
player(p1). player(p2). player(p3).

% Turn order
pos(p1,0). pos(p2,1). pos(p3,2).

% Cards
suspect(green). suspect(mustard). suspect(peacock).
weapon(knife). weapon(rope). weapon(wrench).
room(kitchen). room(ballroom). room(conservatory).

% Event 1: p2 -> green / rope / kitchen
% p3 responds
suggestion(e1, p1, green, rope, kitchen).
responder(e1, p3).
