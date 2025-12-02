% Players
player(p1). player(p2). player(p3). player(p4).

% Turn order
pos(p1,0). pos(p2,1). pos(p3,2). pos(p4,3).

% Cards
suspect(green). suspect(plum). suspect(scarlet). suspect(mustard).
weapon(knife). weapon(revolver). weapon(pipe). weapon(rope).
room(study). room(library). room(lounge).

% Event 1: p2 -> plum / knife / lounge
% No responder (everyone passed)
suggestion(e1, p2, plum, knife, lounge).

