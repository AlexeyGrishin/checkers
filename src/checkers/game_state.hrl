-author("Alex").

-record(checker, {x, y, side, type = simple}).
-record(game_state, {whose_turn, checkers}).