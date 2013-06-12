-module(game_of_life).
-compile(export_all).

-define(STATE, [{0, 0}, {0, 1}, {1, 0}, {1, 1}]). 
-define(MOVES, fun() ->
            lists:filter(fun({0, 0}) -> false;
                            (_) -> true
                end, [{DX, DY} || DX <- [-1, 0, 1], DY <- [-1, 0, 1]])
    end).

neighbours({X, Y}, State) ->
    lists:foldl(fun({DX, DY}, Acc) ->
                case lists:member({X+DX, Y+DY}, State) of
                    true -> Acc + 1;
                    false -> Acc + 0
                end
        end, 0, ?MOVES()).

next_cell_state({X, Y}, State) ->
    case neighbours({X, Y}, State) of
        N when N < 2 -> dead;
        N when N > 3 -> dead;
        _ -> live
    end.

resurect_cell({X, Y}, State) ->
    case neighbours({X, Y}, State) of
        3 -> live;
        _ -> dead
    end.

tick(State) ->
    lists:filter(fun({X, Y}) ->
                case next_cell_state({X, Y}, State) of
                    dead -> false;
                    live -> true
                end
        end, State).

surrounding_dead_cells(State) ->
    lists:flatmap(fun({X, Y}) ->
                lists:flatmap(fun({DX, DY}) ->
                            case lists:member({X+DX, Y+DY}, State) of
                                true -> [];
                                false -> [{X+DX, Y+DY}]
                            end
                    end, ?MOVES())
        end, State).

