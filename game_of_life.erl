-module(game_of_life).
-export([start/0, start/2]).

-define(STATE, [{0, 0}, {0, 1}, {1, 0}, {1, 1}]). 
-define(MOVES, [{DX, DY} || DX <- [-1, 0, 1], DY <- [-1, 0, 1], DX =/= 0 orelse DY =/= 0]).

start() ->
    loop(?STATE, 100).

start(State, NoOfGenerations) ->
    loop(State, NoOfGenerations).

%% @private
loop(_State, 0) ->
    done;
loop(State, Count) ->
    loop(tick(State), Count - 1).

%% @private
neighbours({X, Y}, State) ->
    lists:foldl(fun({DX, DY}, Acc) ->
                case lists:member({X+DX, Y+DY}, State) of
                    true -> Acc + 1;
                    false -> Acc + 0
                end
        end, 0, ?MOVES).

%% @private
next_cell_state({X, Y}, State) ->
    case neighbours({X, Y}, State) of
        N when N < 2 -> dead;
        N when N > 3 -> dead;
        N when N =:= 2 ->
            case lists:member({X,Y}, State) of
                true -> live;
                false -> dead
            end;
        N when N =:= 3 -> live
    end.

%% @private
tick(State) ->
    print_and_return(fun() ->
                lists:filter(fun({X,Y}) ->
                            case next_cell_state({X,Y}, State) of
                                live -> true;
                                dead -> false
                            end
                    end, surrounding_dead_cells(State) ++ State)
        end).

%% @private
surrounding_dead_cells(State) ->
    DeadCells = lists:flatmap(fun({X, Y}) ->
                    lists:flatmap(fun({DX, DY}) ->
                                case lists:member({X+DX, Y+DY}, State) of
                                    true -> [];
                                    false -> [{X+DX, Y+DY}]
                                end
                        end, ?MOVES)
            end, State),
    sets:to_list(sets:from_list(DeadCells)).

%% @private
print_and_return(Fun) ->
    Result = Fun(),
    io:format("RESULT: ~p~n",[Result]),
    Result.

