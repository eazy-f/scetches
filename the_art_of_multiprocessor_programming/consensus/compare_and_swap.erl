-module(compare_and_swap).

-export([new/2, compare_and_swap/3]).
-export([run/1]).

new(Name, Initial) ->
    Table = ets:new(Name, [public]),
    swap(Table, Initial),
    Table.

swap(Cell, Value) ->
    ets:insert(Cell, {value, Value}).

compare_and_swap(Cell, Expected, New) ->
    protect(fun compare_and_swap_unsafe/3, [Cell, Expected, New]).

compare_and_swap_unsafe(Cell, Expected, New) when Expected =:= New ->
    ets:lookup(Cell, value) =:= [{value, Expected}];    
compare_and_swap_unsafe(Cell, Expected, New) ->
    case ets:insert_new(Cell, {lock, self()}) of
	true ->
	    Result = case ets:lookup(Cell, value) of
		[] ->
		    swap(Cell, New),
		    true;
		[{value, OldValue}] when OldValue =:= Expected ->
		    swap(Cell, New),
		    true;
		_ ->
		    false
	    end,
	    ets:delete(Cell, lock),
	    Result;
	false ->
	    false
    end.
    

protect(Fun, Args) ->
    PreviouslyTrapped = collect_trapped(),
    WasTrapped = erlang:process_flag(trap_exit, true),
    Result = erlang:apply(Fun, Args),
    erlang:process_flag(trap_exit, false),
    process_trapped_exits(WasTrapped, collect_trapped()),
    resend_trapped(PreviouslyTrapped),
    Result.

process_trapped_exits(true = _WasTrappedBefore, Exits) ->
    resend_trapped(Exits);
process_trapped_exits(false, Exits) ->
    lists:foreach(fun process_trapped_exit/1, Exits).

resend_trapped(Exits) ->
    [erlang:send(self(), Msg) || Msg <- Exits].

process_trapped_exit({'EXIT', _, normal}) ->
    ok;
process_trapped_exit({'EXIT', _, Reason}) ->
    erlang:exit(Reason).

collect_trapped() ->
    lists:reverse(collect_trapped([])).

collect_trapped(Acc) ->
    receive
	{'EXIT', _, _Reason} = Exit->
	    collect_trapped([Exit | Acc])
    after 0 ->
	    Acc
    end.

run(Workers) ->
    Cell = new('cell1', a),
    [spawn_worker(self(), Cell) || _ <- lists:seq(1, Workers)],
    receive_results(Workers).

receive_results(0) ->
    ok;
receive_results(Workers) ->
    receive
	#{success := Success, failures := Failures} ->
	    io:format("~b - ~b~n", [Success, Failures]),
	    receive_results(Workers - 1)
    after 15000 ->
	    ok
    end.

spawn_worker(Controller, Cell) ->
    erlang:spawn(fun() -> worker_start(Controller, Cell) end).

worker_start(Controller, Cell) ->
    worker_loop(Controller, _Iterations = 1000, Cell, a, b, #{success => 0, failures => 0}).

worker_loop(Controller, 0 = _Iterations, _Cell, _From, _To, Result) ->
    Controller ! Result;
worker_loop(Controller, IterationsLeft, Cell, From, To, Result) ->
    #{success := Success, failures := Failures} = Result,
    NewResult = case compare_and_swap_unsafe(Cell, From, To) of
		    true ->
			Result#{success => Success + 1};
		    false ->
			Result#{failures => Failures + 1}
		end,
    worker_loop(Controller, IterationsLeft - 1, Cell, To, From, NewResult).
    

