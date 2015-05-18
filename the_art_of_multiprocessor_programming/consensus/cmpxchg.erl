%% looks like ets provides compare and change in some way
-module(cmpxchg).
-export([run/1]).

-define(KEY, my_beloved_object).

start() ->
    ets:new(cpmxchg_tbl, [public]).

sompare_and_set(Tid, Val) ->
    ets:insert_new(Tid, {?KEY, Val}).

clear(Tid) ->
    ets:delete(Tid, ?KEY).

decide(Tid, Val) ->
    sompare_and_set(Tid, Val),
    [El] = ets:lookup(Tid, ?KEY),
    El.

spawn_competitor(Tid, Barrier) ->
    spawn_monitor(
      fun() ->
              wait_for_mons([monitor(process, Barrier)]),
              Me = {self(), make_ref()},
              Res = decide(Tid, Me),
              io:format("~p: ~p~n", [self(), Res])
      end
     ).

run(ProcNum) ->
    Tid = start(),
    BarrierMsg = {run, make_ref()},
    Barrier = spawn(fun() -> receive BarrierMsg -> ok end end),
    try
        Mons = [spawn_competitor(Tid, Barrier) || _ <- lists:seq(1, ProcNum)],
        Barrier ! BarrierMsg,
        wait_for_mons(Mons),
        ok
    after
        clear(Tid)
    end.

wait_for_mons(Mons) ->
    [receive {'DOWN', Ref, _, _, _} -> ok end || {_Pid, Ref} <- Mons].
