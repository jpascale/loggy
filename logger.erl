-module(logger).
-export([start/1, stop/1, queue_add/4]).

start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	Clock = time:clock(Nodes),

	% Create an empty hold-back queue.
	Queue = lists:foldr(fun(Elem, List) -> [ {Elem, []} | List] end, [], Nodes),
	loop(Queue, Clock).

loop(Queue, Clock) ->
	receive

		{log, From, Time, Msg} ->
			%log(From, Time, Msg),
			NewClock = time:update(From, Time, Clock),
			NewQueue = queue_add(From, Time, Msg, Queue),
			loop(NewQueue, NewClock);

		stop ->
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

queue_add(Name, Time, Msg, Queue) ->
	{_, NodeList} = lists:keyfind(Name, 1, Queue),
	NewNodeList = [{Time, Msg} | NodeList],
	lists:keyreplace(Name, 1, Queue, {Name, NewNodeList}).