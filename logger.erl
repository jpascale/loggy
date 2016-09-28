-module(logger).
-export([start/1, stop/1, queue_add/4, log/2]).

start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	Clock = time:clock(Nodes),

	% Create an empty hold-back queue.
	%Queue = lists:foldr(fun(Elem, List) -> [ {Elem, []} | List] end, [], Nodes),
	Queue = [],
	loop(Queue, Clock).

loop(Queue, Clock) ->
	receive
		{log, From, Time, Msg} ->
			NewClock = time:update(From, Time, Clock),
			NewQueue = queue_add(From, Time, Msg, Queue),
			NextQueue = log(NewQueue, NewClock),
			loop(NextQueue, NewClock);

		stop ->
			ok
	end.

log_print(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

queue_add(Name, Time, Msg, []) -> [{Name, Time, Msg}];
queue_add(Name, Time, Msg, Queue) -> 
	[First | Rest] = Queue,
	{_, T, _} = First,
	if (T >= Time) -> [{Name, Time, Msg} | Queue];
		true -> [First | queue_add(Name, Time, Msg, Rest)]
	end.
	%[{Name, Time, Msg} | Queue].
	%{_, NodeList} = lists:keyfind(Name, 1, Queue),
	%NewNodeList = [{Time, Msg} | NodeList],
	%lists:keyreplace(Name, 1, Queue, {Name, NewNodeList}).

log(Queue, Clock) ->
	lists:foldr(
			fun(Elem, List) ->
				{Name, Time, Msg} = Elem,
				Safe = time:safe(Time, Clock),
				if (Safe) -> 
						log_print(Name, Time, Msg),
						List;
					true -> [Elem | List]
				end
			end,
			[], Queue).