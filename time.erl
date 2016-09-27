-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% Return an initial Lamport value (could it be 0)
zero() -> 0.

% Return the time T incremented by one (you will probably
% ignore the Name but we will use it later)
inc(Name, T) -> T + 1.


% Merge the two Lamport time stamps (i.e. take the
% maximum value)
merge(Ti, Tj) -> erlang:max(Ti, Tj).

% Return true if Ti is less than or equal to Tj.
leq(Ti, Tj) when Ti =< Tj -> true;
leq(Ti, Tj) -> false.

% Return a clock that can keep track of the nodes
clock(Nodes) -> 
	lists:foldr(fun(Elem, List) -> [ {Elem, 0} | List] end, [], Nodes).

% Return a clock that has been updated
% given that we have received a log message from a node at a given time
update(Node, Time, Clock) -> 
	lists:foldr(fun(Elem, List) -> 
					{A, _} = Elem,
					
					if (A == Node) ->
						[ {A, Time} | List ];
						true -> 
							[Elem | List]
					end
						end, 
							[], Clock).

% Is it safe to log an event that happened at a given
% time, true or false
safe(_, []) -> true;
safe(Time, Clock) -> 
	[{_, ElemTime} | List] = Clock,
	if (ElemTime < Time) -> false;
		true -> safe(Time, List)
	end.