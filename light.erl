-module(light).
-export([init/0, run/1, spawner/2, cross/1]).

sleep(T) ->
	receive
	after T -> true
	end.

init() -> 
	semaphore:start().

run(N) ->
	spawn(light, spawner, [N, vehiclen]),
	spawn(light, spawner, [N, vehiclee]),
	spawn(light, spawner, [N, pedestriann]),
	spawn(light, spawner, [N, pedestriane]),
	spawn(light, spawner, [N, pedestriand]),
	io:format("~n", []).

spawner(0, _) -> true;
spawner(N, A) ->
	spawn(light, cross, [A]),
	sleep(10),
	spawner(N-1, A).

cross(A) when A == pedestriann ->
	semaphore:walkn(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepn();
cross(A) when A == pedestriane ->
	semaphore:walke(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepe();
cross(A) when A == pedestriand ->
	semaphore:walkd(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepd();
cross(A) when A == vehiclen ->
	semaphore:driven(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavevn();
cross(A) when A == vehiclee ->
	semaphore:drivee(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leaveve().

