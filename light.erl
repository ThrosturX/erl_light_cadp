-module(light).
-export([init/0, run/1, spawner/2, cross/1]).

%% Sleep for T milliseconds
sleep(T) ->
	receive
	after T -> true
	end.

%% Starts the simulation
init() -> 
	semaphore:start().

%% Starts the spawner to spawn N of all types
run(N) ->
	spawn(light, spawner, [N, vehiclen]), % N vehicles going north
	spawn(light, spawner, [N, vehiclee]), % N vehicles goint east
	spawn(light, spawner, [N, pedestriann]), % N pedestrians going north
	spawn(light, spawner, [N, pedestriane]), % N pedestrians going earth
	spawn(light, spawner, [N, pedestriand]), % N pedestrians goign diagonal
	io:format("~n", []).

%% Spawns N of A
spawner(0, _) -> true;
spawner(N, A) ->
	spawn(light, cross, [A]),
	sleep(10),
	spawner(N-1, A).

%% Crossing function for pedestrians going north
cross(A) when A == pedestriann ->
	semaphore:walkn(), % Request to cross
	io:format("~p in crossing ~n", [A]),
	sleep(100), % Crossing
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepn(); % Leave crossing

%% Crossing function for pedestrians going east
cross(A) when A == pedestriane ->
	semaphore:walke(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepe();

%% Crossing function for pedestrians going diagonally
cross(A) when A == pedestriand ->
	semaphore:walkd(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavepd();

%% Crossing function for vehicles going north
cross(A) when A == vehiclen ->
	semaphore:driven(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leavevn();

%% Crossing function for vehicles going east
cross(A) when A == vehiclee ->
	semaphore:drivee(),
	io:format("~p in crossing ~n", [A]),
	sleep(100),
	io:format("~p out of crossing ~n", [A]),
	semaphore:leaveve().

