-module(semaphore).
-export([start/0, walkn/0, walke/0, walkd/0, driven/0, drivee/0, leavepn/0, leavepe/0, leavepd/0, leavevn/0, leaveve/0, process/6]).

%% @doc Creates a new semaphore, initialized to N. Use the return value to
%% reference this semaphore.
start() ->
    Pid = spawn(semaphore, process, [0, 0, 0, 0, 3, 3]), %Vn,Ve,Pn,Pe,K1,K2
	register(crossing, Pid).

%% @doc Increment or signal the semaphore.
%% Request to walk north
walkn() ->
    crossing!{self(), walkn},
    receive
	{walk} ->
	    ok
    end.

%% Request to walk east
walke() ->
    crossing!{self(), walke},
    receive
	{walk} ->
	    ok
    end.

%% Request to walk diagonally
walkd() ->
    crossing!{self(), walkd},
    receive
	{walk} ->
	    ok
    end.

%% Request to drive north
driven() ->
    crossing!{self(), driven},
    receive
	{drive} ->
	    ok
    end.

%% Request to drive east
drivee() ->
    crossing!{self(), drivee},
    receive
	{drive} ->
	    ok
    end.

%% @doc Try to decrease the semaphore and block if necessary.
%% Pedestrian leaving crossing north
leavepn() ->
    crossing!{self(), leavepn},
    receive
	{leavep} ->
	    ok
    end.

%% Pedestrian leaving crossing east
leavepe() ->
    crossing!{self(), leavepn},
    receive
	{leavep} ->
	    ok
    end.

%% Pedestrian leaving crossing diagonally
leavepd() ->
	ok.

%% Vehicle leaving crossing north
leavevn() ->
    crossing!{self(), leavevn},
    receive
	{leavev} ->
	    ok
    end.

%% Vehicle leaving crossing east
leaveve() ->
    crossing!{self(), leaveve},
    receive
	{leavev} ->
	    ok
    end.

%% @doc Semaphore process that maintains the semaphore state and
%% coordinates access.
%% @private
%% Traffic controller
%% Vn = number of vehicles crossing to north
%% Ve = number of vehicles crossing to east
%% Pn = number of pedestrians crossing to north
%% Pe = number of pedestrians crossing to east
%% Pd = number of pedestrians crossing diagonally
%% K1 = how many can cross to east 
%% K2 = how many can cross to north
process(Vn, Ve, Pn, Pe, K1, K2) ->
    receive
	{ Pid, walkn } when Ve== 0, K1 > 0 ->
	    Pid!{walk},
	    process(Vn, Ve, Pn+1, Pe, K1-1, K2);
	{ Pid, walke } when Vn == 0, K2 > 0 ->
	    Pid!{walk},
	    process(Vn, Ve, Pn, Pe+1, K1, K2-1);
	{ Pid, walkd } when Vn == 0, Ve == 0, K1 > 0, K2 > 0 ->
	    Pid!{walk},
	    process(Vn, Ve, Pn+1, Pe+1, K1-1, K2-1);
	{ Pid, driven } when Pe == 0, Ve == 0, K2 > 0 ->
	    Pid!{drive},
	    process(Vn+1, Ve, Pn, Pe, K1-1, K2);
	{ Pid, drivee } when Pn == 0, Vn == 0, K2 > 0 ->
	    Pid!{drive},
	    process(Vn, Ve+1, Pn, Pe, K1, K2-1);
	{ Pid, leavepn } ->
	    Pid!{leavep},
		% Last pedestrian going north resets K1
		if Pn == 1 -> KK = 3;
		   true -> KK = K1
		end,
	    process(Vn, Ve, Pn-1, Pe, KK, K2);
	{ Pid, leavepe } ->
	    Pid!{leavep},
		% Last pedestrian going east resets K2
		if Pe == 1 -> KK = 3;
		   true -> KK = K2
		end,
	    process(Vn, Ve, Pn, Pe-1, K1, KK);
	{ Pid, leavevn } ->
	    Pid!{leavev},
		% Last vehicle going north resets K1
		if Vn == 1 -> KK = 3;
		   true -> KK = K1
		end,
	    process(Vn-1, Ve, Pn, Pe, KK, K2);
	{ Pid, leaveve } ->
	    Pid!{leavev},
		% Last vehicle going east resets K2
		if Ve == 1 -> KK = 3;
		   true -> KK = K2
		end,
	    process(Vn, Ve-1, Pn, Pe, K1, KK)
	after
		1000 -> process(Vn, Ve, Pn, Pe, 3, 3)
    end.
