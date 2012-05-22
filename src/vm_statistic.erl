-module(vm_statistic).
-behavior(gen_server).
-export([start/0]).
-export([memory_vm/0, init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


memory_vm() ->
    gen_server:call(?MODULE, {memory}).

init([]) -> 
    {ok, ets:new(?MODULE, [])}.


handle_call({memory}, _From, Tab) ->
    Reply = io:format("Memory:~p~n", [erlang:memory(total)]),
    {reply, Reply, Tab}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
 

