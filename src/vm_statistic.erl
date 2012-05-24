-module(vm_statistic).
-behavior(gen_server).
-export([pusk/3, start/0]).
-export([ports_vm/0, process_vm/0, memory_vm/0, init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3]).

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

pusk() ->
    pusk(memory_vm, process_vm, ports_vm).

memory_vm() ->
    gen_server:call(?MODULE, {memory}).
process_vm() ->
    gen_server:call(?MODULE, {process}).
ports_vm() ->
    gen_server:call(?MODULE, {ports}).

init([]) -> 
    {ok, dets:open_file(?MODULE,[{type, set}])}.


handle_call({memory}, _From, Tab) ->
    Memory = erlang:memory(),
    dets:insert(?MODULE, {Memory}),
    {reply, Memory, Tab};
handle_call({process}, _From, Tab) ->
    Process = erlang:system_info(process_count),
    dets:insert(?MODULE, {Process}),
    {reply, Process, Tab};
handle_call({ports}, _From, Tab) ->
    Ports = erlang:statistics(io),
    dets:insert(?MODULE, {Ports}),
    {reply, Ports, Tab}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> {ok, dets:close(?MODULE)}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
 

