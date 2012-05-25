-module(vm_statistic).
-behavior(gen_server).
-export([start_timer/0, start_statistic/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3]).
-export([ports_vm/0, process_vm/0, memory_vm/0]).

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, dets:open_file(?MODULE, [{type, set}])}.

memory_vm() ->
    gen_server:call(?MODULE, {memory}).
process_vm() ->
    gen_server:call(?MODULE, {process}).
ports_vm() ->
    gen_server:call(?MODULE, {ports}).

start_timer() ->
    timer:apply_interval(3000, vm_statistic, start_statistic, []).

start_statistic() ->
    Memory = {vm_statistic:memory_vm()}, 
    Process = {vm_statistic:process_vm()}, 
    Ports = {vm_statistic:ports_vm()},
    dets:insert(?MODULE, {Memory, Process, Ports}).

handle_call({memory}, _From, Tab) ->
    Memory = erlang:memory(),
    {reply, Memory, Tab};
handle_call({process}, _From, Tab) ->
    Process = erlang:system_info(process_count),
    {reply, Process, Tab};
handle_call({ports}, _From, Tab) ->
    Ports = erlang:statistics(io),
    {reply, Ports, Tab}.
handle_cast(_Msg, State) -> 
    {noreply, State}.
handle_info(_Info, State) -> 
    {noreply, State}.
terminate(_Reason, _State) -> 
    {ok, dets:close(?MODULE)}.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
 

