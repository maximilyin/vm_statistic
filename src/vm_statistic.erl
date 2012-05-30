-module(vm_statistic).
-behavior(gen_server).
-export([start_app/0, start_statistic/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2, terminate/2, code_change/3]).
-export([date_interval/2, context_switches_vm/0, ports_vm/0, process_count_vm/0, process_limit_vm/0, memory_vm/0]).

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, dets:open_file(?MODULE, {type, set})}.

start_app() ->
    io:format("Statitics generator started~n"),
    timer:apply_interval(5000, ?MODULE, start_statistic, []).

start_statistic() ->
    Memory = {vm_statistic:memory_vm()}, 
    Process_Count = {vm_statistic:process_count_vm()},
    Process_Limit = {vm_statistic:process_limit_vm()}, 
    Ports = {vm_statistic:ports_vm()},
    Context_Switches = {vm_statistic:context_switches_vm()},
    Date_Second = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    dets:insert(?MODULE, {Date_Second, Memory, Process_Count, Process_Limit, Ports, Context_Switches}).

date_interval(Min, Max) ->
    dets:select(?MODULE,
        [{{'$1', '_', '_', '_', '_', '_'},
            [{'<=', '$1', '$1'}, {'=>', '$1', '$1'}],
                ['$$']}]).

memory_vm() ->
    gen_server:call(?MODULE, {memory}).
process_count_vm() ->
    gen_server:call(?MODULE, {process_count}).
process_limit_vm() ->
    gen_server:call(?MODULE, {process_limit}).
ports_vm() ->
    gen_server:call(?MODULE, {ports}).
context_switches_vm() ->
    gen_server:call(?MODULE, {context_switches}).


handle_call({memory}, _From, Tab) ->
    Memory = erlang:memory(),
    {reply, Memory, Tab};
handle_call({process_count}, _From, Tab) ->
    Process_Count = erlang:system_info(process_count),
    {reply, Process_Count, Tab};
handle_call({process_limit}, _From, Tab) ->
    Process_Limit = erlang:system_info(process_limit),
    {reply, Process_Limit, Tab};
handle_call({ports}, _From, Tab) ->
    Ports = erlang:statistics(io),
    {reply, Ports, Tab};
handle_call({context_switches}, _From, Tab) ->
    Context_Switches = erlang:statistics(context_switches),
    {reply, Context_Switches, Tab}.
handle_cast(_Msg, State) -> 
    {noreply, State}.
handle_info(_Info, State) -> 
    {noreply, State}.
terminate(_Reason, _State) -> 
    {ok, dets:close(?MODULE)}.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
 

