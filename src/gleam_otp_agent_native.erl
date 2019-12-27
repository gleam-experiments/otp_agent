-module(gleam_otp_agent_native).

% Public API functions
-export([start_link/1, sync/3]).

% Private system exports
-export([init/2, loop/3, system_continue/3, system_terminate/4, write_debug/3,
         system_get_state/1, system_replace_state/2]).

%% Public API

start_link(Fn) ->
  proc_lib:start_link(?MODULE, init, [Fn, self()]).

sync(Agent, Timeout, Fn) ->
    Ref = make_ref(),
    Agent ! {sync, Ref, self(), Fn},
    receive
        {Ref, Reply} -> Reply
    after
        Timeout -> error(gleam_otp_agent_sync_timeout)
    end.

%% Internal callbacks

init(StartFn, Parent) ->
    Debug = sys:debug_options([]),
    case StartFn() of
        {ready, State} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(State, Parent, Debug);

        {continue_init, Continuation} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            handle_next(Continuation(), Parent, Debug);

        {failed, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason}),
            proc_lib:stop(self(), Reason, infinity)
    end.

loop(State, Parent, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

        {async, Fn} when is_function(Fn, 1) ->
            Debug1 = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Fn}),
            handle_next(Fn(State), Parent, Debug1);

        {sync, Ref, From, Fn} when is_function(Fn, 1) ->
            Debug1 = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Fn}),
            {reply, Reply, Next} = Fn(State),
            From ! {Ref, Reply},
            Debug2 = sys:handle_debug(Debug1, fun ?MODULE:write_debug/3, ?MODULE, {out, Reply, From}),
            handle_next(Next, Parent, Debug2)
    end.

handle_next(Next, Parent, Debug) ->
    case Next of
        {next, State} -> loop(State, Parent, Debug);
        {hibernate, State} -> proc_lib:hibernate(?MODULE, loop, [State]);
        {stop, Reason} -> terminate(Reason);
        {continue, Continuation} -> handle_next(Continuation(), Parent, Debug)
    end.

% TODO: log error
% https://github.com/erlang/otp/blob/master/lib/stdlib/src/gen_server.erl#L890-L903
terminate(_Reason) ->
  ok.

%% sys module callbacks

system_continue(Parent, Debug, State) ->
    loop(State, Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NewState = StateFun(State),
    {ok, NewState, NewState}.

write_debug(Device, Event, Name) ->
    io:format(Device, "~p event = ~p~n", [Name, Event]).
