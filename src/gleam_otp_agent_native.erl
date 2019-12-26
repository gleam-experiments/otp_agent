-module(gleam_otp_agent_native).

% Public API functions
-export([start_link/1, async/2, sync/3]).

% Private proc lib exports
-export([init/2, loop/1]).

start_link(Fn) ->
  proc_lib:start_link(?MODULE, init, [Fn, self()]).

init(StartFn, Parent) ->
    case StartFn() of
        {ready, State} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(State);

        {continue_init, Continuation} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            handle_next(Continuation());

        {failed, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason}),
            proc_lib:stop(self(), Reason, infinity)
    end.

loop(State) ->
    receive
        Fn -> handle_next(Fn(State))
    end.

handle_next(Next) ->
    case Next of
        {next, State} -> loop(State);
        {hibernate, State} -> proc_lib:hibernate(?MODULE, loop, [State]);
        {stop, Reason} -> proc_lib:stop(self(), Reason, infinity);
        {continue, Continuation} -> handle_next(Continuation())
    end.

async(Agent, Fn) ->
    Agent ! Fn,
    nil.

sync(Agent, Timeout, Fn) ->
    Ref = make_ref(),
    Caller = self(),
    Agent ! fun(State) ->
        {reply, Reply, Next} = Fn(State),
        Caller ! {Ref, Reply},
        Next
    end,
    receive
        {Ref, Reply} -> Reply
    after
        Timeout -> error(gleam_otp_agent_sync_timeout)
    end.
