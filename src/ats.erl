-module(ats).
-author("khanhhua").

-behaviour(application).
-include("include/records.hrl").

-export([start/2, stop/1]).
-export([simple_loop/1]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  ats_position_service:start_link(),

  Pid = spawn(?MODULE, simple_loop, [0]),
  {ok, Pid}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
simple_loop(State) ->
  NewState = State + 1,

  if
    State =:= 0 ->
      io:format("Looping started...~n"),
      simple_loop(NewState);
    true ->
      receive
        quit -> io:format("Looping terminated~n"), ok
      after
        1000 ->
          io:format("Looping continues...~n"),
          simple_loop(NewState)
      end
  end.
