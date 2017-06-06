-module(ats_position_service).
-behaviour(gen_server).

-define(position_service, ?MODULE).

-include("include/records.hrl").

-export([get_template/0, create/1, search/2]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link() ->
  gen_server:start_link({local, ?position_service}, ?position_service, [], []).

get_template () ->
  gen_server:call(?position_service, {get_template}).

create (Position) ->
  gen_server:call(?position_service, {create, Position}).

search (Query, Limit) ->
  gen_server:call(?position_service, {search, Query, Limit}).

%% IMPLEMENT gen_server behaviour
init([]) ->
  io:format("ATS Position Service started...~n"),
  {ok, [
    {positions, []},
    {nextID, 1}
  ]}.

handle_call(Request, _From, State) ->
  case Request of
    {get_template} ->
      {reply, #position{name="Default", salary=0, id=0}, State};

    {create, Position} ->
      Positions = proplists:get_value(positions, State),
      NextID = proplists:get_value(nextID, State),

      Positions2 = Positions ++ [Position#position{id = NextID}],
      State2 = lists:keyreplace(positions, 1, State, {positions, Positions2}),
      State3 = lists:keyreplace(nextID, 1, State2, {nextID, NextID + 1}),

      {reply, ok, State3};

    {search, #positionQuery{minSalary=MinSalary, positionName=PositionName}, Limit } ->
      Positions = lists:filter(
        fun (#position{name=Name, salary=Salary}) ->
          (Salary >= MinSalary) and (string:str(Name, PositionName) =:= 1)
        end,
        proplists:get_value(positions, State)),

      {reply, Positions, State};
    _ -> {reply, ignored, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
