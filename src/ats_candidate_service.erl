-module(ats_candidate_service).
-behaviour(gen_server).

-define(candidate_service, ?MODULE).

-include("include/records.hrl").

-export([get_template/0, register/1]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link() ->
  gen_server:start_link({local, ?candidate_service}, ?candidate_service, [], []).

get_template () ->
  gen_server:call(?candidate_service, {get_template}).

register (Candidate) ->
  gen_server:call(?candidate_service, {register, Candidate}).

%% IMPLEMENT gen_server behaviour
init([]) ->
  io:format("ATS Candidate Service started...~n"),
  {ok, [
    {candidates, []},
    {nextID, 1}
  ]}.

handle_call(Request, _From, State) ->
  case Request of
    {get_template} ->
      {reply, #candidate{name="Default", yob=0, id=0}, State};

    {register, #candidate{name=NewCandidateName, yob=NewCandidateYob, position_ids=NewPositionIds}} ->
      Candidates = proplists:get_value(candidates, State),
      case lists:filter(
        fun (#candidate{name=Name, yob=Yob}) ->
          (Name =:= NewCandidateName) and (Yob =:= NewCandidateYob)
        end,
        Candidates) of
        [ExistingCandidate] ->
          PositionIds2 = lists:umerge(NewPositionIds, ExistingCandidate#candidate.position_ids),
          ExistingCandidate2 = ExistingCandidate#candidate{position_ids=PositionIds2},

          Candidates2 = lists:ukeymerge(2, [ExistingCandidate2], Candidates),
          State2 = lists:keyreplace(candidates, 1, State, {candidates, Candidates2}),
          {reply, ok, State2};
        [] ->
          NextID = proplists:get_value(nextID, State),
          Candidates2 = Candidates ++ [#candidate{id=NextID, name=NewCandidateName, yob=NewCandidateYob, position_ids=NewPositionIds}],

          State2 = lists:keyreplace(candidates, 1, State, {candidates, Candidates2}),
          State3 = lists:keyreplace(nextID, 1, State2, {nextID, NextID + 1}),

          {reply, ok, State3}

      end;

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
