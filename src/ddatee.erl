
%%=========================================================
%% DDate implementation in erlang
%%=========================================================

-module(ddatee).

-export([date_to_ddate/1, format/1, format/2]).


-type ddate() :: {integer(), integer(), integer()} | atom().


%%---------------------------------------------------------
%% @doc Convert date tuple to ddate
%%---------------------------------------------------------
-spec date_to_ddate(calendar:date()) -> ddate().
date_to_ddate(Date) ->
    ok.

format(Date) ->
    ok.


format(Format, Date) ->
    ok.



%%=========================================================
%% Tests
%%=========================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


%%---------------------------------------------------------
%% Test date to ddate conversion
%%---------------------------------------------------------
date_to_ddate_test_() ->
    Dates = [ {"normal date", {2017, 5, 11}, {28, 2, 3183}},
              {"leap year",   {2016, 2, 29}, st_tibs_day} ],

    [{Test, ?_assertEqual(DDate, date_to_ddate(Date))} ||
        {Test, Date, DDate} <- Dates].



-endif.


