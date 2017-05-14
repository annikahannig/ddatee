
%%=========================================================
%% DDate implementation in erlang
%%=========================================================

-module(ddatee).

-export([date_to_ddate/1, format/1, format/2]).
-export([month_to_season/1]).

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


%%---------------------------------------------------------
%% @doc Convert year to yold
%% @end
%%---------------------------------------------------------
year_to_yold({Year, _, _}) ->
    Year + 1166.


%%---------------------------------------------------------
%% @doc Convert month to season
%%---------------------------------------------------------
month_to_season({_, Month, Day}) when Month == 1;
                                      Month == 2;
                                      Month == 3,  Day =< 14 ->
    chaos;

month_to_season({_, Month, Day}) when Month == 3,  Day > 14;
                                      Month == 4;
                                      Month == 5,  Day =< 26 ->
    discord;

month_to_season({_, Month, Day}) when Month == 5,  Day > 26;
                                      Month == 6;
                                      Month == 7;
                                      Month == 8,  Day =< 7 ->
    confusion;

month_to_season({_, Month, Day}) when Month == 8,  Day > 7;
                                      Month == 9;
                                      Month == 10, Day =< 19 ->
    bureaucracy;

month_to_season({_, Month, Day}) when Month == 10, Day > 19;
                                      Month == 11;
                                      Month == 12 ->
    the_aftermath.


%%=========================================================
%% Tests
%%=========================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").




%%---------------------------------------------------------
%% Test Year to YOLD
%%---------------------------------------------------------
year_to_yold_test_() ->
    Conversions = [{{-1165, 1, 1}, 1},
                   {{0, 1, 1},     1166},
                   {{2017, 1, 1},  3183}],
    [{"convert yold", ?_assertEqual(Yold, year_to_yold(Date))} ||
        {Date, Yold} <- Conversions].

%%---------------------------------------------------------
%% Test month to season conversion 
%%---------------------------------------------------------
month_to_season_test_() ->
   Conversions = [{{2017, 1, 1},   chaos},
                  {{2017, 5, 10},  discord},
                  {{2017, 8, 7},   confusion},
                  {{2017, 8, 8},   bureaucracy},
                  {{2017, 11, 4},  the_aftermath}],

    [{"convert season", ?_assertEqual(Season, month_to_season(Date))} ||
       {Date, Season} <- Conversions]. 



%%---------------------------------------------------------
%% Test date to ddate conversion
%%---------------------------------------------------------
date_to_ddate_text_() ->
    Dates = [ {"normal date", {2017, 5, 11}, {28, 2, 3183}},
              {"leap year",   {2016, 2, 29}, st_tibs_day} ],

    [{Test, ?_assertEqual(DDate, date_to_ddate(Date))} ||
        {Test, Date, DDate} <- Dates].



-endif.


