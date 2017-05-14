
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




-spec date_to_yold(calendar:date()) -> integer().
%%---------------------------------------------------------
%% @doc Convert year to yold
%% @end
%%---------------------------------------------------------
date_to_yold({Year, _, _}) ->
    Year + 1166.



-spec date_to_season(calendar:date()) -> integer().
%%---------------------------------------------------------
%% @doc Get season for date
%% @end
%%---------------------------------------------------------
date_to_season(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) div 73. % nice QSO.



-spec date_to_day(calendar:date()) -> integer().
%%---------------------------------------------------------
%% @doc Get day for date
%% @end
%%---------------------------------------------------------
date_to_day(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) rem 73. % dito!



-spec day_in_year(calendar:date()) -> integer().
%%---------------------------------------------------------
%% @doc Helper: Get day in year.
%% @end
%%---------------------------------------------------------
day_in_year({_, 1, Day}) -> Day;
day_in_year({_, Month, Day} = Date) ->
    Day + lists:sum([days_in_month(M) ||
                     M <- lists:seq(1, Month - 1)]).
    
    


-spec days_in_month(calendar:month()) -> integer().
%%---------------------------------------------------------
%% @doc Helper: Get days for month in a given year.
%% @end
%%---------------------------------------------------------
days_in_month(1)  -> 31;
days_in_month(2)  -> 28; %% approximately
days_in_month(3)  -> 31;
days_in_month(4)  -> 30;
days_in_month(5)  -> 31;
days_in_month(6)  -> 30;
days_in_month(7)  -> 31;
days_in_month(8)  -> 31;
days_in_month(9)  -> 30;
days_in_month(10) -> 31;
days_in_month(11) -> 30;
days_in_month(12) -> 31.


-spec date_to_weekday(calendar:date()) -> integer().
%%---------------------------------------------------------
%% @doc Convert date to weekday
%% @end
%%---------------------------------------------------------
date_to_weekday(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) rem 5.


%%---------------------------------------------------------
%% @doc Get holiday from date
%% @end
%%---------------------------------------------------------
date_to_holiday({_, 1,  5})   -> mungtag;
date_to_holiday({_, 2,  19})  -> chaosflux;
date_to_holiday({_, 29, 2})   -> st_tibs_day;
date_to_holiday({_, 3,  19})  -> mojoday;
date_to_holiday({_, 5,  3})   -> discoflux;
date_to_holiday({_, 5,  31})  -> syaday;
date_to_holiday({_, 7,  15})  -> confuflux.


%%=========================================================
%% Tests
%%=========================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



%%---------------------------------------------------------
%% Test Year to YOLD
%%---------------------------------------------------------
date_to_yold_test_() ->
    Expected = [{{-1165, 1, 1}, 1},
                {{0, 1, 1},     1166},
                {{2017, 1, 1},  3183}],
    [{"convert yold", ?_assertEqual(Yold, date_to_yold(Date))} ||
        {Date, Yold} <- Expected].



%%---------------------------------------------------------
%% Test month to season conversion 
%%---------------------------------------------------------
date_to_season_test_() ->
   Conversions = [{{2017, 1, 1},   1},
                  {{2017, 3, 14},  1},
                  {{2017, 3, 15},  2},
                  {{2017, 5, 26},  2},
                  {{2017, 5, 27},  3},
                  {{2017, 8, 7},   3},
                  {{2017, 8, 8},   4},
                  {{2017, 10, 19}, 4},
                  {{2017, 10, 20}, 5},
                  {{2017, 12, 31}, 5}],

    [{"convert season", ?_assertEqual(Season, date_to_season(Date))} ||
       {Date, Season} <- Conversions]. 



%%---------------------------------------------------------
%% Test weekday caclculation
%%---------------------------------------------------------
date_to_weekday_test_() ->
    Expected = [{{2017, 1, 1},   1},
                {{2017, 1, 2},   2},
                {{2017, 1, 5},   5},
                {{2017, 1, 6},   1},
                {{2017, 1, 7},   2},
                {{2017, 12, 31}, 5}],
    [{"date to weekday", ?_assertEqual(Weekday, date_to_weekday(Date))} ||
        {Date, Weekday} <- Expected].


%%---------------------------------------------------------
%% Test day in year
%%---------------------------------------------------------
day_in_year_test_() ->
    Expected = [{{2017, 1, 1},  1},
                {{2017, 3, 1},  60},
                {{2017, 3, 30}, 89},
                {{2017, 8, 7},  219},
                {{2017, 8, 8},  220}],
    [{"day in year", ?_assertEqual(Day, day_in_year(Date))} ||
        {Date, Day} <- Expected].



%%---------------------------------------------------------
%% Test days in month
%%---------------------------------------------------------
days_in_month_test_() ->
    Expected = [{2, 28},
                {4, 30},
                {5, 31}],
    [{"days in month", ?_assertEqual(Days, days_in_month(Date))} ||
        {Date, Days} <- Expected].



%%---------------------------------------------------------
%% Test date to ddate conversion
%%---------------------------------------------------------
date_to_ddate_text_() ->
    Dates = [ {"normal date", {2017, 5, 11}, {28, 2, 3183}},
              {"leap year",   {2016, 2, 29}, st_tibs_day} ],

    [{Test, ?_assertEqual(DDate, date_to_ddate(Date))} ||
        {Test, Date, DDate} <- Dates].



-endif.


