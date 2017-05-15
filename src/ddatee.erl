
%%=========================================================
%% DDate implementation in erlang
%%=========================================================

-module(ddatee).

-export([date_to_ddate/1, format/1, format/2]).


% DDate date types
-type yold()    :: non_neg_integer().
-type season()  :: 1..5.
-type day()     :: 1..73.

-type holiday() :: mungday |
                   chaoflux |
                   st_tibs_day |
                   mojoday |
                   discoflux |
                   syaday |
                   confuflux |
                   zaraday |
                   bureflux |
                   maladay |
                   afflux.

-type weekday() :: 1..5.

-type ddate() :: {yold(), season(), day()} |
                 {yold(), holiday()}.



%%=========================================================
%% DDate API
%%=========================================================


-spec date_to_ddate(calendar:date()) -> ddate().
%%---------------------------------------------------------
%% @doc Convert date tuple to ddate
%%---------------------------------------------------------
date_to_ddate({Year, 2, 29} = Date) ->
    Yold = date_to_yold(Date),
    case calendar:is_leap_year(Year) of
        true -> {Yold, st_tibs_day}
    end;

date_to_ddate(Date) ->
    {date_to_yold(Date),
     date_to_season(Date),
     date_to_day(Date)}.



format(Date) ->
    implement_me.


format(Format, Date) ->
    implement_me.




-spec date_to_yold(calendar:date()) -> yold().
%%---------------------------------------------------------
%% @doc Convert year to yold
%% @end
%%---------------------------------------------------------
date_to_yold({Year, _, _}) ->
    Year + 1166.



-spec date_to_season(calendar:date()) -> season().
%%---------------------------------------------------------
%% @doc Get season for date
%% @end
%%---------------------------------------------------------
date_to_season(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) div 73. % nice QSO.



-spec date_to_day(calendar:date()) -> day().
%%---------------------------------------------------------
%% @doc Get day for date
%% @end
%%---------------------------------------------------------
date_to_day(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) rem 73. % dito!



-spec day_in_year(calendar:date()) -> non_neg_integer().
%%---------------------------------------------------------
%% @doc Helper: Get day in year.
%% @end
%%---------------------------------------------------------
day_in_year({_, 1, Day}) -> Day;
day_in_year({_, Month, Day} = Date) ->
    Day + lists:sum([days_in_month(M) ||
                     M <- lists:seq(1, Month - 1)]).
    


-spec days_in_month(calendar:month()) -> 28 | 30 | 31.
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



-spec date_to_weekday(calendar:date()) -> weekday().
%%---------------------------------------------------------
%% @doc Convert date to weekday
%% @end
%%---------------------------------------------------------
date_to_weekday(Date) ->
    Day = day_in_year(Date),
    1 + (Day - 1) rem 5.



-spec ddate_to_holiday(ddate()) -> holiday().
%%---------------------------------------------------------
%% @doc Get holiday from ddate
%% @end
%%---------------------------------------------------------
ddate_to_holiday({_, 1, 5})   -> mungday;
ddate_to_holiday({_, 1, 50})  -> chaoflux;
ddate_to_holiday({_, 2, 5})   -> mojoday;
ddate_to_holiday({_, 2, 50})  -> discoflux;
ddate_to_holiday({_, 3, 5})   -> syaday;
ddate_to_holiday({_, 3, 50})  -> confuflux;
ddate_to_holiday({_, 4, 5})   -> zaraday;
ddate_to_holiday({_, 4, 50})  -> bureflux;
ddate_to_holiday({_, 5, 5})   -> maladay;
ddate_to_holiday({_, 5, 50})  -> afflux;
ddate_to_holiday({_, st_tibs_day}) -> st_tibs_day.



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
%% Test holidays
%%---------------------------------------------------------
ddate_to_holiday_test_() ->
    Expected = [{{2017, 1,  5},  mungday},
                {{2017, 2,  19}, chaoflux},
                {{2016, 2,  29}, st_tibs_day},
                {{2017, 3,  19}, mojoday},
                {{2017, 5,  3},  discoflux},
                {{2017, 5,  31}, syaday},
                {{2017, 7,  15}, confuflux},
                {{2017, 8,  12}, zaraday},
                {{2017, 9,  26}, bureflux},
                {{2017, 10, 24}, maladay},
                {{2017, 12, 8},  afflux}],
    [{"date to holiday",
      ?_assertEqual(Holiday, ddate_to_holiday(date_to_ddate(Date)))} ||
        {Date, Holiday} <- Expected].



%%---------------------------------------------------------
%% Test date to ddate conversion
%%---------------------------------------------------------
date_to_ddate_test_() ->
    Dates = [ {"normal date", {2017, 5, 11}, {3183, 2, 58}},
              {"leap year",   {2016, 2, 29}, {3182, st_tibs_day}} ],

    [{Test, ?_assertEqual(DDate, date_to_ddate(Date))} ||
        {Test, Date, DDate} <- Dates].

-endif.

