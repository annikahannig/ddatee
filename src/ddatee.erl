
%%=========================================================
%% DDate implementation in erlang
%%=========================================================

-module(ddatee).

-export([date_to_ddate/1, format/1, format/2]).



%%---------------------------------------------------------
%% DDate date types
%%---------------------------------------------------------
-export_type([ddate/0]).

-type yold()    :: non_neg_integer().
-type season()  :: 1..5.
-type day()     :: 1..73.


-type holiday() :: mungday | chaoflux | mojoday | discoflux |
                   syaday | confuflux | zaraday | bureflux |
                   maladay | afflux | st_tibs_day | none.

-type weekday() :: 1..5.

-type ddate() :: {yold(), holiday()} |
                 {yold(), season(), day()}.

-type tpl() :: string() | atom().

%%=========================================================
%% DDate API
%%=========================================================




-spec date_to_ddate(calendar:date()) -> ddate().
%%---------------------------------------------------------
%% @doc Convert date tuple to ddate
%% @end
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



-spec format(ddate()) -> string().
%%---------------------------------------------------------
%% @doc Format date with default format
%% @end
%%---------------------------------------------------------
format(Date) ->
    format(full, Date).



-spec format(tpl(), ddate()) -> string().
%%---------------------------------------------------------
%% @doc Format ddate with (custom) format.
%%      Tags:
%%        :day:      the numeric day
%%        :season:   the seasons name
%% @end
%%---------------------------------------------------------
format(Format, Date) ->
    Template = format_template(Format),
    E = re:replace(Template, ":day:",         format_day(Date)),
    R = re:replace(E,        ":day_suffix:",  format_day_suffix(Date)),
    I = re:replace(R,        ":season:",      format_season(Date)),
    S = re:replace(I,        ":yold:",        format_yold(Date)), 
    D = re:replace(S,        ":weekday:",     format_weekday(Date)),
    Isco = re:replace(D,     ":season:",      format_season(Date)), 
    Rdia = re:replace(Isco,  ":celebration:", format_celebration(Date)), 
    % This feels wrong:
    binary_to_list(iolist_to_binary(Rdia)).



-spec format_day(ddate()) -> string().
%%---------------------------------------------------------
%% @doc Day in season to string
%% @end
%%---------------------------------------------------------
format_day({_Yold, _Season, Day}) -> integer_to_list(Day).


-spec format_day_suffix(ddate()) -> string().
%%---------------------------------------------------------
%% @doc Get day suffix (1st, 2nd, ...)
%% @end
%%---------------------------------------------------------
format_day_suffix({_Y, _S, D}) when D rem 100 >= 10,
                                    D rem 100 =< 20  -> "th";
format_day_suffix({_Y, _S, D}) when D rem 10 =:= 1   -> "st";
format_day_suffix({_Y, _S, D}) when D rem 10 =:= 2   -> "nd";
format_day_suffix({_Y, _S, D}) when D rem 10 =:= 3   -> "rd";
format_day_suffix(_)                                 -> "th".



-spec format_season(ddate()) -> string().
%%---------------------------------------------------------
%% @doc Translate season to string
%% @end
%%---------------------------------------------------------
format_season({_Y, 1, _D}) -> "Chaos";
format_season({_Y, 2, _D}) -> "Discord";
format_season({_Y, 3, _D}) -> "Confusion";
format_season({_Y, 4, _D}) -> "Bureaucracy";
format_season({_Y, 5, _D}) -> "The Aftermath".
    

-spec format_yold(ddate()) -> string().
%%---------------------------------------------------------
%% @doc Convert YOLD to string
%% @end
%%---------------------------------------------------------
format_yold({Y, _S, _D}) -> integer_to_list(Y).


-spec format_template(tpl()) -> string().
%%---------------------------------------------------------
%% @doc Get format templates
%% @end
%%---------------------------------------------------------
format_template(full) -> 
    ":weekday:, the :day::day_suffix: day of :season: in the YOLD :yold::celebration:";
format_template(short) ->
    ":weekday:, :season: :day:, :yold: YOLD:celebration:";
format_template(Format) ->
    Format.



-spec format_weekday(weekday() | ddate()) -> string().
%%---------------------------------------------------------
%% @doc Format day in discordian week
%% @end
%%---------------------------------------------------------
format_weekday(1) -> "Sweetmourn";
format_weekday(2) -> "Boomtime";
format_weekday(3) -> "Pungenday";
format_weekday(4) -> "Prickle-Prickle";
format_weekday(5) -> "Setting Orange"; 

format_weekday(Date) ->
    format_weekday(ddate_to_weekday(Date)).



-spec format_holiday(holiday()) -> string().
%%---------------------------------------------------------
%% @doc Format holiday
%% @end
%%---------------------------------------------------------
format_holiday(mungday)     -> "Mungday";
format_holiday(chaoflux)    -> "Chaoflux";
format_holiday(mojoday)     -> "Mojoday";
format_holiday(discoflux)   -> "Discoflux";
format_holiday(syaday)      -> "Syaday";
format_holiday(confuflux)   -> "Confuflux";
format_holiday(zaraday)     -> "Zaraday";
format_holiday(bureflux)    -> "Bureflux";
format_holiday(maladay)     -> "Maladay";
format_holiday(afflux)      -> "Afflux";
format_holiday(st_tibs_day) -> "St. Tib's Day";
format_holiday(none)        -> "".


-spec format_celebration(ddate()|holiday()) -> string().
%%---------------------------------------------------------
%% @doc Format celebration
%% @end
%%---------------------------------------------------------
format_celebration(none) -> "";
format_celebration({_Y, _M, _D} = Date) ->
    format_celebration(ddate_to_holiday(Date));

format_celebration(Holiday) ->
    ", celebrate " ++ format_holiday(Holiday).


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
day_in_year({_, Month, Day}) ->
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



-spec ddate_to_weekday(ddate()) -> weekday().
%%---------------------------------------------------------
%% @doc Convert date to weekday
%% @end
%%---------------------------------------------------------
ddate_to_weekday({_Y, Month, Day}) ->
    DayInYear = Day + 73 * (Month - 1),
    1 + (DayInYear - 1) rem 5.



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
ddate_to_holiday({_, st_tibs_day}) -> st_tibs_day;
ddate_to_holiday(_)           -> none.



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
    [{"date to weekday", ?_assertEqual(Weekday, ddate_to_weekday(date_to_ddate(Date)))} ||
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
    Expected = [{{2017, 1,  5},  "Mungday"},
                {{2017, 2,  19}, "Chaoflux"},
                {{2016, 2,  29}, "St. Tib's Day"},
                {{2017, 3,  19}, "Mojoday"},
                {{2017, 5,  3},  "Discoflux"},
                {{2017, 5,  31}, "Syaday"},
                {{2017, 7,  15}, "Confuflux"},
                {{2017, 8,  12}, "Zaraday"},
                {{2017, 9,  26}, "Bureflux"},
                {{2017, 10, 24}, "Maladay"},
                {{2017, 12, 8},  "Afflux"},
                {{2017, 12, 9},  ""}],
    [{"date to holiday",
      ?_assertEqual(Holiday, format_holiday(
                                ddate_to_holiday(
                                    date_to_ddate(Date))))} ||
        {Date, Holiday} <- Expected].



%%---------------------------------------------------------
%% Test day format suffix
%%---------------------------------------------------------
format_day_suffix_test_() ->
    Expected = [{1, "st"},  {2, "nd"},  {3, "rd"},  {4, "th"},  {5, "th"},
                {9, "th"},  {10, "th"}, {11, "th"}, {14, "th"},
                {20, "th"}, {21, "st"}, {32, "nd"}, {40, "th"}],
    [{"get ordinal suffix",
        ?_assertEqual(Suffix, format_day_suffix({2017, 1, D}))} ||
            {D, Suffix} <- Expected].



%%---------------------------------------------------------
%% Test date to ddate conversion
%%---------------------------------------------------------
date_to_ddate_test_() ->
    Dates = [ {"normal date", {2017, 5, 11}, {3183, 2, 58}},
              {"leap year",   {2016, 2, 29}, {3182, st_tibs_day}} ],

    [{Test, ?_assertEqual(DDate, date_to_ddate(Date))} ||
        {Test, Date, DDate} <- Dates].
 

%%---------------------------------------------------------
%% Test formatting
%%---------------------------------------------------------
format_test_() ->
    Expected = [
        {full, {2017, 5, 23},
               "Pungenday, the 70th day of Discord in the YOLD 3183"},
        {short, {2017, 5, 23},
               "Pungenday, Discord 70, 3183 YOLD"}],
    [{"date formatting",
        ?_assertEqual(Text, format(Fmt, date_to_ddate(Date)))} ||
            {Fmt, Date, Text} <- Expected].


-endif.

