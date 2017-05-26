ddatee
=====

An erlang implementation of the discordian calendar.

This works by converting a date tuple `{Year, Month, Day}` to 
a discordian date tuple `{Yold, Season, Day}` and provide 
functions for formatting.


## Usage

Convert a date to ddate
    
    Ddate = ddatee:date_to_ddate({2017, 5, 26}).

This should result in the tuple `{3183, 2, 73}`
which can then be converted in a string by using

    ddatee:format(Ddate).

Which results in 

    "Sweetmourn, the 73rd day of Discord in the YOLD 3183"


### Built in templates

Ddatee ships with two formatting templates included: `short` and `full`.

While `full` mimics the output of the original `ddate` for the current date,
`short` acts like running ddate with a given date, `(e.g. "Sweetmorn, Discord 73, 3183 YOLD")`.


### Custom formats

You can provide a custom format string. `ddatee:format` defaults to `full`
which expands to

    ":weekday:, the :day::day_suffix: day of :season: in the YOLD :yold::celebration:"


The supported format tags are:

    :day:      
    :day_suffix:
    :season:
    :yold:  
    :weekday:  
    :season:    
    :celebration:
               


