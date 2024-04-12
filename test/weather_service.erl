-module(weather_service).


-export([
    generate_message/3
]).


generate_message(Temperature, WindSpeed, Humidity) ->
    #{
        <<"weather">> => #{
            <<"temperature">> => Temperature,
            <<"humidity">> => Humidity,
            <<"wind_speed_kmh">> => WindSpeed
        },
        <<"timestamp">> => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(second))
        )
    }.
