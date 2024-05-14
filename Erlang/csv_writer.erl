-module(csv_writer).
-export([
         csv_writer/3
        ]).

csv_writer(Durations, FileName, AmountOfWorkers) ->
    {ok, File} = file:open(FileName ++ ".csv", [write]),
    write_data(File, Durations, 1, AmountOfWorkers),
    file:close(File).

write_data(_, [], _, _) ->
    ok;
write_data(File, [Duration | Durations], TestNumber, AmountOfWorkers) ->
    io:format(File, "~w, ~.10f, ~w~n", [TestNumber, Duration, AmountOfWorkers]),
    write_data(File, Durations, TestNumber + 1, AmountOfWorkers).
