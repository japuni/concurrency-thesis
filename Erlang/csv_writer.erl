-module(csv_writer).
-export([
         csv_writer/2
        ]).

csv_writer(Durations, FileName) ->
    {ok, File} = file:open(FileName ++ ".csv", [write]),
    write_data(File, Durations, 1),
    file:close(File).

write_data(_, [], _) ->
    ok;
write_data(File, [Duration | Durations], TestNumber) ->
    io:format(File, "~w, ~.10f, ~n", [TestNumber, Duration]),
    write_data(File, Durations, TestNumber + 1).
