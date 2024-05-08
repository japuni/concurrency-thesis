-module(matrix_multiplier).
-export([test/2, generate_matrix/1, multiply_parallel/3, multiply/2]).

test(N, AmountOfWorkers) ->
    {A, B} = generate_matrices(N),
    SequentialResults = run_tests(N, {A, B}, 10, []),
    ParalellResults = run_tests_paralell(N, AmountOfWorkers, {A, B}, 10, []),
    csv_writer(SequentialResults, "Sequential", 1),
    csv_writer(ParalellResults, "Parallel", AmountOfWorkers).

csv_writer(Durations, FileName, AmountOfWorkers) ->
    {ok, File} = file:open(FileName ++ ".csv", [write]),
    write_data(File, Durations, 1, AmountOfWorkers),
    file:close(File).

write_data(_, [], _, _) ->
    ok;
write_data(File, [Duration | Durations], TestNumber, AmountOfWorkers) ->
    io:format(File, "~w, ~.10f, ~w~n", [TestNumber, Duration, AmountOfWorkers]),
    write_data(File, Durations, TestNumber + 1, AmountOfWorkers).

run_tests(_, _, 0, Results) ->
    lists:reverse(Results);

run_tests(N, {A, B}, Count, Results) ->
    Start = erlang:monotonic_time(nanosecond),
    multiply(A, B),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("Sequential: ~p~n", [Duration]),
    run_tests(N, {A, B}, Count - 1, [Duration | Results]).


run_tests_paralell(_, _, _, 0, Results) ->
    lists:reverse(Results);

run_tests_paralell(N, AmountOfWorkers, {A, B}, Count, Results) ->
    Start = erlang:monotonic_time(nanosecond),
    multiply_parallel(A, B, AmountOfWorkers),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("Parallel: ~p~n", [Duration]),
    run_tests_paralell(N, AmountOfWorkers, {A, B}, Count - 1, [Duration | Results]).

multiply_parallel(MatrixA, MatrixB, AmountOfWorkers) ->
    Master = self(),
    WorkPool = spawn(fun() -> workpool(MatrixA, [], Master) end),
    [spawn(fun() -> worker(MatrixB, WorkPool) end) || _ <- lists:seq(1, AmountOfWorkers)],
    receive
        {done, MatrixC} ->
            MatrixC
    end.

workpool([], Refs, Master) ->
    MatrixC = [receive
         {done, Ref, Result} ->
            Result
    end || Ref <- lists:reverse(Refs)],
    Master ! {done, MatrixC};
workpool([Row | Rest], Refs, Master) ->
    receive 
        {ready, Worker, Ref} ->
            Worker ! {work, Row, Ref},
            workpool(Rest, [Ref | Refs], Master)
    end.

worker(MatrixB, WorkPool) ->
    Ref = make_ref(),
    WorkPool ! {ready, self(), Ref},
    receive
        {work, Row, Ref} ->
            Result = for_column_in_matrixB(Row, MatrixB, []),
            WorkPool ! {done, Ref, Result},
            worker(MatrixB, WorkPool)
    after 1000 -> 
              ok
    end.

multiply(MatrixA, MatrixB) ->
    for_row_in_matrixA(MatrixA,MatrixB, []).

for_row_in_matrixA([], _MatrixB, MatrixC) ->
    lists:reverse(MatrixC);
for_row_in_matrixA([Row | Rest], MatrixB, Acc) ->
    RowInMatrixC = for_column_in_matrixB(Row, MatrixB, []),
    for_row_in_matrixA(Rest, MatrixB, [RowInMatrixC | Acc]).


for_column_in_matrixB(_, [[]| _Rest], RowInMatrixC) ->
    lists:reverse(RowInMatrixC);
for_column_in_matrixB(Row, MatrixB, Acc) ->
    {Element, RestOfMatrixBColumns} = for_column_in_row(Row, MatrixB, 0, []),
    for_column_in_matrixB(Row, RestOfMatrixBColumns, [Element | Acc]).

for_column_in_row([], [], Sum, RestOfMatrixB) ->
    {Sum, lists:reverse(RestOfMatrixB)};
for_column_in_row([ColInRow | RestOfRow], [[Head| Tail] | Rest ], Sum, RestOfMatrixB) ->
    for_column_in_row(RestOfRow, Rest, ColInRow * Head + Sum, [Tail | RestOfMatrixB]).

generate_matrices(Size) ->
    {generate_matrix(Size), generate_matrix(Size)}.

generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].
