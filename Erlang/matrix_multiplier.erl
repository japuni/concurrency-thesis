-module(matrix_multiplier).
-compile(export_all).


test(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    Operations = math:pow(N, 3),
    Start = erlang:monotonic_time(nanosecond),
    multiply(A, B),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    Throughput = (Operations / Duration) / 1000,
    io:format("Computation time (sequential): ~f seconds~n", [Duration]),
    io:format("Throughput (sequential): ~p kOp/s", [Throughput]).

test_parallel(N, AmountOfWorkers) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    Operations = math:pow(N, 3),
    Start = erlang:monotonic_time(nanosecond),
    C = multiply_parallel(A, B, AmountOfWorkers),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    Throughput = (Operations / Duration) / 1000,
    io:format("Computation time (parallel): ~f seconds~n", [Duration]),
    io:format("Thrughput time (parallel): ~p kOp/s~n", [Throughput]).

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
%% Summera allt eftersom
for_column_in_row([], [], Sum, RestOfMatrixB) ->
    {Sum, lists:reverse(RestOfMatrixB)};
for_column_in_row([ColInRow | RestOfRow], [[Head| Tail] | Rest ], Sum, RestOfMatrixB) ->
    for_column_in_row(RestOfRow, Rest, ColInRow * Head + Sum, [Tail | RestOfMatrixB]).

generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].
