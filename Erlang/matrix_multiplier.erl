-module(matrix_multiplier).
-export([test/1, generate_matrix/1, multiply_parallel/3, multiply/2]).
read_matrix() ->
    {ok, BinaryContent} = file:read_file("../matrices/1000.txt"),
    Content = binary_to_list(BinaryContent),
    I = string:tokens(Content, "\n"),
    X = lists:flatten(I),
    X.

test(AmountOfWorkers) ->
    Matrix = parse(read_matrix()),
    SequentialResults = run_tests(Matrix, 10, []),
    ParalellResults = run_tests_paralell(AmountOfWorkers, Matrix, 10, []),
    csv_writer:csv_writer(SequentialResults, "SequentialMatrixMultiplier"),
    csv_writer:csv_writer(ParalellResults, "ParallelMatrixMultiplier").

run_tests(_, 0, Results) ->
    lists:reverse(Results);

run_tests(Matrix, Count, Results) ->
    Start = erlang:monotonic_time(nanosecond),
    multiply(Matrix, Matrix),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("Sequential: ~p~n", [Duration]),
    run_tests(Matrix, Count - 1, [Duration | Results]).


run_tests_paralell(_, _, 0, Results) ->
    lists:reverse(Results);

run_tests_paralell(AmountOfWorkers, Matrix, Count, Results) ->
    Start = erlang:monotonic_time(nanosecond),
    multiply_parallel(Matrix, Matrix, AmountOfWorkers),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("Parallel: ~p~n", [Duration]),
    run_tests_paralell(AmountOfWorkers, Matrix, Count - 1, [Duration | Results]).

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

parse(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    Dot = case lists:last(Tokens) of
              {dot, _} -> [];
              _ -> [{dot, 1}]
          end,
    {ok, Result} = erl_parse:parse_term(Tokens ++ Dot),
    Result.
