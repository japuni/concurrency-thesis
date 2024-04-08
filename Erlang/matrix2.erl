-module(matrix2).
-export([multiply/2, test/1, test_parallel/2]).

test(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    Operations = math:pow(N, 3),
    Start = erlang:monotonic_time(nanosecond),
    % FlippedMatrix = flip_matrix(B),
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
    multiply_parallel(A, B, AmountOfWorkers,N),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    Throughput = (Operations / Duration) / 1000,
    io:format("Computation time (parallel): ~f seconds~n", [Duration]),
    io:format("Thrughput time (parallel): ~p kOp/s~n", [Throughput]).

multiply_parallel(MatrixA, MatrixB, Workers, N) ->
    SplitValue = N div Workers,
    WorkerPids = [spawn(fun() -> worker(MatrixB) end) || _ <- lists:seq(1, Workers)],
    WorkList = split_matrix(MatrixA, SplitValue, []),
    io:format("Antal Workers = ~p~nAntal Work = ~p~n", [length(WorkerPids), length(WorkList)]),
    send_work(WorkList, WorkerPids),
    gather_result(WorkerPids, []).

send_work([], []) ->
    ok;
send_work([Work | RestOfWork], [WorkerPid | RestOfWorkers]) ->
    WorkerPid ! {work, self(), Work},
    send_work(RestOfWork, RestOfWorkers).

worker(Matrix) ->
    receive 
        {work, Master, Work} ->
            Result = multiply(Work, Matrix),
            Master ! {self(), Result}
    end.

gather_result([], Acc) ->
    lists:reverse(Acc);
gather_result([Pid | WorkerPids], Acc) ->
    receive
        {Pid, Result} ->
            gather_result(WorkerPids, [Result | Acc])
    end.
transpose([[]|_]) ->
    [];
transpose(B) ->
  [lists:map(fun hd/1, B) | transpose(lists:map(fun tl/1, B))].


red(Pair, Sum) ->
    X = element(1, Pair),   %gets X
    Y = element(2, Pair),   %gets Y
    X * Y + Sum.

%% Mathematical dot product. A x B = d
%% A, B = 1-dimension vector
%% d    = scalar
dot_product(A, B) ->
    lists:foldl(fun red/2, 0, lists:zip(A, B)).


%% Exposed function. Expected result is C = A x B.
multiply(A, B) ->
    %% First transposes B, to facilitate the calculations (It's easier to fetch
    %% row than column wise).
    multiply_internal(A, transpose(B)).


%% This function does the actual multiplication, but expects the second matrix
%% to be transposed.
multiply_internal([Head | Rest], B) ->
    % multiply each row by Y
    Element = multiply_row_by_col(Head, B),

    % concatenate the result of this multiplication with the next ones
    [Element | multiply_internal(Rest, B)];

multiply_internal([], B) ->
    % concatenating and empty list to the end of a list, changes nothing.
    [].


multiply_row_by_col(Row, [Col_Head | Col_Rest]) ->
    Scalar = dot_product(Row, Col_Head),

    [Scalar | multiply_row_by_col(Row, Col_Rest)];

multiply_row_by_col(Row, []) ->
    [].
generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].
split_matrix([], _, Acc) ->
    lists:reverse(Acc);

split_matrix(Matrix, SplitValue, Acc) when length(Matrix) > (SplitValue*2-1) ->
    {Chunk, Rest} = lists:split(SplitValue, Matrix),
    io:format("Lenght of chunk ~p~n", [length(Chunk)]),,
    split_matrix(Rest, SplitValue, [Chunk | Acc]);

split_matrix(Matrix, SplitValue, Acc) ->
    split_matrix([], SplitValue, [Matrix | Acc]).

