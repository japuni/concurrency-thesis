-module(matrix).
-export([test/1, test_parallel/2]).

test(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    Operations = math:pow(N, 3),
    Start = erlang:monotonic_time(nanosecond),
    FlippedMatrix = flip_matrix(B),
    multiply(A, FlippedMatrix),
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
    FlippedMatrix = flip_matrix(B),
    multiply_parallel(A, FlippedMatrix, AmountOfWorkers,N),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    Throughput = (Operations / Duration) / 1000,
    io:format("Computation time (parallel): ~f seconds~n", [Duration]),
    io:format("Thrughput time (parallel): ~p kOp/s~n", [Throughput]).



multiply(MatrixA, MatrixB) ->
    multiply_matrices(MatrixA, MatrixB,[]).

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

    
multiply_matrices([], _, ResultMatrix) ->
    lists:reverse(ResultMatrix);

multiply_matrices([Head | Tail], MatrixB, ResultMatrix) ->
    multiply_matrices(Tail, MatrixB, [calculate_row(Head, MatrixB, []) | ResultMatrix]).

calculate_row(_, [], Acc) ->
    lists:reverse(Acc);
calculate_row(Row, [Column | Rest], Acc) ->
    Val = [X * Y || {X,Y} <- lists:zip(Row,Column)],
    calculate_row(Row, Rest, [lists:sum(Val) | Acc]).

flip_matrix(Matrix) ->
    flip_matrix(Matrix, [], [], []).

flip_matrix([], Column, [], ListsOfColumns) ->
    lists:reverse([Column | ListsOfColumns]);
flip_matrix([], Column, Rest, ListsOfColumns) ->
    flip_matrix(Rest, [], [], [Column | ListsOfColumns]);

flip_matrix([Head | Tail], Column, Rest, ListsOfColumns) when length(Head) > 1 ->
    flip_matrix(Tail, [hd(Head) | Column], [tl(Head) | Rest], ListsOfColumns);

flip_matrix([Head | Tail], Column, Rest, ListsOfColumns) ->
     flip_matrix(Tail, [hd(Head)| Column], Rest, ListsOfColumns).
 
split_matrix([], _, Acc) ->
    lists:reverse(Acc);

split_matrix(Matrix, SplitValue, Acc) when length(Matrix) > (SplitValue*2-1) ->
    {Chunk, Rest} = lists:split(SplitValue, Matrix),
    split_matrix(Rest, SplitValue, [Chunk | Acc]);

split_matrix(Matrix, SplitValue, Acc) ->
    split_matrix([], SplitValue, [Matrix | Acc]).

% Function to generate a matrix of given size
generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].

