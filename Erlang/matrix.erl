-module(matrix).
-export([test/1, test_parallel/2]).

test(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    FlippedMatrix = flip_matrix(B),
    Start = erlang:monotonic_time(nanosecond),
    multiply(A, FlippedMatrix),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("~f seconds~n", [Duration]).

test_parallel(N, AmountOfWorkers) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    FlippedMatrix = flip_matrix(B),
    Start = erlang:monotonic_time(nanosecond),
    multiply_parallel(A, FlippedMatrix, AmountOfWorkers,N),
    End = erlang:monotonic_time(nanosecond),
    Duration = (End - Start) / 1_000_000_000,
    io:format("~f seconds~n", [Duration]).


multiply(MatrixA, MatrixB) ->
    multiply_matrices(MatrixA, MatrixB,[]).

multiply_parallel(MatrixA, MatrixB, Workers, N) ->
    Master = self(),
    SplitValue = N div Workers,
    WorkPool = spawn(fun() -> 
        worker_pool(Master, MatrixA, [], MatrixB, SplitValue) end),
    [spawn(fun() -> worker(WorkPool) end) || _ <- lists:seq(1, Workers)],
    receive
        {Results} ->
            Results
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
 

worker_pool(Master, [], Refs, _, _) ->
    Results = [receive
         {Ref, Result} -> Result 
               after 20000 ->
                       io:format("här gick det åt helvete")
     end || Ref <- Refs],
    Master ! {Results};

worker_pool(Master, WorkList, Refs, Matrix, SplitValue) when length(WorkList) > SplitValue->
    {Work, Rest} = lists:split(SplitValue, WorkList),
    receive
        {ready, Pid} ->
            Ref = make_ref(),
            Pid ! {work, Ref, Work, Matrix},
            worker_pool(Master, Rest, [Ref | Refs], Matrix, SplitValue)
    end;

worker_pool(Master, Work, Refs, Matrix, SplitValue) ->
    receive
        {ready, Pid} ->
            Ref = make_ref(),
            Pid ! {work, Ref, Work, Matrix},
            worker_pool(Master, [], [Ref | Refs], Matrix, SplitValue)
    end.
worker(Master) ->
    Master ! {ready, self()},
    receive 
        {work, Ref, Work, Matrix} ->
            Result = multiply(Work, Matrix),
            Master ! {Ref, Result},
            worker(Master)
    after 200 ->
              ok
    end.

% Function to generate a matrix of given size
generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(0, Size)] || _ <- lists:seq(0, Size)].

