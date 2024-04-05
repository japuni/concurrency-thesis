-module(matrix).
-export([multiply/2, test/1, test2/1]).

test(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    % WorkerPids = [spawn(fun() -> worker() end) || _ <- lists:seq(1, N)],
    Start = erlang:monotonic_time(),
    io:format("Här ~p~n", [multiply(A, B)]),
    End = erlang:monotonic_time(),
    Duration = (End - Start) / 1_000_000,
    io:format("~f seconds~n", [Duration / 1000]).


test2(N) ->
    A = generate_matrix(N),
    B = generate_matrix(N),
    Start = erlang:monotonic_time(),
    io:format("Här ~p~n", [multiply2(A, B)]),
    End = erlang:monotonic_time(),
    Duration = (End - Start) / 1_000_000,
    io:format("~f seconds~n", [Duration / 1000]).


multiply(MatrixA, MatrixB) ->
    multiply_matrices(MatrixA, flip_matrix(MatrixB, [], [], []), []).

multiply_matrices([], _, ResultMatrix) ->
    lists:reverse(ResultMatrix);

multiply_matrices([Head | Tail], MatrixB, ResultMatrix) ->
    multiply_matrices(Tail, MatrixB, [calculate_row(Head, MatrixB, []) | ResultMatrix]).

calculate_row(_, [], Acc) ->
    lists:reverse(Acc);
calculate_row(Row, [Column | Rest], Acc) ->
    Val = ([X * Y || {X,Y} <- lists:zip(Row,Column)]),
    calculate_row(Row, Rest, [lists:sum(Val) | Acc]).

flip_matrix([], Column, [], ListsOfColumns) ->
    lists:reverse([Column | ListsOfColumns]);
flip_matrix([], Column, Rest, ListsOfColumns) ->
    flip_matrix(Rest, [], [], [Column | ListsOfColumns]);

flip_matrix([Head | Tail], Column, Rest, ListsOfColumns) when length(Head) > 1 ->
    flip_matrix(Tail, [hd(Head) | Column], [tl(Head) | Rest], ListsOfColumns);

flip_matrix([Head | Tail], Column, Rest, ListsOfColumns) ->
     flip_matrix(Tail, [hd(Head)| Column], Rest, ListsOfColumns).
 

extract_column([], {Column, Rest}) ->
    {lists:reverse(Column), lists:reverse(Rest)};

extract_column([Head | Tail], {Column, Rest}) when length(Head) > 1 ->
    extract_column(Tail, {[hd(Head) | Column], [tl(Head) | Rest]});

extract_column([Head | Tail], {Column, Rest}) ->
    extract_column(Tail, {[hd(Head)| Column], Rest}).
% Function to generate a matrix of given size
generate_matrix(Size) ->
    [[rand:uniform(10) || _ <- lists:seq(0, Size)] || _ <- lists:seq(0, Size)].


transpose([]) -> [];
transpose([[]|_]) -> [];
transpose(Matrix) ->
  [ [ hd(Row) || Row <- Matrix ] | transpose([ tl(Row) || Row <- Matrix ]) ].

multiply2(MatrixA, MatrixB) ->
  lists:map(fun(Row) -> multiplyRow(Row, MatrixB) end, MatrixA).

multiply2(MatrixA, MatrixB, WorkerPids) ->
    [Pid ! {work, self(), Row, MatrixB} || Pid <- WorkerPids, Row <- MatrixA],
    [receive
         {Pid, Result} ->
             Result
     end
     || Pid <- WorkerPids].


multiplyRow(Row, Matrix) ->
    lists:map(fun(Col) -> multiplyElements(Row, Col)end, transpose(Matrix)).

multiplyElements(Row, Col) ->
    lists:foldl(fun({A, B}, Accumulator) -> A*B + Accumulator end, 0, lists:zip(Row, Col)).

worker() ->
    receive
        {work, From, Row, MatrixB} ->
            Result = multiplyRow(Row, MatrixB),
            From ! {self(), Result}
    end.
