-module(matrix_generator).
-export([generate_matrices/2]).

generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].

generate_matrices(Size, FileName) ->
    Matrices = {generate_matrix(Size), generate_matrix(Size)},
    write_matrices_to_file(Matrices, FileName).

write_matrices_to_file({A, B}, FilePath) ->
    {ok, File} = file:open(FilePath, [write]),
    io:fwrite(File, "Matrix A:~n", []),
    write_matrix_to_file(A, File),
    io:fwrite(File, "Matrix B:~n", []),
    write_matrix_to_file(B, File),
    file:close(File).

write_matrix_to_file(Matrix, File) ->
    lists:foreach(fun(Row) -> io:fwrite(File, "~p~n", [Row]) end, Matrix).
