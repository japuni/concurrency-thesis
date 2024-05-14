-module(matrix_generator).
-export([generate_matrices/2]).

generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].

generate_matrices(Size, FileName) ->
    Matrix = generate_matrix(Size),
    write_matrices_to_file(Matrix, FileName).

write_matrices_to_file(Matrix, FilePath) ->
    {ok, File} = file:open(FilePath, [write]),
    write_matrix_to_file(Matrix, File),
    file:close(File).

write_matrix_to_file(Matrix, File) ->
    lists:foreach(fun(Row) -> io:fwrite(File, "~p,~n", [Row]) end, Matrix).
