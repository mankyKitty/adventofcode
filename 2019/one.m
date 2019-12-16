:- module one.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stream.
:- import_module string.
:- import_module int.
:- import_module float.
:- import_module list.

:- func fuel_cost(int) = int.

fuel_cost(A) =
  int.div(A, 3) - 2.

:- func accum_fuel_cost(string, int) = int.

accum_fuel_cost(Mass, In) =
  fuel_cost(string.det_to_int(Mass)) + In.

:- pred fuel_cost_part_two(int::in, int::out) is det.

fuel_cost_part_two(Mass, Out) :-
  fuel_cost_part_two_n(Mass, 0, Out).

:- pred fuel_cost_part_two_n(int::in, int::in, int::out) is det.
fuel_cost_part_two_n(Mass, Acc, Out) :-
  R = fuel_cost(Mass),
  ( if 0 >= R then
    Out = Acc
  else
    fuel_cost_part_two_n(R, R + Acc, Out)
  ).

:- pred one_stream(io.input_stream::in, int::in, int::out, io::di, io::uo) is det.

one_stream(Stream, Cost, Out, !IO) :-
  io.read_line_as_string(Stream, Result, !IO),
  ( ok(Line) = Result, 
    one_stream(Stream, accum_fuel_cost(string.strip(Line), Cost), Out, !IO)
  ; eof = Result,
    Out = Cost
  ; error(Msg) = Result,
    io.print(Msg, !IO),
    Out = 0
  ).

:- pred one_stream_2(io.input_stream::in, int::in, int::out, io::di, io::uo) is det.

one_stream_2(Stream, Cost, Out, !IO) :-
  io.read_line_as_string(Stream, Result, !IO),
  ( ok(Line) = Result, 
    fuel_cost_part_two(string.det_to_int(string.strip(Line)), NewCost),
    one_stream_2(Stream, Cost + NewCost, Out, !IO)
  ; eof = Result,
    Out = Cost
  ; error(Msg) = Result,
    io.print(Msg, !IO),
    Out = 0
  ).

:- pred test_fuel_cost_two(int::in, io::di, io::uo) is det.

test_fuel_cost_two(N, !IO) :-
  fuel_cost_part_two(N, Result),
  io.format("Answer : %i = %i \n", [i(N), i(Result)], !IO).

main(!IO) :-
  io.open_input("1.in.txt", In, !IO),
  ( if ok(Stream) = In then
    %one_stream(Stream, 0, Result, !IO),
    one_stream_2(Stream, 0, Result, !IO),
    io.print(Result, !IO),
    io.close_input(Stream, !IO)
  else
    io.print(In, !IO)
  ).
