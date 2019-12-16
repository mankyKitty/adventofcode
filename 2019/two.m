:- module two.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module string.
:- import_module char.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module maybe.
:- import_module exception.

:- type status
   ---> continue(list(int), int)
   ;    halt(list(int))
   ;    no_further_instructions(list(int)).

:- type op
   ---> op(int, int, int).

:- pred eval_op((func(int, int) = int)::in, op::in, list(int)::in, list(int)::out) is semidet.

eval_op(Op, op(A, B, Store), Memory, Result) :-
  AVal = list.det_index0(Memory, A),
  BVal = list.det_index0(Memory, B),
  % This function assumes list index starts at 1.
  list.replace_nth(Memory, Store + 1, Op(AVal, BVal), Result).

:- pred eval_instruction(list(int)::in, list(int)::in, int::in, status::out) is multi.

eval_instruction([ Op | Rest ], Memory, Ptr, Out) :-
  ( Op = 1, list.take(3, Rest, [ A, B, Store ]), 
    eval_op(int.plus, op(A, B, Store), Memory, NewMemory),
    Out = continue(NewMemory, Ptr + 4)

  ; Op = 2, list.take(3, Rest, [ A, B, Store ]),
    eval_op(int.times, op(A, B, Store), Memory, NewMemory),
    Out = continue(NewMemory, Ptr + 4)

  ; Op = 99,
    Out = halt(Memory)
  ).
eval_instruction(_, Memory, _, no_further_instructions(Memory)).

:- pred run_codes(list(int)::in, list(int)::in, int::in, maybe_error(list(int), string)::out) is multi.

run_codes(Instructions, Memory, Ptr, Out) :-
  eval_instruction(Instructions, Memory, Ptr, Computation),
  ( Computation = continue(NewMemory, NewPtr),
    ( list.split_list(NewPtr, NewMemory, _, NextInstruction), 
      run_codes(NextInstruction, NewMemory, NewPtr, Out)

    ; Out = ok(NewMemory)
    )
  ; Computation = halt(NewMemory),
    Out = ok(NewMemory)

  ; Computation = no_further_instructions(Mem),
    Out = ok(Mem)
  ).

:- pred find_inputs(list(int), int, int, int, list(int)).
:- mode find_inputs(in, in, in, in, out) is semidet.
:- mode find_inputs(in, in, out, out, out) is multi.
:- mode find_inputs(in, in, in, out, out) is multi.
:- mode find_inputs(in, in, out, in, out) is multi.

find_inputs(BaseMemory, DesiredZero, A, B, Done) :-
  A > 0, B >= 0,

  % Replace position 1 with the value A,
  list.replace_nth(BaseMemory, (1 + 1), A, PostMod1), % cheating

  % and replace position 2 with the value B. 
  list.replace_nth(PostMod1, (2 + 1), B, Memory), % cheating

  % Run the computation
  run_codes(Memory, Memory, 0, ok(Done)),

  % Find the computation that gives us the final memory
  list.det_index0(Done, 0) = DesiredZero.

:- pred is_comma(char::in) is semidet.
is_comma(',').

main(!IO) :-
  Xs = [1,0,0,0,99], run_codes(Xs, Xs, 0, XRes), io.print(XRes, !IO), io.nl(!IO),
  Ys = [2,3,0,3,99], run_codes(Ys, Ys, 0, YRes), io.print(YRes, !IO), io.nl(!IO),
  Zs = [2,4,4,5,99,0], run_codes(Zs, Zs, 0, ZRes), io.print(ZRes, !IO), io.nl(!IO),
  Os = [1,1,1,4,99,5,6,0,99], run_codes(Os, Os, 0, ORes), io.print(ORes, !IO), io.nl(!IO),

  io.open_input("2.in.txt", TextStreamM, !IO),

  ( TextStreamM = ok(Stream),

    % Read entire input
    io.read_file_as_string(Stream, CodeStringM, !IO),

    ( CodeStringM = ok(File),
      RawProgram  = list.map(string.det_to_int, string.split_at_separator(is_comma, string.chomp(File))),

      % Problem one requires the memory be altered
      % To do this, before running the program: 
      % Replace position 1 with the value 12,
      list.det_replace_nth(RawProgram, (1 + 1), 12, PostMod1), % cheating
      % and replace position 2 with the value 2. 
      list.det_replace_nth(PostMod1, (2 + 1), 2, Memory), % cheating
     
      run_codes(Memory, Memory, 0, Result),

      ( Result = ok(NewMemory),
        io.print("What value is left at position 0 after the program halts?\n", !IO),
        list.det_index0(NewMemory, 0) = AtZero,
        io.format("Value at position 0: %i\n", [i(AtZero)], !IO)

      ; Result = error(Err), io.format("Error encountered: %s\n", [s(Err)], !IO)
      )

    ; CodeStringM = error(_, Error), io.print(Error, !IO)
    ),

    % We're done so pack it up.
    io.close_input(Stream, !IO)

  ; TextStreamM = error(Error),
    io.print(Error, !IO)
  ).
