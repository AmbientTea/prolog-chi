:- module(synth, [ synth//2, synth/2 ]).

%! synth(+Type, ?Function)/2 is nondet
synth(Type, Function) :-
    synth(Type, Function, [[], []], _).

%! synth(+Type, ?Function)//2 is nondet
synth(T, Expr) -->
    using(FT, F, apply(FT, T, F, Expr)).

synth(XT -> RT, X -> R) -->
    declare(XT, X),
    synth(RT, R).

%! apply(+ValueType, +TargetType, +Value, -Expression) is nondet
apply(XT, XT, X, X) --> [].
apply(XT -> RT, T, F, Expr) -->
    synth(XT, X),
    apply(RT, T, app(F, X), Expr).

%! declare(+Type, -Variable) is det
declare((T1, T2), (V1, V2)) -->
    declare(T1, V1),
    declare(T2, V2).

declare(T1 -> T2, Var) -->
    fresh_id(f, Var),
    env_add(T1 -> T2, Var).

declare(Type, Var) -->
    { atom(Type) },
    fresh_id(Type, Var),
    env_add(Type, Var).

%! exists(+Type, -Var)//2 is nondet
exists(Type, Var, [Env, Counts], [Env, Counts]) :-
    member(Type - Var, Env).

%! using(-Type, -Variable, :Goal)
using(Type, Var, Goal) -->
    env_remove(Type, Var),
    call(Goal),
    env_add(Type, Var).

env_add(Type, Var), [[Type - Var | Env]] -->
    [Env].
env_remove(Type, Var), [NEnv] -->
    [Env], { select(Type - Var, Env, NEnv) }.
fresh_id(Base, Id), [Env, [Base - NCount | NCounters]] -->
    [Env, Counters],
    {
        select(Base - Count, Counters, NCounters) -> true
        ; Count = 1, NCounters = Counters
    },
    {
        atom_concat(Base, Count, Id),
        NCount is Count + 1
    }.


:- begin_tests(synth).

test(identity, [nondet, true(Function =@= (x1 -> x1))]) :-
    synth(x -> x, Function).

test(application, [nondet, all(Function =@= [(f1 -> f1), (f1 -> a1 -> app(f1,a1))])]) :-
    synth((a -> b) -> a -> b, Function).

test(first, [nondet, true(Function =@= ((a1,b1,c1) -> a1))]) :-
    synth((a, b, c) -> a, Function).

test(constant, [nondet, true(Function =@= (a1 -> b1 -> a1))]) :-
    synth(a -> b -> a, Function).

test(compose, [nondet, true(Function =@= (f1 -> f2 -> a1 -> app(f1, app(f2, a1))))]) :-
    synth((b -> c) -> (a -> b) -> a -> c, Function).

test('apply loop trap', [
         nondet,
         all(Function =@= [
                 (f1 -> f1),
                 (f1 -> x1 -> x1),
                 (f1 -> x1 -> app(f1,x1))
             ])]) :-
    synth((x -> x) -> x -> x, Function).

test('nested application', [
         nondet,
         all(Function =@= [
                 (f1 -> f1),
                 (f1 -> a1 -> app(f1, a1)),
                 (f1 -> a1 -> b1 -> app(app(f1, a1), b1))
             ])]) :-
    synth((a -> b -> c) -> a -> b -> c, Function).

:- end_tests(synth).
