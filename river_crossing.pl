:- dynamic wife/2.
:- dynamic husband/2.
:- dynamic state_db/1.
:- dynamic current_safety_mode/1.

current_safety_mode(strict).

% Генерация пар в базе знаний
generate_pairs(N) :-
    retractall(wife(_, _)),
    between(1, N, I),
    format(atom(W), 'w~w', [I]),
    format(atom(H), 'h~w', [I]),
    assertz(wife(W, H)),
    fail.
generate_pairs(_).

husband(H, W) :- wife(W, H).

% Решатель с поиском в ширину
solve(StartState, TargetState, BoatCapacity, Solution) :-
    retractall(state_db(_)),
    format('Начальное состояние:~n'),
    print_state(StartState),
    bfs([[StartState]], TargetState, BoatCapacity, Solution),
    format('~nРешение найдено.~n'),
    print_solution(Solution).

% Проверка эквивалентности состояний
states_equal(S1, S2) :-
    sort_state(S1, NormS1),
    sort_state(S2, NormS2),
    NormS1 == NormS2.

% Нормализация состояния
sort_state(state(R, L, B), state(SR, SL, B)) :-
    sort(R, SR),
    sort(L, SL).

% Алгоритм поиска в ширину
bfs([[State|Path]|_], Target, _, Solution) :-
    states_equal(State, Target), 
    reverse([State|Path], Solution).

bfs([[State|Path]|Rest], Target, BoatCapacity, Solution) :-
    findall([NextState,State|Path],
            (move(State, NextState, BoatCapacity),
             \+ visited(NextState)),
            NextStates),
    assert_visited(NextStates),
    append(Rest, NextStates, NewQueue),
    bfs(NewQueue, Target, BoatCapacity, Solution).

% Проверка посещения состояния
visited(State) :-
    state_hash(State, Hash),
    state_db(Hash).

% Добавление состояний в базу данных
assert_visited([]).
assert_visited([[S|_]|T]) :-
    state_hash(S, Hash),
    assertz(state_db(Hash)),
    assert_visited(T).

% Перемещение с одного берега на другой
move(state(Right, Left, BoatPos), state(NewRight, NewLeft, NewBoatPos), Capacity) :-
    format('Правый берег: ~w, Левый берег: ~w~n', [Right, Left]),
    (BoatPos == right ->
        boat_group(Right, Capacity, Group, Remaining),
        NewBoatPos = left,
        append(Left, Group, NewLeft),
        NewRight = Remaining
    ;
        boat_group(Left, Capacity, Group, Remaining),
        NewBoatPos = right,
        append(Right, Group, NewRight),
        NewLeft = Remaining
    ),
    check_safety(NewRight),
    check_safety(NewLeft).

% Выбор группы людей, которые сядут в лодку
boat_group(Shore, Max, Group, Remaining) :-
    between(1, Max, Len),
    length(Group, Len),
    append(Group, Remaining, Shore),
    check_safety(Group),
    check_safety(Remaining).

% Проверка безопасности "Ни одна супруга не может находится на берегу или в лодке без супруга"
safe(Group) :-
    forall((member(W, Group), wife(W, H)), member(H, Group)).

% Проверка "Нельзя оставлять чужую жену и чужого мужа вместе без супругов"
unsafe(Group) :-    
    member(W, Group),
    wife(W, H),
    \+ member(H, Group),
    member(M, Group),
    husband(M, WM),
    \+ member(WM, Group).

% Универсальный предикат проверки безопасности
check_safety(Group) :-
    current_safety_mode(Mode),
    (Mode == strict -> safe(Group);
     Mode == relaxed -> \+ unsafe(Group)).

% Изменение режима безопасности (strict, relaxed)
set_safety_mode(Mode) :-
    retractall(current_safety_mode(_)),
    assertz(current_safety_mode(Mode)),
    format('Режим безопасности изменен на ~w~n', [Mode]).

% Хеширование состояний
state_hash(state(R, L, _), Hash) :-
    sort(R, SR), sort(L, SL),
    term_hash([SR, SL], Hash).

% Вывод состояния в понятном формате
print_state(state(Right, Left, BoatPos)) :-
    format('~n'),
    format('   Правый берег: ~40w~n', [Right]),
    format('   Левый берег:  ~40w~n', [Left]),
    (BoatPos == right 
        -> format('   Лодка: right~n');  
    	   format('   Лодка: left~n')),
    format('~n').

% Вывод полного решения
print_solution([]) :- !.
print_solution([State]) :- 
    format('~nФинальное состояние:~n'),
    print_state(State).

print_solution([State1, State2|Rest]) :-
    print_state(State1),
    print_solution([State2|Rest]).

% Пример использования:
%set_safety_mode(relaxed), generate_pairs(5), solve(state([h1, w1, h2, w2, h3, w3, h4, w4, h5, w5], [], right), 
%    state([h2, w2, h3, w3, h4, w4, h5, w5], [h1, w1], left), 
%    3, 
%    Solution).