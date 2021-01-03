:- module(scxml_parser, [
    scxml_parse/1,
    interpret/1,
    run/2,
    list/0,
    clean/0
]).

:- use_module('../web_prolog.pl').

:- use_module(library(sort)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).

:- use_module(library(debug)).

:- prolog_ide(debug_monitor).
%:- prolog_ide(thread_monitor).

%:- openlog(scxml, [ndelay], syslog).
	
    
:- dynamic state/2, 
           to_be_invoked/3,
           parallel/2,
           history/3,
           final/2,
           initial/1,
           initial/2,
           transition/5,
           onexit/2,
           onentry/2,
           n/2,
           running/0,
           event/1,
           internal_queue/1,
           historyValue/2, 
           configuration/1,
           states_to_invoke/1.


:- debug(scxml(parse)).
:- debug(scxml(model)).
:- debug(scxml(event)).
:- debug(scxml(config)).
:- debug(scxml(execute)).
:- debug(scxml(invoke)).
:- debug(scxml(info)).

:- nodebug(http(_)).


scxml_parse(File) :-
    load_xml_file(File, ListOfContent),
    model_generate(ListOfContent, null).
   

model_generate([], _).
model_generate([element(Name, Attrs, Children)|Rest], Parent) :-
    model_generate(Name, Attrs, Children, Parent, NewParent),
    !,
    model_generate(Children, NewParent),
    model_generate(Rest, Parent).
model_generate([_|Rest], Parent) :-
    model_generate(Rest, Parent).
    

model_generate(scxml, Attrs, _Children, Parent, ID) :-
    option(id(ID), Attrs, scxml),
    gennum(N), model_assert(n(N, ID)), 
    model_assert(state(ID, Parent)), 
    option(initial(InitID), Attrs), 
    model_assert(initial(InitID)).
model_generate(state, Attrs, _Children, Parent, ID) :-
    option(id(ID), Attrs), 
    gennum(N), model_assert(n(N, ID)), 
    model_assert(state(ID, Parent)),
    (   option(initial(Initial), Attrs)
    ->  model_assert(transition(init(ID), '', true, [Initial], []))
    ;   true
    ).
model_generate(parallel, Attrs, _Children, Parent, ID) :-
    option(id(ID), Attrs), 
    gennum(N), model_assert(n(N, ID)), 
    model_assert(parallel(ID, Parent)).
model_generate(invoke, Attrs, Children, Parent, _ID) :-
    select_option(type(Type), Attrs, Attrs1, scxml),
    maplist(attr_to_option, Attrs1, Options),
    (   memberchk(Src, Children)
    ->  Options1 = [src_text(Src)|Options]
    ;   Options1 = Options
    ),
    model_assert(to_be_invoked(Parent, Type, Options1)).
model_generate(history, Attrs, _Children, Parent, ID) :-
    option(id(ID), Attrs), 
    option(type(Type), Attrs, shallow), 
    model_assert(history(ID, Parent, Type)).
model_generate(go, Attrs, Children, Parent, _ID) :-    
    option(on(EventAtom), Attrs, ''),
    my_atom_to_term(EventAtom, Event, Bindings0), 
    option(if(CondAtom), Attrs, true), 
    my_atom_to_term(CondAtom, Cond, Bindings1), 
    unify_bindings(Bindings0,  Bindings1, Bindings2), 
    (   option(to(Targets), Attrs)
    ->  atomic_list_concat(TargetList, ' ', Targets)
    ;   TargetList = []
    ),
    children_to_actions(Children, Actions, Bindings2), 
    model_assert(transition(Parent, Event, Cond, TargetList, Actions)).
model_generate(final, Attrs, _Children, Parent, ID) :-
    option(id(ID), Attrs), 
    gennum(N), model_assert(n(N, ID)), 
    model_assert(final(ID, Parent)).
model_generate(initial, _Attrs, _Children, Parent, init(Parent)) :-
    model_assert(initial(init(Parent), Parent)).
model_generate(onentry, _Attrs, Children, Parent, _ID) :-
    children_to_actions(Children, Actions, []), 
    model_assert(onentry(Parent, Actions)).
model_generate(onexit, _Attrs, Children, Parent, _ID) :-
    children_to_actions(Children, Actions, []), 
    model_assert(onexit(Parent, Actions)).
model_generate(datamodel, _Attrs, [Children], _Parent, _ID) :-
    load_datamodel(Children).

load_datamodel(Children) :-
    atom_to_memory_file(Children, Handle),  
    open_memory_file(Handle, read, Stream),  
    read_source(Stream),
    close(Stream).


read_source(Stream) :-
    read(Stream, Term),
    read_source(Term, Stream).

read_source(end_of_file, _Stream) :- !.
read_source(Term, Stream) :-
    expand_and_assert(Term),
    read_source(Stream).


expand_and_assert(Term) :-
    expand_term(Term, ExpandedTerm),
    (   is_list(ExpandedTerm)
    ->  maplist(assert_local, ExpandedTerm)
    ;   assert_local(ExpandedTerm)
    ).


assert_local(:-(Head, Body)) :- !,
    functor(Head, F, N),
    thread_local(F/N),
    assert(:-(Head, Body)).
assert_local(:-Body) :- !,
    call(Body).
assert_local(Fact) :-
    functor(Fact, F, N),
    thread_local(F/N),
    assert(Fact).


children_to_actions([], [], _Bindings).
children_to_actions([Child|Children], [Action|Actions], Bindings) :-
    child_to_action(Child, Action, Bindings), !, 
    children_to_actions(Children, Actions, Bindings).
children_to_actions([_|Children], Actions, Bindings) :-
    children_to_actions(Children, Actions, Bindings).

child_to_action(Children, Action, Bindings) :-
    my_atom_to_term(Children, Expr, Bindings1), 
    unify_bindings(Bindings, Bindings1, _), 
    Action = script(Expr).	

my_atom_to_term(Atom, Term, Bindings) :-
    catch(my_atom_to_term2(Atom, Term, Bindings), E, print_message(error, E)).

my_atom_to_term2('', '', []) :- !.
my_atom_to_term2(Atom, Term, Bindings) :-
    atom_to_term(Atom, Term, Bindings).
    
    
attr_to_option(A=V, Term) :-
    functor(Term, A, 1),
    arg(1, Term, V).
    
    
% Note the use of a union where elements are 
% considered duplicates if they can be unified.
unify_bindings(Bs1, Bs2, Bs3) :-
    union(Bs1, Bs2, Bs3). 


model_assert(Term) :-
    assert(Term).


:- dynamic num/1.
gennum(N) :-
    (   retract(num(N))
    ->  N1 is N+1,
        assert(num(N1))
    ;   N=0,
        assert(num(1))
    ).

    
    
list :-
    listing(state/2),
    listing(to_be_invoked/3),
    listing(initial/1),
    listing(initial/2),
    listing(transition/5),
    listing(parallel/2),
    listing(history/3),
    listing(final/2),
    listing(onexit/2),
    listing(onentry/2),
    listing(n/2).   
   

clean :-
    % Model
    retractall(state(_, _)),
    retractall(to_be_invoked(_, _, _)),
    retractall(initial(_)),
    retractall(initial(_, _)),
    retractall(transition(_, _, _, _, _)),
    retractall(parallel(_, _)),
    retractall(history(_, _, _)),
    retractall(final(_, _)),
    retractall(onexit(_, _)),
    retractall(onentry(_, _)),
    retractall(n(_, _)),
    % Global
    retractall(event(_)),
    retractall(internal_queue(_)),
    retractall(historyValue(_,_)),
    retractall(configuration(_)),
    retractall(states_to_invoke(_)).

% ===



run(File, Pid) :-
    spawn((interpret(File)), Pid).


/** interpret/1
    
The purpose of  this procedure is to  initialize the interpreter and to  start processing. In
order to  interpret an SCXML document,  first (optionally) perform [xinclude]  processing and
(optionally) validate the  document, throwing an exception if validation  fails. Then convert
initial attributes  to <initial> container children  with transitions to the  state specified
by the attribute.  (This step is done purely  to simplify the statement of  the algorithm and
has no  effect on  the system's behavior.  Such transitions will  not contain  any executable
content). Initialize the global data structures, including  the data model. If binding is set
to 'early',  initialize the  data model. Then  execute the global  <script> element,  if any.
Finally call  enterStates on the  initial configuration, set  the global running  variable to
true and start the interpreter's event loop.
*/

interpret(File) :-
    clean,
    scxml_parse(File),
%    debug(scxml(model), '~@', [list]),
    assert(configuration([])), 
    assert(states_to_invoke([])), 
    assert(running),
    model_assert(state(dummy,scxml)),
    initial(Initial),
    message_queue_create(Internal), 
    assert(internal_queue(Internal)),
    enter_states([t(dummy, [Initial], [])]),
%    configuration(Configuration),
%    debug(scxml(config), 'Configuration: ~p', [Configuration]),
    main_event_loop.


/** main_event_loop/0
    
This  loop runs  until  we  enter a  top-level  final state  or  an  external entity  cancels
processing.  In either  case 'running'  will be  set to  false (see  EnterStates, below,  for
termination by  entering a top-level  final state.) At  the top of  the loop, we  have either
just entered the state  machine, or we have just processed an  external event. Each iteration
through the loop consists of four main  steps: 1) Complete the macrostep by repeatedly taking
any internally  enabled transitions,  namely those that  don't require an  event or  that are
triggered by  an internal  event. After each  such transition/microstep, check  to see  if we
have  reached a  final  state. 2)  When  there  are no  more  internally enabled  transitions
available, the  macrostep is done. Execute  any <invoke> tags  for states that we  entered on
the last  iteration through the  loop 3) If  any internal events  have been generated  by the
invokes, repeat  step 1 to  handle any errors  raised by the  <invoke> elements. 4)  When the
internal event queue  is empty, wait for  an external event and then  execute any transitions
that it  triggers. However  special preliminary  processing is  applied to  the event  if the
state has executed  any <invoke> elements. First,  if this event was generated  by an invoked
process,  apply  <finalize>  processing  to  it. Secondly,  if  any  <invoke>  elements  have
autoforwarding set, forward the  event to them. These steps apply  before the transitions are
taken.This event loop thus enforces run-to-completion  semantics, in which the system process
an external  event and  then takes all  the 'follow-up' transitions  that the  processing has
enabled before  looking for another  external event. For  example, suppose that  the external
event queue contains events ext1 and ext2 and  the machine is in state s1. If processing ext1
takes the  machine to s2 and  generates internal event int1,  and s2 contains a  transition t
triggered by  int1, the  system is guaranteed  to take  t, no matter  what transitions  s2 or
other states have  that would be triggered by  ext2. Note that this is true  even though ext2
was already  in the external event  queue when int1  was generated. In effect,  the algorithm
treats the processing of int1 as finishing up the processing of ext1.
*/
    
main_event_loop :-
    (   running
    ->  main_event_loop2
    ;   debug(scxml(info), 'END OF PROCESSING', [])
    ).
    
main_event_loop2 :-
    (   select_transitions(null, EnabledTransitions)
    ->  microstep(EnabledTransitions),
        main_event_loop
    ;   internal_queue(Internal),
        thread_get_message(Internal, Event, [timeout(0)]),
        update_eventdata(Event),
        debug(scxml(event), '   Int. event: ~p', [Event])
    ->  main_event_loop(Event)
    ;   states_to_invoke(States),
        States \= []
    ->  maplist(invoke, States),
        retractall(states_to_invoke(_)),
        assert(states_to_invoke([])), 
        main_event_loop
    ;   receive({Event -> true}),
        update_eventdata(Event),
        debug(scxml(event), '   Ext. event: ~p', [Event]),
        main_event_loop(Event)
    ).
    
main_event_loop(Event) :-
    (   select_transitions(Event, EnabledTransitions)
    ->  microstep(EnabledTransitions),
        main_event_loop
    ;   debug(scxml(info), ' No match for: ~p', [Event]),
        main_event_loop
    ).


/** exit_interpreter/1
    
The purpose  of this procedure  is to exit  the current SCXML  process by exiting  all active
states. If the machine  is in a top-level final state, a Done  event is generated. (Note that
in  this case,  the  final  state will  be  the only  active  state.)  The implementation  of
returnDoneEvent is platform-dependent,  but if this session  is the result of  an <invoke> in
another SCXML session, returnDoneEvent will cause  the event done.invoke.<id> to be placed in
the external  event queue of  that session, where  <id> is the  id generated in  that session
when the <invoke> was executed.
*/

exit_interpreter :-
   retractall(running),
%   halt,
   true.




invoke(State) :-
    to_be_invoked(State, pengine, Options),
    pengine_spawn(Pid, Options),
    debug(scxml(invoke), '      Invoked: pengine ~p at ~p', [Pid, State]),
    raise(spawned(Pid)),
    fail.
invoke(_).





update_eventdata(Event) :-
    retractall(event(_)),
    assert(event(Event)).


microstep(EnabledTransitions) :-
    exit_states(EnabledTransitions),
    execute_transitions(EnabledTransitions),
    enter_states(EnabledTransitions).


   
exit_states(EnabledTransitions) :-
    configuration(Configuration),
    compute_exit_set(EnabledTransitions, Configuration, StatesToExit),
    maplist(states_to_invoke_delete, StatesToExit),
    predsort(exit_order, StatesToExit, SortedStatesToExit),
    (   member(State, SortedStatesToExit), 
        history(H, State, Depth),  
        (  Depth == deep 
        -> findall(S, (member(S, Configuration), is_atomic(S), isDescendant(S, State)), SS)
        ;  findall(S, (member(S, Configuration), has_parent(S, State)), SS)
        ), 
        update_history_value(H, SS), 
        fail
    ;   true
    ), 
    process_states_to_exit(SortedStatesToExit).
    

compute_exit_set(Transitions, Configuration, StatesToExit) :-
    findall(State, (member(t(Source, Targets, _), Transitions), 
                   Targets \= [], 
                   find_LCA([Source|Targets], LCA), 
                   member(State, Configuration), 
                   isDescendant(State, LCA)), 
           StatesToExit).


process_states_to_exit([]).
process_states_to_exit([State|States]) :-
    forall(onexit(State, Content), execute_content(Content)), 
    % TODO: for inv in s.invoke: cancelInvoke(inv)
    configuration_delete(State),
    process_states_to_exit(States).

       

execute_content(Content) :-
    maplist(call, Content).

                   
execute_transitions(EnabledTransitions) :-
    (   member(t(_, _, Children), EnabledTransitions), 
        member(Child, Children), 
        call(Child), 
        fail
    ;   true
    ).
   
enter_states(EnabledTransitions) :-
    compute_entry_set(EnabledTransitions, StatesToEnter), 
    predsort(entry_order, StatesToEnter, SortedStatesToEnter), 
    debug(scxml(info), ' Enter states: ~p', [SortedStatesToEnter]), 
    process_states_to_enter(SortedStatesToEnter),
    configuration(NewConfiguration),
    debug(scxml(config), 'Configuration: ~p', [NewConfiguration]).


compute_entry_set(Transitions, StatesToEnter) :-
    findall(StateToEnter, 
            (member(t(Source, Targets, _), Transitions), 
             Targets \= [], 
             find_LCA([Source|Targets], LCA), 
             member(State, Targets), 
             (  is_history(State)
             ->  (   historyValue(State, States)
                 ->  member(HS, States), 
                     state_to_enter(HS, LCA, StateToEnter)
                 ;   transition(State, '', true, States, _), 
                     member(HS, States), 
                     state_to_enter(HS, LCA, StateToEnter)
                 )
             ;   state_to_enter(State, LCA, StateToEnter)
             )),  
          StatesToEnter).

            
process_states_to_enter([]).
process_states_to_enter([State|States]) :-
    configuration_add(State),
    states_to_invoke_add(State),
    forall(onentry(State, Content), execute_content(Content)),    
    (   is_final(State)
    ->  (   has_parent(State, Parent),
            is_scxml(Parent)
        ->  retractall(running)
        ;   has_parent(State, Parent),
            enqueue_internal_event(done(Parent)),
            has_parent(Parent, Grandparent), 
            (   is_parallel(Grandparent),
                forall(has_parent(Child, Grandparent), is_in_final_state(Child))
            ->  enqueue_internal_event(done(Grandparent))
            ;   true
            )
        )
    ;   true
    ),
    process_states_to_enter(States).
    

configuration_add(State) :-
    configuration(Configuration),
    ord_union(Configuration, [State], NewConfiguration),
    (  NewConfiguration == Configuration
    -> true
    ;  retractall(configuration(_)), 
       assert(configuration(NewConfiguration))
    ).
    
configuration_delete(State) :-
    configuration(Configuration),
    subtract(Configuration, [State], NewConfiguration),
    retractall(configuration(_)), 
    assert(configuration(NewConfiguration)).

            

states_to_invoke_add(State) :-
    states_to_invoke(StatesToInvoke), 
    ord_union(StatesToInvoke, [State], NewStatesToInvoke),
    (  NewStatesToInvoke == StatesToInvoke
    -> true
    ;  retractall(states_to_invoke(_)), 
       assert(states_to_invoke(NewStatesToInvoke))
    ).
    
states_to_invoke_delete(State) :-
    states_to_invoke(StatesToInvoke), 
    subtract(StatesToInvoke, [State], NewStatesToInvoke),
    retractall(states_to_invoke(_)), 
    assert(states_to_invoke(NewStatesToInvoke)).
    
   
   
update_history_value(H,  SS) :-
    retractall(historyValue(H, _)), 
    assert(historyValue(H, SS)).



state_to_enter(State, _Root, State).
state_to_enter(State, _Root, StateToEnter) :-
    is_parallel(State),
    has_parent(Child, State),
    state_to_enter(Child, State, StateToEnter).
state_to_enter(State, _Root, StateToEnter) :-
    is_compound(State),
    transition(init(State), '', true, Children, _),
    member(Child, Children),
    state_to_enter(Child, State, StateToEnter).
state_to_enter(State, Root, StateToEnter) :-
    proper_ancestor(State, Root, StateToEnter).
state_to_enter(State, Root, StateToEnter) :-
    proper_ancestor(State, Root, Ancestor),
    is_parallel(Ancestor),
    has_parent(Child, Ancestor),
    state_to_enter(Child, Ancestor, StateToEnter).



is_in_final_state(S) :-
    is_compound(S),
    has_parent(Child, S),
    is_final(Child),
    configuration(Configuration),
    memberchk(Child, Configuration).
is_in_final_state(S) :-
    is_parallel(S),
    forall(has_parent(Child, S), is_in_final_state(Child)).
   


entry_order(=, State, State).
entry_order(>, State1, State2) :-
    n(N1, State1),
    n(N2, State2),
    N1 > N2, !.
entry_order(<, _State1, _State2).


exit_order(=, State, State).
exit_order(<, State1, State2) :-
    n(N1, State1),
    n(N2, State2),
    N1 > N2, !.
exit_order(>, _State1, _State2).



has_parent(State, Parent) :- state(State, Parent).
has_parent(State, Parent) :- parallel(State, Parent).
has_parent(State, Parent) :- final(State, Parent).
has_parent(State, Parent) :- history(State, Parent,_).


is_parallel(State) :- parallel(State, _).

is_compound(State) :- has_parent(_Child, State). 

is_atomic(State) :- \+ has_parent(_Child, State).

is_history(State) :- history(State, _, _).

is_final(State) :- final(State, _).

is_scxml(State) :- state(State, null).
   
   
select_transitions(Event, EnabledTransitions) :-
    configuration(Configuration), 
    findall(EnabledTransition,
           (	member(State, Configuration),
           		is_atomic(State),
           		select_transition(Event, State, EnabledTransition)   
		   ),
    EnabledTransitions0),
    EnabledTransitions0 \= [],
    list_to_ord_set(EnabledTransitions0, EnabledTransitions),
    maplist(trace_transition, EnabledTransitions). 
        
select_transition(null, State, t(Ancestor, Targets, Actions)) :-
    ancestor(State, null, Ancestor),
    transition(Ancestor, '', Condition, Targets, Actions),
    once(Condition).
select_transition(Event, State, t(Ancestor, Targets, Actions)) :-
    Event \= null,
    ancestor(State, null, Ancestor),
    transition(Ancestor, Event, Condition, Targets, Actions),
    once(Condition).   


trace_transition(t(State, Targets ,_)) :-
    debug(scxml(info), '   Transition: ~p => ~p', [State, Targets]).


proper_ancestor(StateID, RootID, ParentID) :-
    has_parent(StateID, ParentID),
    ParentID \= RootID.
proper_ancestor(StateID, RootID, AncestorID) :-
    has_parent(StateID, ParentID),
    ParentID \= RootID,
    proper_ancestor(ParentID, RootID, AncestorID).


ancestor(StateID, _RootID, StateID).
ancestor(StateID, RootID, AncestorID) :-
    proper_ancestor(StateID, RootID, AncestorID).


isDescendant(StateID, AncestorID) :-
    proper_ancestor(StateID, null, AncestorID).   



find_LCA([S|Ss], Ancestor) :-
    proper_ancestor(S, null, Ancestor),
    forall(member(S0,Ss), isDescendant(S0, Ancestor)),
    !.

enqueue_internal_event(Event) :-
    internal_queue(Internal),
    thread_send_message(Internal, Event).
    
raise(Event) :-
    enqueue_internal_event(Event).
    
/*
raise(Event) :-
    self(Self),
    send(Self, Event).


raise(Event, Delay) :-
    self(Self),
    thread_create(send_after(Self, Event, Delay), _, []).
    
send_after(Parent, Event, Delay) :-
    receive({
       after(Delay) -> 
           Parent ! Event
    }).       
*/


log(Expr) :-
    write(Expr), nl,
    flush_output.


script(Goal) :-
%    debug(scxml(execute), 'About to execute ~p', [Goal]),
    call(Goal),
    debug(scxml(execute), '     Executed: ~p', [Goal]).
   


