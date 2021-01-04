:- module(scxml, [
    scxml_parse/1,
    interpret/1,
    snd/3,
    run/2,
    list/0,
    clean/0
]).

:- use_module('../web_prolog.pl').

:- use_module(library(option)).
:- use_module(library(sort)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(debug)).

:- prolog_ide(debug_monitor).
%:- prolog_ide(thread_monitor).

	
    
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
    debug(scxml(info), '*** Processing file ~p', [File]),
    spawn(interpret(File), Pid, [
        monitor(true)
    ]).


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
    ;   exit_interpreter,
        debug(scxml(info), '*** End of processing (a down message was sent to parent)\n', [])
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
        debug(scxml(event), '   Ext. event: ~p', [Event]),
        (   Event == cancel
        ->  retractall(running),
            main_event_loop
        ;   update_eventdata(Event),
            main_event_loop(Event)
        )
    ).
    
main_event_loop(Event) :-
    (   select_transitions(Event, EnabledTransitions)
    ->  microstep(EnabledTransitions),
        main_event_loop
    ;   debug(scxml(info), '    Unmatched: ~p', [Event]),
        main_event_loop
    ).


/** exit_interpreter/1
    
NOT YET IMPLEMENTED!
    
The purpose  of this procedure  is to exit  the current SCXML  process by exiting  all active
states. If the machine  is in a top-level final state, a Done  event is generated. (Note that
in  this case,  the  final  state will  be  the only  active  state.)  The implementation  of
returnDoneEvent is platform-dependent,  but if this session  is the result of  an <invoke> in
another SCXML session, returnDoneEvent will cause  the event done.invoke.<id> to be placed in
the external  event queue of  that session, where  <id> is the  id generated in  that session
when the <invoke> was executed.
    
procedure exitInterpreter():
    statesToExit = configuration.toList().sort(exitOrder)
    for s in statesToExit:
        for content in s.onexit.sort(documentOrder):
            executeContent(content)
        for inv in s.invoke:
            cancelInvoke(inv)
        configuration.delete(s)
        if isFinalState(s) and isScxmlElement(s.parent):   
            returnDoneEvent(s.donedata)
*/

exit_interpreter :-
    configuration(Configuration),
    predsort(exit_order, Configuration, StatesToExit),
    exit_interpreter(StatesToExit).
   
exit_interpreter([]).
exit_interpreter([State|States]) :-
    forall(onentry(State, Content), execute_content(Content)),
%   for inv in s.invoke:
%       cancelInvoke(inv)
    configuration_delete(State),
    (   is_final(State),
        has_parent(State, Parent),
        is_scxml_element(Parent)
    ->  true
    ;   exit_interpreter(States)
    ).
    

/** select_transitions/2

The  purpose of  the selectTransitions()  procedure is  to collect  the transitions  that are
enabled by this event in the current configuration.

Create an empty  set of enabledTransitions. For  each atomic state , find  a transition whose
'event' attribute matches  event and whose condition evaluates to  true. If multiple matching
transitions are  present, take the first  in document order.  If none are present,  search in
the state's ancestors in  ancestry order until one is found. As soon  as such a transition is
found,  add  it  to  enabledTransitions,  and  proceed  to  the  next  atomic  state  in  the
configuration. If no such  transition is found in the state or its  ancestors, proceed to the
next state  in the configuration.  When all atomic states  have been visited  and transitions
selected, filter out any preempted transitions and return the resulting set.
*/
   
select_transitions(Event, EnabledTransitions) :-
    configuration(Configuration), 
    findall(EnabledTransition,
           (	member(State, Configuration),
           		is_atomic(State),
           		select_transition(Event, State, EnabledTransition)   
		   ),
    EnabledTransitions0),
    EnabledTransitions0 \= [],
    list_to_ord_set(EnabledTransitions0, EnabledTransitions1),
    remove_conflicting_transitions(EnabledTransitions1, EnabledTransitions),
    maplist(trace_transition, EnabledTransitions). 
        
select_transition(null, State, t(Ancestor, Targets, Actions)) :-
    ancestor(State, null, Ancestor),
    transition(Ancestor, '', Condition, Targets, Actions),
    once(Condition),
    !.
select_transition(Event, State, t(Ancestor, Targets, Actions)) :-
    Event \= null,
    ancestor(State, null, Ancestor),
    transition(Ancestor, Event, Condition, Targets, Actions),
    once(Condition),
    !.
    
    
/** remove_conflicting_transitions/2    
    
NOT YET IMPLEMENTED!

enabledTransitions will contain  multiple transitions only if a parallel  state is active. In
that case, we  may have one transition  selected for each of its  children. These transitions
may conflict with each other in the  sense that they have incompatible target states. Loosely
speaking,  transitions are  compatible when  each one  is contained  within a  single <state>
child of  the <parallel>  element. Transitions  that aren't contained  within a  single child
force the  state machine to leave  the <parallel> ancestor  (even if they reenter  it later).
Such transitions conflict with  each other, and with transitions that  remain within a single
<state> child, in that  they may have targets that cannot be  simultaneously active. The test
that  transitions  have  non-intersecting  exit  sets  captures  this  requirement.  (If  the
intersection  is null,  the  source and  targets  of  the two  transitions  are contained  in
separate <state>  descendants of <parallel>. If  intersection is non-null, then  at least one
of the  transitions is  exiting the  <parallel>). When such  a conflict  occurs, then  if the
source state of one of  the transitions is a descendant of the source  state of the other, we
select  the  transition in  the  descendant.  Otherwise we  prefer  the  transition that  was
selected by the earlier  state in document order and discard the  other transition. Note that
targetless  transitions  have empty  exit  sets  and thus  do  not  conflict with  any  other
transitions.We start  with a list of  enabledTransitions and produce a  conflict-free list of
filteredTransitions. For each  t1 in enabledTransitions, we  test it against all  t2 that are
already selected in  filteredTransitions. If there is  a conflict, then if  t1's source state
is a  descendant of t2's source  state, we prefer  t1 and say that  it preempts t2 (so  we we
make a  note to remove  t2 from  filteredTransitions). Otherwise, we  prefer t2 since  it was
selected in an  earlier state in document order,  so we say that it preempts  t1. (There's no
need to  do anything in  this case since t2  is already in  filteredTransitions. Furthermore,
once one transition preempts t1, there is no  need to test t1 against any other transitions.)
Finally,  if  t1  isn't  preempted  by any  transition  in  filteredTransitions,  remove  any
transitions that it preempts and add it to that list.
    
NOT YET IMPLEMENTED!
*/
    
remove_conflicting_transitions(Transitions0, Transitions) :-
    Transitions = Transitions0.
 
 
/** microstep/1

The purpose of the  microstep procedure is to process a single set  of transitions. These may
have been enabled by  an external event, an internal event, or by  the presence or absence of
certain values in the data model at the  current point in time. The processing of the enabled
transitions must  be done in  parallel ('lock  step') in the  sense that their  source states
must first be  exited, then their actions  must be executed, and finally  their target states
entered.If  a single  atomic state  is active,  then enabledTransitions  will contain  only a  single
transition. If  multiple states are active  (i.e., we are  in a parallel region),  then there
may be multiple transitions,  one per active atomic state (though some  states may not select
a transition.) In  this case, the transitions are  taken in the document order  of the atomic
states that selected them.
*/

microstep(EnabledTransitions) :-
    exit_states(EnabledTransitions),
    execute_transition_content(EnabledTransitions),
    enter_states(EnabledTransitions).
    
    
/** exit_states/1
    
Compute the set  of states to exit. Then  remove all the states on statesToExit  from the set
of states that will have invoke processing done  at the start of the next macrostep. (Suppose
macrostep M1 consists of microsteps  m11 and m12. We may enter state s in  m11 and exit it in
m12. We will  add s to statesToInvoke  in m11, and must  remove it in m12.  In the subsequent
macrostep  M2, we  will apply  invoke processing  to all  states that  were entered,  and not
exited, in M1.) Then  convert statesToExit to a list and sort it  in exitOrder.For each state
s in the list, if s has a deep history state  h, set the history value of h to be the list of
all atomic  descendants of  s that  are members in  the current  configuration, else  set its
value  to be  the  list of  all immediate  children  of s  that  are members  of the  current
configuration. Again for  each state s in  the list, first execute any  onexit handlers, then
cancel any ongoing invocations, and finally remove s from the current configuration.
*/
    
exit_states(EnabledTransitions) :-
    configuration(Configuration),
    compute_exit_set(EnabledTransitions, Configuration, StatesToExit),
    maplist(states_to_invoke_delete, StatesToExit),
    predsort(exit_order, StatesToExit, SortedStatesToExit),
    (   member(State, SortedStatesToExit), 
        history(H, State, Depth),  
        (  Depth == deep 
        -> findall(S, (member(S, Configuration), is_atomic(S), is_descendant(S, State)), SS)
        ;  findall(S, (member(S, Configuration), has_parent(S, State)), SS)
        ), 
        update_history_value(H, SS), 
        fail
    ;   true
    ), 
    process_states_to_exit(SortedStatesToExit).

process_states_to_exit([]).
process_states_to_exit([State|States]) :-
    forall(onexit(State, Content), execute_content(Content)), 
    % TODO: for inv in s.invoke: cancelInvoke(inv)
    configuration_delete(State),
    process_states_to_exit(States).
    
    
/** compute_exit_set/3

For  each transition  t in  enabledTransitions,  if t  is  targetless then  do nothing,  else
compute the  transition's domain.  (This will  be the source  state in  the case  of internal
transitions) or  the least  common compound  ancestor state  of the  source state  and target
states of t (in  the case of external transitions. Add to the  statesToExit set all states in
the configuration that are descendants of the domain.
*/    

compute_exit_set(Transitions, Configuration, StatesToExit) :-
    findall(State, (member(t(Source, Targets, _), Transitions), 
                   Targets \= [], 
                   find_LCCA([Source|Targets], LCA), 
                   member(State, Configuration), 
                   is_descendant(State, LCA)), 
           StatesToExit).

     
/** execute_transition_content/1

For each transition in the list of enabledTransitions, execute its executable content.
*/

execute_transition_content(EnabledTransitions) :-
    (   member(t(_, _, Children), EnabledTransitions), 
        member(Child, Children), 
        call(Child), 
        fail
    ;   true
    ).


/** enter_states/1

First, compute  the list of all  the states that  will be entered  as a result of  taking the
transitions in enabledTransitions.  Add them to statesToInvoke so that  invoke processing can
be done at  the start of the next macrostep.  Convert statesToEnter to a list and  sort it in
entryOrder. For each state  s in the list, first add s to  the current configuration. Then if
we are using late binding, and this is the  first time we have entered s, initialize its data
model. Then execute any  onentry handlers. If s's initial state is  being entered by default,
execute any  executable content in the  initial transition. If a  history state in s  was the
target of  a transition, and s  has not been entered  before, execute the content  inside the
history state's default  transition. Finally, if s  is a final state,  generate relevant Done
events. If we have reached a top-level final state,  set running to false as a signal to stop
processing.
*/

enter_states(EnabledTransitions) :-
    compute_entry_set(EnabledTransitions, StatesToEnter), 
    predsort(entry_order, StatesToEnter, SortedStatesToEnter), 
%    debug(scxml(info), ' Enter states: ~p', [SortedStatesToEnter]), 
    process_states_to_enter(SortedStatesToEnter),
    configuration(NewConfiguration),
    debug(scxml(config), 'Configuration: ~p', [NewConfiguration]).


process_states_to_enter([]).
process_states_to_enter([State|States]) :-
    configuration_add(State),
    states_to_invoke_add(State),
    forall(onentry(State, Content), execute_content(Content)),    
    (   is_final(State)
    ->  (   has_parent(State, Parent),
            is_scxml_element(Parent)
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
    
    
/** compute_entry_set/2

Compute the complete set of states that will  be entered as a result of taking 'transitions'.
This value will  be returned in 'statesToEnter'  (which is modified by  this procedure). Also
place in  'statesForDefaultEntry' the  set of  all states whose  default initial  states were
entered. First gather up  all the target states in 'transitions'. Then add  them and, for all
that are  not atomic states,  add all of  their (default) descendants  until we reach  one or
more atomic  states. Then add  any ancestors that  will be entered  within the domain  of the
transition. (Ancestors outside of the domain of the transition will not have been exited.)
*/
    
compute_entry_set(Transitions, StatesToEnter) :-
    findall(StateToEnter, 
            (member(t(Source, Targets, _), Transitions), 
             Targets \= [], 
             find_LCCA([Source|Targets], LCA), 
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


/** state_to_enter/3

NB: state_to_enter/3 corresponds to addDescendantStatesToEnter() & addAncestorStatesToEnter()
in the algorithm. TODO: Rethink this, and its relation to compute_entry_set/2!

The purpose of this  procedure is to add to statesToEnter 'state' and  any of its descendants
that the  state machine will end  up entering when it  enters 'state'. (N.B. If  'state' is a
history pseudo-state, we  dereference it and add  the history value instead.)  Note that this
procedure permanently modifies both statesToEnter  and statesForDefaultEntry. First, if state
is  a history  state then  add either  the history  values associated  with state  or state's
default  target to  statesToEnter. Then  (since the  history value  may not  be an  immediate
descendant  of 'state's  parent) add  any  ancestors between  the history  value and  state's
parent. Else (if state is not a history  state), add state to statesToEnter. Then if state is
a compound  state, add state  to statesForDefaultEntry and recursively  call addStatesToEnter
on its default initial  state(s). Then, since the default initial states  may not be children
of 'state', add any  ancestors between the default initial states  and 'state'. Otherwise, if
state is a parallel state, recursively call  addStatesToEnter on any of its child states that
don't already have a descendant on statesToEnter.
          
Add to statesToEnter any ancestors of 'state'  up to, but not including, 'ancestor' that must
be entered in  order to enter 'state'. If  any of these ancestor states is  a parallel state,
we must fill in its descendants as well.
*/            

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


/** state_to_enter/3

Return true if  s is a compound  <state> and one of  its children is an  active <final> state
(i.e.  is a  member  of  the current  configuration),  or  if s  is  a  <parallel> state  and
isInFinalState is true of all its children.
*/
    
is_in_final_state(S) :-
    is_compound(S),
    has_parent(Child, S),
    is_final(Child),
    configuration(Configuration),
    memberchk(Child, Configuration).
is_in_final_state(S) :-
    is_parallel(S),
    forall(has_parent(Child, S), is_in_final_state(Child)).
    
    
/** find_LCCA/2

The Least  Common Compound  Ancestor is the  <state> or <scxml>  element s  such that s  is a
proper ancestor  of all states on  stateList and no descendant  of s has this  property. Note
that there is guaranteed to be such an  element since the <scxml> wrapper element is a common
ancestor of all  states. Note also that since  we are speaking of proper  ancestor (parent or
parent of a parent, etc.) the LCCA is never a member of stateList.
*/
    
find_LCCA([S|Ss], Ancestor) :-
    proper_ancestor(S, null, Ancestor),
    forall(member(S0,Ss), is_descendant(S0, Ancestor)),
    !.


/** proper_ancestor/3

If state2  is null, returns the  set of all ancestors  of state1 in ancestry  order (state1's
parent followed  by the parent's  parent, etc.  up to an  including the <scxml>  element). If
state2 is non-null, returns  in ancestry order the set of all ancestors  of state1, up to but
not including state2. (A "proper ancestor" of a  state is its parent, or the parent's parent,
or the parent's parent's  parent, etc.))If state2 is state1's parent, or  equal to state1, or
a descendant of state1, this returns the empty set.
*/
    
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


/** is_descendant/2

Returns 'true'  if state1 is a  descendant of state2  (a child, or a  child of a child,  or a
child of a child of a child, etc.) Otherwise returns 'false'.
*/
    
is_descendant(StateID, AncestorID) :-
    proper_ancestor(StateID, null, AncestorID).   


/* end of documented predicates! */
 

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


       
execute_content(Content) :-
    maplist(call, Content).



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

is_scxml_element(State) :- state(State, null).
   


trace_transition(t(State, Targets ,_)) :-
    debug(scxml(info), '   Transition: ~p => ~p', [State, Targets]).



enqueue_internal_event(Event) :-
    internal_queue(Internal),
    thread_send_message(Internal, Event).
    

    
    
raise(Event) :-
    enqueue_internal_event(Event).



snd(Pid, Message, Options) :-
    option(delay(Delay), Options, 0),
    thread_create(send_after(Pid, Message, Delay), CancelPid, []),
    (   option(id(ID), Options)
    ->  register(ID, CancelPid)
    ;   true
    ).
    
send_after(Parent, Message, Delay) :-
    receive({
       after(Delay) -> 
           Parent ! Message,
           debug(scxml(info), '   Sent: ~p', [Message])
    }).       

cancel(ID) :-
    exit(ID, cancel).



log(Expr) :-
    write(Expr), nl,
    flush_output.


script(Goal) :-
%    debug(scxml(execute), 'About to execute ~p', [Goal]),
    call(Goal),
    debug(scxml(execute), '    Execution: ~p', [Goal]).
   


