:- use_module(library(debug)).

	
:- dynamic state/2, 
           parallel/2,
           history/3,
           final/2,
           initial/2,
           transition/5,
           internal_queue/1,
           onexit/2,
           onentry/2,
           continue/0,
           historyValue/2, 
           event/1,
           data/1,
           n/2,
           configuration/1.


:- debug(scxml).

load_xml(File) :-
   load_xml_file(File,ListOfContent),
   process(ListOfContent,null).

:- dynamic num/1.
gennum(N) :-
   (   retract(num(N))
   ->  N1 is N+1,
       assert(num(N1))
   ;   N=0,
       assert(num(1))
   ).
   

process([],_).
process([element(Name,Attrs,Children)|Rest],Parent) :-
   gen_clause(Name,Attrs,Children,Parent,NewParent), 
   !,
   process(Children,NewParent),
   process(Rest,Parent).
process([_|Rest],Parent) :-
   process(Rest,Parent).
   
gen_clause(scxml,Attrs,_Children,Parent,ID) :-
   add_defaults([id=scxml],Attrs,NewAttrs),
   member(id=ID,NewAttrs),
   gennum(N),assert(n(N,ID)),
   assert(state(ID,Parent)),
   member(initial=InitID,NewAttrs),
   assert(initial(InitID)).
gen_clause(state,Attrs,_Children,Parent,ID) :-
   member(id=ID,Attrs),
   gennum(N),assert(n(N,ID)),
   assert(state(ID,Parent)).
gen_clause(parallel,Attrs,_Children,Parent,ID) :-
   member(id=ID,Attrs),
   gennum(N),assert(n(N,ID)),
   assert(parallel(ID,Parent)).
gen_clause(history,Attrs,_Children,Parent,ID) :-
   add_defaults([type=shallow],Attrs,NewAttrs),   
   member(id=ID,NewAttrs),
   member(type=Type,NewAttrs),
   assert(history(ID,Parent,Type)).
gen_clause(transition,Attrs,Children,Parent,ID) :-
   add_defaults([event='',cond='true',target=''],Attrs,NewAttrs),   
   member(event=Event,NewAttrs),
   member(cond=CondAtom,NewAttrs),
   my_atom_to_term(CondAtom,Cond,Bindings),
   member(target=Targets,NewAttrs),
   tokenize_atom(Targets,TargetList),
   children_to_actions(Children,Actions,Bindings),
   assert(transition(Parent,Event,Cond,TargetList,Actions)).
gen_clause(final,Attrs,_Children,Parent,ID) :-
   member(id=ID,Attrs),
   gennum(N),assert(n(N,ID)),
   assert(final(ID,Parent)).
gen_clause(initial,Attrs,_Children,Parent,init(Parent)) :-
   assert(initial(init(Parent),Parent)).
gen_clause(onentry,Attrs,Children,Parent,ID) :-
   children_to_actions(Children,Actions,[]),
   assert(onentry(Parent,Actions)).
gen_clause(onexit,Attrs,Children,Parent,ID) :-
   children_to_actions(Children,Actions,[]),
   assert(onexit(Parent,Actions)).
gen_clause(datamodel,Attrs,[Children],Parent,ID) :-
   atom_to_memory_file(Children,Handle), 
   open_memory_file(Handle,read,Stream), 
   repeat, 
      read_term(Stream,Term,[]),
      (   Term == end_of_file
      ->  free_memory_file(Handle)
      ;   expand_term(Term,ExpandedTerm),
          assert(ExpandedTerm),
          fail
      ).
	  


children_to_actions([],[],_Bindings).
children_to_actions([Child|Children],[Action|Actions],Bindings) :-
   child_to_action(Child,Action,Bindings),!,
   children_to_actions(Children,Actions,Bindings).
children_to_actions([_|Children],Actions,Bindings) :-
   children_to_actions(Children,Actions,Bindings).

child_to_action(Children,Action,Bindings) :-
   my_atom_to_term(Children,Expr,Bindings1),
   unify_bindings(Bindings,Bindings1),
   Action = script(Expr).	


/*
child_to_action(element(raise,Attrs,Children),Action,Bindings) :-
   add_defaults([event='',delay='0s'],Attrs,NewAttrs),
   member(event=EventAtom,NewAttrs),
   my_atom_to_term(EventAtom,Event,Bindings1),
   unify_bindings(Bindings,Bindings1),
   member(delay=IntervalAtom,NewAttrs),
   parse_time_interval(IntervalAtom,Interval),
   Action = raise(Event,Interval), writeln(thisraise(Event,Interval)).
child_to_action(element(log,Attrs,Children),Action,Bindings) :-
   add_defaults([label=null],Attrs,NewAttrs),
   member(expr=ExprAtom,NewAttrs),
   my_atom_to_term(ExprAtom,Expr,Bindings1),
   unify_bindings(Bindings,Bindings1),
   Action = log(Expr).
child_to_action(element(script,Attrs,[Children]),Action,Bindings) :-
   my_atom_to_term(Children,Expr,Bindings1),
   unify_bindings(Bindings,Bindings1),
   Action = script(Expr).
*/

   

my_atom_to_term('','',[]) :- !.
my_atom_to_term(Atom,Term,Bindings) :-
   atom_to_term(Atom,Term,Bindings).


add_defaults([],Attrs,Attrs).
add_defaults([A=V|Defaults],Attrs,NewAttrs) :-
   member(A=_,Attrs),!,
   add_defaults(Defaults,Attrs,NewAttrs).
add_defaults([A=V|Defaults],Attrs,[A=V|NewAttrs]) :-
   add_defaults(Defaults,Attrs,NewAttrs).



unify_bindings([],_Bindings).
unify_bindings([Var=Value|Rest],Bindings) :-
   member(Var=Value,Bindings),
   !,
   unify_bindings(Rest,Bindings).
unify_bindings([_|Rest],Bindings) :-
   unify_bindings(Rest,Bindings).



parse_time_interval(Atom,Interval) :-
   (    member(Suffix,[ms,s]),
        atom_concat(IntervalAtom,Suffix,Atom)
   ->   atom_number(IntervalAtom,IntervalNumber),
        (   Suffix == ms
        ->  Interval is IntervalNumber / 1000
        ;   Interval = IntervalNumber
        )
   ;    write('Error: Could not parse time interval'),nl
   ).
   
   
list :-
   listing(state/2),
   listing(initial/2),
   listing(transition/5),
   listing(parallel/2),
   listing(history/3),
   listing(final/2),
   listing(onexit/2),
   listing(onentry/2).
   



interpret(File) :-   
   load_xml(File),
   retractall(historyValue(_,_)),
   retractall(configuration(_)),
   assert(configuration([scxml,dummy])), 
   assert(state(dummy,scxml)),
   assert(continue),
   initial(Initial),
   message_queue_create(_,[alias(external_queue)]),
   macrostep([transition(dummy,[Initial],[])]).


exit_interpreter :-
   retract(continue).
exit_interpreter.


enqueueScxmlEvent(Event) :-
   (   continue
   ->  (   thread_send_message(external_queue, Event),
           thread_get_message(external_queue, Event0,[timeout(0)]),
	       debug(scxml, 'Received ~p', [Event0]),
           update_eventdata(Event0),
           selectTransitions(Event0,EnabledTransitions),
	       debug(scxml, 'Selected transitions ~p', [EnabledTransitions]),
           macrostep(EnabledTransitions),
           fail
       ;   true
       )
   ;   write('Interpreter is closed.'),nl
   ).


update_eventdata(Event) :-
   retractall(event(_)),
   assert(event(Event)).


macrostep(EnabledTransitions) :-
   microstep(EnabledTransitions),
   microsteps.


microsteps :-
   (   continue
   ->  (   selectTransitions(null,EnabledTransitions)
       ->  microstep(EnabledTransitions),
           microsteps
       ;   retract(internal_queue(Event)),
           selectTransitions(Event,EnabledTransitions),
           microstep(EnabledTransitions),
           microsteps
       ;   true
       )
   ;   true
   ).


microstep(EnabledTransitions) :-
   exitStates(EnabledTransitions),
   executeTransitions(EnabledTransitions),
   enterStates(EnabledTransitions), writeln(EnabledTransitions),
   listing(configuration/1).


   
exitStates(EnabledTransitions) :-
   configuration(Configuration),
   findall(State,(member(transition(Source,Targets,_),EnabledTransitions),
                  Targets \= [],
                  find_LCA([Source|Targets],LCA),
                  member(State,Configuration),
                  isDescendant(State,LCA)),StatesToExit),
   sort(StatesToExit,SortedStatesToExit),
   (   member(State,SortedStatesToExit),
       history(H,State,Depth), 
       (  Depth == deep 
       -> findall(S,(member(S,Configuration),is_atomic(S),isDescendant(S,State)),SS)
       ;  findall(S,(member(S,Configuration),has_parent(S,State)),SS)
       ),
       update_history_value(H,SS),
       fail
   ;   true
   ),
   (   member(State,SortedStatesToExit),
       onexit(State,Children),
       member(Child,Children),
       call(Child),
       fail
   ;   true
   ),
   subtract(Configuration,SortedStatesToExit,NewConfiguration),
   update_configuration(NewConfiguration).


executeTransitions(EnabledTransitions) :-
   (   member(transition(_,_,Children),EnabledTransitions),
       member(Child,Children),
       call(Child),
       fail
   ;   true
   ).
   
enterStates(EnabledTransitions) :-
   findall(StateToEnter,
           (member(transition(Source,Targets,_),EnabledTransitions),
            Targets \= [],
            find_LCA([Source|Targets],LCA),
            member(State,Targets),
            (  is_history(State)
            ->  (   historyValue(State,States)
                ->  member(HS,States),
                    state_to_enter(HS,LCA,StateToEnter)
                ;   transition(State,'',true,States),
                    member(HS,States),
                    state_to_enter(HS,LCA,StateToEnter)
                )
            ;   state_to_enter(State,LCA,StateToEnter)
            )), 
           StatesToEnter),
   sort(StatesToEnter,SortedStatesToEnter),
   (   member(State,SortedStatesToEnter),
       onentry(State,Children),
       member(Child,Children),
       call(Child),
       fail
   ;   true
   ),
   configuration(Configuration),
   union(Configuration,SortedStatesToEnter,NewConfiguration),
   update_configuration(NewConfiguration),
   (   member(State,SortedStatesToEnter),
       is_final(State),
       has_parent(State,Parent),
       has_parent(Parent,Grandparent),
       atom_concat(Parent,'.Done',Event1),
       enqueueInternalEvent(Event1),
       is_parallel(Grandparent),
       forall(has_parent(Child,Grandparent),isInFinalState(Child)),
       atom_concat(Grandparent,'.Done',Event2),
       enqueueInternalEvent(Event2),
       fail
   ;   true
   ),
   (   member(State,NewConfiguration),
       is_final(State),
       has_parent(State,Parent),
       is_scxml(Parent),
       exit_interpreter,
       !
   ;   true
   ).
   
   



enqueueInternalEvent(Event) :-
   assertz(internal_queue(event(Event,''))).


update_configuration(NewConfiguration) :-
   (   retract(configuration(_)),
       fail
   ;   assert(configuration(NewConfiguration))
   ).
   
update_history_value(H,SS) :-
   (   retract(historyValue(H,_)),
       fail
   ;   assert(historyValue(H,SS))
   ).




state_to_enter(State,_Root,State).
state_to_enter(State,_Root,StateToEnter) :-
   is_parallel(State),
   has_parent(Child,State),
   state_to_enter(Child,State,StateToEnter).
state_to_enter(State,_Root,StateToEnter) :-
   is_compound(State),
   transition(init(State),'',true,Children,_),
   member(Child,Children),
   state_to_enter(Child,State,StateToEnter).
state_to_enter(State,Root,StateToEnter) :-
   proper_ancestor(State,Root,StateToEnter).
state_to_enter(State,Root,StateToEnter) :-
   proper_ancestor(State,Root,Ancestor),
   is_parallel(Ancestor),
   has_parent(Child,Ancestor),
   state_to_enter(Child,Ancestor,StateToEnter).



isInFinalState(S) :-
   is_compound(S),
   has_parent(Child,S),
   is_final(Child),
   configuration(Configuration),
   member(Child,Configuration).
isInFinalState(S) :-
   is_parallel(S),
   forall(has_parent(Child,S),isInFinalState(Child)).
   


has_parent(State,Parent) :- state(State,Parent).
has_parent(State,Parent) :- parallel(State,Parent).
has_parent(State,Parent) :- final(State,Parent).
has_parent(State,Parent) :- history(State,Parent,_).


is_parallel(State) :- parallel(State,_).

is_compound(State) :- has_parent(_Child,State). 

is_atomic(State) :- \+ has_parent(_Child,State).

is_history(State) :- history(State,_,_).

is_final(State) :- final(State,_).

is_scxml(State) :- state(State,null).
   
   
selectTransitions(Event,EnabledTransitions) :-
   configuration(Configuration),
   findall(EnabledTransition,
           (	member(StateID,Configuration),
           		is_atomic(StateID),
		   	 	debug(scxml, 'Looking at ~p', [selectTransition(Event,StateID,EnabledTransition)]),
           		selectTransition(Event,StateID,EnabledTransition),
		   	 	debug(scxml, 'Enabled at ~p', [EnabledTransition])	   
		   ),
   EnabledTransitions),
   debug(scxml, 'All enabled at ~p', [EnabledTransitions]),
   EnabledTransitions \= []. 
        
selectTransition(null,State,EnabledTransition) :-
   ancestor(State,null,Ancestor),
   transition(Ancestor,'',Condition,Targets,Children),
   call(Condition),
   EnabledTransition = transition(Ancestor,Targets,Children),
   !.
selectTransition(Event,State,EnabledTransition) :-
   ancestor(State,null,Ancestor),
   transition(Ancestor,Event,Condition,Targets,Children),
   call(Condition),
   EnabledTransition = transition(Ancestor,Targets,Children),
   !.

%liselectTransition(ping,pinger,EnabledTransition).
   


proper_ancestor(StateID,RootID,ParentID) :-
   has_parent(StateID,ParentID),
   ParentID \= RootID.
proper_ancestor(StateID,RootID,AncestorID) :-
   has_parent(StateID,ParentID),
   ParentID \= RootID,
   proper_ancestor(ParentID,RootID,AncestorID).


ancestor(StateID,_RootID,StateID).
ancestor(StateID,RootID,AncestorID) :-
   proper_ancestor(StateID,RootID,AncestorID).


isDescendant(StateID,AncestorID) :-
   proper_ancestor(StateID,null,AncestorID).   



find_LCA([S|Ss],Ancestor) :-
   proper_ancestor(S,null,Ancestor),
   forall(member(S0,Ss),isDescendant(S0,Ancestor)),
   !.


raise(Event,Delay) :-
   writeln(raise(Event,Delay)),
   (   Delay==0
   ->  alarm(0.05,enqueueScxmlEvent(Event),_AlarmID,[remove(true)])
   ;   alarm(Delay,enqueueScxmlEvent(Event),_AlarmID,[remove(true)])
   ).


log(Expr) :-
   write(Expr),nl,
   flush_output.


script(Goal) :-
   call(Goal).
   


   
