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


microstep(EnabledTransitions) :-
   exitStates(EnabledTransitions),
   executeTransitions(EnabledTransitions),
   enterStates(EnabledTransitions),
   listing(configuration/1).
   
   
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
   


   
