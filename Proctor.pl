assign_quiz(quiz(_, Day, Slot, Count), FreeSchedule, AssignedTAs) :-
    day_getter(Day, FreeSchedule, DaySchedule),
    ta_getter(Slot, Count, DaySchedule, AssignedTAs).
day_getter(Day, [day(Day, DaySchedule)|_], DaySchedule).
day_getter(Day, [_|Rest], DaySchedule) :-
    day_getter(Day, Rest, DaySchedule).
ta_getter(Slot, Count, DaySchedule, AssignedTAs) :-
    nth1(Slot, DaySchedule, AvailableTAs),
    length(AvailableTAs, AvailableCount),
    AvailableCount >= Count,
    ta_getter_help(Count, AvailableTAs, AssignedTAs).
ta_getter_help(0, _, []).
ta_getter_help(Count, AvailableTAs, [TA|T]) :-
    Count > 0,
    member(TA, AvailableTAs),
    NewCount is Count - 1,
    select(TA, AvailableTAs, RemainingTAs),
    ta_getter_help(NewCount, RemainingTAs, T).
	
%-------------------------------------------------------------------------------------------------------

assign_quizzes([], _, []).
assign_quizzes([quiz(Course, Day, Slot, Count)|RestQuizzes], FreeSchedule, [proctors(quiz(Course, Day, Slot, Count), AssignedTAs)|RestProctoringSchedule]) :-
    assign_quiz(quiz(Course, Day, Slot, Count), FreeSchedule, AssignedTAs),
	remove_free_list(AssignedTAs,Day,Slot,FreeSchedule,NewFree),
    assign_quizzes(RestQuizzes, NewFree, RestProctoringSchedule).
	
%--------------------------------------------------------------------------------------------------------

assign_proctors(AllTAs ,Quizzes, TeachingSchedule, ProctoringSchedule):-
   free_schedule1(AllTAs, TeachingSchedule, FreeSchedule),
   assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).   
   
%--------------------------------------------------------------------------------------------------------

free_slot([],_,_,[]).
free_slot([ta(Name,DayOff)|RestTA],Day , TeachingSlot, [Name|RestFreeSlot]):-
   Day\=DayOff,
   \+member(Name,TeachingSlot),
   free_slot(RestTA, Day, TeachingSlot, RestFreeSlot).
free_slot([ta(Name,DayOff)|RestTA], Day, TeachingSlot, FreeSlot):-
   (Day = DayOff;
   member(Name, TeachingSlot)),
   free_slot(RestTA, Day , TeachingSlot, FreeSlot).
free_day(_,_,[],[]).
free_day(AllTAs, Day, [TeachingSlot|RestTeaching], [FreeSlotPerm|RestFreeDay]):-
   free_slot(AllTAs, Day , TeachingSlot, FreeSlot),
   permutation(FreeSlot,FreeSlotPerm), % permutations added as tests in test file show permutations although we dont think its necessary logically
   free_day(AllTAs, Day , RestTeaching, RestFreeDay).
free_schedule(_,[],[]).
free_schedule(AllTAs,[day(Day,TeachingDay)|RestTeachingSchedule], [day(Day,FreeDay)|RestFreeSchedule]):-
   free_day(AllTAs, Day, TeachingDay, FreeDay),
   free_schedule(AllTAs, RestTeachingSchedule, RestFreeSchedule).
   
%---------------------------------------------------------------------------------------
% predicate to remove already assigned TAs from freeschedule to avoid collisions in TAs when having quizzes at same time

remove_free(TA, Day, Slot, FreeSchedule, UpdatedFreeSchedule) :-
    nth1(Index, [sat,sun, mon, tue, wed, thu, fri], Day), 
    nth1(Index, FreeSchedule, day(_,FreeSlots)), 
    nth1(Slot, FreeSlots, TAs), 
    delete(TAs, TA, UpdatedTAs),
    replace(FreeSlots, Slot, UpdatedTAs, UpdatedFreeSlots), 
    replace1(FreeSchedule, Index, UpdatedFreeSlots, UpdatedFreeSchedule,Day).	
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).
replace1([_|T], 1, X, [day(Day,X)|T],Day).
replace1([H|T], I, X, [H|R],Day) :-
    I > 1,
    I1 is I - 1,
    replace1(T, I1, X, R,Day).
remove_free_list([], _, _, FreeSchedule, FreeSchedule). 
remove_free_list([TA|RestTAs], Day, Slot, FreeSchedule, UpdatedFreeSchedule) :-
    remove_free(TA, Day, Slot, FreeSchedule, TempFreeSchedule), 
    remove_free_list(RestTAs, Day, Slot, TempFreeSchedule, UpdatedFreeSchedule). 
	
% Same as free_schedule but without permutations to avoid long runtime when calling assign_proctors
%-----------------------------------------------------------------------------------------------------
 
free_slot1([],_,_,[]).
free_slot1([ta(Name,DayOff)|RestTA],Day , TeachingSlot, [Name|RestFreeSlot]):-
   Day\=DayOff,
   \+member(Name,TeachingSlot),
   free_slot1(RestTA, Day, TeachingSlot, RestFreeSlot).
free_slot1([ta(Name,DayOff)|RestTA], Day, TeachingSlot, FreeSlot):-
   (Day = DayOff;
   member(Name, TeachingSlot)),
   free_slot1(RestTA, Day , TeachingSlot, FreeSlot).
free_day1(_,_,[],[]).
free_day1(AllTAs, Day, [TeachingSlot|RestTeaching], [FreeSlot|RestFreeDay]):-
   free_slot1(AllTAs, Day , TeachingSlot, FreeSlot),
   free_day1(AllTAs, Day , RestTeaching, RestFreeDay).
free_schedule1(_,[],[]).
free_schedule1(AllTAs,[day(Day,TeachingDay)|RestTeachingSchedule], [day(Day,FreeDay)|RestFreeSchedule]):-
   free_day1(AllTAs, Day, TeachingDay, FreeDay),
   free_schedule1(AllTAs, RestTeachingSchedule, RestFreeSchedule).





   