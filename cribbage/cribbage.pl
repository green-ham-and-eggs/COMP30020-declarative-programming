% Author: Tabitha Ayudya <tayudya@student.unimelb.edu.au>
% Student ID: 1086208
% Date: 18/09/2020
% This program is written for Project 1 of COMP30020

% cribbage.pl simulates a card game, Cribbage, with the predicates defined as
% hand_value/3 and select_hand/3

% General strategy for hand_value:
% 1. Check for the "one for his nob" condition first as it requires distinction
%    between the Startcard and the pile of cards at Hand
% 2. Pile the Startcard with the Hand cards, and look at their ranks as
%    their numerical values (ace is 1, Jack is 11, Queen is 12, King is 13) and
%    check for flushes, runs, and pairs
% 3. Find the combinations of cards that add up to 15, by looking at the face
%    cards as the value 10
% 4. Record whether or not each condition is satisfied, or how many solutions
%    there are, and calculate the score accordingly

% hand_value/3 returns the point Value of Hand when the start card is
% Startcard. The scoring follows the rules of cribbage
hand_value(Hand, Startcard, Value) :-

    % check for "one for his nob"
    (one_for_nob(Hand, Startcard)
    -> S1 is 1
    ; S1 is 0
    ),

    % Merge the startcard with the hand and sort them
    orderfy_list([Startcard|Hand], Hand1),
    msort(Hand1, New_hand),

    % flush
    (flush(Hand)
    -> S2 is 4,
        ( flush(New_hand)
        -> S3 is 1
        ; S3 is 0
        )
    ; S2 is 0, S3 is 0
    ),

    % runs
    % it is checked in the order of runs of 5, runs of 4, then runs of 3
    ( subsequent_rank(New_hand)
    -> Xrun5 = 1
    ; Xrun5 = 0
    ),
    ( Xrun5 = 0
    -> findall(Run4, run(New_hand,Run4, 4), Run4s),
       length(Run4s, Xrun4),
       ( Xrun4 = 0
       -> findall(Run3, run(New_hand,Run3, 3), Run3s),
          length(Run3s,Xrun3)
       ; Xrun3 = 0
       )
    ; Xrun3 = 0, Xrun4 = 0
    ),
    S4 is 5 * Xrun5 + 4 * Xrun4 + 3 * Xrun3,

    % pairs
    findall(Pair, pairs(New_hand, Pair), Pairs),
    length(Pairs, Lpairs),
    S5 is 2 * Lpairs,

    % face cards have the value of 10 when finding ranks that add up to 15
    fifteenify_list(New_hand, Newer_hand),

    % 15s
    findall(Zs, fifteens(Newer_hand,Zs), Answer),
    length(Answer, X15),
    S6 is 2 * X15,

    % final value
    Value is S6 + S5 + S4 + S3 + S2 + S1.

% General strategy for select_hand:
% 1. Generate all possible hand (4 cards picked from Cards), and all possible
%    startcards (which is all the cards other than the one in Cards)
% 2. Calculate the score of each possible hand match with each possible
%    startcard using the previously defined hand_value/3
% 3. Calculate the average score for each possible hand
% 4. Take the combination of hand with the highest average score

% select_hand/3 takes Cards, which is a list of cards dealt at the start,
% and returns the cards to keep (Hand), and not to keep (Cribcards). The choice
% is determined by the strategy outlined above
select_hand(Cards, Hand, Cribcards) :-

    % since the setof predicate sorts All in ascending order by its average Ave,
    % the last element will be the Ave-Combi pair with the highest expected
    % score
    setof(Ave-Combi, hand_combi(Cards,Combi,Ave), All),
    last(All, _-Hand),
    % in the case of a tie of Average score, the program still just returns
    % one combination of Hand and Cribcards

    subtract(Cards,Hand,Cribcards).

% odererfy/2 takes a card with term card(Rank,Suit) and returns a card with the
% same term but its Rank is now its numerical value of Rank
orderfy(card(Rank,Suit), Orderfied) :-
    ( Rank = ace
    -> Orderfied = card(1,Suit)
    ; Rank = jack
    -> Orderfied = card(11,Suit)
    ; Rank = queen
    -> Orderfied = card(12,Suit)
    ; Rank = king
    -> Orderfied = card(13,Suit)
    ; Orderfied = card(Rank, Suit)
    ).

% orderfy_list/2 does the predicate orderfy on a whole list
orderfy_list([],[]).
orderfy_list([Head|Tail], [Head0|List]) :-
    orderfy(Head, Head0),
    orderfy_list(Tail, List).

% fifteenify/2 takes a card with term card(Rank,Suit) and returns a card with the
% same term but ranks of face cards (J,Q,K) are now of value 10
fifteenify(card(Rank,Suit), Fifteenified) :-
    ( Rank = 11
    -> Fifteenified = card(10,Suit)
    ; Rank = 12
    -> Fifteenified = card(10,Suit)
    ; Rank = 13
    -> Fifteenified = card(10,Suit)
    ; Fifteenified = card(Rank, Suit)
    ).

% fifteenify_list/2 does the predicate fifteenify on a whole list
fifteenify_list([],[]).
fifteenify_list([Head|Tail], [Head0|List]) :-
    fifteenify(Head, Head0),
    fifteenify_list(Tail, List).

% same_suit/2 and same_rank/2 check if two cards are of the same suit or rank,
% respectively
same_suit(card(_,Suit), card(_,Suit)).
same_rank(card(Rank,_), card(Rank,_)).

% get_ranks/2 takes a list of cards (with term card(Rank,Suit)) and returns a
% list of all its ranks
get_ranks([],[]).
get_ranks([card(Rank,_)|Cards],[Rank|Rest]) :-
    get_ranks(Cards,Rest).

% one_for_nob/2 takes a list of cards and a single card Startcard and determines
% where the list satifies the "one for his nob" condition
one_for_nob([Card|Rest],Startcard) :-
    (  Card = card(jack,_)
    -> same_suit(Card,Startcard)
    ;  one_for_nob(Rest,Startcard)
    ).

% flush/1 takes a list of cards and checks if they are all the same suit
flush([_]).
flush([Card1,Card2|Rest]) :-
    same_suit(Card1,Card2),
    flush([Card2|Rest]).

% list_sub/2 holds when the second argument is a sublist of the first. When only
% the first argument is ground, it can be used to generate sublists
list_sub([],[]).
list_sub([_|Xs],Ys) :- list_sub(Xs,Ys).
list_sub([X|Xs],[X|Ys]) :- list_sub(Xs,Ys).

% fifteens/2 takes a list of cards and finds subsets of it whose ranks add
% up to 15
fifteens(Cards,Subsets) :-
    list_sub(Cards,Subsets),
    get_ranks(Subsets, Ranks),
    sum_list(Ranks,15).

% subsequent_rank/1 takes a list of cards and checks is they are in consecutive
% increasing order
subsequent_rank([_]).
subsequent_rank([card(E1,_),card(E2,Suit)|Tail]) :-
    1 is E2 - E1,
    subsequent_rank([card(E2,Suit)|Tail]).

% run/3 takes a list of cards Cards and a number Length, and returns subsets of
% the cards that is a run of the specified length
run(Cards, Run, Length) :-
    list_sub(Cards, Run),
    length(Run, X), X = Length,
    subsequent_rank(Run).

% pairs/2 holds when the second argument is a pair of cards with matching ranks
% from the list. When only the first argument is ground, it can be used to
% generate matching pairs
pairs(Cards, Pairs) :-
    list_sub(Cards, Pairs),
    length(Pairs, X), X = 2,
    Pairs = [card(Rank,_), card(Rank,_)].

% gen_card/3 generates a card Card with term card(Rank,Suit)
gen_card(Ranks, Suits, Card) :-
    member(R,Ranks),member(S,Suits), Card = card(R,S).

% pos_cards/2 takes a list of cards and returns a list of all cards other than
% those of the list
pos_cards(Cards, Pos) :-
    setof(C, gen_card([2,3,4,5,6,7,8,9,10,jack,queen,king,ace],
                      [clubs,diamonds,hearts,spades], C), All),
    subtract(All,Cards,Pos).

% pos_combs/4 takes a list of possible hands and possible startcards. It returns
% the Hand and the Value associated to it, according to the scoring rules of
% hand_value/3
pos_combs(Pos_hand, Pos_start, Hand, Value) :-
    member(Hand,Pos_hand), member(Start,Pos_start),
    hand_value(Hand, Start, Value).

% average/2 takes a list of numbers Vals and returns its average
average(Vals, Ave) :-
    sumlist(Vals, Sum), length(Vals,Len),
    Ave is Sum / Len.

% hand_combi/3 takes a list of cards and returns a combination of it along with
% its expected score value Ave
hand_combi(Cards,Combi,Ave):-
    setof(Hand0, (list_sub(Cards, Hand0), length(Hand0, 4)), Pos_hand),
    pos_cards(Cards, Pos_start),
    bagof(Value,pos_combs(Pos_hand, Pos_start, Combi, Value), Values),
    average(Values, Ave).
