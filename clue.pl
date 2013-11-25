/* CLUE ADVISOR */

/* 
MAX ERLER
Student #: 59661108
CPSC ID: c2w7

THEA SIMPSON
Student #: 51919108
CPSC ID: k8r7
*/

:- dynamic player/1,
		   num_players/1,
           our_character/1,
           suspect_weapon/1,
           suspect_character/1,
           suspect_room/1,
           player_has/2,
           player_asked_for/2.
           
           
% ==========================================================================================================================
% Database section =========================================================================================================
% ==========================================================================================================================

character(miss_scarlet).
character(colonel_mustard).
character(mrs_white).
character(mr_green).
character(mrs_peacock).
character(professor_plum).

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(candlestick).
weapon(pistol).
weapon(wrench).

room(kitchen).
room(ballroom).
room(conservatory).
room(billiard_room).
room(library).
room(study).
room(hall).
room(lounge).
room(dining_room).

% ==========================================================================================================================
% Teardown functions =======================================================================================================
% ==========================================================================================================================


% clears all suspect facts
clear_state :- retractall(suspect_weapon(_)),
               retractall(suspect_character(_)),
               retractall(suspect_room(_)),
               retractall(player(_)),
               retractall(player_has(_, _)),
               retractall(player_asked_for(_, _)).


% ==========================================================================================================================
% Dynamic variable setup functions =========================================================================================
% ==========================================================================================================================


% intialize all suspect weapons, characters and rooms
init_all_suspects :- init_suspect_w, init_suspect_c, init_suspect_r.


% intialize list of suspect weapons
init_suspect_w :- assert(suspect_weapon(rope)),
				          assert(suspect_weapon(pipe)),
				          assert(suspect_weapon(knife)), 
				          assert(suspect_weapon(candlestick)),
				          assert(suspect_weapon(pistol)),
				          assert(suspect_weapon(wrench)).
				          

% intialize list of suspect characters
init_suspect_c :- assert(suspect_character(miss_scarlet)), 
				          assert(suspect_character(colonel_mustard)), 
				          assert(suspect_character(mrs_white)),
				          assert(suspect_character(mr_green)),
				          assert(suspect_character(mrs_peacock)),
				          assert(suspect_character(professor_plum)).
				          

% intialize list of suspect rooms
init_suspect_r :- assert(suspect_room(kitchen)),
				          assert(suspect_room(ballroom)),
				          assert(suspect_room(conservatory)),
				          assert(suspect_room(billiard_room)),
				          assert(suspect_room(library)),
				          assert(suspect_room(study)),
				          assert(suspect_room(hall)),
				          assert(suspect_room(lounge)),
				          assert(suspect_room(dining_room)).


% ==========================================================================================================================
% Main function ============================================================================================================
% ==========================================================================================================================


clue :- init,
        loop.


% ==========================================================================================================================
% Setup functions ==========================================================================================================
% ==========================================================================================================================


% initializes the game
init :- clear_state,
        init_all_suspects,
        prompt_num_players,
        prompt_characters,
        %prompt_character, % I don't think we actually care who we're playing as, so I'll comment this out for now.
        prompt_cards.


% prompts the user for the number of players and sets the player number
prompt_num_players :- write('How many other players are there?\n'),
                      read(Players),
                      set_num_players(Players).
                   

% sets the number of players
set_num_players(end_of_file) :- !.
set_num_players(Players) :- assert(num_players(Players)).


% prompts the user for the name of their character
prompt_character :- write('Who is your character?\n'),
                    read(Character),
                    set_character(Character).
                    

% sets the user's character
set_character(end_of_file) :- !.
set_character(Character) :- character(Character),assert(our_character(Character)).

% prompts the user for their cards
prompt_cards :- write('What are your cards?\n'),
                read(Cards),
                remove_initial_cards(Cards).
                

% removes the user's cards from the list of suspects
remove_initial_cards(end_of_file) :- !.
remove_initial_cards(Cards) :- atomic_list_concat(L, ' ', Cards),
                               remove_cards(L).
                

% ==========================================================================================================================
% Loop function ============================================================================================================
% ==========================================================================================================================


loop :- write('Enter a command:\n'),read(Data),process(Data).


% ==========================================================================================================================
% Processing functions =====================================================================================================
% ==========================================================================================================================


% general process function: dispatches request to handler rule and loops
process(done) :- !.
%process(Data) :- atomic_list_concat([H|T], ' ', Data), process(H, T),loop.


% processes a shown command and keeps track of the cards that player Player has shown us
process(shown) :- prompt_player(Player),
                  prompt_card_remaining_cards(Card), 
                  player_has_card(Player, Card),
                  write('\n  Player '), write(Player), write(' has '), write(Card), write('\n\n'),
                  loop.


% processes a suspect? command and tells whether the given card is still suspected
process(is_suspect) :- prompt_card_all_cards(Card),
                       ( is_suspect(Card)
                         -> write('\n  Yes.\n\n')
                         ; write('\n  No.\n\n')
                       ),
                       loop.


process(show_cards) :- prompt_player(Player),
                       get_cards_of_player(Player, Cards),
                       ( Cards == []
                         -> write('\n  We don\'t know any of this player\'s cards!\n')
                         ;  write('\n  Player '), write(Player), write(' has:\n'), write_cards(Cards)
                       ),
                       write('\n'),
                       loop. 
                       

% processes an asked_for command and keeps track of the cards that player Player has asked for
process(asked_for) :- prompt_player(Player),
                      get_all_characters(AllCharacters),
                      prompt_suspect_character(AllCharacters, Character),
                      get_all_rooms(AllRooms),
                      prompt_suspect_room(AllRooms, Room),
                      get_all_weapons(AllWeapons),
                      prompt_suspect_weapon(AllWeapons, Weapon),
                      Cards = [Character, Room, Weapon],
                      player_asked_for_cards(Player, Cards),
                      write('\n  Player '), write(Player), write(' asked for: '), write(Cards), write('\n\n'),
                      loop.

process(show_remaining_suspects) :- write('\n  Remaining characters:\n'),
                                    get_remaining_characters(Characters),
                                    write_cards(Characters),
                                    write('\n  Remaining rooms:\n'),
                                    get_remaining_rooms(Rooms),
                                    write_cards(Rooms),
                                    write('\n  Remaining weapons:\n'),
                                    get_remaining_weapons(Weapons),
                                    write_cards(Weapons),
                                    write('\n'),
                                    loop.

% ==========================================================================================================================
% Player action functions ==================================================================================================
% ==========================================================================================================================


% suggest a guess based on the weapons, characters and rooms that are still possible suspects
suggestGuess(W, C, R) :- possibleWeapon(W), possibleCharacter(C), possibleRoom(R).


% ==========================================================================================================================
% Check remaining suspects functions =======================================================================================
% ==========================================================================================================================


% checks to see if the given character, weapon, or room is still a suspect
is_suspect(S) :- character(S), !, suspect_character(S).
is_suspect(S) :- weapon(S), !, possibleWeapon(S).
is_suspect(S) :- room(S), !, possibleRoom(S).


% check if a weapon is still a suspect
possibleWeapon(W) :- weapon(W), suspect_weapon(W).


% check if a character is still a suspect
possibleCharacter(C) :- character(C), suspect_character(C).


% check if a room is still a suspect
possibleRoom(R) :- room(R), suspect_room(R).


% ==========================================================================================================================
% Suspect card removal functions ===========================================================================================
% ==========================================================================================================================


% removes a list of cards from the suspect list
remove_cards([]).
remove_cards([H|T]) :- remove_suspect(H),remove_cards(T).


% removes a suspect character, weapon, or room from the list of suspects
remove_suspect(Suspect) :- character(Suspect), !, retract(suspect_character(Suspect)).
remove_suspect(Suspect) :- weapon(Suspect), !, retract(suspect_weapon(Suspect)).
remove_suspect(Suspect) :- room(Suspect), !, retract(suspect_room(Suspect)).


% ==========================================================================================================================
% Track player's cards functions ===========================================================================================
% ==========================================================================================================================

% set a character as a player active in the game
set_player(Number, Characters) :- Index is Number - 1,
								  element_at(Index, Characters, Character),
								  character(Character),
								  assert(player(Character)).

% removes the given card from the list of suspect cards and tracks which player showed us that card
player_has_card(Player, Card) :- player(Player), remove_suspect(Card), assert(player_has(Player, Card)).


% keeps track of the cards that a certain player has asked for
player_asked_for_cards(_, []).
player_asked_for_cards(Player, [Card|T]) :- player(Player),
                                            ( 
                                              player_asked_for(Player, Card)
                                              -> player_asked_for_cards(Player, T)
                                              ; assert(player_asked_for(Player, Card)), player_asked_for_cards(Player, T)
                                            ).
                                           


% ==========================================================================================================================
% HELPER FUNCTIONS =========================================================================================================
% ==========================================================================================================================


% writes out a list of cards
write_cards([]).
write_cards([H|T]) :- write('    '),
                      write(H),
                      write('\n'),
                      write_cards(T).
                      
                      
% gets a list of all players
get_all_players(Players) :- findall(Player, player(Player), Players).                      
                      

% gets a list of all characters
get_all_characters(Characters) :- findall(Char, character(Char), Characters).           


% gets a list of all weapons
get_all_weapons(Weapons) :- findall(Weapon, weapon(Weapon), Weapons).


% gets a list of all rooms
get_all_rooms(Rooms) :- findall(Room, room(Room), Rooms).           
                      
                      
% gets a list of the remaining suspect characters
get_remaining_characters(Characters) :- findall(Char, suspect_character(Char), Characters).           


% gets a list of the remaining suspect weapons
get_remaining_weapons(Weapons) :- findall(Weapon, suspect_weapon(Weapon), Weapons).      


% gets a list of the remaining suspect rooms
get_remaining_rooms(Rooms) :- findall(Room, suspect_room(Room), Rooms). 
     
     
% gets a list of all of the given player's cards
get_cards_of_player(Player, Cards) :- findall(Card, player_has(Player, Card), Cards).

     
% gets the element at the given index in the supplied list
element_at(0, [H|_], Elem) :- Elem = H.                            
element_at(Index, [_|T], Elem) :- NextIndex is Index - 1,
                                  element_at(NextIndex, T, Elem).

                      
% ==========================================================================================================================
% I/O UI FUNCTIONS =========================================================================================================
% ==========================================================================================================================

% prompts the user to input the other player's characters
prompt_characters :- write('\n  Choose other players\' characters:\n'),
												   get_all_characters(Characters),
                                                   write_options(1, Characters),
                                                   write('\n'),
                                				   num_players(Number),
                                                   get_characters(Number, Characters).

% loop prompts user for player characters based on number of players in the game
get_characters(0, _).
get_characters(Number, Characters) :- read(Player),
								      set_player(Player, Characters),
								      New is Number - 1,
								      get_characters(New, Characters).

% prompts the user to input a player 
prompt_player(Player) :- write('\n  Which player?\n'),
                         get_all_players(Players),
                         write_options(1, Players),
                         write('\n'),
                         read(Number),
                         Index is Number - 1,
                         element_at(Index, Players, Player).


% prompts the user to input a card
prompt_card_all_cards(Card) :- write('\n  What kind of card?\n'),
                               write('    1) character\n'),
                               write('    2) room\n'),
                               write('    3) weapon\n\n'),
                               read(Number),
                               prompt_card_kind_all_cards(Number, Card).
                     
                     
% prompts the user to input the kind of card associated with that index
prompt_card_kind_all_cards(1, Card) :- get_all_characters(Characters), prompt_suspect_character(Characters, Card).
prompt_card_kind_all_cards(2, Card) :- get_all_rooms(Rooms), prompt_suspect_room(Rooms, Card).
prompt_card_kind_all_cards(3, Card) :- get_all_weapons(Weapons), prompt_suspect_weapon(Weapons, Card).


% prompts the user to input a card from the list of remaining cards
prompt_card_remaining_cards(Card) :- write('\n  What kind of card?\n'),
                                     write('    1) character\n'),
                                     write('    2) room\n'),
                                     write('    3) weapon\n\n'),
                                     read(Number),
                                     prompt_card_kind_remaining_cards(Number, Card).
                               
                               
% prompts the user to input the kind of card associated with that index from the list of remaining cards
prompt_card_kind_remaining_cards(1, Card) :- get_remaining_characters(Characters), prompt_suspect_character(Characters, Card).
prompt_card_kind_remaining_cards(2, Card) :- get_remaining_rooms(Rooms), prompt_suspect_room(Rooms, Card).
prompt_card_kind_remaining_cards(3, Card) :- get_remaining_weapons(Weapons), prompt_suspect_weapon(Weapons, Card).


% prompts the user to input a character card
prompt_suspect_character(Characters, Character) :- write('\n  Choose a character:\n'),
                                                   write_options(1, Characters),
                                                   write('\n'),
                                                   read(Number),
                                                   Index is Number - 1,
                                                   element_at(Index, Characters, Character).
                                       

% prompts the user to input a weapon card
prompt_suspect_weapon(Weapons, Weapon) :- write('\n  Choose a weapon:\n'),
                                          write_options(1, Weapons),
                                          write('\n'),
                                          read(Number),
                                          Index is Number - 1,
                                          element_at(Index, Weapons, Weapon).    
                                     
                                     
% prompts the user to input a room card
prompt_suspect_room(Rooms, Room) :- write('\n  Choose a room:\n'),
                                    write_options(1, Rooms),
                                    write('\n'),
                                    read(Number),
                                    Index is Number - 1,
                                    element_at(Index, Rooms, Room).                                       
                                       
              
% writes out an option for a list
write_option(N, Suspect) :- write('    '), 
                            write(N), 
                            write(') '), 
                            write(Suspect),
                            write('\n').
                             

% writes out all of the options for a list        
write_options(_, []).
write_options(N, [H|T]) :- write_option(N, H),
                                   Next is N + 1,
                                   write_options(Next, T).




