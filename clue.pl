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
           our_character/1,
           player_character/1,
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
init_suspect_w :- assert(suspect_weapon(dagger)),
				          assert(suspect_weapon(rope)),
				          assert(suspect_weapon(pipe)), 
				          assert(suspect_weapon(candlestick)),
				          assert(suspect_weapon(revolver)),
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
        %prompt_characters,
        %prompt_character, % I don't think we actually care who we're playing as, so I'll comment this out for now.
        prompt_cards.


% prompts the user for the number of players and sets the player number
prompt_num_players :- write('How many players are there?\n'),
                      read(Players),
                      set_num_players(Players).
                   

% sets the number of players
set_num_players(end_of_file) :- !.
set_num_players(Players) :- init_players(Players).


% initializes all players
init_players(1) :- assert(player(1)).
init_players(N) :- assert(player(N)),
                   X is N - 1,
                   init_players(X).


% prompts the user for the name of their character
prompt_character :- write('Who is your character?\n'),
                    read(Character),
                    set_character(Character).
                    

% sets the user's character
set_character(end_of_file) :- !.
set_character(Character) :- character(Character),assert(our_character(Character)).


% prompts the user for the names of the other players' characters
prompt_characters :- write('Who are the others characters?\n'),
                    read(Characters),
                    set_characters(Characters).


% sets the other players' characters
set_characters(end_of_file) :- !.
set_characters(Characters) :- atomic_list_concat(L, ', ', Characters),
                               set_player_characters(L).


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


loop :- write('Enter a command\n'),read(Data),process(Data).


% ==========================================================================================================================
% Processing functions =====================================================================================================
% ==========================================================================================================================


% general process function: dispatches request to handler rule and loops
process(done) :- !.
process(Data) :- atomic_list_concat([H|T], ' ', Data), process(H, T),loop.


% processes a suspect? command and tells whether the given card is still suspected
process('suspect?', [S]) :- is_suspect(S),write('  Yes\n').
process('suspect?', _) :- write('  No\n').


% processes a shown command and keeps track of the cards that player Player has shown us
process('shown', [Player, Card]) :- atom_number(Player, Number),
                                    player(Number), 
                                    player_has_card(Number, Card),
                                    write('  Player '), write(Player), write(' has '), write(Card), write('\n').
process('shown', _) :- write('  Error!\n').


% processes an asked_for command and keeps track of the cards that player Player has asked for
process('asked_for', [Player, Card1, Card2, Card3]) :- atom_number(Player, Number),
                                                       player(Number),
                                                       Cards = [Card1, Card2, Card3],
                                                       player_asked_for_cards(Number, Cards),
                                                       write('  Player '), write(Player), 
                                                       write(' asked for '), write(Cards), write('\n').
process('asked_for', _) :- write('  Error!\n').


process('show_cards', [Player]) :- atom_number(Player, Number),
                                   findall(Card, player_has(Number, Card), Cards),
                                   write_cards(Cards).

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
                                           
                                          
% create players at beginning of game
set_player_characters([]).
set_player_characters([H|T]) :- set_player(H),set_player_characters(T).


% ==========================================================================================================================
% HELPER FUNCTIONS =========================================================================================================
% ==========================================================================================================================


% writes out a list of cards
write_cards([]).
write_cards([H|T]) :- write('  '),
                      write(H),
                      write('\n'),
                      write_cards(T).
                      
                      
get_all_characters(Characters) :- findall(Char, character(Char), Characters).           


get_all_weapons(Weapons) :- findall(Weapon, weapon(Weapon), Weapons).


get_all_rooms(Rooms) :- findall(Room, room(Room), Rooms).           
                      
                      
% ==========================================================================================================================
% I/O UI FUNCTIONS =========================================================================================================
% ==========================================================================================================================


% prompts the user to input a card
prompt_card(Card) :- write('  What kind of card?\n'),
                     write('    1) character\n'),
                     write('    2) room\n'),
                     write('    3) weapon\n'),
                     read(Index),
                     prompt_card_kind(Index, Card).
                     
                     
% prompts the user to input the kind of card associated with that index
prompt_card_kind(1, Card) :- prompt_suspect_character(Card).
prompt_card_kind(2, Card) :- prompt_suspect_room(Card).
prompt_card_kind(3, Card) :- prompt_suspect_weapon(Card).


% prompts the user to input a character card
prompt_suspect_character(Character) :- write('  Choose a character:\n'),
                                       get_all_characters(Characters),
                                       write_suspect_options(1, Characters),
                                       read(Index),
                                       get_suspect(Index, Characters, Character).
                                       

% prompts the user to input a weapon card
prompt_suspect_weapon(Weapon) :- write('  Choose a weapon:\n'),
                                 get_all_weapons(Weapons),
                                 write_suspect_options(1, Weapons),
                                 read(Index),
                                 get_suspect(Index, Weapons, Weapon).    
                                     
                                     
% prompts the user to input a room card
prompt_suspect_room(Room) :- write('  Choose a room:\n'),
                             get_all_rooms(Rooms),
                             write_suspect_options(1, Rooms),
                             read(Index),
                             get_suspect(Index, Rooms, Room).                                       
                                       
                            
% gets the suspect card at the given index in the supplied list of suspects
get_suspect(1, [H|_], Suspect) :- Suspect = H.                            
get_suspect(Index, [_|T], Suspect) :- NextIndex is Index - 1,
                                      get_suspect(NextIndex, T, Suspect).
              
              
% writes out an option for a suspect list
write_suspect_option(N, Suspect) :- write('    '), 
                                    write(N), 
                                    write(') '), 
                                    write(Suspect),
                                    write('\n').
                             

% writes out all of the options for a suspect list        
write_suspect_options(_, []).
write_suspect_options(N, [H|T]) :- write_suspect_option(N, H),
                                   Next is N + 1,
                                   write_suspect_options(Next, T).




