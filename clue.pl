/* CLUE ADVISOR */

/* 
MAX ERLER
Student #:
CPSC ID:

THEA SIMPSON
Student #: 51919108
CPSC ID: k8r7
*/

:- dynamic players/1,
           our_character/1,
           suspect_weapon/1,
           suspect_character/1,
           suspect_room/1.

% Database section
/* NOTE: THESE MAY OR MAY NOT BE CORRECT; I LOOKED THEM UP ON WIKIPEDIA, SO WE
 *       MIGHT NEED TO UPDATE THEM WITH THE ACTUAL NAMES FROM THE RULES.
 */
character(miss_scarlet).
character(colonel_mustard).
character(mrs_white).
character(mr_green).
character(mrs_peacock).
character(professor_plum).
character(mr_boddy). % I think these are all correct apart from this dude ... defs never heard of him

weapon(dagger).
weapon(rope).
weapon(pipe).
weapon(candlestick).
weapon(revolver).
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

% Main function ============================================================================================================
clue :- init,
        loop.

% Setup functions ==========================================================================================================
% initializes the game
init :- init_all_suspects,
        prompt_num_players,
        prompt_character.

% prompts the user for the number of players and sets the player number
prompt_num_players :- write('How many players are there?\n'),
                      read(Players),
                      set_num_players(Players).
                   
% sets the number of players
set_num_players(end_of_file) :- !.
set_num_players(Players) :- assert(players(Players)).

% prompts the user for the name of their character
prompt_character :- write('Who is your character?\n'),
                    read(Character),
                    set_character(Character).
                    
% sets the user's character
set_character(end_of_file) :- !.
set_character(Character) :- character(Character),assert(our_character(Character)).

% deal cards
/* TO DO */

% Dynamic variable setup functions ==============================================================================================

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
				          assert(suspect_character(professor_plum)),
				          assert(suspect_character(mr_boddy)). 

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

% Loop function ============================================================================================================
loop :- write('Enter a command\n'),read(Data),process(Data).

% Processing functions =====================================================================================================
process(done) :- !.
process(Data) :- atomic_list_concat([H|T], ' ', Data), process(H, T).

process(a, Data) :- write(Data),write('\n'),loop.


% Player action functions =====================================================================================================

% suggest a guess based on the weapons, characters and rooms that are still possible suspects
suggestGuess(W, C, R) :- possibleWeapon(W), possibleCharacter(C), possibleRoom(R).

% Check remaining suspects functions ==========================================================================================

% check if a weapon is still a suspect
possibleWeapon(W) :- weapon(W), suspect_weapon(W).

% check if a character is still a suspect
possibleCharacter(C) :- character(C), suspect_character(C).

% check if a room is still a suspect
possibleRoom(R) :- room(R), suspect_room(R).
