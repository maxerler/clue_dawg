:- dynamic players/1,
           our_character/1.

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
character(mr_boddy).

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
clue :- prompt_num_players,
        prompt_character,
        loop.

% Setup functions ==========================================================================================================
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

% Loop function ============================================================================================================
loop :- write('Enter a command\n'),read(Data),process(Data).

% Processing functions =====================================================================================================
process(done) :- !.
process(Data) :- atomic_list_concat([H|T], ' ', Data), process(H, T).

process(a, Data) :- write(Data),write('\n'),loop.

