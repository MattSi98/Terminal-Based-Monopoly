(** The command module parses the user input in order to parse the correct 
    command, verufy that the command is valid, then execute the command.*)

(** [command_parser state user_input] is the tuple (new_state,message) where
    [message] is the message to be printed and [new_state] is the state that
    results from executing the command associated with the given [user_input].
    If the [user_input] does not correspond to any command [message] is a 
    message that prompts for a valid command and [new_state] is [state]. *)
val command_parser : State.t -> string -> (State.t * string)