type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)

(* initialize database *)
let database : account_spec list ref = ref [];;

let initialize (accounts : account_spec list) : unit =
    database := (!database @ accounts) ;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id () : id = 
    Printf.printf("Enter customer id: ");
     let id = Stdlib.read_int () in 
     id ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount () : int = 
    Printf.printf("Enter amount: ");
     let amount = Stdlib.read_int () in 
     amount ;;
     
(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let acquire_act () : action =
     Printf.printf("(B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ");
     let action = Stdlib.read_line () in 
     match action with
     | "B" -> Balance
     | "-" -> Withdraw (acquire_amount ())
     | "+" -> Deposit (acquire_amount ())
     | "=" -> Next
     | "X" -> Finished 
     | _ -> raise (Failure ("Invalid Input")) ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (id : id) : int = 
  match List.filter (fun x -> x.id = id) !database with 
  | [] -> raise Not_found
  | hd :: _ -> hd.balance ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (id : id) : string = 
  match List.filter (fun x -> x.id = id) !database with 
  | [] -> raise Not_found
  | hd :: _ -> hd.name ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance (id : id) (bal : int) : unit =
  let rec update_balance' (id : id) (bal : int) (searched : account_spec list)
                          (to_search : account_spec list) : unit =
    match to_search with
    | [] -> raise Not_found
    | hd :: tl ->
      if hd.id = id then 
        let hd' = {id = hd.id; name = hd.name; balance = bal} in
        database := hd' :: (searched @ !database) 
      else
        update_balance' id bal (hd :: searched) tl in 
  update_balance' id bal [] !database ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)

let present_message (message : string) : unit = 
    (Printf.printf "%s\n" message) ;; 

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)

let deliver_cash (cash : int) : unit = 
    present_message (string_of_int cash) ;; 
