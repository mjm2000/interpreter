open Printf
open Hashtbl 
open Map

module VarMap = Map.Make(String)

type stackValue = BOOL of  string option * bool | INT of string option *int | ERROR | STRING of string option * string  | UNIT of string * stackValue |NAME of string|FUNCALL of string * string |INOUTFUNCALL of string * string  

type command = 
    ADD | SUB | MUL | DIV | PUSH of stackValue | 
PRINTLN|POP|REM|SWAP|TOSTRING|QUIT|NEG|CAT|OR|AND|NOT|LT|BIND|EQ|IF
    |LET|END|FUNDEC of string * string| RETURN |FUNEND|CALL 
    |INOUTFUNDEC of string * string | ENDOFFUNC|BINDPARAM of string * string | MERGE of string * string  

type varType = FUNC of command list * varType VarMap.t * string | VAR of stackValue| INOUTFUNC of command list * varType VarMap.t * string 

     
let interpreter ((input : string), (output : string) ) : unit =
    let ic = open_in input
    in 

    let oc = open_out output 

    in 

    let rec loop_read acc =
        try 
            let l = String.trim(input_line ic) in 
            loop_read (l::acc)
        with 
            | End_of_file -> List.rev acc

    in
    let str_list = loop_read []
    in 

    let isInt s :bool = 
        let code = String.get s 0 in
        '0'<= code && code <= '9' || code == '-'
    
    in
    (*Problem here*)
    let rec to_int_f s iacc i md =
         if i < (String.length s) then
            match s.[i] with
            | '-'->  to_int_f s (iacc) (i+1) (-1)
            | dig when (dig < '0' || dig > '9') ->
                None
            |dig-> let dig_val = (Char.code dig - 48) in 
                to_int_f s (iacc*10+dig_val) (i+1) md
        else Some(iacc * md)
    in 
    let to_int s = to_int_f s 0 0 1
    in
    
    let com_to_string com = match com with
        |BOOL(Some name,_)-> name
        |BOOL(_,tf) -> if tf then ":true:" else ":false:"
        |INT(Some name,_)-> name
        |INT(_,num) -> string_of_int num
        |STRING(Some name,_)-> name
        |STRING(_,str) -> String.sub str 1 (String.length str -2) 
        |NAME(name) ->name
        |UNIT(_,_) -> ":unit:"
        |FUNCALL(str,_) -> String.concat "" ("Function"::str::[])
        |INOUTFUNCALL(str,_) -> String.concat "" ("Function"::str::[]) 
        |ERROR -> ":error:"

    in
    let str_2_value s  = match s with
        |":true:"-> BOOL(None,true)
        |":false:"-> BOOL(None,false)
        |":unit:"-> UNIT("",ERROR)
        | num when isInt num -> 
            (match to_int num with 
            |Some (nm) ->INT(None,nm)
            |None -> ERROR )
        | str when str.[0] == '"' -> STRING(None,str)   
        | name -> NAME(name) 
    in

    let str_2_com s  = 
        match s with
        |"add" -> ADD
        |"sub" -> SUB
        |"mul" -> MUL
        |"div" -> DIV
        |"neg" -> NEG
        |"toString"->TOSTRING
        |"rem" -> REM
        |"pop" -> POP
        |"swap" -> SWAP
        |"quit" -> QUIT
        |"println"->PRINTLN
        |"cat"->CAT
        |"and"-> AND
        |"or"-> OR
        |"funEnd"-> FUNEND
        |"return"-> RETURN
        |"not"-> NOT
        |"equal"->EQ
        |"lessThan"->LT
        |"bind"->BIND 
        |"let"-> LET
        |"end"-> END
        |"if"->IF
        |"call"->CALL
        |str when String.length str >= 12  && String.equal "inOutFun" (String.sub str 0 8) ->
            let fun_str = String.sub str 9 (String.length str -9) in
            let fun_list = String.split_on_char ' ' fun_str in
            let fun_nam = List.hd fun_list in 
            let fun_arg = List.nth fun_list 1 in 
            INOUTFUNDEC(fun_nam,fun_arg)
        |str when String.equal "fun" (String.sub str 0 3) ->
            let fun_str = String.sub str 4 (String.length str -4) in
            let fun_list = String.split_on_char ' ' fun_str in
            let fun_nam = List.hd fun_list in 
            let fun_arg = List.nth fun_list 1 in 
            FUNDEC(fun_nam,fun_arg)
        |str -> 
            let pushv = String.sub str 5 (String.length str -5) in
            PUSH(str_2_value pushv )

    in
    let comList = List.map str_2_com str_list
    in
        let scom_2_str com  = 
        match com with
        |ADD-> "add"
        |SUB-> "sub"
        |MUL-> "mul"
        |DIV-> "div"
        |NEG-> "neg"
        |TOSTRING->"toString"
        |REM -> "rem" 
        |POP -> "pop" 
        |SWAP-> "swap"
        |QUIT -> "quit"
        |PRINTLN->"println"
        |FUNDEC(name,_) -> String.concat "" ["fun ";name]
        |PUSH(str) -> String.concat "" ["push";"(";com_to_string str;") | " ]
        |CAT->"cat"
        |AND-> "and"
        | OR-> "OR" 
        |FUNEND-> "funEnd"
        |INOUTFUNDEC(_,_) -> "inoutfundec"
        |RETURN-> "return"
        |NOT-> "not"
        |EQ-> "EQUIL" 
        |LT-> "LessTahn" 
        |BIND -> "bind" 
        |LET-> "LET"
        |END-> "end"
        |IF->  "if"
        |ENDOFFUNC->"ENDOFFUNC"
        |BINDPARAM(_,_)->"bindparam"
        |MERGE(_,_)->"merge"
        |CALL->"call"
    in 
        let varToString v= match v with
            |VAR(str)->String.concat "" ["VAR ";com_to_string str]
            |INOUTFUNC(_,_,_)->"INOUTFUNC"
            |FUNC(_,_,_)-> "FUNC" 
        in
   
      let rec print_stack st=  match st with
        |hd::stack-> Printf.printf "%s, " (com_to_string hd);  
            print_stack stack
        |_->Printf.printf "empty\n" 
    in
      let rec print_coms st=  match st with
        |hd::stack-> Printf.printf "%s, " (scom_2_str hd);
            print_coms stack
        |_->Printf.printf "empty\n" 
    in
      let print_map m =
          (VarMap.iter (fun a b-> printf "name: %s type: %s \n" a (varToString b) ) ) m; in
      
    let set_label head arg_name = match head with 
                    |INT(_,value) -> INT(Some arg_name,value)
                    |STRING(_,value) -> STRING(Some arg_name,value)
                    |BOOL(_,value)->BOOL(Some arg_name,value) 
                    |NAME(_) -> NAME(arg_name)
                    |FUNCALL(_,f_arg)-> FUNCALL(arg_name, f_arg)
                    |INOUTFUNCALL(_,f_arg)-> FUNCALL(arg_name, f_arg)
                    |UNIT(_,value)-> UNIT(arg_name,value)
                    |ERROR->ERROR
                in
    let dprint st cl =
       (match cl with 
            |(x::xs)-> printf "%s " (scom_2_str x); 
            | _->print_string "end"; 
            );  
        print_stack st;  
    in
    let rec processor cl st  bs  m =
        (*dprint st cl;*) 
        match (cl, st) with
        |(BINDPARAM(arg_name,var_name)::rest_of_command,stack)->
            let temp_arg_value = VarMap.find arg_name m in
            
            let arg_value = match temp_arg_value with 
                |VAR(av) -> VAR(set_label av var_name) 
                | av-> av
            in
            (match (bs) with
            |((top_stack,top_map )::rest_of_bs)->
                let added_map = VarMap.add var_name arg_value top_map in 
                processor rest_of_command stack ((top_stack,added_map)::rest_of_bs) m
            |_->processor rest_of_command (ERROR::st) bs m)
        |(MERGE(outerFunc,innerFunc)::rest_of_command,stack)->
            let outerLookup = VarMap.find outerFunc m in

            printf "outer: %s,inner: %s\n" outerFunc innerFunc; 

            print_map m;

            let innerLookup = VarMap.find innerFunc m in
            (match (outerLookup,innerLookup) with
            |(FUNC(outerList,outerMap,argo),FUNC(innerList,innerMap,argi) )->
                let newInnerFunc = FUNC(innerList,m,argi) in
                let nmp = VarMap.add innerFunc newInnerFunc m  in
                processor rest_of_command stack bs nmp 
            |_-> printf "error\n"
            )

        | (CALL::rest_of_command, value::FUNCALL(str,arg)::rest_of_stack)->
            (match (VarMap.find str m) with  
            |FUNC(functions,map,a) -> 
                let n_vm = VarMap.add a (VAR(value)) map in 

                processor (List.rev functions @ rest_of_command) rest_of_stack ((rest_of_stack,m)::bs) n_vm 
            |_-> processor rest_of_command (ERROR::st) bs m)
        | (PUSH(value)::CALL::rest_of_command,FUNCALL(str,arg)::rest_of_stack)->
            
            let n_value = match value with
            | NAME(name_str) -> (
                match VarMap.find_opt name_str m  with
                | Some v -> 
                        v
                | None -> 
                    VAR(value)
                )
            | _->
                VAR(value) in
            (match (VarMap.find str m) with  
            |FUNC(functions,map,a) -> 
                printf "nfuc: %s\n" str;
                let n_vm = VarMap.add a n_value map in 
                processor (List.rev functions @ rest_of_command) rest_of_stack ((rest_of_stack,m)::bs) n_vm 
            |_-> processor rest_of_command (ERROR::value::st) bs m)
        | (PUSH(NAME(var_name))::CALL::rest_of_command,INOUTFUNCALL(func_name,arg_name)::stack)->
            
            (match (VarMap.find_opt func_name m,VarMap.find_opt var_name m) with  
            |(Some INOUTFUNC(ENDOFFUNC::RETURN::functions,map,a),Some VAR(arg_value) ) -> 
                let nsv= match arg_value with 
                    |INT(_,value) -> INT(Some arg_name,value)
                    |STRING(_,value) -> STRING(Some arg_name,value)
                    |BOOL(_,value)->BOOL(Some arg_name,value) 
                    |_-> arg_value 
                in
                let add_map = VarMap.add arg_name (VAR(nsv)) map in 
                
                let out_value = VarMap.remove var_name add_map in 
                let n_funcs = BINDPARAM(arg_name,var_name)::RETURN::ENDOFFUNC::rest_of_command in
                processor (List.rev functions @ n_funcs) stack ((stack,m)::bs) out_value
            |(Some INOUTFUNC(ENDOFFUNC::functions,map,a),Some VAR(arg_value) ) -> 
                
                let nsv= match arg_value with 
                    |INT(_,value) -> INT(Some arg_name,value)
                    |STRING(_,value) -> STRING(Some arg_name,value)
                    |BOOL(_,value)->BOOL(Some arg_name,value) 
                    |_-> arg_value 
                in
                let add_map = VarMap.add arg_name (VAR(nsv)) map in 
                
                let out_value = VarMap.remove var_name add_map in 
                let n_funcs = BINDPARAM(arg_name,var_name)::ENDOFFUNC::rest_of_command in
                processor (List.rev functions @ n_funcs) stack ((stack,m)::bs) out_value 
            |(_,_)->  
                    processor rest_of_command (ERROR::NAME(var_name)::INOUTFUNCALL(func_name,arg_name)::stack) bs m)
        | (CALL::rest_of_commands,head_value::INOUTFUNCALL(func_name,arg_name)::rest_of_stack)->
            let var_name_opt= match head_value  with 
                    |INT(Some str,value) -> Some str 
                    |STRING(Some str,value) ->Some str
                    |BOOL(Some str,value)-> Some str 
                    |NAME(str)-> Some str 
                    |INOUTFUNCALL(str,_)-> Some str
                    |FUNCALL(str,_)-> Some str 
                    |UNIT(str,_) -> Some str 
                    |_-> None 
            in
            (match (VarMap.find_opt func_name m,var_name_opt) with  
            |(Some INOUTFUNC(ENDOFFUNC::RETURN::functions,map,a), Some var_name ) -> 
                let nsv = set_label head_value var_name in              
                let add_map = VarMap.add arg_name (VAR(nsv)) map in 
                let out_value =  VarMap.remove var_name add_map in 
                let n_funcs = BINDPARAM(arg_name,var_name)
                    ::RETURN
                    ::ENDOFFUNC
                    ::rest_of_commands
                in
                processor (List.rev functions @ n_funcs) rest_of_stack ((rest_of_stack,m)::bs) out_value
            |(Some INOUTFUNC(ENDOFFUNC::functions,map,a),Some var_name ) -> 
                
                let nsv = set_label head_value var_name   
                in
                let add_map = VarMap.add arg_name (VAR(nsv)) map in 
                
                let out_value = VarMap.remove var_name add_map in 
                let n_funcs = BINDPARAM(arg_name,var_name)
                    ::ENDOFFUNC
                    ::rest_of_commands 
                in

                processor (List.rev functions @ n_funcs) rest_of_stack ((rest_of_stack,m)::bs) out_value 
            | (_,_)-> 

                processor rest_of_commands (ERROR::st) bs m
            )

        | (PUSH(value)::CALL::rest_of_command,stack)-> 
            processor rest_of_command (ERROR::value::stack) bs m
        | (CALL::rest_of_command,stack)->
            processor rest_of_command (ERROR::stack) bs m
        | (FUNDEC(name,arg)::FUNEND::rest_of_commands,stack) ->
           (match (VarMap.find name m) with
            |FUNC(ls,mp,arg) -> 
                let nm = VarMap.add name (FUNC((ENDOFFUNC::ls),mp,arg)) m in 
                processor rest_of_commands (UNIT(name,FUNCALL(name,arg))::stack) bs nm
            |_->()
                )
        | (ENDOFFUNC :: rest_of_commands,stack)->
            (match bs with
            |(top,map)::rest_of_bs-> 
                processor rest_of_commands top rest_of_bs map
            |_->
                processor rest_of_commands (ERROR::stack) bs m
            )
        |(INOUTFUNDEC(name,arg)::FUNEND::rest_of_commands,stack) ->
            (match (VarMap.find name m) with
            |INOUTFUNC(ls,mp,arg) -> 
                let value = (INOUTFUNC((ENDOFFUNC::ls),mp,arg)) in
                let nm = VarMap.add name value m in 
                processor rest_of_commands (UNIT(name,INOUTFUNCALL(name,arg))::stack) bs nm
            |_->()
                ) 
        |(FUNEND::rest_of_commands,stack)-> 
           processor rest_of_commands (ERROR::stack) bs m
        |(FUNDEC(name,arg)::rest_of_commands,stack) ->
            let addCommandToFunc func command = (match func with
                |FUNC(ls,mp,arg)-> FUNC(command::ls,mp,arg)
                |_-> func)
            in
            let addSubToFunc func subFunc label subLabel = 
                (match (func,subFunc) with
                    |(FUNC(ls,mp,farg),FUNC(sls,smp,sarg) ) ->
                        let nmp = VarMap.add subLabel (FUNC(sls,smp,sarg)) mp in
                        (if String.equal label "lambda1"  then (
                            printf "par:%s sub:%s \n" label subLabel;
                            print_map nmp
                            )
                         else ()
                        );

                        let nFuncall = FUNCALL(subLabel,sarg) in 
                        let merge = MERGE(label,subLabel) in 
                        let newUnit = UNIT(subLabel,nFuncall) in 
                        FUNC((PUSH(newUnit)::merge::ls),nmp,farg )
                    |(_,_)->func)
            in

            let rec makeFunc func funcLabel commands = match commands with
                |FUNEND::roc ->
                    (addCommandToFunc func ENDOFFUNC ,roc)
                |FUNDEC(subName,subArg)::roc ->
                    let subFunc = makeFunc (FUNC([],m,subArg)) subName roc in
                    (match subFunc with 
                    |(sfunc,ls) -> 
                        let newFunc= (addSubToFunc func sfunc funcLabel subName)
                        in
                        makeFunc newFunc subName ls 
                    )
                |(top::roc)-> 
                    let newFunc= addCommandToFunc func top in
                    makeFunc newFunc funcLabel roc
                |_-> (VAR(ERROR),commands)
            in
            (*funcStuff*)
            (match (makeFunc  (FUNC([],m,arg)) name  rest_of_commands ) with 
               |(FUNC(funcComs,fmap,farg ),ls)-> 
                    let nfmap = VarMap.add  name (FUNC(funcComs,fmap,farg)) fmap in
                    let nm = VarMap.add name (FUNC(funcComs,nfmap,farg)) m in
                    let nstack = (UNIT(name,FUNCALL(name,farg))::stack) in
                    processor ls nstack bs nm 
                |_->printf "error"
            )
        |(INOUTFUNDEC(name,arg)::com::rest_of_commands,stack) ->
            let n_commands = INOUTFUNDEC(name,arg)::rest_of_commands in
            (match (VarMap.find_opt name m) with 
            |Some INOUTFUNC(ls,mp,arg) ->
                let n_vm = VarMap.add name (INOUTFUNC(com::ls,m,arg)) m in
                processor n_commands stack bs n_vm 
            |None -> 
                let n_vm = VarMap.add name (INOUTFUNC(com::[],m,arg))  m in
                processor n_commands stack bs n_vm   
            |_-> 
                processor n_commands (ERROR::stack) bs m
            )   
        |(RETURN::rest_of_commands,stack)->
            (match (stack,bs) with  
                |(UNIT(str,value)::rofstack,(top,tp)::s)->
                    let n_m = VarMap.add str (VAR(value)) tp in
                    let nl = (UNIT(str,value)::top) in
                    processor rest_of_commands nl ((nl,n_m)::s) n_m 
                |(FUNCALL(str,arg)::rofstack,(top,tp)::s)->  
                    let lookup = VarMap.find str m in 
                    printf "yo\n";
                    let n_m = VarMap.add str lookup tp in 
                    print_map n_m;
                    printf "hey\n";
                (match lookup with FUNC(ls,_,_)-> print_coms ls|_->());
                    let nl = (FUNCALL(str,arg)::top) in
                    processor rest_of_commands nl ((nl,n_m)::s) n_m 
                |(return::rofstack,(top,tp)::s)->  
                   let nreturn= match return with
                        |INT(_,value)-> INT(None,value)
                        |BOOL(_,value)-> BOOL(None,value)
                        |STRING(_,value)->STRING(None,value)
                        |_->return
                    in
                   let nl = (nreturn::top) in
                   processor rest_of_commands nl ((nl,tp)::s) tp 
               |_-> 
                   processor rest_of_commands (ERROR::stack) bs m )
 
        |(FUNDEC(name,arg)::rest_of_commands,stack)->
            processor rest_of_commands (ERROR::stack) bs m 
        |(INOUTFUNDEC(name,arg)::rest_of_commands,stack)->
            processor rest_of_commands (ERROR::stack) bs m 
        | (LET::rest_of_commands,stack  )->
            processor rest_of_commands stack ((stack,m)::bs) m
        | (END::rest_of_commands,stack )->
            (match (stack,bs) with  
               |(UNIT(str,value)::rofstack,((top,tp)::s) )->
                    let nmp = VarMap.add str (VAR(value)) tp in
                    processor rest_of_commands (UNIT(str,value)::top) s nmp
               |(return::rofstack,(top,ts)::s)->  
                  (*     printf "%s end\n" (com_to_string return); *)
                   let nl = (return::top) in
                   processor rest_of_commands nl s ts 
               |_-> 
                   processor rest_of_commands (ERROR::stack) bs m )
        | (PUSH(NAME(str))::rest_of_commands,stack)->
            (match (VarMap.find_opt str m) with
            |Some FUNC(_,_,arg)->    
                processor rest_of_commands (FUNCALL(str,arg)::stack) bs m
            |Some INOUTFUNC(_,_,arg)->
                processor rest_of_commands (INOUTFUNCALL(str,arg)::stack) bs m
            |Some VAR(sv)->
                

                let nsv= match sv with 
                    |INT(_,value) -> INT(Some str,value)
                    |STRING(_,value) -> STRING(Some str,value)
                    |BOOL(_,value)->BOOL(Some str,value) 
                    |_-> sv
                in 
                processor rest_of_commands (nsv::stack) bs m
            |None ->  
                processor rest_of_commands (NAME(str)::stack) bs m
            )
        | (PUSH(value)::rest_of_commands,stack ) ->
            processor rest_of_commands (value :: stack) bs  m
        | (ADD::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack)->

            processor rest_of_commands (INT(None,b+a)::rest_of_stack) bs m
        | (ADD::rest_of_commands,stack ) -> 
            print_stack stack ;
            processor rest_of_commands (ERROR :: stack) bs m
        | (SUB::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack)->
            processor rest_of_commands (INT(None,b-a)::rest_of_stack) bs  m  
        | (SUB::rest_of_commands,stack ) -> 
            processor rest_of_commands (ERROR :: stack) bs m
        | (MUL::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack)->
            processor rest_of_commands (INT(None,b*a)::rest_of_stack) bs m   
        | (MUL::rest_of_commands,stack ) -> 
            processor rest_of_commands (ERROR :: stack) bs m
        | (DIV::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack)
            when a!=0->
            processor rest_of_commands (INT(None,b/a)::rest_of_stack) bs m
        | (DIV::rest_of_commands,stack ) -> 
            processor rest_of_commands (ERROR :: stack) bs m        
        | (REM::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack)
            when a!=0->
            processor rest_of_commands (INT(None,b mod a)::rest_of_stack) bs m
        | (REM::rest_of_commands,stack) -> 
            processor rest_of_commands (ERROR :: stack) bs m
         | (POP::rest_of_commands,UNIT(name,_)::rest_of_stack ) ->
            processor rest_of_commands rest_of_stack  bs m 
        | (POP::rest_of_commands,top::rest_of_stack ) ->
            processor rest_of_commands rest_of_stack bs m
        | (POP::rest_of_commands,stack )-> 
            processor rest_of_commands (ERROR::stack) bs m 
        | (SWAP::rest_of_commands,first::second::rest_of_stack ) ->
            processor rest_of_commands (second::first::rest_of_stack) bs m
        | (SWAP::rest_of_commands, stack)->
            processor rest_of_commands (ERROR::stack) bs m 
        | (TOSTRING::rest_of_commands,value::rest_of_stack)->
            processor rest_of_commands ( STRING(None,com_to_string value) :: rest_of_stack) bs m
        | (TOSTRING::rest_of_commands, stack ) ->
            processor rest_of_commands (ERROR::stack) bs  m
        | (PRINTLN::rest_of_commands,(STRING(_,str)::rest_of_stack )) ->
            fprintf oc "%s\n" str;
            processor rest_of_commands rest_of_stack bs m
        |  (PRINTLN::rest_of_commands, stack ) ->
            processor rest_of_commands (ERROR::stack) bs m
        |(NEG::rest_of_commands,((INT(_,a)::rest_of_stack)))-> 
            processor rest_of_commands (INT(None,a * (-1) )::rest_of_stack) bs m 
        | (NEG::rest_of_commands,stack) ->
            processor rest_of_commands (ERROR::stack) bs m
        | (CAT::rest_of_commands,STRING(_,a)::STRING(_,b)::rest_of_stack )->
                let n_a = String.sub a 1 (String.length a -1) in

                let n_b = String.sub b 0 (String.length b -1) in
            processor rest_of_commands (STRING(None,String.concat "" [n_b;n_a]) :: rest_of_stack) bs m
        | (CAT::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::stack) bs m 
        | (AND::rest_of_commands,BOOL(_,a)::BOOL(_,b)::rest_of_stack )->
            processor rest_of_commands (BOOL(None,a && b)::rest_of_stack) bs m 
        | (AND::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::stack) bs m
        | (OR::rest_of_commands,BOOL(_,a)::BOOL(_,b)::rest_of_stack )->
             processor rest_of_commands (BOOL(None,a || b)::rest_of_stack) bs m
        | (OR::rest_of_commands,stack)->
            processor rest_of_commands (ERROR::stack) bs m
        | (NOT::rest_of_commands,(BOOL(_,a)::rest_of_stack))->
             processor rest_of_commands (BOOL(None, not a )::rest_of_stack) bs m 
        | (NOT::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::stack) bs m
        | (EQ::rest_of_commands,BOOL(_,a)::BOOL(_,b)::rest_of_stack )->
             processor rest_of_commands (BOOL(None,a == b)::rest_of_stack) bs m  
        | (EQ::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack )->
             processor rest_of_commands (BOOL(None,a == b)::rest_of_stack) bs m
        | (EQ::rest_of_commands,a::b::stack)->
            
            processor rest_of_commands (ERROR::stack) bs m 
        | (EQ::rest_of_commands,stack)->
            processor rest_of_commands (ERROR::stack) bs m 
        | (LT::rest_of_commands,INT(_,a)::INT(_,b)::rest_of_stack )->
             processor rest_of_commands (BOOL(None,b < a)::rest_of_stack) bs m 
        | (LT::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::stack) bs m
        | (BIND::rest_of_commands,(NAME(name)::NAME(_)::rest_of_stack)) ->
            processor rest_of_commands (ERROR::st) bs m
        | (BIND::rest_of_commands,stack_value::NAME(name)::rest_of_stack) ->
            (match stack_value with
            |INT(_,value)->
                let n_stack_value = (INT(Some name,value)) in
                let nm = VarMap.add name (VAR( n_stack_value )) m in
                processor rest_of_commands (UNIT(name,n_stack_value)::rest_of_stack) bs nm
            |STRING(_,value)->
                let n_stack_value = (STRING(Some name,value)) in
                let nm = VarMap.add name (VAR( n_stack_value )) m in
                processor rest_of_commands (UNIT(name,n_stack_value)::rest_of_stack) bs nm
            |BOOL(_,value)->
                let n_stack_value = (BOOL(Some name,value)) in
                let nm = VarMap.add name (VAR( n_stack_value )) m in
                processor rest_of_commands (UNIT(name,n_stack_value)::rest_of_stack) bs nm
            |FUNCALL(fname,arg)->
                let lookup = VarMap.find fname m in 
                let nm = VarMap.add name lookup m in
                processor rest_of_commands (UNIT(fname,FUNCALL(name,arg)  )::rest_of_stack) bs nm 
            |UNIT(_,value) ->
                let n_stack_value = value in
                let nm = VarMap.add name (VAR( n_stack_value )) m in
                processor rest_of_commands (UNIT(name,value)::rest_of_stack) bs nm
            |_-> processor rest_of_commands (ERROR::st)  bs m


            )
        | (BIND::rest_of_commands,stack_value::bind_val::rest_of_stack) ->
            (match (bind_val) with
            |INT(Some name, value)->   
                let nm = VarMap.add name (VAR(stack_value)) m in
                processor rest_of_commands (UNIT(name,bind_val)::rest_of_stack) bs nm
            |STRING(Some name, value)->   
                let nm = VarMap.add name (VAR(stack_value)) m in
                processor rest_of_commands (UNIT(name,bind_val)::rest_of_stack) bs nm
            |BOOL(Some name, value)->   
                let nm = VarMap.add name (VAR(stack_value)) m in
                processor rest_of_commands (UNIT(name,stack_value)::rest_of_stack) bs nm

            |_->
                processor rest_of_commands (ERROR::st)  bs m
            )
        | (BIND::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::stack)  bs m
        | (IF::rest_of_commands,a::b::BOOL(_,c)::rest_of_stack )->
            if c then processor rest_of_commands (a::rest_of_stack) bs m
            else processor rest_of_commands (b::rest_of_stack) bs m
        | (IF::rest_of_commands,stack )->
            processor rest_of_commands (ERROR::st) bs m
        |  ([] ,stack) -> 
            ()
        | (QUIT::rest_of_commands,stack)-> () 
                  
    in
        processor comList [] [] VarMap.empty 
    ;;
    interpreter ("./file.txt","output1") 
