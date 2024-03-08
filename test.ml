let foo = ref 0

let [@inline never] test x = 
        let y = if x / 3 = 0 then x else x + 4 in
         if x > 2 then begin
                incr foo;
                if y < 4 then
                string_of_int (x + 2) 
                else ""
        end
        else "no"
let () = print_endline (test 2)
