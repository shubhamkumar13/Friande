/// Take the first n elements, preserving the nesting structure
let rec takeNested n list =
    let rec loop accg accr n list =
        let addg x =
            match x with
            | [] -> accr
            | _ -> Array.rev x :: accr

        match list with
        | _ when n <= 0 -> List.rev (addg accg)
        | [] :: xss -> loop accg accr n xss
        | [ x ] :: xss -> loop [] (addg (x :: accg)) (n - 1) xss
        | (x :: xs) :: xss -> loop (x :: accg) accr (n - 1) (xs :: xss)
        | [] -> failwith "takeNested: insufficient number of elements"

    loop [] [] n list

let x = 0

let _ = takeNested 10 [| [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |]
