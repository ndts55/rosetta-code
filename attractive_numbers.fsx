// attractive_numbers.fsx

// taken from Prime Decomposition
let decompose_prime n =
    let rec loop c p =
        if c < (p * p) then [c]
        elif c % p = 0 then p :: (loop (c / p) p)
        else loop c (p + 1)
    loop n 2

// taken from Primality by trial division
let rec primes = Seq.cache(Seq.append (seq[ 2; 3; 5 ]) (Seq.unfold (fun state -> Some(state, state + 2)) 7 |> Seq.filter (fun x -> is_prime x)))
and is_prime number =
    let rec is_prime_core number current limit =
        let cprime = primes |> Seq.nth current
        if cprime >= limit then
            true
        else if number % cprime = 0 then
            false
        else
            is_prime_core number (current + 1) (number/cprime)
 
    if number = 2 then
        true
    else if number < 2 then
        false
    else
        is_prime_core number 0 number

// for each number decide
// map to list of prime divisors
// map to length
// map to is_prime

let is_attractive = decompose_prime >> List.length >> is_prime
[1..120] |> List.filter is_attractive |> List.iter (printf "%d ")