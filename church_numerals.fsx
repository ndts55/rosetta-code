type INumeral =
    abstract Apply : ('a -> 'a) -> 'a -> 'a

let zero = {new INumeral with override __.Apply _ x = x}
let successor (n: INumeral) = {new INumeral with override __.Apply f x = f (n.Apply f x)}
let addition (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = m.Apply f (n.Apply f x)}
let multiplication (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = m.Apply (n.Apply f) x}
let exponential (m: INumeral) (n: INumeral) = {new INumeral with override __.Apply f x = n.Apply m.Apply f x}

let ntoi (n: INumeral) = n.Apply ((+) 1) 0
let iton i = List.fold (>>) id (List.replicate i successor) zero

let c3 = iton 3
let c4 = successor c3

[addition c3 c4;
multiplication c3 c4;
exponential c4 c3;
exponential c3 c4]
|> List.map ntoi
|> printfn "%A"