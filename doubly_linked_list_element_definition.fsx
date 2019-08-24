type 'a DLElm = {
    mutable prev: 'a DLElm option
    data: 'a
    mutable next: 'a DLElm option
}

let e = {
    prev = None
    data = 4
    next = Some {
        prev = Some {
            prev = None
            data = 10
            next = None
        }
        data = 5
        next = None
    }
}

printfn "%A" e