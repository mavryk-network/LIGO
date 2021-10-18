let rec unvalid (n:int):int =
    let res = unvalid (n) + unvalid (n) in
    res + 1
