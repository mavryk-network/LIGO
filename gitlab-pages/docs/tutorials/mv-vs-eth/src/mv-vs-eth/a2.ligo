[@entry]
function main (const parameter : bytes; const storage : int) : list (operation) * int is
  if parameter = 0xbc1ecb8e
  then ((nil : list (operation)), storage + 1)
  else
    if parameter = 0x36e44653
    then ((nil : list (operation)), storage - 1)
    else (failwith ("Unknown entrypoint") : list (operation) * int)