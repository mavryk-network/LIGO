function sub (const store : mav; const delta : mav) : option (mav) is 
  store - delta

function main (const _ : unit; const store : mav) : list (operation) * mav is
  ((nil : list (operation)), Option.unopt (sub (store, 1mav)))