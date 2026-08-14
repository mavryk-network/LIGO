type transfer is
  [@layout comb]
  record [
    [@annot from] address_from : address;
    [@annot to] address_to : address;
    value : nat
  ]