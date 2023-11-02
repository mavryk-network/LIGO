function main (const p : key_hash) : address is {
  const c : contract (unit) = Mavryk.implicit_account (p);
} with Mavryk.address (c)
