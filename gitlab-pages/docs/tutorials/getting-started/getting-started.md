---
id: getting-started
title: Quickstart
---

This section is aimed at newcomers to LIGO and Mavryk smart contracts.
In this tutorial, we will go through the following steps:

- Writing a simple contract
- Testing the contract
- Deploying the contract to Mavryk

## Before you start

- Install LIGO on your computer as described in [Installation](../../intro/installation).
You can also use the [Online IDE](https://ide.mavryk.org) but these instructions are for working with LIGO locally.

- Select a syntax.
You can use JsLIGO for a JavaScript-like syntax or CameLIGO for an OCaml-like syntax.

  This page provides instructions for both syntaxes.
  To show the instructions for your syntax, select it under "Syntax Preference" at the top left of the page.

- Optional: Install the [IDE plugin](../../intro/editor-support) for your IDE.

## What is a smart contract?

LIGO is a domain-specific language for writing smart contracts on the Mavryk blockchain.
A smart contract is a piece of code stored on a blockchain.
It contains a set of instructions and rules to trigger them.
After it is deployed, it becomes immutable, but a user can trigger the execution of the code without modifying it.

A smart contract is composed of three elements:

- Its balance: a contract is a kind of Mavryk account, and can receive and send mav tokens
- Its storage: data that is dedicated to and can be read and written by the contract
- Its code: one or more entrypoints, which are a kind of function that can be called either from outside the chain or from other contracts

For more information about smart contracts on Mavryk, see [Smart contracts](https://documentation.mavryk.org/smart-contracts) on documentation.mavryk.org.

## Writing the smart contract code

These instructions walk you through creating a contract that acts as a counter.
It stores a single integer and provides entrypoints that allow users to pass an integer to add or subtract from the current value of the counter.

The only way to change the integer in storage is through these entrypoints.
That illustrates how you can store data in a smart contract and enforce specific rules about how that data can change.

Follow these steps to write the code for the contract:

<Syntax syntax="cameligo">

1. On your computer, create a folder to store the contract file, such as `ligo_tutorial`.
1. Create a file with the extension `.mligo`, which indicates CameLIGO code, such as `counter.mligo`.
1. In any code or text editor, add this code to the file:

   ```cameligo
   type storage = int
   type return_type = operation list * storage
   ```

   This code defines two data types:

      - A type that represents the storage for the contract.
      In this case, the contract stores an integer, but contracts can define more complex data types to store more data.
      - A type that represents what the contract entrypoints return.
      LIGO entrypoints always return a list of operations to run after the entrypoint completes and the new state of the storage.

1. Add an entrypoint named `add` that accepts an integer as a parameter and adds it to the storage value:

   ```cameligo
   [@entry] let add (n : int) (storage : storage) : result = [], storage + n
   ```

   This line creates a function and annotates it with `[@entry]`, which indicates that it is an entrypoint that users can call.
   Contracts can also include internal functions that are not annotated.

   The function accepts an integer and the current state of the storage as parameters.
   All entrypoints receive the storage state as their final parameter.
   The function returns an empty list of operations to run next and the new state of the storage, which in this case is the current integer in storage plus the integer that the caller passed.

1. Similarly, add an entrypoint named `sub` that accepts an integer and subtracts it from the storage value:

   ```cameligo
   [@entry] let sub (n : int) (storage : storage) : result = [], storage - n
   ```

The complete contract looks like this:

```cameligo group=a
type storage = int
type return_type = operation list * storage

[@entry] let add (n : int) (storage : storage) : return_type = [], storage + n
[@entry] let sub (n : int) (storage : storage) : return_type = [], storage - n
```

</Syntax>

<Syntax syntax="jsligo">

1. On your computer, create a folder to store the contract file, such as `ligo_tutorial`.
1. Create a file with the extension `.jsligo`, which indicates JsLIGO code, such as `counter.jsligo`.
1. In any code or text editor, add this code to the file:

   ```jsligo
   type storage = int;
   type return_type = [list<operation>, storage];
   ```

   This code defines two data types:

      - A type that represents the storage for the contract.
      In this case, the contract stores an integer, but contracts can define more complex data types to store more data.
      - A type that represents what the contract entrypoints return.
      LIGO entrypoints always return a list of operations to run after the entrypoint completes and the new state of the storage.

1. Add an entrypoint named `add` that accepts an integer as a parameter and adds it to the storage value:

   ```jsligo
   @entry
   const add = (n : int, storage : storage) : return_type => [[], storage + n];
   ```

   These lines create a function and annotate it with `@entry`, which indicates that it is an entrypoint that users can call.
   Contracts can also include internal functions that are not annotated.

   The function accepts an integer and the current state of the storage as parameters.
   All entrypoints receive the storage state as their final parameter.
   The function returns an empty list of operations to run next and the new state of the storage, which in this case is the current integer in storage plus the integer that the caller passed.

1. Similarly, add an entrypoint named `sub` that accepts an integer and subtracts it from the storage value:

   ```jsligo
   @entry
   const sub = (n : int, storage : storage) : return_type => [[], storage - n];
   ```

The complete contract looks like this:

```jsligo group=a
type storage = int;
type return_type = [list<operation>, storage];

@entry
const add = (n : int, storage : storage) : return_type => [[], storage + n];

@entry
const sub = (n : int, storage : storage) : return_type => [[], storage - n];
```

</Syntax>

<Syntax syntax="pascaligo">

1. On your computer, create a folder to store the contract file, such as `ligo_tutorial`.
1. Create a file with the extension `.ligo`, which indicates PascaLIGO code, such as `counter.ligo`.
1. In any code or text editor, add this code to the file:

   ```pascaligo
   type storage is int
   type return_type is list (operation) * storage
   ```

   This code defines two data types:

      - A type that represents the storage for the contract.
      In this case, the contract stores an integer, but contracts can define more complex data types to store more data.
      - A type that represents what the contract entrypoints return.
      LIGO entrypoints always return a list of operations to run after the entrypoint completes and the new state of the storage.

1. Add an entrypoint named `add` that accepts an integer as a parameter and adds it to the storage value:

   ```pascaligo
   [@entry] function add (const n : int; const storage : storage) : return_type is
     ((nil : list (operation)), storage + n)
   ```

   This code creates a function and annotates it with `[@entry]`, which indicates that it is an entrypoint that users can call.
   Contracts can also include internal functions that are not annotated.

   The function accepts an integer and the current state of the storage as parameters.
   All entrypoints receive the storage state as their final parameter.
   The function returns an empty list of operations to run next and the new state of the storage, which in this case is the current integer in storage plus the integer that the caller passed.

1. Similarly, add an entrypoint named `sub` that accepts an integer and subtracts it from the storage value:

   ```pascaligo
   [@entry] function sub (const n : int; const storage : storage) : return_type is
     ((nil : list (operation)), storage - n)
   ```

The complete contract looks like this:

```pascaligo group=a
type storage is int
type return_type is list (operation) * storage

[@entry] function add (const n : int; const storage : storage) : return_type is
  ((nil : list (operation)), storage + n)
[@entry] function sub (const n : int; const storage : storage) : return_type is
  ((nil : list (operation)), storage - n)
```

</Syntax>

That's all that is necessary for a very simple LIGO smart contract.

## Trying out the contract

To make sure the contract works as intended, you can use the `run dry-run` command to simulate a call to one of its entrypoints.
For example, to simulate a call to the `add` entrypoint, pass the file name, the entrypoint and parameter, and the current state of the storage, as in this example:

<Syntax syntax="cameligo">

```bash
ligo run dry-run counter.mligo 'Add(3)' '5'
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo run dry-run counter.jsligo 'Add(3)' '5'
```

</Syntax>

<Syntax syntax="pascaligo">

```bash
ligo run dry-run counter.ligo 'Add(3)' '5'
```

</Syntax>

This command sets the initial storage value to 5 and passes 3 to the `add` entrypoint.
Note that the function name in the code starts with a lower-case letter but the `run dry-run` command uses entrypoint names that start with an upper-case letter.
The result is the return value of the entrypoint, which includes an empty list of operations to run next and the new value of the storage:

```
( LIST_EMPTY() , 8 )
```

This way you can test entrypoints manually with different storage states.

## Testing the contract

Testing contracts is critical because their code cannot be changed after deployment.
For a more powerful way to test contracts than the `run dry-run` command, you can include automated tests in the contract code.

Follow these steps to add an automated test to the contract:

<Syntax syntax="cameligo">

1. In your contract file, wrap the existing code in a module:

   ```cameligo
   module Counter = struct
     type storage = int
     type return_type = operation list * storage

     [@entry] let add (n : int) (storage : storage) : return_type = [], storage + n
     [@entry] let sub (n : int) (storage : storage) : return_type = [], storage - n
   end
   ```

   Modules in CameLIGO provide a scope to the identifiers (names of types, functions, variables, etc.) to prevent collisions between them.
   You can access identifiers that are within a module with dot notation, as in `<module>.<identifier>`.

1. At the end of the file, outside of the module, add a function named `test_add`:

   ```cameligo
   let test_add =
   ```

1. Add code to _originate_ (deploy) the contract to the test environment:

   ```cameligo
   let initial_storage = 10 in
   let orig = Test.Next.Originate.contract (contract_of Counter) initial_storage 0mav in
   ```

   This command simulates deploying the contract, setting its initial storage to 10, and setting its initial balance to 0 mav.

1. Add code to call the `add` entrypoint and pass the value 32:

   ```cameligo
   let _ = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "add" orig.taddr) 32 0mav in
   ```

1. Add code to verify that the new value of the storage is correct:

   ```cameligo
   Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage + 32)
   ```

   The complete code looks like this:

   ```cameligo group=b
   module Counter = struct
     type storage = int
     type return_type = operation list * storage

     [@entry] let add (n : int) (storage : storage) : return_type = [], storage + n
     [@entry] let sub (n : int) (storage : storage) : return_type = [], storage - n
   end

   let test_add =
     let initial_storage = 10 in
     let orig = Test.Next.Originate.contract (contract_of Counter) initial_storage 0mav in
     let _ = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "add" orig.taddr) 32 0mav in
     Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage + 32)
   ```

1. Run this command to run the test:

   ```bash
   ligo run test counter.mligo
   ```

</Syntax>

<Syntax syntax="jsligo">

1. In your contract file, wrap the existing code in a namespace:

   ```jsligo
   namespace Counter {
     type storage = int;
     type return_type = [list<operation>, storage];

     @entry
     const add = (n : int, storage : storage) : return_type => [[], storage + n];

     @entry
     const sub = (n : int, storage : storage) : return_type => [[], storage - n];
   };
   ```

   Namespaces in JsLIGO provide a scope to the identifiers (names of types, functions, variables, etc.) to prevent collisions between them.
   You can access identifiers that are within a namespace with dot notation, as in `<module>.<identifier>`.

1. At the end of the file, outside of the namespace, add a function named `test_add`:

   ```jsligo
   const test_add = (() => {

   }) ()
   ```

1. Inside the function, add code to _originate_ (deploy) the contract to the test environment:

   ```jsligo
   const initial_storage = 10 as int;
   const orig = Test.Next.Originate.contract(contract_of(Counter), initial_storage, 0mav);
   ```

   This command simulates deploying the contract, setting its initial storage to 10, and setting its initial balance to 0 mav.

1. Add code to call the `add` entrypoint and pass the value 32:

   ```jsligo
   Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("add", orig.taddr), 32 as int, 0mav);
   ```

1. Add code to verify that the new value of the storage is correct:

   ```jsligo
   return Assert.assert(Test.Next.Typed_address.get_storage(orig.taddr) == initial_storage + 32);
   ```

   The complete code looks like this:

   ```jsligo group=b
   namespace Counter {
     type storage = int;
     type return_type = [list<operation>, storage];

     @entry
     const add = (n : int, storage : storage) : return_type => [[], storage + n];

     @entry
     const sub = (n : int, storage : storage) : return_type => [[], storage - n];
   };

   const test_add = (() => {
     const initial_storage = 10 as int;
     const orig = Test.Next.Originate.contract(contract_of(Counter), initial_storage, 0mav);
     Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("add", orig.taddr), 32 as int, 0mav);
     return Assert.assert(Test.Next.Typed_address.get_storage(orig.taddr) == initial_storage + 32);
   }) ()
   ```

1. Run this command to run the test:

   ```bash
   ligo run test counter.jsligo
   ```

</Syntax>

<Syntax syntax="pascaligo">

1. In your contract file, wrap the existing code in a module:

   ```pascaligo
   module Counter is {
     type storage is int
     type return_type is list (operation) * storage

     [@entry] function add (const n : int; const storage : storage) : return_type is
       ((nil : list (operation)), storage + n)
     [@entry] function sub (const n : int; const storage : storage) : return_type is
       ((nil : list (operation)), storage - n)
   }
   ```

   Modules in PascaLIGO provide a scope to the identifiers (names of types, functions, variables, etc.) to prevent collisions between them.
   You can access identifiers that are within a module with dot notation, as in `<module>.<identifier>`.

1. At the end of the file, outside of the module, add a constant named `test_add`:

   ```pascaligo
   const test_add = block {
   ```

1. Add code to _originate_ (deploy) the contract to the test environment:

   ```pascaligo
   const initial_storage : int = 10;
   const orig = Test.Next.Originate.contract (contract_of Counter, initial_storage, 0mav);
   ```

   This command simulates deploying the contract, setting its initial storage to 10, and setting its initial balance to 0 mav.

1. Add code to call the `add` entrypoint and pass the value 32:

   ```pascaligo
   const _r = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint ("add", orig.taddr), 32, 0mav);
   ```

1. Add code to verify that the new value of the storage is correct:

   ```pascaligo
   } with Assert.assert (Test.Next.Typed_address.get_storage (orig.taddr) = initial_storage + 32)
   ```

   The complete code looks like this:

   ```pascaligo group=b
   module Counter is {
     type storage is int
     type return_type is list (operation) * storage

     [@entry] function add (const n : int; const storage : storage) : return_type is
       ((nil : list (operation)), storage + n)
     [@entry] function sub (const n : int; const storage : storage) : return_type is
       ((nil : list (operation)), storage - n)
   }

   const test_add = block {
     const initial_storage : int = 10;
     const orig = Test.Next.Originate.contract (contract_of Counter, initial_storage, 0mav);
     const _r = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint ("add", orig.taddr), 32, 0mav);
   } with Assert.assert (Test.Next.Typed_address.get_storage (orig.taddr) = initial_storage + 32)
   ```

1. Run this command to run the test:

   ```bash
   ligo run test counter.ligo
   ```

</Syntax>

The output shows that the test ran successfully:

```
Everything at the top-level was executed.
- test_add exited with value ().
```

## Compiling the contract

Mavryk runs contracts in the Michelson stack-based language, so you must compile your contract from LIGO to Michelson.
The process also looks for errors in the LIGO code.

Run this command to compile the contract:

<Syntax syntax="cameligo">

```bash
ligo compile contract counter.mligo -m Counter -o counter.mv
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo compile contract counter.jsligo -m Counter -o counter.mv
```

</Syntax>

<Syntax syntax="pascaligo">

```bash
ligo compile contract counter.ligo -m Counter -o counter.mv
```

</Syntax>

The command writes the output of the compilation to the file `counter.mv`.
The compiled Michelson contract looks like this:

```michelson
{ parameter (or (int %sub) (int %add)) ;
  storage int ;
  code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } }
```

If you see any errors, make sure your code looks like the code in the previous section.

## Setting up the Mavkit client and a local wallet

The Mavkit client is a command-line tool that lets you send transactions to Mavryk, including deploying and calling smart contracts.
These instructions show how to install the client, connect it to the Basenet test network, and get some mav tokens to pay transaction fees.

The Basenet test network is just like the Mavryk mainnet, so you can use it to try out your contracts in a live environment before you deploy them to mainnet.

1. Install the Mavkit client, which sends transactions to Mavryk.
(The Mavkit suite includes many programs, but for now all you need is the Mavkit client.)

   - For Ubuntu, Windows WSL, and Linux distributions that use `apt`, run these commands:

   ```bash
   REPO="ppa:mavrykdynamics/mavryk"
   sudo add-apt-repository -y $REPO && sudo apt-get update
   sudo apt-get install -y mavryk-client
   ```

   - For Fedora and Linux distributions that use Copr, run these commands:

   ```bash
   REPO="@MavrykDynamics/Mavryk"
   dnf copr enable -y $REPO && dnf update -y
   dnf install -y mavryk-client
   ```

   - For MacOS, use `brew`:

   ```bash
   brew tap mavryk-network/mavryk-packaging-stable
   brew install mavryk-client
   ```

   - For other methods, see https://protocol.mavryk.org/introduction/howtoget.html.

1. Verify that you have at least version 20 of the Mavkit client by running `mavkit-client --version` and verifying that the version is at least 20.0.

1. Set the Mavkit client to use the Basenet test network:

   - If you just installed Mavkit for the first time, run this command:

      ```bash
      mavkit-client -E https://basenet.rpc.mavryk.network config init
      ```

   - If you already had Mavkit installed, run this command:

      ```bash
      mavkit-client -E https://basenet.rpc.mavryk.network config update
      ```

1. Verify that you are using Basenet by running `mavkit-client config show` and verifying that the `endpoint` field shows `https://basenet.rpc.mavryk.network`, as in this example:

   ```
   { "base_dir": "/Users/me/.mavryk-client",
     "endpoint": "https://basenet.rpc.mavryk.network", "web_port": 8080,
     "confirmations": 0 }
   ```

1. Optional: Disable the warning message about using a test network by running this command:

   ```bash
   export MAVRYK_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y
   ```

1. In the Mavkit client, create a wallet by running this command:

   ```bash
   mavkit-client gen keys local_wallet
   ```

1. Get your account's address by running this command:

   ```bash
   mavkit-client show address local_wallet
   ```

   The Mavkit client prints the address of the new wallet in the `hash` field.
   The wallet address begins with `mv1`, as in this example:

   ```bash
   Hash: mv1dW9Mk...........H67L
   Public Key: edp.............................bjbeDj
   ```

   You need the wallet address to send funds to the wallet, to deploy the contract, and to send transactions to the contract.

1. Copy your account's address, which starts with `mv1`.

1. In a web browser, go to the Basenet faucet at https://basenet.faucet.mavryk.network/.

1. Paste your address into the "Fund any address" field and send some mav to your account.
20 mav is enough to start with, and you can always return to the faucet for more.

1. Verify that your account has mav by running this command:

   ```bash
   mavkit-client get balance for local_wallet
   ```

Now you have an account and funds that you can use to work with Mavryk.

## Deploying the contract

To deploy (or originate) the contract you need:

- The compiled Michelson code of the contract
- The starting value for the contract storage, compiled as a Michelson expression
- A small amount of mav tokens to pay the transaction fee

1. Get the compiled code of the contract by running this command, where `[FILENAME]` is the name of your LIGO file.

   ```bash
   ligo compile contract [FILENAME] -m Counter -o counter.mv
   ```

1. Get the compiled value of the contract storage by running this command:

   ```bash
   ligo compile storage -m Counter [FILENAME] '0'
   ```

   You can put any integer value for the initial storage value as the last parameter in this command.

   The result is the compiled value of the integer in Michelson, which is the same as it is in LIGO.
   In this case the LIGO storage value maps 1:1 to its Michelson representation.
   More complex data types like records and maps look different in Michelson than in LIGO.

1. Deploy the contract by running this command, putting the initial storage value in the `--init` argument:

   ```bash
   mavkit-client originate contract counter \
     transferring 0 from local_wallet \
     running counter.mv \
     --init 5 --burn-cap 0.5
   ```

   This command deploys the contract to the currently selected Mavryk network.
   It includes these parts:

      - It uses the Mavkit client `originate contract` command to originate the contract and assigns the local name `counter` to the contract
      - It includes 0 tokens from your wallet with the transaction, but the `--burn-cap` argument allows the transaction to take up to 0.1 MVRK from your wallet for fees.
      - It sets the initial value of the contract storage with the `--init` argument.

   If the contract deploys successfully, Mavkit shows the address of the new contract, as in this example:

   ```bash
   New contract KT1Nnk.................UFsJrq originated.
   The operation has only been included 0 blocks ago.
   We recommend to wait more.
   ```

1. Copy the contract address, which starts with `KT1`.

1. Optional: Run the command `mavkit-client get balance for local_wallet` to get the updated balance of your wallet.

1. Verify that the contract deployed successfully by finding it on a block explorer:

   1. Open a Mavryk block explorer such as [Nexus](https://nexus.mavryk.org).

   1. Set the explorer to Basenet instead of Mainnet.

   1. Paste the contract address into the search field and press Enter.

   1. Go to the Storage tab to see that the initial value of the storage is the integer that you provided.

Now the contract is deployed and unchangeable.
Anyone can call its entrypoints to change the storage value.

## Calling the contract

You can call the contract from many different Mavryk clients, including web applications, block explorers, and the Mavkit client.
To call the contract from the Mavkit client, pass the entrypoint name and parameter to the `mavkit-client transfer` command, as in this example:

```bash
mavkit-client --wait none transfer 0 from local_wallet \
  to counter --entrypoint 'add' --arg '5' --burn-cap 0.1
```

Like the `originate contract` command, this command takes the address or local name of the contract, an amount of mav to include, and a maximum fee.
It also includes the name of the entrypoint and the Michelson-encoded parameter to send to it.

Entrypoints don't return values, so the output shows only that the transaction was added successfully.
To verify that the transaction ran, you can check the value of the contract storage, either by looking it up in a block explorer or using the Mavkit client, as in this example:

```bash
mavkit-client get contract storage for counter
```

## Next steps

Now you have a simple LIGO smart contract and can test it, deploy it, and call it.
You can use it as a starting point to write your own contracts and experiment with LIGO.

You can also continue with the [Taco shop tutorial](../taco-shop/mavryk-taco-shop-smart-contract) to learn more about programming with LIGO.
