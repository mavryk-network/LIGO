#![warn(missing_docs)]
/**
 * The LIGO runtime for WASM.
 */
 
use std::fs;
use std::io::Write;
use std::mem;

use num_bigint::BigInt;

mod datatype;
use datatype::*;


/**
 * Load data from storage.
 * 
 * It's 
 */
#[no_mangle]
pub extern "C" fn load() -> DataType {
  // we assume the stored data is correct
  let storage_file = fs::read_to_string("storage").expect("Failed to open storage.");
  let storage: DataType = serde_json::from_str(&storage_file).expect("Could not understand what's in the storage.");
  return storage;
}

#[no_mangle]
pub extern "C" fn store(er: &DataType) {
  // we assume the rest of the LIGO pipeline ensures this is correct
  let storage: String = serde_json::to_string(er).expect("Could not convert storage of contract to JSON.");
  let mut file = std::fs::OpenOptions::new().write(true).truncate(true).open("storage").expect("Could not create storage file");
  file.write_all(storage.as_bytes()).expect("Could not write storage to file.");
}

// extern "C" {
//   // pub fn entrypoint2 (er: BigInt);
//   pub fn entrypoint (er: DataType) -> DataType;
// }

// this is for debugging:
//
#[no_mangle]
pub extern "C" fn entrypoint (er: Wrapped<DataType>) -> Wrapped<DataType> {
  let operations:DataType = DataType::Operations(Option::None);

  let b = BigInt::from(9313312u32) * BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32);
  let storage:DataType = DataType::Int(to_wrap(&b));
  
  // let result = operations.wrap();
  // println!("xxx1: {:?}", &result);
  let result = DataType::Tuple(
    Node {
      value: operations,
      next: Option::Some(Node {
          value: storage,
          next: Option::None 
      }.wrap())
    }.wrap()
  ).wrap();
  result
}

macro_rules! mem_layout {
  ($elem:expr, $size:expr) => (
      println!("Address : {:p}, ", $elem);
      println!("Pretty  : {:?}", $elem);
      println!("  Size  : {:?} x 32bits", $size );
      
      print!  ("Layout : ");
      let mut i: usize = 0;
      
      let x:[i32; $size] = unsafe { mem::transmute_copy($elem) } ;
      while i < $size {
        print!("{:?}, ", x[i]);        
        i = i + 1;
      }
      println!("");
      print!  ("         ");
      i = 0;
      while i < $size {
        print!("{:#02x}, ", x[i]);        
        i = i + 1;
      }

      println!("\n----");
  );
}

fn print_datatype(a: &DataType) {
  mem_layout!(a,2);
  match a {
    DataType::Operations(ax) => { 
      println!("Operations:");

    }
    _ => println!("Not implemented yet")
  }
}

#[no_mangle]
pub extern "C" fn _start () {
  let ep = load();
  println!("Loaded: {:?}.", ep);
  let wrapped_ep = ep.wrap();
  let er: Wrapped<DataType> = unsafe { entrypoint(wrapped_ep) };
  
  let er = &er.unwrap();

  match &er {
    DataType::Tuple(a) => {
      println!("Yes, a tuple: {:?}", a);

      
    }
    _ => ()
  };
 
  store(er);
}
