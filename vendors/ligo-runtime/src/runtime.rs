/**
 * The LIGO runtime for WASM.
 */

use std::fs;
// use std::io::prelude::*;
use std::io::Write;
// use std::fs::File;

// Serialization
use serde::{Serialize, Deserialize};

// Persistent Data Structures
use rpds::RedBlackTreeMap;
use rpds::RedBlackTreeSet;
use rpds::List;

// GMP specific code
mod gmp {
  use serde::{Serialize, Deserialize};
  use std::cmp::Ordering;
  use std::cmp::Ordering::Greater;
  use std::cmp::Ordering::Less;
  use std::cmp::Ordering::Equal;

  #[derive(Serialize, Deserialize, Debug)]
  #[repr(C)]
  pub struct Int {
    mp_alloc: u32,
    mp_size:  u32,
    mp_limbs: Box<u32>,
  }

  #[allow(improper_ctypes)]
  extern {
    fn __gmpz_comp (a: *const Int, b: *const Int, result: *const i32);
  }

  impl Ord for Int {
    fn cmp(&self, other: &Self) -> Ordering {
      let gmp_cmp: i32 = 0;
      unsafe {
        __gmpz_comp(self, other, &gmp_cmp);
      }
      if gmp_cmp > 1 {
        Greater
      } else if gmp_cmp == 0 {
        Equal
      } else {
        Less
      }
    }
  }

  impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
    }
  }

  impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
      self.cmp(other) == Equal
    }
  }

  impl Eq for Int {}

}

use gmp::Int;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum DataType {
  // Address(*const CStr), TODO: implement the serde traits for this.
  Int(Box<Int>),
  Nat(Box<Int>),
  Mutez(Box<Int>),
  Timestamp(i32),
  Bool(bool),
  // String(*const CStr),
  // Bytes(*const CStr),
  ListItem(Box<List<DataType>>),
  Tuple(Box<List<DataType>>),
  Set(Box<RedBlackTreeSet<DataType>>),
  Map(Box<RedBlackTreeMap<DataType, DataType>>)
}

#[repr(C)]
pub struct EntrypointTuple {
  parameter: DataType,
  storage: DataType,
}

#[derive(Serialize, Deserialize, Debug)]
#[repr(C)]
pub enum Operations {
  Transaction, 
  Delegate,
}

#[derive(Serialize, Deserialize, Debug)]
#[repr(C)]
pub struct EntrypointResult {
  operations: Operations,
  storage: DataType,
}

#[no_mangle]
pub extern "C" fn load() -> EntrypointTuple {
  let storage_file = fs::read_to_string("storage").expect("Failed to open storage.");
  let parameter_file = fs::read_to_string("parameter").expect("Failed to open parameter.");
  let storage: DataType = serde_json::from_str(&storage_file).expect("Could not understand what's in the storage.");
  let parameter: DataType = serde_json::from_str(&parameter_file).expect("Could not understand what's in the parameter.");
  return EntrypointTuple {
    parameter: parameter,
    storage: storage
  };
}

#[no_mangle]
pub extern "C" fn store(er: EntrypointResult) {
  let storage: String = serde_json::to_string(&er.storage).expect("Could not convert storage of contract to JSON.");
  let mut file = std::fs::OpenOptions::new().write(true).truncate(true).open("storage").expect("Could not create storage file");
  file.write_all(storage.as_bytes()).expect("Could not write storage to file.");
  let operations: String = serde_json::to_string(&er.operations).expect("Could not convert operations of contract to JSON.");
  let mut file = std::fs::OpenOptions::new().write(true).truncate(true).open("operations").expect("Could not create operations file");
  file.write_all(operations.as_bytes()).expect("Could not write operations to file.");
}
