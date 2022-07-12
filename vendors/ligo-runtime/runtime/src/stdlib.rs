#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

//!
// This file is specifically used to expose files with the `expose` macro.
// Here functions should be implemented that are needed for LIGO wasm to
// run.

use ligo_runtime_macros::*;
use num_traits::ToPrimitive;

use crate::datatype::*;

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_neg(i: DataType) -> DataType {
  match i {
    DataType::Int(i) => {
      let i = to_bigint(i.unwrap());
      let x = -i;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_add(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i + i2;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_sub(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i - i2;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_mul(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i * i2;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_div(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i / i2;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_mod(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());

      let x = i % i2; // TODO: probably not correct...
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

/*
  | C_NOT
  | C_AND
  | C_OR
  | C_XOR
  | C_LSL
  | C_LSR
*/

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_not(i: DataType) -> DataType {
  match i {
    DataType::Int(i) => {
      let i = to_bigint(i.unwrap());
      let x = !i;
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_and(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i & i2; // TODO: probably not correct...
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_or(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let x = i | i2; // TODO: probably not correct...
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_lsl(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let i2 = i2.to_i64().unwrap();
      let x = i << i2; // TODO: support bigger numbers
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_lsr(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let i2 = i2.to_i64().unwrap();
      let x = i >> i2; // TODO: support bigger numbers
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

#[expose_fn]
#[no_mangle]
pub extern "C" fn c_list_iter(i: DataType, b: DataType) -> DataType {
  match (i, b) {
    (DataType::Int(i), DataType::Int(i2)) => {
      let i = to_bigint(i.unwrap());
      let i2 = to_bigint(i2.unwrap());
      let i2 = i2.to_i64().unwrap();
      let x = i >> i2; // TODO: support bigger numbers
      DataType::Int(to_wrap(&x).wrap())
    }
    _ => {
      panic!("Not supported")
    }
  }
}

// This is a hack, and should ideally completely be removed.
#[produce_file]
fn _todo_find_something_better() {}
