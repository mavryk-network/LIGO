// #![no_std]

#![warn(missing_docs)]

/**
 * The LIGO runtime for WASM.
 */
use quote::quote;

use wasi::{Ciovec, Errno, fd_write, Size};

use num_bigint::BigInt;

use ligo_runtime_macros::*;

mod stdlib;
use stdlib::*;

mod datatype;
use datatype::*;

extern crate alloc;
use alloc::alloc::alloc;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn print(s: &str) -> Result<Size, Errno> {
  assert!(
      s.len() <= u32::MAX as usize,
      "please don't store >4GB of text in a string, thanks"
  );
  let ciovec = Ciovec {
      buf: s.as_ptr(),
      buf_len: s.len() as usize,
  };
  let ciovec_ptr = &[ciovec];
  unsafe { fd_write(1, ciovec_ptr) }
}

/**
 * Load data from storage.
 */
#[no_mangle]
pub extern "C" fn load() -> DataType {
  let fd = unsafe { wasi::path_open(3, 0, "storage", wasi::OFLAGS_CREAT, wasi::RIGHTS_FD_FILESTAT_GET | wasi::RIGHTS_PATH_OPEN | wasi::RIGHTS_FD_READ, 0, 0).unwrap() };
  let fs = unsafe { wasi::fd_filestat_get(fd).unwrap() };
  let storage_ptr = unsafe { alloc(alloc::alloc::Layout::array::<u64>(fs.size as usize).unwrap()) };
  let iovec = wasi::Iovec {
      buf: storage_ptr,
      buf_len: fs.size as usize,
  };
  unsafe { wasi::fd_read(fd, &[iovec]).unwrap() };
  unsafe { wasi::fd_close(fd).unwrap() };
  let s = unsafe { alloc::string::String::from_raw_parts(storage_ptr, fs.size as usize, fs.size as usize) };
  let storage: DataType = serde_json::from_str(&s).expect("Could not understand what's in the storage.");
  return storage;
}

/**
 * 
 */
#[no_mangle]
pub extern "C" fn store(er: &DataType) {
  // we assume the rest of the LIGO pipeline ensures this is correct
  let mut storage = serde_json::to_string(er).expect("Could not convert storage of contract to JSON.");
  let vec = unsafe { storage.as_mut_vec() };
  let ciovec = wasi::Ciovec {
      buf: vec.as_ptr() as *const _,
      buf_len: vec.len(),
  };  
  let fd = unsafe { wasi::path_open(3, 0, "storage", 0, wasi::RIGHTS_FD_WRITE, 0, 0).unwrap() };
  unsafe { wasi::fd_write(fd,  &[ciovec]).unwrap() };
  unsafe { wasi::fd_close(fd).unwrap() };
}


extern "C" {
  pub fn entrypoint (er: &Wrapped<DataType>) -> Wrapped<DataType>;
}



// this is for debugging:
//
// #[no_mangle]
// pub extern "C" fn entrypoint (er: Wrapped<DataType>) -> Wrapped<DataType> {
  

//   let operations:DataType = DataType::Operations(Option::None);
//   let b = BigInt::from(9313312u32) * BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32);
//   int_neg(&to_wrap(&b));
//   let storage:DataType = DataType::Int(to_wrap(&b).wrap());
//   let result = DataType::Tuple(
//     Node {
//       value: operations.wrap(),
//       next: Option::Some(Node {
//           value: storage.wrap(),
//           next: Option::None 
//       }.wrap())
//     }.wrap()
//   ).wrap();
//   result
// }

#[no_mangle]
pub extern "C" fn _start () {
  let ep = load();
  ep.print();
  // let ep = DataType::Bool(false);
  
  match &ep {
    DataType::Tuple (node) => {
      let node:&Node = node.unwrap();
      if let Option::Some(d) = &node.next {
        // let d = &d.unwrap().value;
        let d = d.unwrap();
        // let d = &d.value;
        println!("Yes: {:?}", d);
        /*
        let result = DataType::Tuple(
//     Node {
//       value: operations.wrap(),
//       next: Option::Some(Node {
//           value: storage.wrap(),
//           next: Option::None 
//       }.wrap())
//     }.wrap()
//   ).wrap();
//   result
        */

        let x = DataType::Tuple (
          Node {
            value: d.value,
            next: Option::Some ( Node {
              value: d.value,
              next: Option::None
            }.wrap())
          }.wrap()
        );
        println!("Yes 2: {:?}", x);
        let er: Wrapped<DataType> = unsafe { entrypoint(&x.wrap()) };
        er.print();
        let er: &DataType = er.unwrap();
        store(er);
      } else {
        println!("Nooooooooo")
      }
    },
    _ => {
      panic!("Not supported datatype in storage.")
    }
  }
  // let wrapped_ep = ep.wrap();

  
}