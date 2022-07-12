#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
// #![no_std]
//!
//  Code for the LIGO runtime.
//
//  This code basically:
//  - loads the storage
//  - calls the generated code by the LIGO wasm backend
//  - stores data into the storage

use wasi::{fd_write, Ciovec, Errno, Size};

mod stdlib;

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
    buf:     s.as_ptr(),
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
  let fd = unsafe {
    wasi::path_open(
      3,
      0,
      "storage",
      wasi::OFLAGS_CREAT,
      wasi::RIGHTS_FD_FILESTAT_GET | wasi::RIGHTS_PATH_OPEN | wasi::RIGHTS_FD_READ,
      0,
      0,
    )
    .unwrap()
  };
  let fs = unsafe { wasi::fd_filestat_get(fd).unwrap() };
  let storage_ptr = unsafe { alloc(alloc::alloc::Layout::array::<u64>(fs.size as usize).unwrap()) };
  let iovec = wasi::Iovec {
    buf:     storage_ptr,
    buf_len: fs.size as usize,
  };
  unsafe { wasi::fd_read(fd, &[iovec]).unwrap() };
  unsafe { wasi::fd_close(fd).unwrap() };
  let s = unsafe {
    alloc::string::String::from_raw_parts(storage_ptr, fs.size as usize, fs.size as usize)
  };
  let storage: DataType =
    serde_json::from_str(&s).expect("Could not understand what's in the storage.");
  return storage;
}

/**
 * Store the data.
 */
#[no_mangle]
pub extern "C" fn store(er: &DataType) {
  // we assume the rest of the LIGO pipeline ensures this is correct
  let mut storage =
    serde_json::to_string(er).expect("Could not convert storage of contract to JSON.");
  let vec = unsafe { storage.as_mut_vec() };
  let ciovec = wasi::Ciovec {
    buf:     vec.as_ptr() as *const _,
    buf_len: vec.len(),
  };
  let fd = unsafe {
    wasi::path_open(
      3,
      0,
      "storage",
      wasi::OFLAGS_TRUNC,
      wasi::RIGHTS_FD_WRITE,
      0,
      0,
    )
    .unwrap()
  };
  unsafe { wasi::fd_write(fd, &[ciovec]).unwrap() };
  unsafe { wasi::fd_close(fd).unwrap() };
}

extern "C" {
  /// The entrypoint of the smart contract. This should be generated by the LIGO
  /// wasm backend.
  pub fn entrypoint(er: &Wrapped<DataType>) -> Wrapped<DataType>;
}

/**  
 * The starting point of the generated wasm file.
 *
 * It loads the data from storage, calls the LIGO wasm backend generated
 * code, and stores the data once done. Currently the operations and
 * parameters are _not_ properly handled.
 *
 */
#[no_mangle]
pub extern "C" fn _start() {
  let ep = load();
  ep.print();
  // let ep = DataType::Bool(false);

  match &ep {
    DataType::Tuple(node) => {
      let node: &Node = node.unwrap();
      if let Option::Some(d) = &node.next {
        // let d = &d.unwrap().value;
        let d = d.unwrap();

        let x = DataType::Tuple(
          Node {
            value: node.value, // TODO: change into actual parameter...
            next:  Option::Some(
              Node {
                value: d.value,
                next:  Option::None,
              }
              .wrap(),
            ),
          }
          .wrap(),
        );
        println!("Input here: {:?}", x);
        let er: Wrapped<DataType> = unsafe { entrypoint(&x.wrap()) };
        er.print();
        let er: &DataType = er.unwrap();

        println!("Yes 4: {:?}", er);
        er.print();
        store(er);
      } else {
        panic!("Not supported datatype in storage. ")
      }
    }
    _ => {
      panic!("Not supported datatype in storage.")
    }
  }
}
