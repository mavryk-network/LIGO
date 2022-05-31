#![warn(missing_docs)]
// #![no_std]

/**
 * The LIGO runtime for WASM.
 */

use wasi::{Ciovec, Errno, fd_write, Size};

use core::mem;

use num_bigint::BigInt;

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
  pub fn entrypoint (er: Wrapped<DataType>) -> Wrapped<DataType>;
}



// this is for debugging:
//
// #[no_mangle]
// pub extern "C" fn entrypoint (er: Wrapped<DataType>) -> Wrapped<DataType> {
//   let operations:DataType = DataType::Operations(Option::None);
//   let b = BigInt::from(9313312u32) * BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32)* BigInt::from(9313312u32);
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

use crate::mem::size_of;

#[no_mangle]
pub extern "C" fn _start () {
  // let ep = load();
  let ep = DataType::Bool(false);
  let wrapped_ep = ep.wrap();
  let er: Wrapped<DataType> = unsafe { entrypoint(wrapped_ep) };
  er.print();
  let er: &DataType = er.unwrap();
  
  store(er);
}
