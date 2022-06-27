#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

//! The data types for the LIGO runtime.
//
//  Important parts:
//  - needs to work with interop
//  - needs to work with Serde for serialization and deserialization
//  - helper classes for the ligo wasm code generation are created with help 
//    of the `expose_datatype` macro.

use core::fmt::Debug;
use core::mem;
use core::mem::size_of;
use core::mem::ManuallyDrop;

use ligo_runtime_macros::*;
use num_bigint::BigInt;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

extern crate alloc;
use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct Wrapped<T> {
  pub data: *mut T,
}

pub trait Sup<'a>: Wrap<'a> {}

impl<'a, U: core::cmp::PartialEq> Sup<'a> for U {}

impl<'de, T: Sup<'de> + Serialize> Serialize for Wrapped<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let b = self.unwrap();
    b.serialize(serializer)
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct String {
  data: *mut u8,
  len:  usize,
  cap:  usize,
}

impl Serialize for String {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let s = self;
    let s = unsafe { alloc::string::String::from_raw_parts(s.data, s.len, s.cap) };
    s.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for String {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    let mut d: alloc::string::String = Deserialize::deserialize(deserializer)?;
    let s = unsafe { d.as_mut_vec() };
    let s = s.as_mut_ptr();
    let l = d.len();
    let c = d.capacity();

    Ok(String {
      data: s,
      len:  l,
      cap:  c,
    })
  }
}

impl Debug for String {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let s = self;
    let s = unsafe { alloc::string::String::from_raw_parts(s.data, s.len, s.cap) };
    write!(f, "{:?}", s)?;
    Ok(())
  }
}

impl<'de, T: Sup<'de> + Deserialize<'de>> Deserialize<'de> for Wrapped<T> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    let l: T = Deserialize::deserialize(deserializer)?;
    Ok(l.wrap())
  }
}

impl<U: core::fmt::Debug> Debug for Wrapped<U> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let b: &U = &self.unwrap();
    write!(f, "Wrapped {{ {:?} }}", b)?;
    Ok(())
  }
}

pub trait Wrap<'a> {
  fn wrap(self) -> Wrapped<Self>
  where
    Self: Sized;
}

impl<'a, U> Wrap<'a> for U {
  fn wrap(self) -> Wrapped<Self> {
    let b = Box::into_raw(Box::new(self));
    Wrapped { data: b }
  }
}

pub trait Unwrap<T> {
  fn unwrap(&self) -> &T
  where
    Self: Sized;
}

impl<T> Unwrap<T> for Wrapped<T> {
  fn unwrap(&self) -> &T {
    let r = unsafe { Box::leak(Box::from_raw(self.data)) };
    r
  }
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub enum Option<T> {
  None,
  Some(T),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct BigIntWrap {
  data:     *mut u8,
  len:      usize,
  capacity: usize,
}

pub fn to_wrap(i: &BigInt) -> BigIntWrap {
  let vec = BigInt::to_signed_bytes_le(i);
  let mut vec = ManuallyDrop::new(vec);
  let data = vec.as_mut_ptr();
  let len = vec.len();
  let capacity = vec.capacity();
  BigIntWrap {
    data:     data,
    len:      len,
    capacity: capacity,
  }
}

pub fn to_bigint(i: &BigIntWrap) -> BigInt {
  let v = unsafe { Vec::from_raw_parts(i.data, i.len, i.capacity) };
  let v = ManuallyDrop::new(v);
  let r = BigInt::from_signed_bytes_le(v.as_slice());
  r
}

impl Serialize for BigIntWrap {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let data = to_bigint(self);
    data.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for BigIntWrap {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    let (sign, data) = Deserialize::deserialize(deserializer)?;
    let big_int = BigInt::from_biguint(sign, data);
    let r = Ok(to_wrap(&big_int));
    r
  }
}

impl Debug for BigIntWrap {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let data: BigInt = to_bigint(self);
    data.fmt(f)
  }
}

macro_rules! mem_layout {
  ($elem:expr, $size:expr) => {
    println!("Address : {:p}, ", $elem);
    println!("Pretty  : {:?}", $elem);
    println!("  Size  : {:?} x 32bits", $size);

    print!("Layout : ");
    let mut i: usize = 0;

    let x: [i32; $size] = unsafe { mem::transmute_copy($elem) };
    while i < $size {
      print!("{:?}, ", x[i]);
      i = i + 1;
    }
    println!("");
    print!("         ");
    i = 0;
    while i < $size {
      print!("{:#02x}, ", x[i]);
      i = i + 1;
    }
    println!("\n----");
  };
}

pub trait MemoryLayout {
  fn print(&self);
}

impl MemoryLayout for BigIntWrap {
  fn print(&self) {
    mem_layout!(self, size_of::<Self>() / 4);
    let start: u32 = self.data as u32;
    let mut n: usize = 0;
    while n < self.len {
      let pos = start + (n as u32);
      let addr = pos as *const i32;
      let x: u8 = unsafe { *addr as u8 };
      print!("{:?}, ", x);
      n = n + 1;
    }
    println!("\n----");
  }
}

impl MemoryLayout for DataType {
  fn print(&self) {
    mem_layout!(self, size_of::<Self>() / 4);
    match self {
      DataType::Int(i) | DataType::Nat(i) | DataType::Mutez(i) => {
        mem_layout!(i, size_of::<Wrapped<BigIntWrap>>() / 4);
        let i = i.unwrap();
        i.print()
      }

      DataType::Timestamp(ix) => {
        println!("Timestamp: {:?}", ix);
      }
      DataType::Bool(b) => {
        println!("Bool: {:?}", b);
      }
      DataType::ListItem(n) | DataType::Tuple(n) => {
        mem_layout!(n, size_of::<Wrapped<Node>>() / 4);
        let n = n.unwrap();
        n.print()
      }
      DataType::Set(_rb) | DataType::Map(_rb) => {
        panic!()
      }
      DataType::Operations(Option::Some(o)) => {
        mem_layout!(o, size_of::<Wrapped<OperationNode>>());
        let o = o.unwrap();
        o.print()
      }
      DataType::Operations(Option::None) => {
        println!("Operations::None");
      }
      DataType::String(_s) => {
        panic!()
      }
    }
  }
}

impl MemoryLayout for Node {
  fn print(&self) {
    mem_layout!(self, size_of::<Self>() / 4);
    let value = &self.value;
    value.print();
    let n = &self.next;
    match n {
      Option::Some(s) => {
        mem_layout!(&s, size_of::<Wrapped<Node>>() / 4);
        let v = s.unwrap();
        v.print()
      }
      Option::None => (),
    }
  }
}

impl MemoryLayout for OperationNode {
  fn print(&self) {
    mem_layout!(self, size_of::<Self>() / 4);
    let value = &self.value;
    mem_layout!(value, size_of::<Wrapped<OperationNode>>() / 4);
    let value = value.unwrap();
    value.print();
    let n = &self.next;
    match n {
      Option::Some(s) => {
        mem_layout!(&s, size_of::<Wrapped<OperationNode>>() / 4);
        let v = s.unwrap();
        v.print()
      }
      Option::None => (),
    }
  }
}

impl MemoryLayout for Operation {
  fn print(&self) {
    panic!()
  }
}

impl MemoryLayout for Wrapped<DataType> {
  fn print(&self) {
    mem_layout!(self, size_of::<Wrapped<DataType>>() / 4);
    let u = self.unwrap();
    u.print()
  }
}

/**
 * These are the datatypes used by code generated from the smart contract.
 */
#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub enum DataType {
  Int(Wrapped<BigIntWrap>),
  Nat(Wrapped<BigIntWrap>),
  Mutez(Wrapped<BigIntWrap>),
  Timestamp(i32),
  Bool(bool),
  ListItem(Wrapped<Node>),
  Tuple(Wrapped<Node>),
  Set(Wrapped<RBNode>),
  Map(Wrapped<RBNode>),
  // Map(Box<RedBlackTreeMap<Box<Self>, Box<Self>>>),
  Operations(Option<Wrapped<OperationNode>>),
  String(Wrapped<String>),
  // Bytes(Box<String>),
  // Address(Box<String>),

  // TODO: add other data types
}

/**
 *
 *
 */
#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct Node {
  pub value: Wrapped<DataType>,
  pub next:  Option<Wrapped<Node>>,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub enum Operation {
  Transaction(Transaction),
  Delegate(Delegate),
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct OperationNode {
  pub value: Wrapped<Operation>,
  pub next:  Option<Wrapped<OperationNode>>,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub enum Color {
  Red,
  Black,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct RBNode {
  pub parent: Option<Wrapped<RBNode>>,
  pub value:  Wrapped<DataType>,
  pub depth:  usize,
  pub left:   Option<Wrapped<RBNode>>,
  pub right:  Option<Wrapped<RBNode>>,
  pub color:  Color,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct Transaction {
  pub action:   DataType,
  pub amount:   BigIntWrap,
  pub contract: BigIntWrap,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
#[expose_datatype]
pub struct Delegate {
  pub delegate: Option<DataType>,
}

// This is a hack, and should ideally completely be removed.
#[produce_datatype_file]
fn _todo_find_something_better() {}
