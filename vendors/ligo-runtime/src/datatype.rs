use std::mem::ManuallyDrop;

use serde::{Serialize, Deserialize, Serializer, Deserializer};

use core::fmt::Debug;

// Big numbers
use num_bigint::{ BigInt };

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct Wrapped<T> {
  pub data: *mut T
}

pub trait Sup<'a>: Wrap<'a> {}

impl<'a, U: std::cmp::PartialEq> Sup<'a> for U {}

impl<'de, T:Sup<'de> + Serialize> Serialize for Wrapped<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
      let b = self.unwrap();
      b.serialize(serializer)
    }
}


impl<'de, T: Sup<'de> + Deserialize<'de>> Deserialize<'de> for Wrapped<T> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
      D: Deserializer<'de>,
  {
    let l:T = Deserialize::deserialize(deserializer)?;
    Ok(l.wrap()) 
  }
}

impl<U:std::fmt::Debug> Debug for Wrapped<U> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let b:&U = &self.unwrap(); 
    write!(f, "Wrapped {{ {:?} }}", b)?;
    Ok(())
  }
}


pub trait Wrap<'a> {
  fn wrap(self) -> Wrapped<Self> where Self:Sized;
}

impl<'a, U> Wrap<'a> for U {
  fn wrap(self) -> Wrapped<Self> {
    let b = Box::into_raw(Box::new(self));
    Wrapped {
      data: b
    }
  }
}

pub trait Unwrap<T> {
  fn unwrap(&self) -> &T where Self:Sized;
}

impl<T> Unwrap<T> for Wrapped<T> {
  fn unwrap(&self) -> &T {
    unsafe { Box::leak(Box::from_raw(self.data)) }
  }
}


#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum Option<T> {
  None,
  Some(T),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct BigIntWrap {
  datax:     *mut u8,
  len:      usize,
  capacity: usize
}

pub fn to_wrap (i: &BigInt) -> BigIntWrap {
  let mut vec = BigInt::to_signed_bytes_le(i);
  let data = vec.as_mut_ptr();
  let len = vec.len();
  let capacity = vec.capacity();
  // std::mem::forget(vec);
  ManuallyDrop::new(vec);
  // unsafe { Box::leak(Box::from_raw(data)) };
  BigIntWrap { 
    datax:     data,
    len:      len,
    capacity: capacity,
  }
}

pub fn to_bigint (i: &BigIntWrap) -> BigInt {
  let v = unsafe { Vec::from_raw_parts(i.datax, i.len, i.capacity) };
  let r = BigInt::from_signed_bytes_le(v.as_slice());
  ManuallyDrop::new(v);
  // std::mem::forget(v);
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
    let data:BigInt = to_bigint(self);
    data.fmt(f)
  }
}

/**
 * These are the datatypes used by code generated from the smart contract.
 */
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum DataType {
  Int(BigIntWrap), 
  Nat(BigIntWrap), 
  Mutez(BigIntWrap),
  Timestamp(i32),
  Bool(bool),
  ListItem(Wrapped<Node>),
  Tuple(Wrapped<Node>),
  Set(Wrapped<RBNode>),
  Map(Wrapped<RBNode>),
  // Map(Box<RedBlackTreeMap<Box<Self>, Box<Self>>>),
  Operations(Option<Wrapped<OperationNode>>)
  // String(Box<String>),
  // Bytes(Box<String>),
  // Address(Box<String>),

  // TODO: add other data types
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct Node {
  pub value: DataType,
  pub next: Option<Wrapped<Node>>
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum Operation {
  Transaction(Transaction), 
  Delegate(Delegate)
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct OperationNode {
  pub value: Operation,
  pub next: Option<Wrapped<OperationNode>>
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum Color {
  Red,
  Black
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct RBNode {
  pub parent: Option<DataType>,
  pub val:    DataType,
  pub depth:  usize,
  pub left:   Option<DataType>,
  pub right:  Option<DataType>,
  pub color:  Color
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct Transaction {
  pub action: DataType,
  pub amount: BigIntWrap,
  pub contract: BigIntWrap
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct Delegate {
  pub delegate: Option<DataType>,
}
