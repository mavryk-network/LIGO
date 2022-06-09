
use ligo_runtime_macros::*;

use crate::datatype::*;

#[expose]
#[no_mangle]
pub extern "C" fn int_neg (i: DataType) -> DataType {
    match i {
        DataType::Int(i) => {
            let i = to_bigint(i.unwrap());
            let x = -i;
            DataType::Int(to_wrap(&x).wrap())
        },
        _ => {
            panic!("Not supported")
        }
    }
}

#[expose]
#[no_mangle]
pub extern "C" fn int_add (i: DataType, b: DataType) -> DataType {
    match (i, b) {
        (DataType::Int(i), DataType::Int(i2))  => {
            let i = to_bigint(i.unwrap());
            let i2 = to_bigint(i2.unwrap());
            let x = i + i2;
            DataType::Int(to_wrap(&x).wrap())
        },
        _ => {
            panic!("Not supported")
        }
    }
}

#[expose]
#[no_mangle]
pub extern "C" fn int_sub (i: DataType, b: DataType) -> DataType {
    match (i, b) {
        (DataType::Int(i), DataType::Int(i2))  => {
            let i = to_bigint(i.unwrap());
            let i2 = to_bigint(i2.unwrap());
            let x = i - i2;
            DataType::Int(to_wrap(&x).wrap())
        },
        _ => {
            panic!("Not supported")
        }
    }
}


// This is a hack, and should ideally completely be removed.
#[produce_file]
fn todo_find_something_better() {}