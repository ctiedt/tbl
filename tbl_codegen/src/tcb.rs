#![allow(unused)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use cranelift::{
    codegen::ir::immediates::Offset32,
    prelude::{types, FunctionBuilder, InstBuilder, MemFlags, Value},
};

mod bindings {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

macro_rules! access_tcb_member {
    ($member:tt, $type:expr, $load:ident, $store:ident) => {
        pub fn $load(builder: &mut FunctionBuilder, tcb: Value) -> Value {
            builder.ins().load(
                $type,
                MemFlags::new(),
                tcb,
                Offset32::new(std::mem::offset_of!(bindings::tcb_t, $member) as i32),
            )
        }

        pub fn $store(builder: &mut FunctionBuilder, tcb: Value, v: Value) {
            builder.ins().store(
                MemFlags::new(),
                v,
                tcb,
                Offset32::new(std::mem::offset_of!(bindings::tcb_t, $member) as i32),
            );
        }
    };
}

access_tcb_member!(id, types::I64, load_id, store_id);
access_tcb_member!(copy_locals, types::I8, load_copy_locals, store_copy_locals);
access_tcb_member!(once_mask, types::I64, load_once_mask, store_once_mask);
// TODO: What about pointer sizes other than 64bit?
access_tcb_member!(locals, types::I64, load_locals, store_locals);
