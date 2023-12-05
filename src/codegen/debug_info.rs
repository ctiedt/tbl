use std::{convert::Infallible, sync::Arc};

use cranelift::codegen::{
    gimli::{
        self,
        write::{Address, AttributeValue, DwarfUnit, EndianVec, Sections},
        DW_AT_low_pc, DW_AT_name, DW_AT_producer, DW_TAG_subprogram, Encoding, SectionId,
    },
    isa::TargetIsa,
};
use cranelift_object::{object::SectionKind, ObjectProduct};
use miette::IntoDiagnostic;

use super::context::{CodeGenContext, FunctionContext};

pub struct DebugInfoGenerator {
    dwarf: DwarfUnit,
}

impl DebugInfoGenerator {
    pub fn new(target: Arc<dyn TargetIsa>) -> Self {
        let encoding = Encoding {
            address_size: target.pointer_bytes(),
            format: gimli::Format::Dwarf64,
            version: 5,
        };
        Self {
            dwarf: DwarfUnit::new(encoding),
        }
    }

    pub fn generate(
        &mut self,
        ctx: &CodeGenContext,
        object: &mut ObjectProduct,
    ) -> miette::Result<()> {
        let root_id = self.dwarf.unit.root();
        let root = self.dwarf.unit.get_mut(root_id);
        root.set(
            DW_AT_producer,
            AttributeValue::StringRef(self.dwarf.strings.add("tbl v0.1")),
        );
        root.set(DW_AT_low_pc, AttributeValue::Address(Address::Constant(0)));

        for func in ctx.functions.values() {
            self.generate_function_di(func, object);
        }

        let mut sections = Sections::new(EndianVec::new(gimli::LittleEndian));
        self.dwarf.write(&mut sections).into_diagnostic()?;

        sections
            .for_each(|id, data| {
                let segment = object
                    .object
                    .segment_name(cranelift_object::object::write::StandardSegment::Debug)
                    .to_vec();
                let sec_id = object.object.add_section(
                    segment,
                    id.name().bytes().collect(),
                    if id == SectionId::EhFrame {
                        SectionKind::ReadOnlyData
                    } else {
                        SectionKind::Debug
                    },
                );
                object.object.section_mut(sec_id).set_data(
                    data.clone().into_vec(),
                    if id == SectionId::EhFrame { 8 } else { 1 },
                );
                Ok::<(), Infallible>(())
            })
            .into_diagnostic()?;

        Ok(())
    }

    fn generate_function_di(&mut self, func: &FunctionContext, object: &mut ObjectProduct) {
        let root = self.dwarf.unit.root();
        let dwarf_fn = self.dwarf.unit.add(root, DW_TAG_subprogram);

        let func_symbol_id = object.function_symbol(func.func_id);
        let func_symbol = object.object.symbol(func_symbol_id);

        let die = self.dwarf.unit.get_mut(dwarf_fn);

        die.set(
            DW_AT_low_pc,
            AttributeValue::Address(Address::Constant(func_symbol.value)),
        );
        die.set(DW_AT_name, AttributeValue::String(func_symbol.name.clone()));
    }
}
