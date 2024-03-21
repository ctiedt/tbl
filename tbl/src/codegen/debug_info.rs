use std::{collections::HashMap, convert::Infallible, sync::Arc};

use cranelift::codegen::{
    gimli::{
        self,
        write::{
            Address, AttributeValue, DwarfUnit, EndianVec, FileId, LineProgram, LineString,
            Sections, UnitEntryId, Writer,
        },
        DW_ATE_unsigned, DW_AT_byte_size, DW_AT_comp_dir, DW_AT_decl_column, DW_AT_decl_file,
        DW_AT_decl_line, DW_AT_encoding, DW_AT_external, DW_AT_frame_base, DW_AT_high_pc,
        DW_AT_location, DW_AT_low_pc, DW_AT_name, DW_AT_producer, DW_AT_prototyped,
        DW_AT_stmt_list, DW_AT_type, DW_OP_call_frame_cfa, DW_OP_fbreg, DW_TAG_base_type,
        DW_TAG_formal_parameter, DW_TAG_subprogram, DW_TAG_unspecified_parameters, Encoding,
        LineEncoding, RunTimeEndian, SectionId,
    },
    isa::TargetIsa,
};
use cranelift_object::{
    object::{write::Relocation, RelocationEncoding, RelocationKind, SectionKind},
    ObjectProduct,
};
use miette::IntoDiagnostic;

use super::{
    context::{CodeGenContext, FunctionContext},
    Config,
};
use cranelift_module::FuncId;

pub struct DebugInfoGenerator {
    dwarf: DwarfUnit,
    type_defs: HashMap<String, UnitEntryId>,
    file_id: FileId,
}

impl DebugInfoGenerator {
    pub fn new(target: Arc<dyn TargetIsa>, config: Config) -> Self {
        let encoding = Encoding {
            address_size: target.pointer_bytes(),
            format: gimli::Format::Dwarf32,
            version: 5,
        };
        let mut dwarf = DwarfUnit::new(encoding);

        let only_filename = config.filename.file_name().unwrap().to_str().unwrap();
        //let dirname = std::env::current_dir().unwrap();
        let dirname = config.filename.parent().unwrap().to_str().unwrap();
        //let dirname = dirname.to_str().unwrap();

        let mut line_program = LineProgram::new(
            encoding,
            LineEncoding::default(),
            LineString::new(dirname.as_bytes(), encoding, &mut dwarf.line_strings),
            LineString::new(only_filename.as_bytes(), encoding, &mut dwarf.line_strings),
            None,
        );

        let filename_lstr = LineString::new(
            only_filename,
            line_program.encoding(),
            &mut dwarf.line_strings,
        );
        let dirname_lstr =
            LineString::new(dirname, line_program.encoding(), &mut dwarf.line_strings);
        let dir_id = line_program.add_directory(dirname_lstr);

        let file_id = line_program.add_file(filename_lstr, dir_id, None);

        dwarf.unit.line_program = line_program;

        let root_id = dwarf.unit.root();
        let root = dwarf.unit.get_mut(root_id);
        root.set(
            DW_AT_producer,
            AttributeValue::StringRef(dwarf.strings.add("tbl v0.1")),
        );
        root.set(
            DW_AT_name,
            AttributeValue::StringRef(dwarf.strings.add(only_filename)),
        );
        root.set(
            DW_AT_comp_dir,
            AttributeValue::StringRef(dwarf.strings.add(dirname)),
        );

        root.set(DW_AT_stmt_list, AttributeValue::Data1(0));

        Self {
            dwarf,
            type_defs: HashMap::new(),
            file_id,
        }
    }

    pub fn generate(
        &mut self,
        ctx: &CodeGenContext,
        object: &mut ObjectProduct,
    ) -> miette::Result<()> {
        self.generate_primitive_types();

        for func in ctx.functions.values() {
            self.generate_function_di(func, object, ctx);
        }

        let mut sections = Sections::new(DebugInfoWriter::new(RunTimeEndian::Little));
        self.dwarf.write(&mut sections).into_diagnostic()?;

        let mut section_map = HashMap::new();
        sections
            .for_each_mut(|id, section| {
                if !section.writer.slice().is_empty() {
                    let segment = object
                        .object
                        .segment_name(cranelift_object::object::write::StandardSegment::Debug)
                        .to_vec();
                    let section_id = object.object.add_section(
                        segment,
                        id.name().bytes().collect(),
                        if id == SectionId::EhFrame {
                            SectionKind::ReadOnlyData
                        } else {
                            SectionKind::Debug
                        },
                    );
                    let symbol_id = object.object.section_symbol(section_id);
                    object.object.section_mut(section_id).set_data(
                        section.writer.take(),
                        if id == SectionId::EhFrame { 8 } else { 1 },
                    );
                    section_map.insert(id, (section_id, symbol_id));
                }
                Ok::<(), Infallible>(())
            })
            .into_diagnostic()?;

        sections
            .for_each(|id, section| {
                if let Some(section_id) = section_map.get(&id) {
                    for reloc in &section.relocs {
                        let symbol_id = object.function_symbol(FuncId::from_u32(reloc.name as u32));
                        let (symbol, symbol_offset) =
                            object.object.symbol_section_and_offset(symbol_id).unwrap();

                        object
                            .object
                            .add_relocation(
                                section_id.0,
                                Relocation {
                                    offset: u64::from(reloc.offset),
                                    size: reloc.size * 8,
                                    kind: reloc.kind,
                                    encoding: RelocationEncoding::Generic,
                                    symbol,
                                    addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
                                },
                            )
                            .unwrap();
                    }
                }
                Ok::<(), Infallible>(())
            })
            .into_diagnostic()?;

        Ok(())
    }

    fn generate_function_di(
        &mut self,
        func: &FunctionContext,
        object: &mut ObjectProduct,
        ctx: &CodeGenContext,
    ) {
        let root = self.dwarf.unit.root();
        let dwarf_fn = self.dwarf.unit.add(root, DW_TAG_subprogram);

        let func_symbol_id = object.function_symbol(func.func_id);
        let func_symbol = object.object.symbol(func_symbol_id);

        let die = self.dwarf.unit.get_mut(dwarf_fn);

        die.set(DW_AT_external, AttributeValue::FlagPresent);
        die.set(DW_AT_prototyped, AttributeValue::FlagPresent);
        die.set(DW_AT_name, AttributeValue::String(func_symbol.name.clone()));

        if !func.is_external {
            die.set(
                DW_AT_low_pc,
                AttributeValue::Address(Address::Symbol {
                    symbol: func.func_id.as_u32() as usize,
                    addend: 0,
                }),
            );
            die.set(DW_AT_high_pc, AttributeValue::Data8(func_symbol.size));
        }

        die.set(
            DW_AT_decl_file,
            AttributeValue::FileIndex(Some(self.file_id)),
        );
        // die.set(
        //     DW_AT_decl_line,
        //     AttributeValue::Udata(func.location.line as u64),
        // );
        // die.set(
        //     DW_AT_decl_column,
        //     AttributeValue::Udata(func.location.column as u64),
        // );
        die.set(
            DW_AT_frame_base,
            AttributeValue::Block(vec![DW_OP_call_frame_cfa.0]),
        );

        if let Some(ref ty) = func.returns {
            if let Some(prim_ty) = self.type_defs.get(&ty.name()) {
                die.set(DW_AT_type, AttributeValue::UnitRef(*prim_ty));
            }
        }

        let mut param_offset = 0;
        for (name, ty) in &func.params {
            while param_offset % self.dwarf.unit.address_size() != 0 {
                param_offset += 1;
            }
            let param_id = self.dwarf.unit.add(dwarf_fn, DW_TAG_formal_parameter);
            let param_die = self.dwarf.unit.get_mut(param_id);

            if let Some(prim_ty) = self.type_defs.get(&ty.name()) {
                param_die.set(DW_AT_type, AttributeValue::UnitRef(*prim_ty));
                param_die.set(
                    DW_AT_location,
                    AttributeValue::Block(vec![DW_OP_fbreg.0, param_offset]),
                );
            }

            param_die.set(
                DW_AT_name,
                AttributeValue::StringRef(self.dwarf.strings.add(name.as_str())),
            );

            param_offset += ctx.type_size(ty, self.dwarf.unit.address_size() as usize) as u8;
        }

        if func.is_variadic {
            self.dwarf.unit.add(dwarf_fn, DW_TAG_unspecified_parameters);
        }
    }

    fn generate_primitive_types(&mut self) {
        let root = self.dwarf.unit.root();
        for sign in ['i', 'u'] {
            for width in [8, 16, 32, 64] {
                let ty_name = format!("{sign}{width}");
                let int_id = self.dwarf.unit.add(root, DW_TAG_base_type);
                self.type_defs.insert(ty_name.clone(), int_id);
                let int_die = self.dwarf.unit.get_mut(int_id);
                int_die.set(
                    DW_AT_name,
                    AttributeValue::StringRef(self.dwarf.strings.add(ty_name)),
                );
                int_die.set(DW_AT_byte_size, AttributeValue::Data1(width / 8));
                int_die.set(DW_AT_encoding, AttributeValue::Encoding(DW_ATE_unsigned));
            }
        }
        let any_id = self.dwarf.unit.add(root, DW_TAG_base_type);
        self.type_defs.insert("any".into(), any_id);
        let any_die = self.dwarf.unit.get_mut(any_id);
        any_die.set(
            DW_AT_name,
            AttributeValue::StringRef(self.dwarf.strings.add("any")),
        );
    }
}

#[derive(Clone)]
pub(crate) struct DebugReloc {
    pub(crate) offset: u32,
    pub(crate) size: u8,
    pub(crate) name: usize,
    pub(crate) addend: i64,
    pub(crate) kind: RelocationKind,
}

#[derive(Clone)]
struct DebugInfoWriter {
    writer: EndianVec<RunTimeEndian>,
    relocs: Vec<DebugReloc>,
}

impl DebugInfoWriter {
    fn new(endian: RunTimeEndian) -> Self {
        Self {
            writer: EndianVec::new(endian),
            relocs: vec![],
        }
    }
}

impl Writer for DebugInfoWriter {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                let offset = self.len() as u64;
                self.relocs.push(DebugReloc {
                    offset: offset as u32,
                    size,
                    name: symbol,
                    addend,
                    kind: RelocationKind::Absolute,
                });
                self.write_udata(0, size)
            }
        }
    }
}
