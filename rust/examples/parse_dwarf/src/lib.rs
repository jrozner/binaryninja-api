use binaryninja::architecture::Architecture;
use binaryninja::binaryview::{BinaryView, BinaryViewExt};
use binaryninja::databuffer::DataBuffer;
use binaryninja::debuginfo::{
    CustomDebugInfoParser, DebugInfo, DebugInfoParser, FunctionInfo, FunctionInfoBuilder,
    NameAndType, PrototypeDebugInfo, TypeInfo,
};
use binaryninja::symbol::{Symbol, SymbolType};
use binaryninja::types::{
    NamedTypeReference, NamedTypeReferenceClass, QualifiedName, Structure, Type,
};

use gimli::{
    constants,
    Attribute,
    AttributeValue::{Encoding, UnitRef},
    // BigEndian,
    DebuggingInformationEntry,
    Dwarf,
    DwarfFileType,
    // EntriesTreeNode,
    Error,
    LittleEndian,
    Reader,
    SectionId,
    Unit,
    UnitOffset,
    UnitSectionOffset,
};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

// TODO : Don't redefine types in typelibs?

// gimli::read::load only takes structures containing &[u8]'s, but we need to keep the data buffer alive until it's done using that
//   I don't think that the `Arc` is needed, but I couldn't figure out how else to implement the traits properly without it
#[derive(Clone)]
struct DataBufferWrapper(Arc<DataBuffer>);
impl DataBufferWrapper {
    fn new(buf: DataBuffer) -> Self {
        DataBufferWrapper(Arc::new(buf))
    }
}
impl Deref for DataBufferWrapper {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        self.0.get_data()
    }
}
impl Debug for DataBufferWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("DataBufferWrapper")
            .field("0", &"I'm too lazy to do this right")
            .finish()
    }
}
unsafe impl gimli::StableDeref for DataBufferWrapper {}
unsafe impl gimli::CloneStableDeref for DataBufferWrapper {}
type CustomReader<Endian> = gimli::EndianReader<Endian, DataBufferWrapper>;

// TODO : This only gets one kind of base entry (for...functions?), we should check for overlap and whatnot to parse specific types of base entries
fn get_base_entry<R: Reader>(
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
) -> UnitOffset<<R as Reader>::Offset> {
    if let Ok(Some(UnitRef(offset))) = entry.attr_value(constants::DW_AT_specification) {
        let entry = unit.entry(offset).unwrap();
        get_base_entry(unit, &entry)
    } else if let Ok(Some(UnitRef(offset))) = entry.attr_value(constants::DW_AT_abstract_origin) {
        let entry = unit.entry(offset).unwrap();
        get_base_entry(unit, &entry)
    } else {
        entry.offset()
    }
}

fn get_name<R: Reader>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
) -> Option<String> {
    if let Ok(Some(attr_val)) = entry.attr_value(constants::DW_AT_name) {
        if let Ok(attr_string) = dwarf.attr_string(&unit, attr_val) {
            if let Ok(attr_string) = attr_string.to_string() {
                Some(attr_string.to_string())
            } else {
                None
            }
        } else {
            None
        }
    } else if let Ok(Some(UnitRef(offset))) = entry.attr_value(constants::DW_AT_specification) {
        let entry = unit.entry(offset).unwrap();
        get_name(dwarf, unit, &entry)
    } else if let Ok(Some(UnitRef(offset))) = entry.attr_value(constants::DW_AT_abstract_origin) {
        let entry = unit.entry(offset).unwrap();
        get_name(dwarf, unit, &entry)
    } else {
        None
    }
}

fn get_raw_name<R: Reader>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
) -> Option<String> {
    if let Ok(Some(attr_val)) = entry.attr_value(constants::DW_AT_linkage_name) {
        if let Ok(attr_string) = dwarf.attr_string(&unit, attr_val) {
            if let Ok(attr_string) = attr_string.to_string() {
                Some(attr_string.to_string())
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn get_start_address<R: Reader>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
) -> Option<u64> {
    // TODO : Need to cover more address DIE address representations:
    //   DW_AT_ranges
    if let Ok(Some(attr_val)) = entry.attr_value(constants::DW_AT_low_pc) {
        Some(dwarf.attr_address(&unit, attr_val).unwrap().unwrap())
    } else if let Ok(Some(attr_val)) = entry.attr_value(constants::DW_AT_entry_pc) {
        Some(dwarf.attr_address(&unit, attr_val).unwrap().unwrap())
    } else {
        None
    }
}

fn get_attr_as_u64<R: Reader>(attr: Attribute<R>) -> Option<u64> {
    if let Some(value) = attr.u8_value() {
        Some(value.into())
    } else if let Some(value) = attr.u16_value() {
        Some(value.into())
    } else if let Some(value) = attr.udata_value() {
        Some(value.into())
    } else if let Some(value) = attr.sdata_value() {
        Some(value as u64)
    } else {
        None
    }
}

fn get_attr_as_usize<R: Reader>(attr: Attribute<R>) -> Option<usize> {
    if let Some(value) = attr.u8_value() {
        Some(value.into())
    } else if let Some(value) = attr.u16_value() {
        Some(value.into())
    } else if let Some(value) = attr.udata_value() {
        Some(value as usize)
    } else if let Some(value) = attr.sdata_value() {
        Some(value as usize)
    } else {
        None
    }
}

// TODO : Make this non-copy
fn get_attr_string<'a, R: 'a + Reader>(
    dwarf: &'a Dwarf<R>,
    unit: &'a Unit<R>,
    entry: &'a DebuggingInformationEntry<R>,
) -> String {
    // TODO : This shouldn't need a else case, since I should never be calling this on a DIE without this attribute
    // TODO : Also, rename the variables here
    if let Ok(Some(thing)) = entry.attr_value(constants::DW_AT_name) {
        let attr_name: R = dwarf.attr_string(&unit, thing).unwrap();
        let thing = attr_name.to_string().unwrap_or_default(); // TODO : remove or_default
        String::from(thing.as_ref())
    } else {
        "".to_string()
    }
}

// This function iterates up through the dependency references, adding all the types along the way until there are no more or stopping at the first one already tracked, then returns the UID of the type of the given DIE
fn get_type<'a, A: Architecture, R: Reader<Offset = usize>>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
    mut debug_info: &mut PrototypeDebugInfo<A, UnitOffset>,
) -> Option<UnitOffset> {
    // If this node (and thus all its referenced nodes) has already been processed, just return the offset
    if debug_info.contains_type(entry.offset()) {
        return Some(entry.offset());
    }

    // Recurse
    // TODO : Need to consider specification and abstract origin?
    let mut result = None;
    if let Ok(Some(UnitRef(offset))) = entry.attr_value(constants::DW_AT_type) {
        let entry = unit.entry(offset).unwrap();
        result = get_type(&dwarf, &unit, &entry, &mut debug_info);
    }
    let parent = result;

    // If this node (and thus all its referenced nodes) has already been processed (during recursion), just return the offset
    if debug_info.contains_type(entry.offset()) {
        return Some(entry.offset());
    }

    // Collect the required information to create a type and add it to the type map. Also, add the dependencies of this type to the type's typeinfo
    // Create the type, make a typeinfo for it, and add it to the debug info
    // TODO : Add this type to the type map thing
    // TODO : Add this type's dependency to the type's info
    let type_def: Option<Type> = match entry.tag() {
        constants::DW_TAG_base_type => {
            // All base types have:
            //   DW_AT_name
            //   DW_AT_encoding (our concept of type_class)
            //   DW_AT_byte_size and/or DW_AT_bit_size
            //   *DW_AT_endianity (assumed default for arch)
            //   *DW_AT_data_bit_offset (assumed 0)
            //   *Some indecation of signedness?
            //   * = Optional

            result = Some(entry.offset());
            // TODO : Namespaces?
            // TODO : By spec base types need to have a name, what if it's spec non-conforming?
            let name = get_attr_string(&dwarf, &unit, &entry);

            // TODO : Handle other size specifiers (bits, offset, high_pc?, etc)
            let size: usize =
                get_attr_as_usize(entry.attr(constants::DW_AT_byte_size).unwrap().unwrap())
                    .unwrap();

            match entry.attr_value(constants::DW_AT_encoding) {
                // TODO : Need more binaries to see what's going on
                Ok(Some(Encoding(encoding))) => {
                    match encoding {
                        constants::DW_ATE_address => None,
                        constants::DW_ATE_boolean => Some(Type::bool()),
                        constants::DW_ATE_complex_float => None,
                        constants::DW_ATE_float => Some(Type::named_float(size, name)),
                        constants::DW_ATE_signed => Some(Type::named_int(size, true, name)),
                        constants::DW_ATE_signed_char => Some(Type::named_int(size, true, name)),
                        constants::DW_ATE_unsigned => Some(Type::named_int(size, false, name)),
                        constants::DW_ATE_unsigned_char => Some(Type::named_int(size, false, name)),
                        constants::DW_ATE_imaginary_float => None,
                        constants::DW_ATE_packed_decimal => None,
                        constants::DW_ATE_numeric_string => None,
                        constants::DW_ATE_edited => None,
                        constants::DW_ATE_signed_fixed => None,
                        constants::DW_ATE_unsigned_fixed => None,
                        constants::DW_ATE_decimal_float => Some(Type::named_float(size, name)), // TODO : How is this different from binary floating point, ie. DW_ATE_float?
                        constants::DW_ATE_UTF => Some(Type::named_int(size, false, name)), // TODO : Verify
                        constants::DW_ATE_UCS => None,
                        constants::DW_ATE_ASCII => None, // Some sort of array?
                        constants::DW_ATE_lo_user => None,
                        constants::DW_ATE_hi_user => None,
                        _ => None, // Anything else is invalid at time of writing (gimli v0.23.0)
                    }
                }
                _ => None,
            }
        }
        // bn::Types::Structure related things
        //  Steps to parsing a structure:
        //    Create a phony type representing the structure
        //    Parse the size of the structure and create a Structure instance
        //    Recurse on the DIE's children to create all their types (any references back to the the current DIE will be NamedTypeReferences to a phony type)
        //    Populate the members of the structure, create a structure_type, and register it with the DebugInfo
        constants::DW_TAG_structure_type => {
            // First things first, let's register a reference type for this struct for any children to grab while we're still building this type
            let name = get_attr_string(&dwarf, &unit, &entry);
            debug_info.add_type(TypeInfo::new(
                Type::named_type(NamedTypeReference::new(
                    NamedTypeReferenceClass::StructNamedTypeClass,
                    Type::generate_auto_demangled_type_id(name.clone()),
                    QualifiedName::from(name),
                )),
                entry.offset(),
                HashSet::new(),
            ));

            // Create structure with proper size
            // TODO : Parse the size but properly
            let size =
                get_attr_as_u64(entry.attr(constants::DW_AT_byte_size).unwrap().unwrap()).unwrap();
            let mut new_structure = Structure::new();
            new_structure.set_width(size);

            // Get all the children and populate
            // TODO : Make in to its own function?
            let mut tree = unit.entries_tree(Some(entry.offset())).unwrap();
            let mut children = tree.root().unwrap().children();
            while let Ok(Some(child)) = children.next() {
                let label_value = match child.entry().offset().to_unit_section_offset(unit) {
                    UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    UnitSectionOffset::DebugTypesOffset(o) => o.0,
                };
                // TODO : Remove `if let` guard; types will always exist once this plugin is complete
                if let Some(child_type_id) =
                    get_type(&dwarf, &unit, &child.entry(), &mut debug_info)
                {
                    if child.entry().tag() == constants::DW_TAG_member {
                        let child_offset = get_attr_as_u64(
                            child
                                .entry()
                                .attr(constants::DW_AT_data_member_location)
                                .unwrap()
                                .unwrap(),
                        )
                        .unwrap();
                        let child_name = get_attr_string(&dwarf, &unit, &child.entry());

                        // TODO : Remove `if let` guard; types will always exist once this plugin is complete
                        if let Some(child_type) = debug_info.get_type(child_type_id) {
                            new_structure.insert(child_type, child_name, child_offset);
                        } else {
                            println!("Type for #0x{:08x} was not created!", label_value);
                        }
                    }
                } else {
                    println!("Type for #0x{:08x} could not be created!", label_value);
                }
            }
            // End children recursive block

            debug_info.remove_type(entry.offset());
            result = Some(entry.offset());
            Some(Type::structure_type(&mut new_structure))
            // None
        }
        constants::DW_TAG_class_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_union_type => {
            result = Some(entry.offset());
            None
        }
        // Enum
        constants::DW_TAG_enumeration_type => {
            result = Some(entry.offset());
            None
        }
        // Basic types
        constants::DW_TAG_typedef => {
            // All base types have:
            //   DW_AT_name
            //   *DW_AT_type (TODO : Otherwise.....abstract origin?)
            //   * = Optional

            let name = get_attr_string(&dwarf, &unit, &entry);

            result = Some(entry.offset());
            // TODO : Remove `if let` guard; types will always exist once this plugin is complete
            if let Some(thing) = debug_info.get_type(parent.unwrap()) {
                Some(Type::named_type_from_type(name, thing))
            } else {
                let label_value = match entry.offset().to_unit_section_offset(unit) {
                    UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    UnitSectionOffset::DebugTypesOffset(o) => o.0,
                };
                println!("Typedef for #0x{:08x} could not be created!", label_value);
                None
            }
        }
        constants::DW_TAG_pointer_type => {
            // All types types have:
            //   DW_AT_type
            //   ?DW_AT_name
            //   ?DW_AT_address
            //   ?DW_AT_allocated
            //   ?DW_AT_associated
            //   ?DW_AT_data_location
            //   * = Optional

            // TODO : We assume the parent has a name?  Might we need to resolve it deeper?
            result = Some(entry.offset());

            // TODO : Remove guards; types will always exist once this plugin is complete (except for void*)
            // TODO : use Type::pointer_of_width - BNCreatePointerTypeOfWidth instead, since pointers always give a width here
            let label_value = match entry.offset().to_unit_section_offset(unit) {
                UnitSectionOffset::DebugInfoOffset(o) => o.0,
                UnitSectionOffset::DebugTypesOffset(o) => o.0,
            };
            if let Some(parent) = parent {
                if let Some(thing) = debug_info.get_type(parent) {
                    Some(Type::pointer(
                        &debug_info.arch,
                        &Type::named_type_from_type(
                            get_attr_string(&dwarf, &unit, &unit.entry(parent).unwrap()),
                            thing,
                        ),
                    ))
                } else {
                    println!("Pointer to #0x{:08x} could not be created!", label_value);
                    None
                }
            } else {
                Some(Type::pointer(&debug_info.arch, &Type::void()))
            }
        }
        constants::DW_TAG_array_type => {
            // All array types have:
            //    DW_AT_type
            //   *DW_AT_name
            //   *DW_AT_ordering
            //   *DW_AT_byte_stride or DW_AT_bit_stride
            //   *DW_AT_byte_size or DW_AT_bit_size
            //   *DW_AT_allocated
            //   *DW_AT_associated and
            //   *DW_AT_data_location
            //   * = Optional
            //   For multidimensional arrays, DW_TAG_subrange_type or DW_TAG_enumeration_type

            // TODO : How to do the name, if it has one?
            // TODO : size
            result = Some(entry.offset());

            // TODO : Remove `if let` guard; types will always exist once this plugin is complete
            if let Some(thing) = debug_info.get_type(parent.unwrap()) {
                Some(Type::array(thing, 0))
            } else {
                let label_value = match entry.offset().to_unit_section_offset(unit) {
                    UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    UnitSectionOffset::DebugTypesOffset(o) => o.0,
                };
                println!("Array of #0x{:08x} could not be created!", label_value);
                None
            }
        }
        constants::DW_TAG_string_type => {
            result = Some(entry.offset());
            None
        }
        // Strange Types
        constants::DW_TAG_unspecified_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_subroutine_type => {
            result = Some(entry.offset());
            None
        }
        // Unusual Types
        constants::DW_TAG_ptr_to_member_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_set_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_subrange_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_file_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_thrown_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_interface_type => {
            result = Some(entry.offset());
            None
        }
        // Weird types
        constants::DW_TAG_reference_type => {
            // This is the l-value for the complimentary r-value following in the if-else chain
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_rvalue_reference_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_restrict_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_shared_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_volatile_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_packed_type => {
            result = Some(entry.offset());
            None
        }
        constants::DW_TAG_const_type => {
            result = Some(entry.offset());
            None
        }
        _ => None,
    };

    // Wrap our resultant type in a TypeInfo so that the internal DebugInfo class can manage it
    if let Some(type_def) = type_def {
        if let Some(parent) = parent {
            debug_info.add_type(TypeInfo::new(
                type_def,
                entry.offset(),
                [parent].iter().cloned().collect(), // TODO : Do I need to clone?
            ));
        } else {
            debug_info.add_type(TypeInfo::new(type_def, entry.offset(), HashSet::new()));
        }
    }
    result
}

fn get_parameters<'a, A: Architecture, R: Reader<Offset = usize>>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
    mut debug_info: &mut PrototypeDebugInfo<A, UnitOffset>,
) -> Option<Vec<NameAndType<UnitOffset>>> {
    // TODO : Get tree for entry
    // TODO : (Might need to flip the last two things)
    // TODO : Collect the formal parameters and unspecified's as well

    if !entry.has_children() {
        None
    } else {
        // We make a new tree from the current entry to iterate over its children
        // TODO : We could instead pass the `entries` object down from parse_dwarf to avoid parsing the same object multiple times
        let mut sub_die_tree = unit.entries_tree(Some(entry.offset())).unwrap();
        let root = sub_die_tree.root().unwrap();

        let mut result = vec![];
        let mut children = root.children();
        while let Some(child) = children.next().unwrap() {
            match child.entry().tag() {
                constants::DW_TAG_formal_parameter => {
                    // TODO : Remove the safety-rails; when this plugin is complete, it'll always be safe to unwrap()
                    if let (Some(parameter_name), Some(parameter_type)) = (
                        get_name(&dwarf, &unit, &child.entry()),
                        get_type(&dwarf, &unit, &child.entry(), &mut debug_info),
                    ) {
                        result.push(NameAndType::new(parameter_name, parameter_type));
                    }
                    // let parameter_name = get_name(&dwarf, &unit, &child.entry()).unwrap();
                    // let parameter_type =
                    //     get_type(&dwarf, &unit, &child.entry(), &mut debug_info).unwrap();
                    // result.push(NameAndType::new(parameter_name, parameter_type));
                }
                constants::DW_TAG_unspecified_parameters => (),
                _ => (),
            }
        }
        Some(result)
    }
}

fn process_function_entry<A: Architecture, R: Reader<Offset = usize>>(
    dwarf: &Dwarf<R>,
    unit: &Unit<R>,
    entry: &DebuggingInformationEntry<R>,
    namespace_qualifiers: &mut Vec<(isize, String)>,
    entry_map: &mut HashMap<UnitOffset, FunctionInfoBuilder<UnitOffset>>,
    mut debug_info: &mut PrototypeDebugInfo<A, UnitOffset>,
) {
    // TODO : fn get_children(...) -> u64; Skip dfs for however many children there are
    // TODO : Handle OOT, stubs/trampolines

    // Functions can be declared and defined in different parts of the tree, and decls and defs can hold different parts of the information we need
    //   But there /should/ (TODO : Verify) be only one unique "base" DIE for each function
    let base_entry = get_base_entry(&unit, &entry);

    // Collect function properties (if they exist in this DIE)
    let raw_name = get_raw_name(&dwarf, &unit, &entry);
    let function_name = get_name(&dwarf, &unit, &entry);
    let address = get_start_address(&dwarf, &unit, &entry);

    // The DIE does not contain any namespace information, so we track the namespaces and build the symbol ourselves
    // TODO : Build the namespace string outside of this function once for each namespace change instead of for every single function?
    let full_name: Option<String>;
    if let Some(function_name) = function_name {
        let mut full_name_builder = "".to_string();
        for (_, namespace) in namespace_qualifiers {
            full_name_builder = format!("{}{}::", full_name_builder, namespace);
        }
        full_name = Some(format!("{}{}", full_name_builder, function_name));
    } else {
        full_name = None;
    }

    // print!(
    //     "Name: {:?}\nRaw: {:?}\naddr: {:?}\n",
    //     &full_name, &raw_name, &address
    // );

    let return_type = get_type(&dwarf, &unit, &entry, &mut debug_info);
    let parameters = get_parameters(&dwarf, &unit, &entry, &mut debug_info);

    // Assuming that the base entry is unique (TODO : Verify), then we just need to add the data we collected
    match entry_map.get_mut(&base_entry) {
        Some(old_function_prototype) => {
            // Replace raw name
            if let (Some(old), Some(new)) = (&old_function_prototype.raw_name, &raw_name) {
                if old.len() > new.len() {
                    old_function_prototype.raw_name = raw_name;
                }
            } else if raw_name.is_some() {
                old_function_prototype.raw_name = raw_name;
            }

            // Replace full name
            if let (Some(old), Some(new)) = (&old_function_prototype.full_name, &full_name) {
                if old.len() > new.len() {
                    old_function_prototype.full_name = full_name;
                }
            } else if full_name.is_some() {
                old_function_prototype.raw_name = full_name;
            }

            // Replace return type
            if return_type.is_some() {
                old_function_prototype.return_type = return_type;
            }

            // Replace function arguments
            if let Some(parameters) = parameters {
                old_function_prototype.parameters = parameters;
            }

            // Replace function address
            if address.is_some() {
                old_function_prototype.address = address;
            }
        }
        None => {
            entry_map.insert(
                base_entry,
                FunctionInfoBuilder {
                    full_name: full_name,
                    raw_name: raw_name,
                    address: address,
                    return_type: return_type,
                    parameters: parameters.unwrap_or_else(|| vec![]),
                },
            );
        }
    }
}

fn parse_dwarf<A: Architecture>(
    mut debug_info: &mut PrototypeDebugInfo<A, UnitOffset>,
    view: &BinaryView,
    dwo_file: bool,
) {
    // TODO : This only works for non-DWO files, but it should be able to work for both (there's some function call to set GIMLI into DWO mode)

    // TODO : Handle Endianity
    let get_section_data_little =
        |section_id: SectionId| -> Result<CustomReader<LittleEndian>, Error> {
            let section_name;
            if dwo_file && section_id.dwo_name().is_some() {
                section_name = section_id.dwo_name().unwrap();
            } else if dwo_file {
                return Ok(CustomReader::new(
                    DataBufferWrapper::new(DataBuffer::default()),
                    LittleEndian,
                ));
            } else {
                section_name = section_id.name();
            }

            if let Ok(section) = view.section_by_name(section_name) {
                let offset = section.start();
                let len = section.len();
                if len == 0 {
                    return Ok(CustomReader::new(
                        DataBufferWrapper::new(DataBuffer::default()),
                        LittleEndian,
                    ));
                }

                if let Ok(read_buffer) = view.read_buffer(offset, len as usize) {
                    return Ok(CustomReader::new(
                        DataBufferWrapper::new(read_buffer),
                        LittleEndian,
                    ));
                }
                return Err(Error::Io);
            } else {
                return Ok(CustomReader::new(
                    DataBufferWrapper::new(DataBuffer::default()),
                    LittleEndian,
                ));
            }
        };

    let empty_reader_little = |_: SectionId| -> Result<CustomReader<LittleEndian>, Error> {
        Ok(CustomReader::new(
            DataBufferWrapper::new(DataBuffer::default()),
            LittleEndian,
        ))
    };

    let mut entry_map: HashMap<UnitOffset, FunctionInfoBuilder<UnitOffset>> = HashMap::new();

    let mut dwarf = Dwarf::load(&get_section_data_little, &empty_reader_little).unwrap();
    if dwo_file {
        dwarf.file_type = DwarfFileType::Dwo;
    }
    let mut iter = dwarf.units();
    while let Some(header) = iter.next().unwrap() {
        let unit = dwarf.unit(header).unwrap();
        let mut entries = unit.entries();
        let mut depth = 0;

        let mut namespace_qualifiers: Vec<(isize, String)> = vec![];

        // Header entry
        if let Some((delta_depth, _entry)) = entries.next_dfs().unwrap() {
            depth += delta_depth;
            println!("Parsing compilation unit: ");
            // simple_print_attrs(&dwarf, &unit, entry.attrs());
        }

        // All entries under header
        while let Ok(Some((delta_depth, entry))) = entries.next_dfs() {
            depth += delta_depth;
            assert!(depth >= 0); // TODO : Properly handle this

            namespace_qualifiers.retain(|&(entry_depth, _)| entry_depth < depth);

            match entry.tag() {
                constants::DW_TAG_namespace => {
                    namespace_qualifiers.push((depth, get_name(&dwarf, &unit, &entry).unwrap()))
                }
                constants::DW_TAG_class_type => {
                    namespace_qualifiers.push((depth, get_name(&dwarf, &unit, &entry).unwrap()))
                }
                constants::DW_TAG_structure_type => {
                    // TODO : Is this necessary?
                    if let Some(name) = get_name(&dwarf, &unit, &entry) {
                        namespace_qualifiers.push((depth, name))
                    } else {
                        println!("Couldn't get name for structure_type");
                    }
                }
                constants::DW_TAG_subprogram => process_function_entry(
                    &dwarf,
                    &unit,
                    &entry,
                    &mut namespace_qualifiers,
                    &mut entry_map,
                    &mut debug_info,
                ),
                _ => (),
            }
        }
    }

    // Much of the data we collected is likely incomplete, so we need to filter those away
    for (_, function) in entry_map.into_iter() {
        if let Some(address) = function.address {
            let raw_name;
            let full_name;

            if let Some(name) = &function.raw_name {
                raw_name = name.clone();
            } else if let Some(name) = &function.full_name {
                raw_name = name.clone();
            } else {
                raw_name = format!("sub_{:x}", address);
            }

            if let Some(name) = function.full_name {
                full_name = name;
            } else if let Some(name) = function.raw_name {
                full_name = name;
            } else {
                full_name = format!("sub_{:x}", address);
            }

            debug_info.add_function(FunctionInfo::new(
                Symbol::new(SymbolType::Function, raw_name, address)
                    .short_name(full_name.clone())
                    .full_name(full_name)
                    .create(),
                function.return_type,
                function.parameters,
            ));
        }
    }
}

struct DWARFParser;

impl CustomDebugInfoParser for DWARFParser {
    fn is_valid(&self, view: &BinaryView) -> bool {
        view.section_by_name(".debug_info").is_ok()
            || view.section_by_name(".debug_info.dwo").is_ok()
            || (view.parent_view().is_ok()
                && (view
                    .parent_view()
                    .unwrap()
                    .section_by_name(".debug_info")
                    .is_ok())
                || view
                    .parent_view()
                    .unwrap()
                    .section_by_name(".debug_info.dwo")
                    .is_ok())
    }

    fn parse_info(&self, mut _debug_info: &mut DebugInfo, view: &BinaryView) {
        let mut debug_info = PrototypeDebugInfo::new(view.default_arch().unwrap());

        if view.section_by_name(".debug_info").is_ok() {
            parse_dwarf(&mut debug_info, &view, false);
        } else if view.parent_view().is_ok()
            && view
                .parent_view()
                .unwrap()
                .section_by_name(".debug_info")
                .is_ok()
        {
            parse_dwarf(&mut debug_info, &view.parent_view().unwrap(), false);
        } else if view.section_by_name(".debug_info.dwo").is_ok() {
            parse_dwarf(&mut debug_info, &view, true);
        } else if view.parent_view().is_ok()  // TODO : I don't think this is possible
            && view
                .parent_view()
                .unwrap()
                .section_by_name(".debug_info.dwo")
                .is_ok()
        {
            parse_dwarf(&mut debug_info, &view.parent_view().unwrap(), true);
        }

        let default_type = Type::void();
        debug_info.sort_by_address();
        for function in debug_info.functions() {
            let return_type_string;
            if let Some(t) = function.return_type() {
                return_type_string = debug_info.get_type(t).unwrap();
            } else {
                return_type_string = &default_type;
            }

            print!(
                "0x{:08x}: {} {}(",
                function.symbol().address(),
                return_type_string,
                function.symbol().full_name()
            );

            // for param in function.parameters() {
            //     print!(
            //         "{} {}, ",
            //         debug_info.get_type(param.r#type()).unwrap(),
            //         param.name()
            //     );
            // }
            println!(")");
        }
        println!("Function Count: {}", debug_info.functions().len());
    }
}

#[no_mangle]
pub extern "C" fn CorePluginInit() -> bool {
    DebugInfoParser::register("DWARF", DWARFParser {});
    true
}
