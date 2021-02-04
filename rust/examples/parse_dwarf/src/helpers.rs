// Copyright 2021 Vector 35 Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use binaryninja::databuffer::DataBuffer;

use gimli::{
    constants, Attribute, AttributeValue::UnitRef, DebuggingInformationEntry, Dwarf, Reader, Unit,
    UnitOffset,
};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

// gimli::read::load only takes structures containing &[u8]'s, but we need to keep the data buffer alive until it's done using that
//   I don't think that the `Arc` is needed, but I couldn't figure out how else to implement the traits properly without it
#[derive(Clone)]
pub(crate) struct DataBufferWrapper(Arc<DataBuffer>);
impl DataBufferWrapper {
    pub(crate) fn new(buf: DataBuffer) -> Self {
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
pub(crate) type CustomReader<Endian> = gimli::EndianReader<Endian, DataBufferWrapper>;

// TODO : This only gets one kind of base entry (for...functions?), we should check for overlap and whatnot to parse specific types of base entries
pub(crate) fn get_base_entry<R: Reader>(
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

pub(crate) fn get_name<R: Reader>(
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

pub(crate) fn get_raw_name<R: Reader>(
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

pub(crate) fn get_start_address<R: Reader>(
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

pub(crate) fn get_attr_as_u64<R: Reader>(attr: Attribute<R>) -> Option<u64> {
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

pub(crate) fn get_attr_as_usize<R: Reader>(attr: Attribute<R>) -> Option<usize> {
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
pub(crate) fn get_attr_string<'a, R: 'a + Reader>(
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
