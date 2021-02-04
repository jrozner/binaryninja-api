// // Copyright 2021 Vector 35 Inc.
// //
// // Licensed under the Apache License, Version 2.0 (the "License");
// // you may not use this file except in compliance with the License.
// // You may obtain a copy of the License at
// //
// // http://www.apache.org/licenses/LICENSE-2.0
// //
// // Unless required by applicable law or agreed to in writing, software
// // distributed under the License is distributed on an "AS IS" BASIS,
// // WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// // See the License for the specific language governing permissions and
// // limitations under the License.

// /////////////////////////
// // FunctionInfoBuilder

// // TODO : Function local variables
// pub struct FunctionInfoBuilder<T: Eq + Hash + Copy> {
//     pub full_name: Option<String>,
//     pub raw_name: Option<String>,
//     pub address: Option<u64>,
//     pub return_type: Option<T>,
//     pub parameters: Vec<NameAndType<T>>,
// }

// impl<T: Eq + Hash + Copy> FunctionInfoBuilder<T> {
//     pub fn new() -> Self {
//         FunctionInfoBuilder {
//             full_name: None,
//             raw_name: None,
//             address: None,
//             return_type: None,
//             parameters: vec![],
//         }
//     }
// }

// //////////////////////
// // DebugInfoBuilder

// // TODO : Including the arch might not be a great idea...but also maybe it is?  Perhaps just the BV instead
// pub struct DebugInfoBuilder<A: Architecture, T: Eq + Hash + Copy> {
//     functions: Vec<FunctionInfo<T>>,
//     types: HashMap<T, TypeInfo<T>>,
//     pub arch: A,
// }

// impl<A: Architecture, T: Eq + Hash + Copy> DebugInfoBuilder<A, T> {
//     pub fn new(arch: A) -> Self {
//         DebugInfoBuilder {
//             functions: vec![],
//             types: HashMap::new(),
//             arch,
//         }
//     }

//     // TODO : Delete this and make the vector of functions a HashSet
//     pub fn sort_by_address(&mut self) {
//         self.functions
//             .sort_by(|a, b| a.symbol().address().cmp(&b.symbol().address()));
//     }

//     pub fn functions(&self) -> &Vec<FunctionInfo<T>> {
//         &self.functions
//     }

//     pub fn get_function_at(&self, addr: u64) -> Option<&FunctionInfo<T>> {
//         for function in &self.functions {
//             if addr == function.name.address() {
//                 return Some(function);
//             }
//         }
//         None
//     }

//     pub fn get_functions_at(&self, addr: u64) -> Vec<&FunctionInfo<T>> {
//         let mut result = vec![];
//         for function in &self.functions {
//             if addr == function.name.address() {
//                 result.push(function);
//             }
//         }
//         result
//     }

//     pub fn add_function(&mut self, func: FunctionInfo<T>) {
//         self.functions.push(func);
//     }

//     pub fn remove_function(&mut self, func: &FunctionInfo<T>) {
//         for (i, function) in self.functions.iter().enumerate() {
//             if func == function {
//                 self.functions.remove(i);
//                 return;
//             }
//         }
//     }

//     pub fn add_type(&mut self, type_info: TypeInfo<T>) {
//         for dependency_uid in &type_info.dependencies {
//             self.types
//                 .get_mut(&dependency_uid)
//                 .expect("Dependency for type does not exist")
//                 .dependents
//                 .insert(type_info.uid);
//         }

//         assert!(self.types.insert(type_info.uid, type_info).is_none());
//     }

//     pub fn get_type(&self, type_uid: T) -> Option<&Type> {
//         if let Some(type_info) = self.types.get(&type_uid) {
//             Some(&type_info.t)
//         } else {
//             None
//         }
//     }

//     pub fn remove_type(&mut self, type_uid: T) {
//         self.types.remove(&type_uid);
//     }

//     pub fn contains_type(&self, type_uid: T) -> bool {
//         self.types.get(&type_uid).is_some()
//     }
// }
