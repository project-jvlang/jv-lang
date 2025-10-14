use jv_ir::types::JavaType;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use thiserror::Error;

const ACC_STATIC: u16 = 0x0008;

#[derive(Debug, Error)]
pub enum ClassParseError {
    #[error("unexpected end of class file")]
    UnexpectedEof,
    #[error("invalid class file magic header")]
    InvalidMagic,
    #[error("unsupported constant pool tag {tag}")]
    UnsupportedConstant { tag: u8 },
    #[error("invalid constant pool index {index}")]
    InvalidConstantIndex { index: u16 },
    #[error("invalid UTF-8 string in constant pool: {0}")]
    Utf8Decode(#[from] std::string::FromUtf8Error),
    #[error("malformed descriptor: {0}")]
    InvalidDescriptor(String),
}

#[derive(Debug, Clone)]
pub struct ParsedClass {
    pub fqcn: String,
    pub package: String,
    pub static_fields: Vec<(String, JavaType)>,
    pub static_methods: Vec<(String, JavaMethodSignature)>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JavaMethodSignature {
    pub parameters: Vec<JavaType>,
    pub return_type: JavaType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StaticMember {
    pub owner: String,
    pub member: String,
    pub kind: StaticMemberKind,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum StaticMemberKind {
    Field { ty: JavaType },
    Method { signature: JavaMethodSignature },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInfo {
    pub name: String,
    pub exports: HashSet<String>,
}

pub fn parse_class(bytes: &[u8]) -> Result<ParsedClass, ClassParseError> {
    let mut reader = ClassReader::new(bytes);
    reader.expect_magic()?;
    let _minor_version = reader.read_u2()?;
    let _major_version = reader.read_u2()?;
    let constant_pool = ConstantPool::parse(&mut reader)?;

    let _access_flags = reader.read_u2()?;
    let this_class = reader.read_u2()?;
    let _super_class = reader.read_u2()?;

    let _interfaces_count = reader.read_u2()?;
    for _ in 0.._interfaces_count {
        reader.read_u2()?;
    }

    let mut static_fields = Vec::new();
    let fields_count = reader.read_u2()?;
    for _ in 0..fields_count {
        let access_flags = reader.read_u2()?;
        let name_index = reader.read_u2()?;
        let descriptor_index = reader.read_u2()?;
        let attributes_count = reader.read_u2()?;

        let name = constant_pool.utf8(name_index)?;
        let descriptor = constant_pool.utf8(descriptor_index)?;

        if access_flags & ACC_STATIC != 0 {
            if let Ok(java_type) = parse_field_descriptor(descriptor) {
                static_fields.push((name.to_string(), java_type));
            }
        }

        skip_attributes(&mut reader, attributes_count)?;
    }

    let mut static_methods = Vec::new();
    let methods_count = reader.read_u2()?;
    for _ in 0..methods_count {
        let access_flags = reader.read_u2()?;
        let name_index = reader.read_u2()?;
        let descriptor_index = reader.read_u2()?;
        let attributes_count = reader.read_u2()?;

        let name = constant_pool.utf8(name_index)?;
        let descriptor = constant_pool.utf8(descriptor_index)?;

        if access_flags & ACC_STATIC != 0 && name != "<clinit>" {
            if let Ok(signature) = parse_method_descriptor(descriptor) {
                static_methods.push((name.to_string(), signature));
            }
        }

        skip_attributes(&mut reader, attributes_count)?;
    }

    // Skip class attributes (not required for symbol metadata)
    let attributes_count = reader.read_u2()?;
    skip_attributes(&mut reader, attributes_count)?;

    let class_name = constant_pool.class_name(this_class)?;
    let fqcn = class_name.replace('/', ".");
    let package = fqcn
        .rsplit_once('.')
        .map(|(pkg, _)| pkg.to_string())
        .unwrap_or_else(|| String::new());

    Ok(ParsedClass {
        fqcn,
        package,
        static_fields,
        static_methods,
    })
}

pub fn parse_module_info(bytes: &[u8]) -> Result<ModuleInfo, ClassParseError> {
    let mut reader = ClassReader::new(bytes);
    reader.expect_magic()?;
    let _minor_version = reader.read_u2()?;
    let _major_version = reader.read_u2()?;
    let constant_pool = ConstantPool::parse(&mut reader)?;

    let _access_flags = reader.read_u2()?;
    let _this_class = reader.read_u2()?;
    let _super_class = reader.read_u2()?;
    let interfaces_count = reader.read_u2()?;
    for _ in 0..interfaces_count {
        reader.read_u2()?;
    }

    let fields = reader.read_u2()?;
    for _ in 0..fields {
        skip_member(&mut reader)?;
    }

    let methods = reader.read_u2()?;
    for _ in 0..methods {
        skip_member(&mut reader)?;
    }

    let attributes_count = reader.read_u2()?;
    let mut module_info: Option<ModuleInfo> = None;
    for _ in 0..attributes_count {
        let attribute_name_index = reader.read_u2()?;
        let attribute_length = reader.read_u4()? as usize;
        let attribute_name = constant_pool.utf8(attribute_name_index)?;
        if attribute_name == "Module" {
            let slice = reader.read_slice(attribute_length)?;
            let mut sub_reader = ClassReader::new(slice);
            let module_name_index = sub_reader.read_u2()?;
            let _module_flags = sub_reader.read_u2()?;
            let _module_version_index = sub_reader.read_u2()?;

            let requires_count = sub_reader.read_u2()?;
            for _ in 0..requires_count {
                sub_reader.read_u2()?; // requires_index
                sub_reader.read_u2()?; // requires_flags
                sub_reader.read_u2()?; // requires_version_index
            }

            let exports_count = sub_reader.read_u2()?;
            let mut exports = HashSet::new();
            for _ in 0..exports_count {
                let exports_index = sub_reader.read_u2()?;
                sub_reader.read_u2()?; // exports_flags
                let exports_to_count = sub_reader.read_u2()?;
                for _ in 0..exports_to_count {
                    sub_reader.read_u2()?;
                }

                let pkg = constant_pool.package_name(exports_index)?;
                exports.insert(pkg.replace('/', "."));
            }

            let opens_count = sub_reader.read_u2()?;
            for _ in 0..opens_count {
                sub_reader.read_u2()?; // opens_index
                sub_reader.read_u2()?; // opens_flags
                let opens_to_count = sub_reader.read_u2()?;
                for _ in 0..opens_to_count {
                    sub_reader.read_u2()?;
                }
            }

            let uses_count = sub_reader.read_u2()?;
            for _ in 0..uses_count {
                sub_reader.read_u2()?;
            }

            let provides_count = sub_reader.read_u2()?;
            for _ in 0..provides_count {
                sub_reader.read_u2()?; // provides_index
                let impl_count = sub_reader.read_u2()?;
                for _ in 0..impl_count {
                    sub_reader.read_u2()?;
                }
            }

            let module_name = constant_pool.module_name(module_name_index)?;
            module_info = Some(ModuleInfo {
                name: module_name,
                exports,
            });

            // The attribute slice has been fully read; no need to skip further bytes.
        } else {
            reader.skip(attribute_length)?;
        }
    }

    module_info.ok_or_else(|| {
        ClassParseError::InvalidDescriptor("module-info missing Module attribute".into())
    })
}

#[derive(Debug, Clone)]
enum Constant {
    Utf8(String),
    Class { name_index: u16 },
    Module { name_index: u16 },
    Package { name_index: u16 },
    Other,
    Unusable,
}

struct ConstantPool {
    entries: Vec<Constant>,
}

impl ConstantPool {
    fn parse(reader: &mut ClassReader<'_>) -> Result<Self, ClassParseError> {
        let count = reader.read_u2()? as usize;
        let mut entries = Vec::with_capacity(count);
        entries.push(Constant::Unusable); // index 0 unused

        let mut index = 1;
        while index < count {
            let tag = reader.read_u1()?;
            let entry = match tag {
                1 => {
                    let length = reader.read_u2()? as usize;
                    let bytes = reader.read_slice(length)?;
                    let string = String::from_utf8(bytes.to_vec())?;
                    Constant::Utf8(string)
                }
                3 | 4 => {
                    reader.skip(4)?;
                    Constant::Other
                }
                5 | 6 => {
                    reader.skip(8)?;
                    entries.push(Constant::Unusable);
                    index += 1;
                    Constant::Other
                }
                7 => {
                    let name_index = reader.read_u2()?;
                    Constant::Class { name_index }
                }
                8 => {
                    reader.read_u2()?; // string index
                    Constant::Other
                }
                9 | 10 | 11 => {
                    reader.skip(4)?;
                    Constant::Other
                }
                12 => {
                    reader.read_u2()?;
                    reader.read_u2()?;
                    Constant::Other
                }
                15 => {
                    reader.skip(3)?;
                    Constant::Other
                }
                16 => {
                    reader.read_u2()?;
                    Constant::Other
                }
                18 => {
                    reader.skip(4)?;
                    Constant::Other
                }
                19 => {
                    let name_index = reader.read_u2()?;
                    Constant::Module { name_index }
                }
                20 => {
                    let name_index = reader.read_u2()?;
                    Constant::Package { name_index }
                }
                other => return Err(ClassParseError::UnsupportedConstant { tag: other }),
            };

            entries.push(entry);
            index += 1;
        }

        Ok(Self { entries })
    }

    fn get(&self, index: u16) -> Result<&Constant, ClassParseError> {
        self.entries
            .get(index as usize)
            .ok_or(ClassParseError::InvalidConstantIndex { index })
    }

    fn utf8(&self, index: u16) -> Result<&str, ClassParseError> {
        match self.get(index)? {
            Constant::Utf8(value) => Ok(value.as_str()),
            _ => Err(ClassParseError::InvalidConstantIndex { index }),
        }
    }

    fn class_name(&self, index: u16) -> Result<String, ClassParseError> {
        match self.get(index)? {
            Constant::Class { name_index } => Ok(self.utf8(*name_index)?.to_string()),
            _ => Err(ClassParseError::InvalidConstantIndex { index }),
        }
    }

    fn package_name(&self, index: u16) -> Result<String, ClassParseError> {
        match self.get(index)? {
            Constant::Package { name_index } => Ok(self.utf8(*name_index)?.to_string()),
            Constant::Class { name_index } => Ok(self.utf8(*name_index)?.to_string()),
            _ => Err(ClassParseError::InvalidConstantIndex { index }),
        }
    }

    fn module_name(&self, index: u16) -> Result<String, ClassParseError> {
        match self.get(index)? {
            Constant::Module { name_index } => Ok(self.utf8(*name_index)?.to_string()),
            _ => Err(ClassParseError::InvalidConstantIndex { index }),
        }
    }
}

struct ClassReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> ClassReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn expect_magic(&mut self) -> Result<(), ClassParseError> {
        const MAGIC: u32 = 0xCAFEBABE;
        let magic = self.read_u4()?;
        if magic != MAGIC {
            return Err(ClassParseError::InvalidMagic);
        }
        Ok(())
    }

    fn read_u1(&mut self) -> Result<u8, ClassParseError> {
        if self.pos >= self.data.len() {
            return Err(ClassParseError::UnexpectedEof);
        }
        let value = self.data[self.pos];
        self.pos += 1;
        Ok(value)
    }

    fn read_u2(&mut self) -> Result<u16, ClassParseError> {
        if self.pos + 2 > self.data.len() {
            return Err(ClassParseError::UnexpectedEof);
        }
        let value = u16::from_be_bytes([self.data[self.pos], self.data[self.pos + 1]]);
        self.pos += 2;
        Ok(value)
    }

    fn read_u4(&mut self) -> Result<u32, ClassParseError> {
        if self.pos + 4 > self.data.len() {
            return Err(ClassParseError::UnexpectedEof);
        }
        let value = u32::from_be_bytes([
            self.data[self.pos],
            self.data[self.pos + 1],
            self.data[self.pos + 2],
            self.data[self.pos + 3],
        ]);
        self.pos += 4;
        Ok(value)
    }

    fn read_slice(&mut self, len: usize) -> Result<&'a [u8], ClassParseError> {
        if self.pos + len > self.data.len() {
            return Err(ClassParseError::UnexpectedEof);
        }
        let slice = &self.data[self.pos..self.pos + len];
        self.pos += len;
        Ok(slice)
    }

    fn skip(&mut self, len: usize) -> Result<(), ClassParseError> {
        if self.pos + len > self.data.len() {
            return Err(ClassParseError::UnexpectedEof);
        }
        self.pos += len;
        Ok(())
    }
}

fn skip_attributes(reader: &mut ClassReader<'_>, count: u16) -> Result<(), ClassParseError> {
    for _ in 0..count {
        reader.read_u2()?; // attribute_name_index
        let length = reader.read_u4()? as usize;
        reader.skip(length)?;
    }
    Ok(())
}

fn skip_member(reader: &mut ClassReader<'_>) -> Result<(), ClassParseError> {
    reader.read_u2()?; // access_flags
    reader.read_u2()?; // name_index
    reader.read_u2()?; // descriptor_index
    let attributes_count = reader.read_u2()?;
    skip_attributes(reader, attributes_count)?;
    Ok(())
}

fn parse_field_descriptor(descriptor: &str) -> Result<JavaType, ClassParseError> {
    let mut parser = DescriptorParser::new(descriptor);
    let ty = parser.parse_type()?;
    if parser.remaining() != 0 {
        return Err(ClassParseError::InvalidDescriptor(descriptor.to_string()));
    }
    Ok(ty)
}

fn parse_method_descriptor(descriptor: &str) -> Result<JavaMethodSignature, ClassParseError> {
    let mut parser = DescriptorParser::new(descriptor);
    parser.expect('(')?;
    let mut parameters = Vec::new();
    while !parser.peek_char(')')? {
        parameters.push(parser.parse_type()?);
    }
    parser.expect(')')?;
    let return_type = if parser.peek_char('V')? {
        parser.advance(1);
        JavaType::Void
    } else {
        parser.parse_type()?
    };

    if parser.remaining() != 0 {
        return Err(ClassParseError::InvalidDescriptor(descriptor.to_string()));
    }

    Ok(JavaMethodSignature {
        parameters,
        return_type,
    })
}

struct DescriptorParser<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> DescriptorParser<'a> {
    fn new(descriptor: &'a str) -> Self {
        Self {
            bytes: descriptor.as_bytes(),
            pos: 0,
        }
    }

    fn remaining(&self) -> usize {
        self.bytes.len().saturating_sub(self.pos)
    }

    fn expect(&mut self, ch: char) -> Result<(), ClassParseError> {
        if self.remaining() < 1 {
            return Err(ClassParseError::InvalidDescriptor(String::new()));
        }
        if self.bytes[self.pos] != ch as u8 {
            return Err(ClassParseError::InvalidDescriptor(format!(
                "expected '{}' in descriptor",
                ch
            )));
        }
        self.pos += 1;
        Ok(())
    }

    fn advance(&mut self, count: usize) {
        self.pos += count;
    }

    fn peek_char(&self, ch: char) -> Result<bool, ClassParseError> {
        if self.remaining() < 1 {
            return Err(ClassParseError::InvalidDescriptor(String::new()));
        }
        Ok(self.bytes[self.pos] == ch as u8)
    }

    fn parse_type(&mut self) -> Result<JavaType, ClassParseError> {
        if self.remaining() == 0 {
            return Err(ClassParseError::InvalidDescriptor(String::new()));
        }

        let start = self.bytes[self.pos];
        match start {
            b'B' => {
                self.pos += 1;
                Ok(JavaType::Primitive("byte".to_string()))
            }
            b'C' => {
                self.pos += 1;
                Ok(JavaType::Primitive("char".to_string()))
            }
            b'D' => {
                self.pos += 1;
                Ok(JavaType::Primitive("double".to_string()))
            }
            b'F' => {
                self.pos += 1;
                Ok(JavaType::Primitive("float".to_string()))
            }
            b'I' => {
                self.pos += 1;
                Ok(JavaType::Primitive("int".to_string()))
            }
            b'J' => {
                self.pos += 1;
                Ok(JavaType::Primitive("long".to_string()))
            }
            b'S' => {
                self.pos += 1;
                Ok(JavaType::Primitive("short".to_string()))
            }
            b'Z' => {
                self.pos += 1;
                Ok(JavaType::Primitive("boolean".to_string()))
            }
            b'L' => self.parse_reference_type(),
            b'[' => self.parse_array_type(),
            _ => Err(ClassParseError::InvalidDescriptor(format!(
                "unexpected descriptor tag '{}'",
                start as char
            ))),
        }
    }

    fn parse_reference_type(&mut self) -> Result<JavaType, ClassParseError> {
        self.expect('L')?;
        let start = self.pos;
        while self.pos < self.bytes.len() && self.bytes[self.pos] != b';' {
            self.pos += 1;
        }
        if self.pos >= self.bytes.len() {
            return Err(ClassParseError::InvalidDescriptor(
                "unterminated reference descriptor".into(),
            ));
        }
        let name = &self.bytes[start..self.pos];
        self.pos += 1; // consume ';'
        let fqcn = String::from_utf8(name.to_vec())?.replace('/', ".");
        Ok(JavaType::Reference {
            name: fqcn,
            generic_args: Vec::new(),
        })
    }

    fn parse_array_type(&mut self) -> Result<JavaType, ClassParseError> {
        let mut dimensions = 0;
        while self.remaining() > 0 && self.bytes[self.pos] == b'[' {
            dimensions += 1;
            self.pos += 1;
        }
        let element = self.parse_type()?;
        Ok(JavaType::Array {
            element_type: Box::new(element),
            dimensions,
        })
    }
}
