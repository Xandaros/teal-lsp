use std::{cell::RefCell, collections::HashMap, convert::TryFrom, rc::Rc};

use anyhow::{Error, Result};
use once_cell::{sync::Lazy, unsync::OnceCell};
use tower_lsp::lsp_types::Diagnostic;
use tree_sitter::{Language, Node, Query, TreeCursor};

use crate::Document;

macro_rules! node_kind {
    ($decl:ident, $name:literal) => {
        static $decl: Lazy<u16> = Lazy::new(|| LANGUAGE.id_for_node_kind($name, true));
    };
}

macro_rules! field_kind {
    ($decl:ident, $name:literal) => {
        static $decl: Lazy<u16> = Lazy::new(|| LANGUAGE.field_id_for_name($name).unwrap());
    };
}

static LANGUAGE: Lazy<Language> = Lazy::new(|| tree_sitter_teal::language());
node_kind!(PROGRAM, "program");
node_kind!(RECORD_DECLARATION, "record_declaration");
node_kind!(IDENTIFIER, "identifier");
node_kind!(TYPEARGS, "typeargs");
node_kind!(RECORD_BODY, "record_body");
node_kind!(RECORD_ARRAY_TYPE, "record_array_type");
node_kind!(RECORD_ENTRY, "record_entry");
node_kind!(SIMPLE_TYPE, "simple_type");
node_kind!(TYPE_INDEX, "type_index");
node_kind!(TABLE_TYPE, "table_type");
node_kind!(FUNCTION_TYPE, "function_type");
node_kind!(ARG, "arg");
node_kind!(VARARGS, "varargs");
field_kind!(KEY, "key");
field_kind!(STRING_KEY, "string_key");
field_kind!(CONTENT, "content");
field_kind!(TYPE, "type");
field_kind!(NAME, "name");
field_kind!(ARGUMENTS, "arguments");
field_kind!(RETURN_TYPE, "return_type");

trait Parsable: Sized {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self>;
}

#[derive(Debug, Clone)]
struct FunctionType {
    arguments: Vec<Type>,
    varargs: Option<Box<Type>>,
    return_types: Vec<Type>,
}

#[derive(Clone, Debug)]
enum Type {
    Basic(BasicType),
    Array(BasicType),
    Tuple(Vec<BasicType>),
    Map(Box<Type>, Box<Type>),
    Function(FunctionType),
    UserDefined(usize),
    Unresolved(String),
}

#[derive(Clone, Copy, Debug)]
enum BasicType {
    Any,
    Nil,
    Boolean,
    Integer,
    Number,
    String,
    Thread,
}

impl TryFrom<&str> for BasicType {
    type Error = ();

    fn try_from(s: &str) -> ::std::result::Result<Self, ()> {
        Ok(match s {
            "any" => Self::Any,
            "nil" => Self::Nil,
            "boolean" => Self::Boolean,
            "integer" => Self::Integer,
            "number" => Self::Number,
            "string" => Self::String,
            "thread" => Self::Thread,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug)]
struct EnumDeclaration {
    name: String,
    id: usize,
    members: Vec<Type>,
}

#[derive(Clone, Debug)]
struct RecordDeclaration {
    name: String,
    array_type: Option<Type>,
    type_args: Vec<String>,
    members: HashMap<String, RecordMember>,
}

#[derive(Clone, Debug)]
struct RecordField {
    name: String,
    typ: Type,
}

#[derive(Clone, Debug)]
enum RecordMember {
    Field(RecordField),
    Declaration(Declaration),
}

#[derive(Clone, Debug)]
enum Declaration {
    Enum(EnumDeclaration),
    Record(RecordDeclaration),
}

#[derive(Clone, Debug)]
struct Scope {
    declarations: HashMap<String, usize>,
    variables: HashMap<String, Type>,
    parent: Option<Rc<Scope>>,
}

impl Parsable for RecordField {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self> {
        let node = cursor.node();
        let key = node
            .child_by_field_id(*KEY)
            .or_else(|| node.child_by_field_id(*STRING_KEY))
            .unwrap_or_else(|| panic!("Could not get key for node {:?}", node));
        let name = if key.kind_id() == *IDENTIFIER {
            if let Ok(text) = key.utf8_text(document.source.as_bytes()) {
                text
            } else {
                // TODO: emit warning
                ""
            }
        } else {
            if let Some(node) = key.child_by_field_id(*CONTENT) {
                if let Ok(text) = node.utf8_text(document.source.as_bytes()) {
                    text
                } else {
                    // TODO: Emit warning
                    ""
                }
            } else {
                ""
            }
        };

        let typ = node.child_by_field_id(*TYPE).unwrap();

        Ok(RecordField {
            name: name.to_string(),
            typ: Type::parse(document, &mut typ.walk(), types)?,
        })
    }
}

impl Parsable for RecordDeclaration {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self> {
        let node = cursor.node();
        let name = node
            .child_by_field_id(*NAME)
            .unwrap_or_else(|| panic!("No name for record {:?}", node))
            .utf8_text(document.source.as_bytes())
            .unwrap_or_else(|_| panic!("Could not decode record name for {:?}", node));

        let mut type_args = Vec::new();
        let mut members: HashMap<String, RecordMember> = HashMap::new();
        let mut array_type = None;

        cursor.goto_first_child();
        loop {
            let node = cursor.node();

            if node.kind_id() == *TYPEARGS {
                type_args = Vec::with_capacity(node.child_count());
                cursor.goto_first_child();

                loop {
                    let node = cursor.node();

                    if node.kind_id() == *IDENTIFIER {
                        match node.utf8_text(document.source.as_bytes()) {
                            Ok(text) => type_args.push(text.to_string()),
                            Err(_) => {
                                type_args.push("#ERROR#".to_string());
                                //TODO: Emit warning
                            }
                        }
                    }

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }

                cursor.goto_parent();
            } else if node.kind_id() == *RECORD_BODY {
                members = HashMap::with_capacity(node.child_count());
                cursor.goto_first_child();

                loop {
                    let node = cursor.node();

                    if node.kind_id() == *RECORD_ARRAY_TYPE {
                        cursor.goto_first_child();
                        match cursor.node().utf8_text(document.source.as_bytes()) {
                            Ok(typ) => {
                                array_type = Some(Type::parse(document, cursor, types)?);
                            }
                            Err(_) => {
                                //TODO: Emit warning
                            }
                        }
                        cursor.goto_parent();
                    } else if node.kind_id() == *RECORD_ENTRY {
                        let member = RecordField::parse(document, &mut node.walk(), types)?;
                        members.insert(member.name.clone(), RecordMember::Field(member));
                    }

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }

                cursor.goto_parent();
            }

            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();

        let decl = RecordDeclaration {
            name: name.to_string(),
            array_type,
            members,
            type_args,
        };
        println!("{:?}", decl);
        Ok(decl)
    }
}

impl Parsable for Type {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self> {
        // _type: $ => prec(2, choice(
        //   $.simple_type,
        //   $.type_index,
        //   $.table_type,
        //   $.function_type,
        //   $.type_union,
        //   seq("(", $._type, ")")
        // )),
        println!("Parsing type: {:?}", cursor.node().to_sexp());

        let node = cursor.node();

        if node.kind_id() == *SIMPLE_TYPE {
            let text = node.utf8_text(document.source.as_bytes())?;

            if let Ok(typ) = BasicType::try_from(text) {
                Ok(Type::Basic(typ))
            } else {
                todo!()
            }
        } else if node.kind_id() == *TYPE_INDEX {
            todo!();
        } else if node.kind_id() == *TABLE_TYPE {
            cursor.goto_first_child();

            let key_type = Type::parse(document, cursor, types)?;
            cursor.goto_next_sibling();
            let value_type = Type::parse(document, cursor, types)?;

            cursor.goto_parent();
            Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
        } else if node.kind_id() == *FUNCTION_TYPE {
            // Parse arguments
            let arguments = node.child_by_field_id(*ARGUMENTS).unwrap();

            let mut arg_list = Vec::with_capacity(arguments.named_child_count());
            let mut varargs = None;
            let mut type_cursor: Option<TreeCursor> = None;
            for argument in arguments.named_children(&mut node.walk()) {
                let typ = argument.child_by_field_id(*TYPE).unwrap();
                let mut type_cursor = if let Some(ref mut cursor) = type_cursor {
                    cursor.reset(typ);
                    cursor
                } else {
                    type_cursor = Some(typ.walk());
                    type_cursor.as_mut().unwrap()
                };

                if let Ok(typ) = Type::parse(document, &mut type_cursor, types) {
                    if argument.kind_id() == *ARG {
                        arg_list.push(typ);
                    } else if argument.kind_id() == *VARARGS {
                        varargs = Some(Box::new(typ));
                    }
                } else {
                    // TODO: emit error
                }
            }

            // Parse return types
            let mut return_list = Vec::new();
            if let Some(return_types) = node.child_by_field_id(*RETURN_TYPE) {
                return_list = Vec::with_capacity(return_types.named_child_count());

                for return_type in return_types.named_children(&mut return_types.walk()) {
                    let mut type_cursor = if let Some(ref mut cursor) = type_cursor {
                        cursor.reset(return_type);
                        cursor
                    } else {
                        type_cursor = Some(return_type.walk());
                        type_cursor.as_mut().unwrap()
                    };

                    if let Ok(typ) = Type::parse(document, &mut type_cursor, types) {
                        return_list.push(typ);
                    } else {
                        // TODO: emit error
                    }
                }
            }
            Ok(Type::Function(FunctionType {
                arguments: arg_list,
                varargs,
                return_types: return_list,
            }))
        } else {
            todo!()
        }
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            declarations: HashMap::new(),
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn find_variable(&self, name: &str) -> Option<Type> {
        if let Some(typ) = self.variables.get(name) {
            return Some(typ.clone());
        }
        if let Some(ref parent) = self.parent {
            parent.find_variable(name)
        } else {
            None
        }
    }

    fn find_type(&self, name: &str) -> Option<usize> {
        if let Some(declaration) = self.declarations.get(name) {
            return Some(declaration.clone());
        }
        if let Some(ref parent) = self.parent {
            parent.find_type(name)
        } else {
            None
        }
    }

    fn child(self: Rc<Self>) -> Rc<Self> {
        let mut ret = Self::new();
        ret.parent = Some(self);
        Rc::new(ret)
    }
}

impl Document {
    pub(crate) fn check_types(&self) -> Vec<Diagnostic> {
        fn do_check(
            document: &Document,
            cursor: &mut TreeCursor,
            types: &mut Vec<Declaration>,
            scope: &mut Scope,
        ) -> Vec<Diagnostic> {
            let ret = Vec::new();

            let node = cursor.node();
            if node.kind_id() == *PROGRAM {
                cursor.goto_first_child();
                do_check(document, cursor, types, scope);
                cursor.goto_parent();
            } else if node.kind_id() == *RECORD_DECLARATION {
                if let Ok(decl) = RecordDeclaration::parse(document, cursor, types) {
                    let name = decl.name.clone();
                    types.push(Declaration::Record(decl));
                    scope.declarations.insert(name, types.len() - 1);
                }
            }

            ret
        }

        let mut global_scope = Scope::new();
        let mut types = Vec::new();
        do_check(self, &mut self.tree.walk(), &mut types, &mut global_scope)
    }
}
