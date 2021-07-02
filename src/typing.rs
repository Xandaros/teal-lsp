use std::{cell::RefCell, collections::HashMap, convert::TryFrom, rc::Rc};

use anyhow::{Error, Result};
use once_cell::sync::Lazy;
use tower_lsp::lsp_types::Diagnostic;
use tree_sitter::{Language, Node, Query, TreeCursor};

use crate::Document;

trait Parsable: Sized {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self>;
}

#[derive(Clone, Debug)]
enum Type {
    Basic(BasicType),
    Array(BasicType),
    Tuple(Vec<BasicType>),
    Map(Box<Type>, Box<Type>),
    Function(Vec<BasicType>, Vec<BasicType>),
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

macro_rules! node_kind {
    ($decl:ident, $name:literal) => {
        static $decl: Lazy<u16> = Lazy::new(|| LANGUAGE.id_for_node_kind($name, true));
    };
}

impl Parsable for RecordField {
    fn parse(
        document: &Document,
        cursor: &mut TreeCursor,
        types: &Vec<Declaration>,
    ) -> Result<Self> {
        static LANGUAGE: Lazy<Language> = Lazy::new(|| tree_sitter_teal::language());
        static KEY: Lazy<u16> = Lazy::new(|| LANGUAGE.field_id_for_name("key").unwrap());
        static STRING_KEY: Lazy<u16> =
            Lazy::new(|| LANGUAGE.field_id_for_name("string_key").unwrap());
        static CONTENT: Lazy<u16> = Lazy::new(|| LANGUAGE.field_id_for_name("content").unwrap());
        static TYPE: Lazy<u16> = Lazy::new(|| LANGUAGE.field_id_for_name("type").unwrap());
        node_kind!(IDENTIFIER, "identifier");

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
        static LANGUAGE: Lazy<Language> = Lazy::new(|| tree_sitter_teal::language());
        static NAME: Lazy<u16> = Lazy::new(|| LANGUAGE.field_id_for_name("name").unwrap());
        node_kind!(TYPEARGS, "typeargs");
        node_kind!(RECORD_BODY, "record_body");
        node_kind!(RECORD_ARRAY_TYPE, "record_array_type");
        node_kind!(RECORD_ENTRY, "record_entry");
        node_kind!(IDENTIFIER, "identifier");

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
        let text = node.utf8_text(document.source.as_bytes())?;

        println!("Got text: {}", text);

        if let Ok(typ) = BasicType::try_from(text) {
            return Ok(Type::Basic(typ));
        }

        Ok(Self::Basic(BasicType::Any)) // TODO
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
            static LANGUAGE: Lazy<Language> = Lazy::new(|| tree_sitter_teal::language());
            node_kind!(PROGRAM, "program");
            node_kind!(RECORD_DECLARATION, "record_declaration");

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
