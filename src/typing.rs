use std::{cell::RefCell, collections::HashMap, convert::TryFrom, mem, ops::Deref, rc::Rc};

use anyhow::{Error, Result};
use once_cell::{sync::Lazy, unsync::OnceCell};
use tower_lsp::lsp_types::*;
use tree_sitter::{Language, Node, Query, Range, TreeCursor};

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
node_kind!(FIELD, "field");
node_kind!(SIMPLE_TYPE, "simple_type");
node_kind!(TYPE_INDEX, "type_index");
node_kind!(TABLE_TYPE, "table_type");
node_kind!(FUNCTION_TYPE, "function_type");
node_kind!(TYPE_UNION, "type_union");
node_kind!(ARG, "arg");
node_kind!(VARARGS, "varargs");
node_kind!(METAMETHOD, "metamethod");
field_kind!(KEY, "key");
field_kind!(STRING_KEY, "string_key");
field_kind!(CONTENT, "content");
field_kind!(TYPE, "type");
field_kind!(NAME, "name");
field_kind!(ARGUMENTS, "arguments");
field_kind!(RETURN_TYPE, "return_type");

fn mk_diagnostic(range: Range, message: String, severity: DiagnosticSeverity) -> Diagnostic {
    let mut ret = Diagnostic::default();
    ret.source = Some("teal-lsp".to_string());
    ret.range = tower_lsp::lsp_types::Range::new(
        Position {
            line: range.start_point.row as u32,
            character: range.start_point.column as u32,
        },
        Position {
            line: range.end_point.row as u32,
            character: range.end_point.column as u32,
        },
    );
    ret.message = message;
    ret.severity = Some(severity);
    ret
}

enum TypeLookupError {
    NotFound,
    Unloaded(usize),
}

#[derive(Debug, Clone)]
struct FunctionType {
    arguments: Vec<Type>,
    varargs: Option<Box<Type>>,
    return_types: Vec<Type>,
}

#[derive(Clone, Debug)]
enum Type {
    Any,
    Basic(BasicType),
    Array(BasicType),
    Tuple(Vec<BasicType>),
    Map(Box<Type>, Box<Type>),
    Function(FunctionType),
    UserDefined(usize),
    Union(Box<Type>, Box<Type>),
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

#[derive(Debug)]
struct Scope {
    declarations: HashMap<String, usize>,
    unloaded_declarations: HashMap<String, (usize, usize)>,
    variables: HashMap<String, Type>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn resolve(
        scope: Rc<RefCell<Self>>,
        document: &Document,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node>,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let unloaded_declarations = scope.borrow().unloaded_declarations.clone();
        for (name, (node_id, type_id)) in unloaded_declarations.into_iter() {
            if Scope::resolve_one(
                Rc::clone(&scope),
                document,
                types,
                nodes,
                diagnostics,
                name.clone(),
            )
            .is_none()
            {
                diagnostics.push(mk_diagnostic(
                    nodes.get(&node_id).unwrap().range(),
                    format!("Type not found: {}", name),
                    DiagnosticSeverity::Error,
                ));
            }
        }
    }

    fn resolve_one(
        scope: Rc<RefCell<Self>>,
        document: &Document,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node>,
        diagnostics: &mut Vec<Diagnostic>,
        name: String,
    ) -> Option<usize> {
        println!("Resolving: {}", name);
        if let Some(typeid) = scope.borrow().declarations.get(&name) {
            return Some(*typeid);
        }
        let decl_entry = { scope.borrow().unloaded_declarations.get(&name).map(|x| *x) };
        if let Some((node_id, id)) = decl_entry {
            let node = nodes.get(&node_id).unwrap();

            if node.kind_id() == *RECORD_DECLARATION {
                scope.borrow_mut().unloaded_declarations.remove(&name);
                scope.borrow_mut().declarations.insert(name.clone(), id);
                let record = RecordDeclaration::parse(
                    document,
                    &mut node.walk(),
                    Rc::clone(&scope),
                    types,
                    nodes,
                    diagnostics,
                )
                .ok()?;
                let decl = Declaration::Record(record);
                types[id] = Some(decl);
                println!("Finished resolving {}", name);
                return Some(id);
            } else {
                todo!()
            }
        } else if let Some(parent) = scope.borrow().parent.clone() {
            Scope::resolve_one(parent, document, types, nodes, diagnostics, name)
        } else {
            println!("Failed to resolve: {}", name);
            None
        }
    }
}

impl RecordField {
    fn parse<'a>(
        document: &Document,
        cursor: &mut TreeCursor<'a>,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node>,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Result<Self> {
        println!("Parse record field");
        let node = cursor.node();
        let key = node
            .child_by_field_id(*KEY)
            .or_else(|| node.child_by_field_id(*STRING_KEY))
            .unwrap_or_else(|| panic!("Could not get key for node {:?}", node));
        let name = if key.kind_id() == *IDENTIFIER {
            if let Ok(text) = key.utf8_text(document.source.as_bytes()) {
                text
            } else {
                diagnostics.push(mk_diagnostic(
                    key.range(),
                    "UTF-8 decode failed".to_string(),
                    DiagnosticSeverity::Error,
                ));
                ""
            }
        } else {
            if let Some(node) = key.child_by_field_id(*CONTENT) {
                if let Ok(text) = node.utf8_text(document.source.as_bytes()) {
                    text
                } else {
                    diagnostics.push(mk_diagnostic(
                        node.range(),
                        "UTF-8 decode failed".to_string(),
                        DiagnosticSeverity::Error,
                    ));
                    ""
                }
            } else {
                ""
            }
        };

        let typ = node.child_by_field_id(*TYPE).unwrap();

        Ok(RecordField {
            name: name.to_string(),
            typ: Type::parse(document, &mut typ.walk(), scope, types, nodes, diagnostics)?,
        })
    }
}

impl RecordDeclaration {
    fn get_name<'a>(document: &'a Document, node: &Node) -> &'a str {
        node.child_by_field_id(*NAME)
            .unwrap_or_else(|| panic!("No name for record {:?}", node))
            .utf8_text(document.source.as_bytes())
            .unwrap_or_else(|_| panic!("Could not decode record name for {:?}", node))
    }

    fn resolve(
        &mut self,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        for (name, member) in self.members.iter_mut() {
            if let RecordMember::Declaration(decl) = member {
                if let Declaration::Record(ref mut record_decl) = decl {
                    record_decl.resolve(Rc::clone(&scope), types, diagnostics);
                }
            }
        }
    }
}

impl RecordDeclaration {
    fn parse<'a>(
        document: &Document,
        cursor: &mut TreeCursor<'a>,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node<'a>>,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Result<Self> {
        println!("Parse record decl");
        let node = cursor.node();
        let name = Self::get_name(document, &node);
        let id = types.len();

        let mut type_args = Vec::new();
        let mut members: HashMap<String, RecordMember> = HashMap::new();
        let mut declarations: HashMap<String, Declaration> = HashMap::new();
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
                                diagnostics.push(mk_diagnostic(
                                    node.range(),
                                    "UTF-8 decode failed".to_string(),
                                    DiagnosticSeverity::Error,
                                ));
                            }
                        }
                    }

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }

                cursor.goto_parent();
            } else if node.kind_id() == *RECORD_BODY {
                let mut record_scope = Scope::child(Rc::clone(&scope));

                // Parse records, enums, and typedefs first
                {
                    cursor.goto_first_child();

                    loop {
                        let node = cursor.node();

                        if node.kind_id() == *RECORD_DECLARATION {
                            check_type(
                                document,
                                cursor,
                                Rc::clone(&record_scope),
                                types,
                                nodes,
                                diagnostics,
                                false,
                            );
                        }

                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }

                    cursor.goto_parent();
                }

                // ...then everything else
                {
                    cursor.goto_first_child();

                    loop {
                        let node = cursor.node();

                        if node.kind_id() == *RECORD_ARRAY_TYPE {
                            cursor.goto_first_child();
                            match cursor.node().utf8_text(document.source.as_bytes()) {
                                Ok(typ) => {
                                    array_type = Some(Type::parse(
                                        document,
                                        cursor,
                                        Rc::clone(&record_scope),
                                        types,
                                        nodes,
                                        diagnostics,
                                    )?);
                                }
                                Err(_) => {
                                    diagnostics.push(mk_diagnostic(
                                        cursor.node().range(),
                                        "UTF-8 decode failed".to_string(),
                                        DiagnosticSeverity::Error,
                                    ));
                                }
                            }
                            cursor.goto_parent();
                        } else if node.kind_id() == *FIELD {
                            let member = RecordField::parse(
                                document,
                                &mut node.walk(),
                                Rc::clone(&record_scope),
                                types,
                                nodes,
                                diagnostics,
                            )?;
                            members.insert(member.name.clone(), RecordMember::Field(member));
                            nodes.insert(node.id(), node);
                        }

                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }

                    cursor.goto_parent();
                }
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
        Ok(decl)
    }
}

impl Type {
    fn parse<'a>(
        document: &Document,
        cursor: &mut TreeCursor<'a>,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node>,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Result<Self> {
        println!("Parse type");
        // _type: $ => prec(2, choice(
        //   $.simple_type,
        //   $.type_index,
        //   $.table_type,
        //   $.function_type,
        //   $.type_union,
        //   seq("(", $._type, ")")
        // )),
        let node = cursor.node();

        if node.kind_id() == *SIMPLE_TYPE {
            let text = node.utf8_text(document.source.as_bytes())?;

            if let Ok(typ) = BasicType::try_from(text) {
                Ok(Type::Basic(typ))
            } else {
                match scope.borrow().find_type(text) {
                    Ok(typeid) => Ok(Type::UserDefined(typeid)),
                    Err(TypeLookupError::Unloaded(node)) => {
                        println!("Loading type: {}", text);
                        if let Some(typeid) = Scope::resolve_one(
                            Rc::clone(&scope),
                            document,
                            types,
                            nodes,
                            diagnostics,
                            text.to_string(),
                        ) {
                            println!("Successfully resolved: {}", text);
                            println!("{:?}", scope);
                            Ok(Type::UserDefined(typeid))
                        } else {
                            todo!()
                        }
                    }
                    Err(TypeLookupError::NotFound) => {
                        println!("Could not look up type: {}", text);
                        println!("{:#?}", scope);
                        diagnostics.push(mk_diagnostic(
                            node.range(),
                            format!("Type not found: {}", text),
                            DiagnosticSeverity::Error,
                        ));
                        Ok(Type::Any)
                    }
                }
            }
        } else if node.kind_id() == *TYPE_INDEX {
            todo!();
        } else if node.kind_id() == *TABLE_TYPE {
            cursor.goto_first_child();

            let key_type = Type::parse(
                document,
                cursor,
                Rc::clone(&scope),
                types,
                nodes,
                diagnostics,
            )?;
            cursor.goto_next_sibling();
            let value_type = Type::parse(document, cursor, scope, types, nodes, diagnostics)?;

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

                if let Ok(typ) = Type::parse(
                    document,
                    &mut type_cursor,
                    Rc::clone(&scope),
                    types,
                    nodes,
                    diagnostics,
                ) {
                    if argument.kind_id() == *ARG {
                        arg_list.push(typ);
                        // arg_list.push(NodeType {
                        //     typ,
                        //     node: argument.clone(),
                        // });
                    } else if argument.kind_id() == *VARARGS {
                        varargs = Some(Box::new(typ));
                        // varargs = Some(Box::new(NodeType {
                        //     typ,
                        //     node: argument.clone(),
                        // }));
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

                    if let Ok(typ) = Type::parse(
                        document,
                        &mut type_cursor,
                        Rc::clone(&scope),
                        types,
                        nodes,
                        diagnostics,
                    ) {
                        return_list.push(typ);
                        // return_list.push(NodeType {
                        //     typ,
                        //     node: return_type.clone(),
                        // });
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
        } else if node.kind_id() == *TYPE_UNION {
            cursor.goto_first_child();

            let left = Type::parse(
                document,
                cursor,
                Rc::clone(&scope),
                types,
                nodes,
                diagnostics,
            )?;
            cursor.goto_next_sibling();
            cursor.goto_next_sibling();
            let right = Type::parse(document, cursor, scope, types, nodes, diagnostics)?;

            assert!(!cursor.goto_next_sibling());

            cursor.goto_parent();
            Ok(Type::Union(Box::new(left), Box::new(right)))
        } else {
            panic!("Unknown type node: {:?}", node);
        }
    }
}

impl<'a> Scope {
    fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            declarations: HashMap::new(),
            unloaded_declarations: HashMap::new(),
            variables: HashMap::new(),
            parent: None,
        }))
    }

    fn find_variable(&self, name: &str) -> Option<Type> {
        if let Some(typ) = self.variables.get(name) {
            return Some(typ.clone());
        }
        if let Some(ref parent) = self.parent {
            parent.borrow().find_variable(name)
        } else {
            None
        }
    }

    fn find_type(&self, name: &str) -> Result<usize, TypeLookupError> {
        println!("Find type: {}", name);
        println!("{:?}", self);
        if let Some((node, type_id)) = self.unloaded_declarations.get(name) {
            println!("Lazy: {}", name);
            return Err(TypeLookupError::Unloaded(*node));
        } else if let Some(declaration) = self.declarations.get(name) {
            return Ok(*declaration);
        }
        if let Some(ref parent) = self.parent {
            parent.borrow().find_type(name)
        } else {
            Err(TypeLookupError::NotFound)
        }
    }

    fn child(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Scope>> {
        let mut ret = Self::new();
        ret.borrow_mut().parent = Some(parent);
        ret
    }
}

fn check_type<'a>(
    document: &Document,
    cursor: &mut TreeCursor<'a>,
    global_scope: Rc<RefCell<Scope>>,
    types: &mut Vec<Option<Declaration>>,
    nodes: &mut HashMap<usize, Node<'a>>,
    diagnostics: &mut Vec<Diagnostic>,
    lazy: bool,
) {
    let node = cursor.node();
    if node.kind_id() == *PROGRAM {
        if !cursor.goto_first_child() {
            // Empty program
            return;
        }

        loop {
            check_type(
                document,
                cursor,
                Rc::clone(&global_scope),
                types,
                nodes,
                diagnostics,
                lazy,
            );

            if !cursor.goto_next_sibling() {
                break;
            }
        }

        cursor.goto_parent();
    } else if node.kind_id() == *RECORD_DECLARATION {
        let id = types.len();
        let name = RecordDeclaration::get_name(document, &node);
        nodes.insert(node.id(), node.clone());
        if lazy {
            println!("Inserting unloaded decl {}", name);
            types.push(None);
            global_scope
                .borrow_mut()
                .unloaded_declarations
                .insert(name.to_string(), (node.id(), id));
        } else {
            types.push(None);
            global_scope
                .borrow_mut()
                .declarations
                .insert(name.to_string(), id);
            if let Ok(decl) =
                RecordDeclaration::parse(document, cursor, global_scope, types, nodes, diagnostics)
            {
                let name = decl.name.clone();
                types[id] = Some(Declaration::Record(decl));
            }
        }
    }
}

impl Document {
    pub(crate) fn check_types(&self, diagnostics: &mut Vec<Diagnostic>) {
        let mut global_scope = Scope::new();
        let mut types = Vec::new();
        let mut nodes = HashMap::new();
        check_type(
            self,
            &mut self.tree.walk(),
            global_scope.clone(),
            &mut types,
            &mut nodes,
            diagnostics,
            true,
        );
        Scope::resolve(
            global_scope.clone(),
            self,
            &mut types,
            &mut nodes,
            diagnostics,
        );
    }
}
