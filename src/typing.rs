use std::{
    borrow::Cow, cell::RefCell, collections::HashMap, convert::TryFrom, mem, ops::Deref, rc::Rc,
};

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
node_kind!(NUMBER, "number");
node_kind!(TABLE_TYPE, "table_type");
node_kind!(FUNCTION_TYPE, "function_type");
node_kind!(TYPE_UNION, "type_union");
node_kind!(ARG, "arg");
node_kind!(VARARGS, "varargs");
node_kind!(METAMETHOD, "metamethod");
node_kind!(VAR_DECLARATION, "var_declaration");
node_kind!(VAR, "var");
node_kind!(ERROR, "ERROR");
field_kind!(KEY, "key");
field_kind!(STRING_KEY, "string_key");
field_kind!(CONTENT, "content");
field_kind!(TYPE, "type");
field_kind!(NAME, "name");
field_kind!(ARGUMENTS, "arguments");
field_kind!(RETURN_TYPE, "return_type");
field_kind!(SCOPE, "scope");
field_kind!(DECLARATORS, "declarators");
field_kind!(INITIALIZERS, "initializers");
field_kind!(TYPE_ANNOTATION, "type_annotation");

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
    Basic(BasicType),
    Array(BasicType),
    Tuple(Vec<BasicType>),
    Map(Box<Type>, Box<Type>),
    Function(FunctionType),
    UserDefined(usize),
    Union(Box<Type>, Box<Type>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    Declaration(usize),
}

#[derive(Clone, Debug)]
enum Declaration {
    Enum(EnumDeclaration),
    Record(RecordDeclaration),
}

#[derive(Debug)]
pub(crate) struct Scope {
    declarations: HashMap<String, usize>,
    unloaded_declarations: HashMap<String, (usize, usize)>,
    variables: HashMap<String, Option<Type>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

#[derive(Debug, Clone, Copy)]
enum DeclaredScope {
    Local,
    Global,
}

impl From<BasicType> for Type {
    fn from(basic: BasicType) -> Self {
        Self::Basic(basic)
    }
}

impl DeclaredScope {
    fn parse(document: &Document, node: Node, diagnostics: &mut Vec<Diagnostic>) -> Self {
        let text = node.utf8_text(document.source.as_bytes());
        match text {
            Ok("local") => DeclaredScope::Local,
            Ok("global") => DeclaredScope::Global,
            Ok(_) => unreachable!(),
            Err(_) => {
                diagnostics.push(mk_diagnostic(
                    node.range(),
                    "UTF-8 decode failed".to_string(),
                    DiagnosticSeverity::Error,
                ));
                DeclaredScope::Global
            }
        }
    }
}

impl BasicType {
    fn to_string(&self) -> &'static str {
        match *self {
            BasicType::Any => "any",
            BasicType::Nil => "nil",
            BasicType::Boolean => "boolean",
            BasicType::Integer => "integer",
            BasicType::Number => "number",
            BasicType::String => "string",
            BasicType::Thread => "thread",
        }
    }
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
        if let Some(typeid) = scope.borrow().declarations.get(&name) {
            return Some(*typeid);
        }
        let decl_entry = { scope.borrow().unloaded_declarations.get(&name).map(|x| *x) };
        if let Some((node_id, id)) = decl_entry {
            let node = nodes.get(&node_id).unwrap();

            if node.kind_id() == *RECORD_DECLARATION {
                scope.borrow_mut().unloaded_declarations.remove(&name);
                scope.borrow_mut().declarations.insert(name.clone(), id);
                RecordDeclaration::load(
                    document,
                    &mut node.walk(),
                    Rc::clone(&scope),
                    types,
                    nodes,
                    diagnostics,
                    id,
                )
                .ok()?;
                return Some(id);
            } else {
                todo!()
            }
        } else if let Some(parent) = scope.borrow().parent.clone() {
            Scope::resolve_one(parent, document, types, nodes, diagnostics, name)
        } else {
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
            if let RecordMember::Declaration(id) = member {
                let mut decl = types[*id].as_ref().unwrap().clone();
                if let Declaration::Record(ref mut record_decl) = decl {
                    record_decl.resolve(Rc::clone(&scope), types, diagnostics);
                }
                types[*id] = Some(decl);
            }
        }
    }

    fn load<'a>(
        document: &Document,
        cursor: &mut TreeCursor<'a>,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node<'a>>,
        diagnostics: &mut Vec<Diagnostic>,
        id: usize,
    ) -> Result<()> {
        let node = cursor.node();
        let name = Self::get_name(document, &node);

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
                            let id = types.len();
                            let name = RecordDeclaration::get_name(document, &node);
                            nodes.insert(node.id(), node.clone());
                            types.push(None);
                            record_scope
                                .borrow_mut()
                                .declarations
                                .insert(name.to_string(), id);
                            if let Ok(_) = RecordDeclaration::load(
                                document,
                                cursor,
                                Rc::clone(&record_scope),
                                types,
                                nodes,
                                diagnostics,
                                id,
                            ) {
                                members.insert(name.to_string(), RecordMember::Declaration(id));
                            }
                        }

                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }

                    cursor.goto_parent();
                }

                types[id] = Some(Declaration::Record(RecordDeclaration {
                    name: name.to_string(),
                    array_type: array_type.clone(),
                    type_args: type_args.clone(),
                    members: members.clone(),
                }));

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
        types[id] = Some(Declaration::Record(decl.clone()));
        Ok(())
    }
}

impl Type {
    fn to_string(&self, types: &mut Vec<Option<Declaration>>) -> Cow<str> {
        match *self {
            Type::Basic(kind) => Cow::from(kind.to_string()),
            Type::Array(_) => Cow::from("array"),
            Type::Tuple(_) => Cow::from("tuple"),
            Type::Map(_, _) => Cow::from("map"),
            Type::Function(_) => Cow::from("function"),
            Type::UserDefined(id) => match types[id].as_ref().unwrap() {
                Declaration::Enum(_) => todo!(),
                Declaration::Record(record) => Cow::from(format!("Record {}", record.name)),
            },
            Type::Union(_, _) => Cow::from("union"),
        }
    }

    fn unify<'a>(
        expected: &'a Type,
        actual: &'a Type,
        range: Range,
        types: &mut Vec<Option<Declaration>>,
        diagnostics: Option<&mut Vec<Diagnostic>>,
    ) -> &'a Type {
        use Type::*;
        match expected {
            Basic(BasicType::Any) => return actual,
            Basic(BasicType::Number) => {
                if let Basic(actual) = actual {
                    if *actual == BasicType::Number || *actual == BasicType::Integer {
                        return expected;
                    }
                }
            }
            Basic(basic) => {
                if let Basic(basic_actual) = actual {
                    if basic == basic_actual {
                        return actual;
                    }
                }
            }
            Array(_) => todo!(),
            Tuple(_) => todo!(),
            Map(_, _) => todo!(),
            Function(_) => todo!(),
            UserDefined(_) => todo!(),
            Union(left, right) => {}
        }
        if let Some(diagnostics) = diagnostics {
            diagnostics.push(mk_diagnostic(
                range,
                format!(
                    "mismatched types\nexpected `{}`, found `{}`",
                    expected.to_string(types),
                    actual.to_string(types)
                ),
                DiagnosticSeverity::Error,
            ));
        }
        expected
    }

    fn parse<'a>(
        document: &Document,
        cursor: &mut TreeCursor<'a>,
        scope: Rc<RefCell<Scope>>,
        types: &mut Vec<Option<Declaration>>,
        nodes: &mut HashMap<usize, Node>,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Result<Self> {
        // _type: $ => prec(2, choice(
        //   $.simple_type,
        //   $.type_index,
        //   $.table_type,
        //   $.function_type,
        //   $.type_union,
        //   seq("(", $._type, ")")
        // )),
        let node = cursor.node();

        if node.kind_id() == *IDENTIFIER {
            let text = node.utf8_text(document.source.as_bytes())?;

            if let Ok(typ) = BasicType::try_from(text) {
                Ok(Type::Basic(typ))
            } else {
                match scope.borrow().find_type(text) {
                    Ok(typeid) => Ok(Type::UserDefined(typeid)),
                    Err(TypeLookupError::Unloaded(node)) => {
                        if let Some(typeid) = Scope::resolve_one(
                            Rc::clone(&scope),
                            document,
                            types,
                            nodes,
                            diagnostics,
                            text.to_string(),
                        ) {
                            Ok(Type::UserDefined(typeid))
                        } else {
                            todo!()
                        }
                    }
                    Err(TypeLookupError::NotFound) => {
                        diagnostics.push(mk_diagnostic(
                            node.range(),
                            format!("Type not found: {}", text),
                            DiagnosticSeverity::Error,
                        ));
                        Ok(BasicType::Any.into())
                    }
                }
            }
        } else if node.kind_id() == *SIMPLE_TYPE {
            let name = node.child_by_field_id(*NAME).unwrap();

            Type::parse(document, &mut name.walk(), scope, types, nodes, diagnostics)
        } else if node.kind_id() == *TYPE_INDEX {
            cursor.goto_first_child();

            let left_node = cursor.node();
            let left = Type::parse(document, cursor, scope, types, nodes, diagnostics)?;
            let left_text = left_node.utf8_text(document.source.as_bytes())?;
            cursor.goto_next_sibling();
            let period = cursor.node();
            cursor.goto_next_sibling();
            let right_node = cursor.node();
            let right_text = right_node.utf8_text(document.source.as_bytes())?;

            cursor.goto_parent();

            match &left {
                Type::Basic(BasicType::Any) => Ok(BasicType::Any.into()),
                Type::UserDefined(typeid) => {
                    if let Some(typ) = types[*typeid].as_ref() {
                        match typ {
                            Declaration::Enum(_) => todo!(),
                            Declaration::Record(record) => {
                                if let Some(decl) = record.members.get(right_text) {
                                    match decl {
                                        RecordMember::Field(_) => todo!(),
                                        RecordMember::Declaration(inner_id) => {
                                            Ok(Type::UserDefined(*inner_id))
                                        }
                                    }
                                } else {
                                    diagnostics.push(mk_diagnostic(
                                        Range {
                                            start_byte: right_node.start_byte(),
                                            end_byte: right_node.end_byte(),
                                            start_point: right_node.start_position(),
                                            end_point: right_node.end_position(),
                                        },
                                        format!("{} not found in {}", right_text, left_text),
                                        DiagnosticSeverity::Error,
                                    ));
                                    Ok(BasicType::Any.into())
                                }
                            }
                        }
                    } else {
                        Ok(BasicType::Any.into())
                    }
                }
                typ => {
                    let type_kind = left.to_string(types);
                    let message = {
                        match typ {
                            Type::Basic(basic) => {
                                format!("Attempt to index basic type {}", basic.to_string())
                            }
                            _ => format!("Attempt to index {} type", type_kind),
                        }
                    };
                    diagnostics.push(mk_diagnostic(
                        Range {
                            start_byte: left_node.start_byte(),
                            end_byte: right_node.end_byte(),
                            start_point: left_node.start_position(),
                            end_point: right_node.end_position(),
                        },
                        message,
                        DiagnosticSeverity::Error,
                    ));
                    Ok(BasicType::Any.into())
                }
            }
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
        } else if node.kind_id() == *ERROR {
            // Syntax error - we're not responsible for those, so no diagnostic
            Ok(BasicType::Any.into())
        } else {
            panic!("Unknown type node: {:?}", node);
        }
    }
}

impl Scope {
    pub(crate) fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            declarations: HashMap::new(),
            unloaded_declarations: HashMap::new(),
            variables: HashMap::new(),
            parent: None,
        }))
    }

    fn find_variable(&self, name: &str) -> Option<Option<Type>> {
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
        if let Some((node, type_id)) = self.unloaded_declarations.get(name) {
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

    pub(crate) fn child(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Scope>> {
        let mut ret = Self::new();
        ret.borrow_mut().parent = Some(parent);
        ret
    }

    pub(crate) fn get_global(scope: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        if let Some(ref parent) = scope.borrow().parent {
            Scope::get_global(Rc::clone(parent))
        } else {
            scope.clone()
        }
    }
}

fn parse_expression(
    document: &Document,
    cursor: &mut TreeCursor,
    scope: Rc<RefCell<Scope>>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Type> {
    let node = cursor.node();
    if node.kind_id() == *NUMBER {
        Ok(BasicType::Number.into()) // TODO: integers
    } else {
        diagnostics.push(mk_diagnostic(
            node.range(),
            "Unknown expression kind".to_string(),
            DiagnosticSeverity::Information,
        ));
        Ok(BasicType::Any.into())
    }
}

fn assign_variable<T: Into<String> + AsRef<str>>(
    document: &Document,
    scope: Rc<RefCell<Scope>>,
    name: T,
    typ: Type,
    range: Range,
    types: &mut Vec<Option<Declaration>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let new_typ = match scope.borrow().variables.get(name.as_ref()) {
        Some(Some(existing_type)) => {
            // Type found and has a type
            Type::unify(existing_type, &typ, range, types, Some(diagnostics)).clone()
        }
        Some(None) => {
            // Variable still uninitialized
            typ
        }
        None => {
            // Variable not found
            diagnostics.push(mk_diagnostic(
                range,
                format!("Unknown variable: {}", name.as_ref()),
                DiagnosticSeverity::Error,
            ));
            return;
        }
    };
    scope
        .borrow_mut()
        .variables
        .insert(name.into(), Some(new_typ));
}

fn check_type<'a>(
    document: &Document,
    cursor: &mut TreeCursor<'a>,
    scope: Rc<RefCell<Scope>>,
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
                Rc::clone(&scope),
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
            types.push(None);
            scope
                .borrow_mut()
                .unloaded_declarations
                .insert(name.to_string(), (node.id(), id));
        } else {
            types.push(None);
            scope.borrow_mut().declarations.insert(name.to_string(), id);
            RecordDeclaration::load(document, cursor, scope, types, nodes, diagnostics, id);
        }
    } else if node.kind_id() == *VAR_DECLARATION {
        let declared_scope = node
            .child_by_field_id(*SCOPE)
            .map_or(DeclaredScope::Global, |node| {
                DeclaredScope::parse(document, node, diagnostics)
            });

        let declarators = node.child_by_field_id(*DECLARATORS).unwrap();

        let mut variable_list = Vec::with_capacity(declarators.named_child_count());
        let mut var_nodes = Vec::with_capacity(declarators.named_child_count());

        let mut signature_list = Vec::new();
        let mut signature_nodes = Vec::new();

        let mut declarator_cursor = declarators.walk();
        declarator_cursor.goto_first_child();
        loop {
            let node = declarator_cursor.node();

            if node.kind_id() == *VAR {
                let name = node.child_by_field_id(*NAME).unwrap();
                let text = match name.utf8_text(document.source.as_bytes()) {
                    Ok(text) => text,
                    _ => {
                        diagnostics.push(mk_diagnostic(
                            node.range(),
                            "UTF-8 decode failed".to_string(),
                            DiagnosticSeverity::Error,
                        ));
                        "#ERROR#"
                    }
                };

                scope.borrow_mut().variables.insert(text.to_string(), None);
                variable_list.push(text);
                var_nodes.push(name);
            }

            if !declarator_cursor.goto_next_sibling() {
                break;
            }
        }
        declarator_cursor.goto_parent();

        if let Some(signatures) = node.child_by_field_id(*TYPE_ANNOTATION) {
            let mut signature_cursor = signatures.walk();
            signature_list = Vec::with_capacity(signature_cursor.node().named_child_count());
            signature_nodes = Vec::with_capacity(signature_cursor.node().named_child_count());

            signature_cursor.goto_first_child();

            let mut idx = 0;

            loop {
                let current_node = signature_cursor.node();
                if current_node.is_named() {
                    let typ = if let Ok(typ) = Type::parse(
                        document,
                        &mut signature_cursor,
                        Rc::clone(&scope),
                        types,
                        nodes,
                        diagnostics,
                    ) {
                        signature_list.push(typ.clone());
                        typ
                    } else {
                        diagnostics.push(mk_diagnostic(
                            current_node.range(),
                            "UTF-8 decode failed".to_string(),
                            DiagnosticSeverity::Error,
                        ));
                        signature_list.push(BasicType::Any.into());
                        BasicType::Any.into()
                    };
                    signature_nodes.push(current_node);

                    if idx < variable_list.len() {
                        scope
                            .borrow_mut()
                            .variables
                            .insert(variable_list[idx].to_string(), Some(typ));
                    } else {
                        while signature_cursor.goto_next_sibling() {}
                        let last_node = signature_cursor.node();
                        diagnostics.push(mk_diagnostic(
                            Range {
                                start_byte: current_node.start_byte(),
                                end_byte: last_node.end_byte(),
                                start_point: current_node.start_position(),
                                end_point: last_node.end_position(),
                            },
                            "More type annotations than variables in declaration".to_string(),
                            DiagnosticSeverity::Warning,
                        ));
                        break;
                    }

                    idx += 1;
                }

                if !signature_cursor.goto_next_sibling() {
                    break;
                }
            }

            signature_cursor.goto_parent();
        }

        if let Some(initializers) = node.child_by_field_id(*INITIALIZERS) {
            let mut initializer_cursor = initializers.walk();

            initializer_cursor.goto_first_child();
            let mut idx = 0;

            loop {
                if initializer_cursor.node().is_named() {
                    let current_node = initializer_cursor.node();
                    let typ = parse_expression(
                        document,
                        &mut initializer_cursor,
                        Rc::clone(&scope),
                        diagnostics,
                    )
                    .unwrap_or(BasicType::Any.into()); // TODO

                    if idx < variable_list.len() {
                        let name = variable_list[idx];
                        assign_variable(
                            document,
                            Rc::clone(&scope),
                            name,
                            typ,
                            current_node.range(),
                            types,
                            diagnostics,
                        );
                    } else {
                        diagnostics.push(mk_diagnostic(
                            Range {
                                start_byte: current_node.start_byte(),
                                end_byte: node.end_byte(),
                                start_point: current_node.start_position(),
                                end_point: node.end_position(),
                            },
                            "More values than variables in declaration".to_string(),
                            DiagnosticSeverity::Warning,
                        ));
                        break;
                    }
                    idx += 1;
                }

                if !initializer_cursor.goto_next_sibling() {
                    break;
                }
            }

            if idx < variable_list.len() {
                let start_node = var_nodes[idx];
                let end_node = var_nodes[var_nodes.len() - 1];
                let range = Range {
                    start_byte: start_node.start_byte(),
                    end_byte: end_node.end_byte(),
                    start_point: start_node.start_position(),
                    end_point: end_node.end_position(),
                };
                diagnostics.push(mk_diagnostic(
                    range,
                    "Not all variables initialized".to_string(),
                    DiagnosticSeverity::Information,
                ));
                for name in &variable_list[idx..] {
                    scope
                        .borrow_mut()
                        .variables
                        .insert(name.to_string(), Some(BasicType::Nil.into()));
                }
            }

            initializer_cursor.goto_parent();
        } else {
            for name in variable_list {
                scope
                    .borrow_mut()
                    .variables
                    .insert(name.to_string(), Some(BasicType::Nil.into()));
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
        println!("{:#?}", global_scope.borrow().variables);
    }
}
