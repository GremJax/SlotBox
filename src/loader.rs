use std::{collections::HashMap, fs};

use crate::{lexer::{self, Span}, parser::{self, Identifier, ParseError, ParsedAtlas, Statement}};

#[derive(Debug, Clone)]
pub enum LoadError {
    Error { span: Span, message: String },
    FileNotFound { span: Span, location: String },
    ParseError { location: String, error: ParseError },
    LexerError { location: String, error: ParseError },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Error { span, message } =>
                write!(f, "{}: {}", span, message),
                
            LoadError::FileNotFound { span, location } =>
                write!(f, "{}: File not found: {}", span, location),
                
            LoadError::ParseError { location, error } =>
                write!(f, "Could not parse file: {}\n{}", location, error),
                
            LoadError::LexerError { location, error } =>
                write!(f, "Could not tokenize file: {}\n{}", location, error),
        }
    }
}

pub type Filename = String;

#[derive(Debug, Clone)]
pub enum NamespaceKind {
    Namespace,
    Shape,
    Atlas
}

pub type NamespaceTree = Vec<Identifier>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: Identifier,
    kind: NamespaceKind,
    children: HashMap<Identifier, Namespace>,
    azimuths: Vec<Identifier>,
}

impl Namespace {
    pub fn traverse(&self, tree: &mut NamespaceTree) -> Option<&Namespace> {
        let name = match tree.pop() {
            Some(name) => name,
            None => return None
        };

        if self.name == name { return Some(&self) }
        match self.children.get(&name) {
            Some(child) => child.traverse(tree),
            None => None
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AtlasLocation {
    pub url: String,
    pub subspace: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub struct AtlasMapping {
    pub from: Identifier,
    pub to: AtlasLocation
}

pub struct Loader {
    pub source_dir: String,
    pub files: HashMap<AtlasLocation, Vec<Statement>>,
    pub root: Namespace,
}

impl Loader {

    pub fn new(source_dir: &str) -> Self {
        Loader { 
            source_dir: format!("{}",source_dir),
            files: HashMap::new(), 
            root: Namespace {
                name: "".to_string(),
                kind: NamespaceKind::Atlas,
                children: HashMap::new(),
                azimuths: Vec::new(),
            }
        }
    }

    pub fn load_program(&mut self, atlas_path: &str) -> Result<(), LoadError> {
        let atlas = self.load_atlas(atlas_path)?;
        self.root.name = atlas.name;

        for mapping in atlas.mappings {
            let ast = match self.load_file(mapping.to)? {
                Some(ast) => ast,
                None => continue,
            };

            let name = mapping.from;
            let namespace = Self::load_namespace(name.clone(), ast)?;
            self.root.children.insert(name, namespace);
        }

        Ok(())
    }

    pub fn load_namespace(name:Identifier, statements:&Vec<Statement>) -> Result<Namespace, LoadError> {
        let mut children = HashMap::new();
        let mut azimuths = Vec::new();

        for statement in statements {
            match statement {
                Statement::DeclareShape { name, slot_ids, .. } => {
                    let azimuths = slot_ids.iter().map(|raw| raw.name.clone()).collect();
                    let namespace = Namespace { name:name.clone(), kind:NamespaceKind::Shape, children:HashMap::new(), azimuths };
                    children.insert(name.clone(), namespace);
                }
                Statement::Namespace { name, content, .. } => {
                    let namespace = Self::load_namespace(name.clone(), content)?;
                    children.insert(name.clone(), namespace);
                }
                Statement::DeclareAzimuth { azimuth, .. } => {
                    azimuths.push(azimuth.name.clone());
                }
                Statement::Block ( statements ) => {
                    let namespace = Self::load_namespace(format!("block"), statements)?;
                    for (name, child) in namespace.children {
                        children.insert(name, child);
                    }
                    for azimuth in namespace.azimuths {
                        azimuths.push(azimuth);
                    }
                }
                _ => {}
            }
        }
        Ok(Namespace{name, kind:NamespaceKind::Namespace, children, azimuths})
    }

    pub fn load_atlas(&self, atlas_path: &str) -> Result<ParsedAtlas, LoadError> {
        let atlas_source = match fs::read_to_string(format!("{}/{}", self.source_dir, atlas_path)) {
            Err(_) => return Err(LoadError::FileNotFound{span:Span::new(0,0), location:atlas_path.to_string()}),
            Ok(source) => source,
        };
        let atlas_tokens = match lexer::tokenize(&atlas_source, true) {
            Err(error) => return Err(LoadError::LexerError{location:atlas_path.to_string(), error}),
            Ok(tokens) => tokens,
        };
        let atlas = match parser::parse_atlas_file(atlas_tokens) {
            Err(error) => return Err(LoadError::ParseError{location:atlas_path.to_string(), error}),
            Ok(ast) => ast,
        };
        Ok(atlas)
    }

    pub fn load_file(&mut self, location: AtlasLocation) -> Result<Option<&Vec<Statement>>, LoadError> {
        if location.subspace.is_some() { todo!() }
        if self.files.contains_key(&location) { return Ok(None) }

        let source = match fs::read_to_string(format!("{}/{}", self.source_dir, location.url)) {
            Err(_) => return Err(LoadError::FileNotFound{span:Span::new(0,0), location:location.url}),
            Ok(source) => source,
        };
        let tokens = match lexer::tokenize(&source, false) {
            Err(error) => return Err(LoadError::LexerError{location:location.url, error}),
            Ok(tokens) => tokens,
        };
        let ast = match parser::parse(tokens) {
            Err(error) => return Err(LoadError::ParseError{location:location.url, error}),
            Ok(ast) => ast,
        };
        
        self.files.insert(location.clone(), ast);
        Ok(self.files.get(&location))
    }

    pub fn get_azimuths(&self, name:Identifier, using:Vec<NamespaceTree>) -> Vec<NamespaceTree> {
        let mut azimuths = Vec::new();
        for (tree, namespace) in self.get_namespaces(using){
            if namespace.azimuths.contains(&name) {
                let mut found_tree = tree.clone();
                found_tree.push(name.clone());
                azimuths.push(found_tree);
            }
        }
        azimuths
    }

    pub fn get_namespaces(&self, using:Vec<NamespaceTree>) -> Vec<(NamespaceTree, &Namespace)> {
        let mut namespaces = Vec::new();
        for tree in using {
            match self.root.traverse(&mut tree.clone()) {
                Some(namespace) => namespaces.push((tree, namespace)),
                None => {}
            }
        }
        namespaces
    }

}

pub fn load(source_dir: &str, atlas_path: &str) -> Result<Loader, LoadError> {
    let mut loader = Loader::new(source_dir);
    loader.load_program(atlas_path)?;
    Ok(loader)
}