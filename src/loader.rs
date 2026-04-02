use std::{collections::HashMap, fs};

use crate::{AzimuthFlags, lexer::{self, Span}, parser::{self, Expression, Identifier, RawMapping, ParseError, ParsedAtlas, ShapeExpression, Statement}};

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

#[derive(Debug, Clone)]
pub struct LoadedAzimuth {
    pub name: Identifier,
    pub id: u32,
    pub flags: AzimuthFlags,
    pub kind: ShapeExpression,
    pub default_value: Option<Expression>,
}

pub type Filename = String;

#[derive(Debug, Clone, Default)]
pub enum NamespaceKind {
    #[default] Namespace,
    Shape{parents: Vec<Identifier>, mappings: Vec<RawMapping>, generics: Vec<ShapeExpression>},
    Atlas
}

pub type NamespaceId = Vec<Identifier>;

#[derive(Debug, Clone, Default)]
pub struct Namespace {
    pub span: Span,
    pub name: Identifier,
    pub id: u32,
    pub kind: NamespaceKind,
    pub children: Vec<Namespace>,
    pub azimuths: Vec<LoadedAzimuth>,
    pub dependencies: Vec<NamespaceId>,
}

impl Namespace {
    pub fn traverse(&self, tree: &mut NamespaceId) -> Option<&Namespace> {
        //println!("My name is {}. Current tree: {:?}", self.name, tree);

        let name = match tree.pop() {
            Some(name) => name,
            None => return Some(&self)
        };
        
        //println!("Found child: {:?} from children: {:?}", self.children.get(&name), self.children.keys());

        match self.children.iter().find(|child| child.name == name) {
            Some(child) => child.traverse(tree),
            None => None
        }
    }

    pub fn traverse_mut(&mut self, tree: &mut NamespaceId) -> Option<&mut Namespace> {
        //println!("My name is {}. Current tree: {:?}", self.name, tree);

        let name = match tree.pop() {
            Some(name) => name,
            None => return Some(self)
        };
        
        //println!("Found child: {:?} from children: {:?}", self.children.get(&name), self.children.keys());

        match self.children.iter_mut().find(|child| child.name == name) {
            Some(child) => child.traverse_mut(tree),
            None => None
        }
    }

    pub fn get_azimuth(&self, identifier: &Identifier) -> Option<&LoadedAzimuth> {
        match self.azimuths.iter().find(|az| &az.name == identifier) {
            Some(az) => Some(az),
            None => {
                for child in &self.children {
                    match child.get_azimuth(identifier) {
                        Some(az) => return Some(az),
                        None => {},
                    }
                }
                None
            }
        }
    }

    pub fn has_static(&self) -> bool {
        for azimuth in &self.azimuths {
            if azimuth.flags.is_static { return true }
        }
        false
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
    pub load_order: Vec<(AtlasLocation, Identifier, u32)>,
    pub root: Namespace,
    pub next_az_id: u32,
    pub next_ns_id: u32,
    pub extensions: Vec<Namespace>,
}

impl Loader {

    pub fn new(source_dir: &str) -> Self {
        Loader { 
            source_dir: source_dir.to_string(),
            files: HashMap::new(), 
            root: Namespace {
                span: Span::new(0,0, source_dir.to_string()),
                name: "".to_string(),
                id: 0,
                kind: NamespaceKind::Atlas,
                children: Vec::new(),
                azimuths: Vec::new(),
                dependencies: Vec::new(),
            },
            load_order: Vec::new(),
            next_az_id: 0,
            next_ns_id: 0,
            extensions: Vec::new(),
        }
    }

    pub fn next_azimuth_id(&mut self) -> u32 {
        let id = self.next_az_id;
        self.next_az_id += 1;
        id
    }

    pub fn next_namespace_id(&mut self) -> u32 {
        let id = self.next_ns_id;
        self.next_ns_id += 1;
        id
    }

    pub fn load_program(&mut self, atlas_path: &str) -> Result<(), LoadError> {
        let atlas = self.load_atlas(atlas_path)?;
        self.root.name = atlas.name;

        for mapping in atlas.mappings {
            let ast = match self.load_file(Span::new(0,0,atlas_path.to_string()), mapping.to.clone())? {
                Some(ast) => ast,
                None => continue,
            };
            
            self.files.insert(mapping.to.clone(), ast);
            let statements = self.files.get(&mapping.to).unwrap().clone();

            let name = mapping.from;
            let namespace = self.load_namespace(Span::new(0,0, mapping.to.url.clone()), name.clone(), statements)?;
            let id = namespace.id;
            self.root.children.push(namespace);
            self.load_order.push((mapping.to, name, id));
        }

        for extension in &mut self.extensions.clone() {
            self.apply_extension(extension)?;
        }

        Ok(())
    }

    pub fn load_namespace(&mut self, span:Span, name:Identifier, statements:Vec<Statement>) -> Result<Namespace, LoadError> {
        let mut children = Vec::new();
        let mut azimuths = Vec::new();
        let mut dependencies = Vec::new();

        for statement in statements {
            match statement {
                Statement::Using { package, .. } => {
                    dependencies.push(package);
                }
                Statement::DeclareShape { span, name, slot_ids, parents, mappings, generics, extension, .. } => {
                    let azimuths: Vec<LoadedAzimuth> = slot_ids.iter()
                        .map(|raw| LoadedAzimuth{
                            name:raw.name.clone(), 
                            id: self.next_azimuth_id(),
                            kind:raw.value_type.clone(),
                            default_value: raw.set_value.clone(),
                            flags: raw.flags.clone(),
                        }).collect();
                    let namespace = Namespace { 
                        span,
                        name:name.clone(), 
                        id: self.next_namespace_id(), 
                        kind:NamespaceKind::Shape{ parents, mappings, generics}, 
                        children:Vec::new(), 
                        dependencies:Vec::new(),
                        azimuths 
                    };
                    
                    if extension {
                        self.extensions.push(namespace);
                    } else {
                        children.push(namespace);
                    }
                }
                Statement::Namespace { span, name, content, .. } => {
                    let namespace = self.load_namespace(span, name.clone(), content)?;
                    children.push(namespace);
                }
                Statement::DeclareAzimuth { azimuth, .. } => {
                    azimuths.push(LoadedAzimuth{
                        name: azimuth.name.clone(), 
                        id: self.next_azimuth_id(),
                        kind: azimuth.value_type.clone(),
                        default_value: azimuth.set_value.clone(),
                        flags: azimuth.flags.clone(),
                    });
                }
                Statement::Block ( statements ) => {
                    let namespace = self.load_namespace(span.clone(), format!("block"), statements)?;
                    for child in namespace.children {
                        children.push(child);
                    }
                    for azimuth in namespace.azimuths {
                        azimuths.push(azimuth);
                    }
                    for dependency in namespace.dependencies {
                        dependencies.push(dependency);
                    }
                }
                _ => {}
            }
        }
        Ok(Namespace{span, name, id: self.next_namespace_id(), kind:NamespaceKind::Namespace, children, azimuths, dependencies})
    }

    pub fn load_atlas(&self, atlas_path: &str) -> Result<ParsedAtlas, LoadError> {
        let atlas_source = match fs::read_to_string(format!("{}/{}", self.source_dir, atlas_path)) {
            Err(_) => return Err(LoadError::FileNotFound{span:Span::new(0,0, atlas_path.to_string()), location:atlas_path.to_string()}),
            Ok(source) => source,
        };
        let atlas_tokens = match lexer::tokenize(&atlas_source, atlas_path.to_string(), true) {
            Err(error) => return Err(LoadError::LexerError{location:atlas_path.to_string(), error}),
            Ok(tokens) => tokens,
        };
        let atlas = match parser::parse_atlas_file(atlas_tokens) {
            Err(error) => return Err(LoadError::ParseError{location:atlas_path.to_string(), error}),
            Ok(ast) => ast,
        };
        Ok(atlas)
    }

    pub fn load_file(&self, span: Span, location: AtlasLocation) -> Result<Option<Vec<Statement>>, LoadError> {
        if location.subspace.is_some() { todo!() }
        if self.files.contains_key(&location) { return Ok(None) }

        let source = fs::read_to_string(format!("{}/{}", self.source_dir, location.url))
            .map_err(|_| LoadError::FileNotFound{span, location:location.url.clone()})?;

        let tokens = lexer::tokenize(&source, location.url.clone(), false)
            .map_err(|error| LoadError::LexerError{location:location.url.clone(), error})?;

        let ast = parser::parse(tokens)
            .map_err(|error| LoadError::ParseError{location:location.url.clone(), error})?;

        Ok(Some(ast))
    }

    pub fn get_azimuths(&self, name:Identifier, using:Vec<NamespaceId>) -> Vec<(NamespaceId, &LoadedAzimuth)> {
        let mut azimuths = Vec::new();
        for (tree, namespace) in self.get_namespaces(using){
            
            if let Some(found) = namespace.get_azimuth(&name) {
                let mut found_tree = tree.clone();
                found_tree.push(name.clone());
                azimuths.push((found_tree, found));
            }
        }
        azimuths
    }

    pub fn get_namespaces_matching(&self, identifier: Identifier, using:Vec<NamespaceId>) -> Vec<(NamespaceId, &Namespace)> {
        let mut namespaces = Vec::new();
        for mut tree in using {
            tree.push(identifier.clone());
            //println!("Searching {:?}", tree);
            tree.reverse();
            match self.root.traverse(&mut tree.clone()) {
                Some(namespace) => namespaces.push((tree, namespace)),
                None => {}
            }
        }
        namespaces
    }

    pub fn get_namespaces(&self, using:Vec<NamespaceId>) -> Vec<(NamespaceId, &Namespace)> {
        let mut namespaces = Vec::new();
        for mut tree in using {
            tree.reverse();
            match self.root.traverse(&mut tree.clone()) {
                Some(namespace) => namespaces.push((tree, namespace)),
                None => {}
            }
        }
        namespaces
    }

    pub fn get_namespace_mut(&mut self, path:&mut NamespaceId) -> Option<&mut Namespace> {
        self.root.traverse_mut(path)
    }

    pub fn apply_extension(&mut self, extension:&mut Namespace) -> Result<(), LoadError> {
        let path = {
            let found = self.get_namespaces_matching(extension.name.clone(), extension.dependencies.clone());
            if found.len() == 0 {
                return Err(LoadError::Error{span:extension.span.clone(), message:format!("Shape not found: {} with path {:?}", extension.name, extension.dependencies)});
            }
            else if found.len() > 1 {
                return Err(LoadError::Error{span:extension.span.clone(), message:format!("Ambiguous extension: {:?}", found)});
            }
            found[0].clone().0
        };

        let (ext_parents, ext_mappings, ext_generics) = match &mut extension.kind {
            NamespaceKind::Shape { parents, mappings, generics } => (parents, mappings, generics),
            _ => unreachable!(),
        };
        
        let base = match self.get_namespace_mut(&mut path.clone()) {
            None => return Err(LoadError::Error{span:extension.span.clone(), message:format!("Shape somehow not found: {} with path {:?}", extension.name, path)}),
            Some(base) => base,
        };
        
        match &mut base.kind {
            NamespaceKind::Shape{ parents, mappings, generics } => {
                parents.append(ext_parents);
                mappings.append(ext_mappings);
                generics.append(ext_generics);
                base.azimuths.append(&mut extension.azimuths);
                base.children.append(&mut extension.children);
            }
            _ => return Err(LoadError::Error{span:extension.span.clone(), message:format!("Extension for non-shape: {:?}", base)}),
        }
        
        Ok(())
    }

}

pub fn load(source_dir: &str, atlas_path: &str) -> Result<Loader, LoadError> {
    let mut loader = Loader::new(source_dir);
    loader.load_program(atlas_path)?;
    println!("\nLoaded namespaces: {:?}\n", loader.root);
    Ok(loader)
}