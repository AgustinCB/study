use crate::types::{LoxFunction, Statement, StatementType, SourceCodeLocation, ProgramError};
use crate::value::Value;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn function_declaration_to_lox_funxtion(arguments: &[String], body: &[Box<Statement>], location: &SourceCodeLocation, environments: &[Rc<RefCell<HashMap<String, Value>>>]) -> LoxFunction {
    LoxFunction {
        arguments: arguments.to_vec(),
        body: body.iter().map(|s| (**s).clone()).collect(),
        environments: environments.to_vec(),
        location: location.clone(),
    }
}

fn statement_list_to_function_hash_map(statements: &[&Statement], environments: &[Rc<RefCell<HashMap<String, Value>>>]) -> HashMap<String, LoxFunction> {
    let mut functions = HashMap::default();
    for s in statements {
        match &s.statement_type {
            StatementType::FunctionDeclaration { arguments, body, name, } => {
                functions.insert(name.clone(), function_declaration_to_lox_funxtion(arguments, body, &s.location, &environments));
            }
            _ => panic!("Unexpected method"),
        }
    }
    functions
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    methods: HashMap<String, LoxFunction>,
    getters: HashMap<String, LoxFunction>,
    setters: HashMap<String, LoxFunction>,
    superclass: Option<Box<LoxClass>>,
    pub name: String,
    pub static_instance: LoxObject,
}

impl LoxClass {
    pub fn new(
        name: String,
        static_method_list: &[&Statement],
        method_list: &[&Statement],
        getters: &[&Statement],
        setters: &[&Statement],
        superclass: Option<LoxClass>,
        environments: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    ) -> LoxClass {
        let methods = statement_list_to_function_hash_map(method_list, &environments);
        let getters = statement_list_to_function_hash_map(getters, &environments);
        let setters = statement_list_to_function_hash_map(setters, &environments);
        let mut static_methods = vec![];
        for ms in static_method_list {
            match &ms.statement_type {
                StatementType::FunctionDeclaration { arguments, body, name, } => {
                    static_methods.push((name.clone(), function_declaration_to_lox_funxtion(arguments, body, &ms.location, &environments)));
                }
                _ => panic!("Unexpected method"),
            }
        }
        let static_instance = LoxObject::new_static(name.clone(), &static_methods, superclass.clone());
        LoxClass {
            getters,
            methods,
            name,
            setters,
            static_instance,
            superclass: superclass.map(Box::new),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxObject {
    properties: Rc<RefCell<HashMap<String, Value>>>,
    getters: HashMap<String, LoxFunction>,
    setters: HashMap<String, LoxFunction>,
    superclass: Option<Box<LoxObject>>,
    pub class_name: String,
}

impl LoxObject {
    pub fn new(class: LoxClass) -> LoxObject {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        let superclass = class.superclass.map(|c| *c).map(LoxObject::new).map(Box::new);
        let mut object = LoxObject {
            class_name: class.name,
            getters: HashMap::default(),
            properties: properties.clone(),
            setters: HashMap::default(),
            superclass,
        };
        for (name, mut f) in class.methods {
            f.bind(object.clone());
            properties.borrow_mut().insert(name, Value::Function(f));
        }
        for (name, mut f) in class.getters {
            f.bind(object.clone());
            object.getters.insert(name, f);
        }
        for (name, mut f) in class.setters {
            f.bind(object.clone());
            object.setters.insert(name, f);
        }
        object
    }

    fn new_static(class_name: String, methods: &[(String, LoxFunction)], superclass: Option<LoxClass>) -> LoxObject {
        let properties = Rc::new(RefCell::new(HashMap::default()));
        for (name, function) in methods {
            properties.borrow_mut().insert(name.clone(), Value::Function(function.clone()));
        }
        let superclass = superclass.map(|c|
            LoxObject::new_static(c.name.clone(), &c.methods.clone().into_iter().collect::<Vec<(String, LoxFunction)>>(), c.superclass.map(|c| *c))
        ).map(Box::new);
        LoxObject {
            getters: HashMap::new(),
            setters: HashMap::new(),
            class_name,
            properties,
            superclass,
        }
    }

    pub fn init(
        &self,
        values: &[Value],
        locals: &HashMap<usize, usize>,
        location: &SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        if let Some(s) = &self.superclass {
            s.init(values, locals, location)?;
        }
        if let Some(Value::Function(f)) = self.properties.borrow().get("init") {
            f.eval(values, locals)?;
            Ok(())
        } else if values.len() != 0 {
            Err(ProgramError {
                message: format!(
                    "Wrong number of arguments: Received {}, expected {}",
                    values.len(),
                    0,
                ),
                location: location.clone(),
            })
        } else {
            Ok(())
        }
    }

    pub fn get_setter(&self, name: &str) -> Option<LoxFunction> {
        self.setters.get(name).cloned()
    }

    pub fn get_getter(&self, name: &str) -> Option<LoxFunction> {
        self.getters.get(name).cloned()
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let v = self.properties.borrow().get(name).cloned();
        if v.is_some() {
            v
        } else {
            self.superclass.as_ref().map(|s| s.get(name)).flatten()
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.properties.borrow_mut().insert(name, value);
    }
}
