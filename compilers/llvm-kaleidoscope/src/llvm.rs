use crate::{ExprAst, FunctionPrototype, Statement, Operation, UnaryOperation};
use llvm_sys::{LLVMBuilder, LLVMModule, LLVMRealPredicate};
use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction};
use llvm_sys::core::{LLVMModuleCreateWithName, LLVMDisposeModule, LLVMCreateBuilder,
                     LLVMDisposeBuilder, LLVMConstReal, LLVMDoubleType, LLVMBuildFAdd, LLVMBuildFSub,
                     LLVMBuildFMul, LLVMBuildFRem, LLVMBuildFCmp, LLVMBuildUIToFP, LLVMBuildAnd,
                     LLVMBuildOr, LLVMGetNamedFunction, LLVMBuildCall, LLVMAddFunction,
                     LLVMFunctionType, LLVMGetEntryBasicBlock, LLVMAppendBasicBlock, LLVMCountParams,
                     LLVMGetParam, LLVMBuildRet, LLVMDumpModule};
use llvm_sys::prelude::*;
use std::ffi::CString;
use std::collections::HashMap;

struct Module {
    module: *mut LLVMModule,
    strings: Vec<CString>
}

impl Module {
    fn new(name: &str) -> Module {
        unsafe {
            Module {
                module: LLVMModuleCreateWithName(
                    CString::new(name).unwrap().to_bytes_with_nul().as_ptr() as *const _
                ),
                strings: Vec::new(),
            }
        }
    }
    fn add_new_string(&mut self, s: &str) -> *const i8 {
        let s = CString::new(s).unwrap();
        let ptr = s.as_ptr() as *const _;
        self.strings.push(s);
        ptr
    }
    fn print(&self) {
        unsafe { LLVMDumpModule(self.module) };
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.module);
        }
    }
}

struct Builder {
    builder: *mut LLVMBuilder,
}

impl Builder {
    fn new() -> Builder {
        unsafe {
            Builder {
                builder: LLVMCreateBuilder(),
            }
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

#[derive(Debug)]
pub(crate) enum CodeGenerationError {
    VariableNotFound(String),
    FunctionAlreadyDefined(String),
    WrongParamsNumber(usize, usize),
    ErrorCreatingFunction,
}

pub(crate) struct Context {
    module: Module,
    builder: Builder,
    symbols: HashMap<String, LLVMValueRef>,
}

impl Context {
    pub(crate) fn new(name: &str) -> Context {
        Context {
            module: Module::new(name),
            builder: Builder::new(),
            symbols: HashMap::new(),
        }
    }

    pub(crate) fn print(&self) {
        self.module.print();
    }
}

pub(crate) trait CodeGenerator {
    fn codegen(&self, context: &mut Context) -> Result<LLVMValueRef, CodeGenerationError>;
}

#[inline]
unsafe fn generate_cmp_code(context: &mut Context, l: LLVMValueRef, r: LLVMValueRef, op: LLVMRealPredicate) -> LLVMValueRef {
    let tmp = LLVMBuildFCmp(
        context.builder.builder,
        op,
        l,
        r,
        context.module.add_new_string("tmpgt"),
    );
    LLVMBuildUIToFP(
        context.builder.builder,
        tmp,
        LLVMDoubleType(),
        context.module.add_new_string("tmpuitofp"),
    )
}

#[inline]
unsafe fn to_bool(context: &mut Context, v: LLVMValueRef) -> LLVMValueRef {
    LLVMBuildFCmp(
        context.builder.builder,
        LLVMRealPredicate::LLVMRealUNE,
        v,
        LLVMConstReal(LLVMDoubleType(), 0f64),
        context.module.add_new_string("tmpboolne"),
    )
}

impl CodeGenerator for ExprAst {
    fn codegen(&self, context: &mut Context) -> Result<LLVMValueRef, CodeGenerationError> {
        match self {
            ExprAst::Number(f) =>
                Ok(unsafe { LLVMConstReal(LLVMDoubleType(), *f) }),
            ExprAst::Variable(i) =>
                context.symbols.get(i)
                    .ok_or(CodeGenerationError::VariableNotFound(i.clone()))
                    .map(|v| v.to_owned()),
            ExprAst::BinaryExpression(
               left,
               operation,
               right,
           )  => {
                let l = left.codegen(context)?;
                let r = right.codegen(context)?;
                match operation {
                    Operation::Sum => Ok(unsafe {
                        LLVMBuildFAdd(context.builder.builder, l, r, context.module.add_new_string("tmpadd"))
                    }),
                    Operation::Difference => Ok(unsafe {
                        LLVMBuildFSub(context.builder.builder, l, r, context.module.add_new_string("tmpsub"))
                    }),
                    Operation::Multiplication => Ok(unsafe {
                        LLVMBuildFMul(context.builder.builder, l, r, context.module.add_new_string("tmpmult"))
                    }),
                    Operation::Division => Ok(unsafe {
                        LLVMBuildFMul(context.builder.builder, l, r, context.module.add_new_string("tmpdiv"))
                    }),
                    Operation::Modulo => Ok(unsafe {
                        LLVMBuildFRem(context.builder.builder, l, r, context.module.add_new_string("tmprem"))
                    }),
                    Operation::IsGreaterThan => Ok(unsafe {
                        generate_cmp_code(context, l, r, LLVMRealPredicate::LLVMRealUGT)
                    }),
                    Operation::IsLesserThan => Ok(unsafe {
                        generate_cmp_code(context, l, r, LLVMRealPredicate::LLVMRealULT)
                    }),
                    Operation::IsEqualsTo => Ok(unsafe {
                        generate_cmp_code(context, l, r, LLVMRealPredicate::LLVMRealUEQ)
                    }),
                    Operation::IsNotEqualsTo => Ok(unsafe {
                        generate_cmp_code(context, l, r, LLVMRealPredicate::LLVMRealUNE)
                    }),
                    Operation::And => Ok(unsafe {
                        LLVMBuildAnd(
                            context.builder.builder,
                            to_bool(context, l),
                            to_bool(context, r),
                            context.module.add_new_string("tmpand"),
                        )
                    }),
                    Operation::Or => Ok(unsafe {
                        LLVMBuildOr(
                            context.builder.builder,
                            to_bool(context, l),
                            to_bool(context, r),
                            context.module.add_new_string("tmpor"),
                        )
                    }),
                }
            }
            ExprAst::Call(fname, args) => {
                let f = unsafe {
                    LLVMGetNamedFunction(
                        context.module.module,
                        CString::new(fname.as_str()).unwrap().as_ptr()
                    )
                };
                let count = unsafe { LLVMCountParams(f) } as usize;
                if count == args.len() {
                    let mut arg_vals = args.iter()
                        .map(|v| v.codegen(context))
                        .collect::<Result<Vec<LLVMValueRef>, CodeGenerationError>>()?;
                    Ok(unsafe {
                        LLVMBuildCall(
                            context.builder.builder,
                            f,
                            arg_vals.as_mut_ptr(),
                            args.len() as u32,
                            context.module.add_new_string("tmpcall"),
                        )
                    })
                } else {
                    Err(CodeGenerationError::WrongParamsNumber(args.len(), count))
                }
            }
            ExprAst::Unary(operator, expr) => {
                let operand = expr.codegen(context)?;
                match operator {
                    UnaryOperation::ChangeSign => {
                        Ok(unsafe {
                            LLVMBuildFMul(
                                context.builder.builder,
                                operand,
                                LLVMConstReal(LLVMDoubleType(), -1f64),
                                context.module.add_new_string("tmpchangesign"),
                            )
                        })
                    }
                }
            }
            ExprAst::Grouping(v) => v.codegen(context),
        }
    }
}

fn codegen_prototype(prototype: &FunctionPrototype, context: &mut Context) -> LLVMValueRef {
    let mut argument_types: Vec<LLVMTypeRef> = prototype.1.iter().map(|_| unsafe {
        LLVMDoubleType()
    }).collect();
    let function_type = unsafe {
        LLVMFunctionType(LLVMDoubleType(), argument_types.as_mut_ptr(), argument_types.len() as u32, false as i32)
    };
    unsafe {
        LLVMAddFunction(context.module.module, context.module.add_new_string(&prototype.0), function_type)
    }
}

impl CodeGenerator for Statement {
    fn codegen(&self, context: &mut Context) -> Result<LLVMValueRef, CodeGenerationError> {
        match self {
            Statement::ExpressionStatement(e) => e.codegen(context),
            Statement::ExternFunction(f) => Ok(codegen_prototype(f, context)),
            Statement::Function(p, b) => {
                let prev_f = unsafe {
                    LLVMGetNamedFunction(context.module.module, CString::new(p.0.as_str()).unwrap().as_ptr())
                        .as_mut()
                };
                let f = match prev_f {
                    None => codegen_prototype(&p, context),
                    Some(v) => v as *mut _,
                };
                let current_basic_block = unsafe {
                    LLVMGetEntryBasicBlock(f).as_ref()
                };
                match current_basic_block {
                    None => {
                        let _bb = unsafe {
                            LLVMAppendBasicBlock(f, context.module.add_new_string("entry"))
                        };
                        let n_params = unsafe { LLVMCountParams(f) } as usize;
                        context.symbols.clear();
                        for i in 0..n_params-1 {
                            let param = unsafe {
                                LLVMGetParam(f, i as u32)
                            };
                            context.symbols.insert(p.1[i].clone(), param);
                        }
                        let return_value = b.codegen(context)?;
                        unsafe {
                            LLVMBuildRet(context.builder.builder, return_value)
                        };
                        let result = unsafe {
                            LLVMVerifyFunction(f, LLVMVerifierFailureAction::LLVMPrintMessageAction)
                        };
                        if result > 0 {
                            Err(CodeGenerationError::ErrorCreatingFunction)
                        } else {
                            Ok(f)
                        }
                    },
                    Some(_) => {
                        Err(CodeGenerationError::FunctionAlreadyDefined(p.0.clone()))
                    },
                }
            }
        }
    }
}