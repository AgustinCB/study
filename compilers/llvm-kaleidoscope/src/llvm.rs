use crate::{ExprAst, Statement, Operation};
use llvm_sys::{LLVMBuilder, LLVMModule, LLVMRealPredicate};
use llvm_sys::prelude::*;
use llvm_sys::core::{LLVMModuleCreateWithName, LLVMDisposeModule, LLVMCreateBuilder, LLVMDisposeBuilder, LLVMConstReal, LLVMDoubleType, LLVMBuildFAdd, LLVMBuildFSub, LLVMBuildFMul, LLVMBuildFRem, LLVMBuildFCmp, LLVMBuildUIToFP, LLVMBuildAnd, LLVMBuildOr};
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
}

pub(crate) struct Context {
    module: Module,
    builder: Builder,
    symbols: HashMap<String, LLVMValueRef>,
}

impl Context {
    fn new(name: &str) -> Context {
        Context {
            module: Module::new(name),
            builder: Builder::new(),
            symbols: HashMap::new(),
        }
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
            _ => panic!("Not implemented yet")
        }
    }
}

impl CodeGenerator for Statement {
    fn codegen(&self, context: &mut Context) -> Result<LLVMValueRef, CodeGenerationError> {
        match self {
            Statement::ExpressionStatement(e) => e.codegen(context),
            _ => panic!("Not implemented yet"),
        }
    }
}