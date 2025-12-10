use std::collections::HashMap;
use std::sync::Arc;

use crate::constraints::types::{Constraint, TypeRef, TypeVarId};
use crate::hir::HirFile;
use crate::{FileInput, ParserDatabase, lower_to_hir};
use jv_ast::expression::{Argument, Expression};
use jv_ast::statement::Statement;
use jv_ast::types::{Literal, TypeAnnotation};

struct Collector {
    next_typevar: u32,
    bindings: HashMap<String, TypeRef>,
    constraints: Vec<Constraint>,
}

impl Collector {
    fn new() -> Self {
        Self {
            next_typevar: 0,
            bindings: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    fn fresh_typevar(&mut self) -> TypeRef {
        let id = self.next_typevar;
        self.next_typevar += 1;
        TypeRef::TypeVar(TypeVarId(id))
    }

    fn literal_type(lit: &Literal) -> TypeAnnotation {
        match lit {
            Literal::Number(_) => TypeAnnotation::Simple("Number".into()),
            Literal::String(_) => TypeAnnotation::Simple("String".into()),
            Literal::Boolean(_) => TypeAnnotation::Simple("Boolean".into()),
            Literal::Null => {
                TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple("Any".into())))
            }
            Literal::Character(_) => TypeAnnotation::Simple("Char".into()),
            Literal::Regex(regex) => TypeAnnotation::Simple(regex.pattern.clone()),
        }
    }

    fn type_for_identifier(&mut self, name: &str) -> TypeRef {
        if let Some(t) = self.bindings.get(name) {
            return t.clone();
        }
        let ty = self.fresh_typevar();
        self.bindings.insert(name.to_string(), ty.clone());
        ty
    }

    fn collect_expression(&mut self, expr: &Expression) -> TypeRef {
        match expr {
            Expression::Literal(lit, _) => TypeRef::Annotation(Self::literal_type(lit)),
            Expression::Identifier(name, _) => self.type_for_identifier(name),
            Expression::Unary { operand, .. } => self.collect_expression(operand),
            Expression::Binary { left, right, .. } => {
                let l = self.collect_expression(left);
                let r = self.collect_expression(right);
                self.constraints
                    .push(Constraint::Equal(l.clone(), r.clone()));
                l
            }
            Expression::Call { function, args, .. } => {
                let func_ty = self.collect_expression(function);
                let mut arg_tys = Vec::new();
                for arg in args {
                    let ty = match arg {
                        Argument::Positional(expr) => self.collect_expression(expr),
                        Argument::Named { value, .. } => self.collect_expression(value),
                    };
                    arg_tys.push(ty);
                }
                let ret_ty = self.fresh_typevar();
                self.constraints.push(Constraint::Callable {
                    function: func_ty.clone(),
                    args: arg_tys,
                    ret: ret_ty.clone(),
                });
                ret_ty
            }
            Expression::MemberAccess {
                object, property, ..
            }
            | Expression::NullSafeMemberAccess {
                object, property, ..
            } => {
                let obj_ty = self.collect_expression(object);
                let field_ty = self.fresh_typevar();
                self.constraints.push(Constraint::HasField {
                    target: obj_ty,
                    field: property.clone(),
                    field_type: field_ty.clone(),
                });
                field_ty
            }
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                let obj_ty = self.collect_expression(object);
                let idx_ty = self.collect_expression(index);
                let elem_ty = self.fresh_typevar();
                self.constraints.push(Constraint::Subtype {
                    sub: idx_ty,
                    sup: TypeRef::Annotation(TypeAnnotation::Simple("Number".into())),
                });
                self.constraints.push(Constraint::HasField {
                    target: obj_ty,
                    field: "[]".to_string(),
                    field_type: elem_ty.clone(),
                });
                elem_ty
            }
            Expression::Block { statements, .. } => {
                let snapshot = self.bindings.clone();
                let mut last_ty = TypeRef::Annotation(TypeAnnotation::Simple("Unit".into()));
                for stmt in statements {
                    last_ty = self.collect_statement(stmt);
                }
                self.bindings = snapshot;
                last_ty
            }
            Expression::When { arms, else_arm, .. } => {
                // 簡易: 各アームの body を同一型に揃える。
                let result_ty = self.fresh_typevar();
                for arm in arms {
                    let body_ty = self.collect_expression(&arm.body);
                    self.constraints
                        .push(Constraint::Equal(result_ty.clone(), body_ty));
                }
                if let Some(else_arm) = else_arm {
                    let else_ty = self.collect_expression(else_arm);
                    self.constraints
                        .push(Constraint::Equal(result_ty.clone(), else_ty));
                }
                result_ty
            }
            Expression::If {
                then_branch,
                else_branch,
                ..
            } => {
                let then_ty = self.collect_expression(then_branch);
                let else_ty = else_branch
                    .as_ref()
                    .map(|e| self.collect_expression(e))
                    .unwrap_or_else(|| TypeRef::Annotation(TypeAnnotation::Simple("Unit".into())));
                let res = self.fresh_typevar();
                self.constraints
                    .push(Constraint::Equal(res.clone(), then_ty));
                self.constraints
                    .push(Constraint::Equal(res.clone(), else_ty));
                res
            }
            Expression::Lambda {
                body, parameters, ..
            } => {
                let snapshot = self.bindings.clone();
                let param_types: Vec<TypeRef> = parameters
                    .iter()
                    .map(|p| {
                        if let Some(ty) = &p.type_annotation {
                            TypeRef::Annotation(ty.clone())
                        } else {
                            self.fresh_typevar()
                        }
                    })
                    .collect();
                for (param, ty) in parameters.iter().zip(param_types.iter()) {
                    self.bindings.insert(param.name.clone(), ty.clone());
                }
                let ret_ty = match body.as_ref() {
                    Expression::Block { statements, .. } => {
                        let mut last = TypeRef::Annotation(TypeAnnotation::Simple("Unit".into()));
                        for stmt in statements {
                            last = self.collect_statement(stmt);
                        }
                        last
                    }
                    other => self.collect_expression(other),
                };
                self.bindings = snapshot;
                let fn_ty = self.fresh_typevar();
                self.constraints.push(Constraint::Callable {
                    function: fn_ty.clone(),
                    args: param_types,
                    ret: ret_ty.clone(),
                });
                fn_ty
            }
            Expression::Array { elements, .. } => {
                let elem_ty = self.fresh_typevar();
                let arr_ty = self.fresh_typevar();
                self.constraints.push(Constraint::HasField {
                    target: arr_ty.clone(),
                    field: "[]".to_string(),
                    field_type: elem_ty.clone(),
                });
                for elem in elements {
                    let t = self.collect_expression(elem);
                    self.constraints.push(Constraint::Equal(elem_ty.clone(), t));
                }
                arr_ty
            }
            Expression::Tuple { elements, .. } => {
                let tuple_ty = self.fresh_typevar();
                for (idx, elem) in elements.iter().enumerate() {
                    let t = self.collect_expression(elem);
                    self.constraints.push(Constraint::HasField {
                        target: tuple_ty.clone(),
                        field: format!("[{}]", idx),
                        field_type: t.clone(),
                    });
                }
                tuple_ty
            }
            Expression::TypeCast { expr, target, .. } => {
                let inner = self.collect_expression(expr);
                let tgt = TypeRef::Annotation(target.clone());
                self.constraints.push(Constraint::Subtype {
                    sub: inner,
                    sup: tgt.clone(),
                });
                tgt
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                let body_ty = self.collect_expression(body);
                let result_ty = body_ty.clone();
                for clause in catch_clauses {
                    let catch_ty = self.collect_expression(&clause.body);
                    self.constraints
                        .push(Constraint::Equal(result_ty.clone(), catch_ty));
                }
                if let Some(fin) = finally_block {
                    let fin_ty = self.collect_expression(fin);
                    self.constraints
                        .push(Constraint::Equal(result_ty.clone(), fin_ty));
                }
                result_ty
            }
            Expression::This(_) | Expression::Super(_) => self.fresh_typevar(),
            _ => self.fresh_typevar(),
        }
    }

    fn collect_statement(&mut self, stmt: &Statement) -> TypeRef {
        match stmt {
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let binding_ty = self.fresh_typevar();
                self.bindings.insert(name.clone(), binding_ty.clone());
                let init_ty = self.collect_expression(initializer);
                self.constraints
                    .push(Constraint::Equal(binding_ty.clone(), init_ty));
                if let Some(ann) = type_annotation {
                    self.constraints.push(Constraint::Equal(
                        binding_ty.clone(),
                        TypeRef::Annotation(ann.clone()),
                    ));
                }
                binding_ty
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let binding_ty = self.fresh_typevar();
                self.bindings.insert(name.clone(), binding_ty.clone());
                if let Some(init) = initializer {
                    let init_ty = self.collect_expression(init);
                    self.constraints
                        .push(Constraint::Equal(binding_ty.clone(), init_ty));
                }
                if let Some(ann) = type_annotation {
                    self.constraints.push(Constraint::Equal(
                        binding_ty.clone(),
                        TypeRef::Annotation(ann.clone()),
                    ));
                }
                binding_ty
            }
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                ..
            } => {
                let snapshot = self.bindings.clone();
                let fn_ty = self.fresh_typevar();
                self.bindings.insert(name.clone(), fn_ty.clone());
                let param_tys: Vec<TypeRef> = parameters
                    .iter()
                    .map(|p| {
                        if let Some(ty) = &p.type_annotation {
                            TypeRef::Annotation(ty.clone())
                        } else {
                            self.fresh_typevar()
                        }
                    })
                    .collect();
                // パラメータ名を bindings に入れることで本体中の Identifier を同一 TypeRef にする。
                for (param, ty) in parameters.iter().zip(param_tys.iter()) {
                    self.bindings.insert(param.name.clone(), ty.clone());
                }
                let body_ty = self.collect_expression(body);
                let ret_ty = return_type
                    .as_ref()
                    .map(|ty| TypeRef::Annotation(ty.clone()))
                    .unwrap_or(body_ty.clone());
                self.constraints.push(Constraint::Callable {
                    function: fn_ty.clone(),
                    args: param_tys,
                    ret: ret_ty.clone(),
                });
                self.constraints
                    .push(Constraint::Equal(ret_ty.clone(), body_ty));
                self.bindings = snapshot;
                fn_ty
            }
            Statement::Assignment { target, value, .. } => {
                let target_ty = self.collect_expression(target);
                let value_ty = self.collect_expression(value);
                self.constraints
                    .push(Constraint::Equal(target_ty.clone(), value_ty));
                target_ty
            }
            Statement::Expression { expr, .. } => self.collect_expression(expr),
            Statement::Return { value, .. } => value
                .as_ref()
                .map(|v| self.collect_expression(v))
                .unwrap_or_else(|| TypeRef::Annotation(TypeAnnotation::Simple("Unit".into()))),
            _ => TypeRef::Annotation(TypeAnnotation::Simple("Unit".into())),
        }
    }
}

/// HIR から制約を収集する。
pub fn collect_constraints(db: &dyn ParserDatabase, file: FileInput) -> Arc<Vec<Constraint>> {
    let hir: Arc<HirFile> = lower_to_hir(db, file);
    let mut collector = Collector::new();
    for stmt in &hir.statements {
        collector.collect_statement(stmt);
    }
    Arc::new(collector.constraints)
}
