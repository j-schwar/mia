//! `Visitor` and `Transformer` traits for walking ASTs.

use super::*;

/// A visitor capable of mutating AST nodes.
pub trait MutVisitor: Sized {
	fn visit_ident(&mut self, ident: &mut Ident) {
		noop_visit_ident(self, ident);
	}

	fn visit_type_value(&mut self, tv: &mut TypeValue) {
		noop_visit_type_value(self, tv);
	}

	fn visit_literal(&mut self, lit: &mut Literal) {
		noop_visit_literal(self, lit);
	}

	fn visit_expression(&mut self, expr: &mut Expression) {
		noop_visit_expression(self, expr);
	}

	fn visit_statement(&mut self, stmt: &mut Statement) {
		noop_visit_statement(self, stmt);
	}

	fn visit_code_block(&mut self, block: &mut CodeBlock) {
		noop_visit_code_block(self, block);
	}

	fn visit_parameter(&mut self, param: &mut Parameter) {
		noop_visit_parameter(self, param);
	}

	fn visit_function_body(&mut self, body: &mut FunctionBody) {
		noop_visit_function_body(self, body);
	}

	fn visit_function_definition(&mut self, func: &mut FunctionDefinition) {
		noop_visit_function_definition(self, func);
	}

	fn visit_translation_unit(&mut self, tu: &mut TranslationUnit) {
		noop_visit_translation_unit(self, tu);
	}
}

pub fn noop_visit_ident<V>(_v: &mut V, _ident: &Ident)
where
	V: MutVisitor,
{
}

pub fn noop_visit_type_value<V>(v: &mut V, TypeValue { name, .. }: &mut TypeValue)
where
	V: MutVisitor,
{
	v.visit_ident(name);
}

pub fn noop_visit_literal<V>(_v: &mut V, _lit: &Literal)
where
	V: MutVisitor,
{
}

pub fn noop_visit_expression<V>(v: &mut V, expr: &mut Expression)
where
	V: MutVisitor,
{
	use Expression::*;

	match expr {
		Variable(i) => v.visit_ident(i),

		Literal(l) => v.visit_literal(l),

		Call { name, args } => {
			v.visit_ident(name);
			for a in args {
				v.visit_expression(a);
			}
		}

		Prefix { op: _, expr } => v.visit_expression(expr),

		Infix { op: _, lhs, rhs } => {
			v.visit_expression(lhs);
			v.visit_expression(rhs);
		}

		IfElse {
			condition,
			true_block,
			false_block,
		} => {
			v.visit_expression(condition);
			v.visit_code_block(true_block);
			v.visit_code_block(false_block);
		}

		Group(expr) => v.visit_expression(expr),
	}
}

pub fn noop_visit_statement<V>(v: &mut V, stmt: &mut Statement)
where
	V: MutVisitor,
{
	use Statement::*;

	match stmt {
		Expr(e) => v.visit_expression(e),

		Define {
			ident,
			is_mutable: _,
			type_value,
			value,
		} => {
			v.visit_ident(ident);
			if let Some(tv) = type_value {
				v.visit_type_value(tv);
			}
			v.visit_expression(value);
		}

		Assign { ident, value } => {
			v.visit_ident(ident);
			v.visit_expression(value);
		}

		Return(e) => v.visit_expression(e),
	}
}

pub fn noop_visit_code_block<V>(v: &mut V, CodeBlock { statements }: &mut CodeBlock)
where
	V: MutVisitor,
{
	for statement in statements {
		v.visit_statement(statement);
	}
}

pub fn noop_visit_parameter<V>(v: &mut V, Parameter { ident, type_value }: &mut Parameter)
where
	V: MutVisitor,
{
	v.visit_ident(ident);
	if let Some(tv) = type_value {
		v.visit_type_value(tv);
	}
}

pub fn noop_visit_function_body<V>(v: &mut V, body: &mut FunctionBody)
where
	V: MutVisitor,
{
	use FunctionBody::*;

	match body {
		Expr(e) => v.visit_expression(e),
		Block(b) => v.visit_code_block(b),
	}
}

pub fn noop_visit_function_definition<V>(
	v: &mut V,
	FunctionDefinition {
		ident,
		parameters,
		return_type,
		body,
	}: &mut FunctionDefinition,
) where
	V: MutVisitor,
{
	v.visit_ident(ident);
	for parameter in parameters {
		v.visit_parameter(parameter);
	}
	if let Some(rt) = return_type {
		v.visit_type_value(rt);
	}
	v.visit_function_body(body);
}

pub fn noop_visit_translation_unit<V>(
	v: &mut V,
	TranslationUnit { functions }: &mut TranslationUnit,
) where
	V: MutVisitor,
{
	for function in functions {
		v.visit_function_definition(function);
	}
}

/// AST visitor capable of removing, replacing, and reordering nodes.
pub trait Transformer: Sized {
	fn transform_ident(&mut self, ident: Ident) -> Ident {
		noop_transform_ident(self, ident)
	}

	fn transform_type_value(&mut self, tv: TypeValue) -> TypeValue {
		noop_transform_type_value(self, tv)
	}

	fn transform_literal(&mut self, lit: Literal) -> Literal {
		noop_transform_literal(self, lit)
	}

	fn transform_expression(&mut self, expr: Expression) -> Expression {
		noop_transform_expression(self, expr)
	}

	fn transform_statement(&mut self, stmt: Statement) -> Statement {
		noop_transform_statement(self, stmt)
	}

	fn transform_code_block(&mut self, block: CodeBlock) -> CodeBlock {
		noop_transform_code_block(self, block)
	}

	fn transform_parameter(&mut self, param: Parameter) -> Parameter {
		noop_transform_parameter(self, param)
	}

	fn transform_function_body(&mut self, body: FunctionBody) -> FunctionBody {
		noop_transform_function_body(self, body)
	}

	fn transform_function_definition(&mut self, func: FunctionDefinition) -> FunctionDefinition {
		noop_transform_function_definition(self, func)
	}

	fn transform_translation_unit(&mut self, tu: TranslationUnit) -> TranslationUnit {
		noop_transform_translation_unit(self, tu)
	}
}

pub fn noop_transform_ident<T>(_t: &mut T, ident: Ident) -> Ident
where
	T: Transformer,
{
	ident
}

pub fn noop_transform_type_value<T>(t: &mut T, tv: TypeValue) -> TypeValue
where
	T: Transformer,
{
	let name = t.transform_ident(tv.name);
	TypeValue::new(name, tv.indirection, tv.mutable)
}

pub fn noop_transform_literal<T>(_t: &mut T, lit: Literal) -> Literal
where
	T: Transformer,
{
	lit
}

pub fn noop_transform_expression<T>(t: &mut T, expr: Expression) -> Expression
where
	T: Transformer,
{
	use Expression::*;

	match expr {
		Variable(i) => Variable(t.transform_ident(i)),

		Literal(l) => Literal(t.transform_literal(l)),

		Call { name, args } => {
			let name = t.transform_ident(name);
			let args = args
				.into_iter()
				.map(|e| t.transform_expression(e))
				.collect();
			Call { name, args }
		}

		Prefix { op, expr } => {
			let expr = t.transform_expression(*expr);
			Prefix {
				op,
				expr: Box::new(expr),
			}
		}

		Infix { op, lhs, rhs } => {
			let lhs = t.transform_expression(*lhs);
			let rhs = t.transform_expression(*rhs);
			Infix {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			}
		}

		IfElse {
			condition,
			true_block,
			false_block,
		} => {
			let condition = t.transform_expression(*condition);
			let true_block = t.transform_code_block(*true_block);
			let false_block = t.transform_code_block(*false_block);
			IfElse {
				condition: Box::new(condition),
				true_block: Box::new(true_block),
				false_block: Box::new(false_block),
			}
		}

		Group(e) => Group(Box::new(t.transform_expression(*e))),
	}
}

pub fn noop_transform_statement<T>(t: &mut T, stmt: Statement) -> Statement
where
	T: Transformer,
{
	use Statement::*;

	match stmt {
		Expr(e) => Expr(t.transform_expression(e)),

		Define {
			ident,
			is_mutable,
			type_value,
			value,
		} => {
			let ident = t.transform_ident(ident);
			let type_value = type_value.map(|tv| t.transform_type_value(tv));
			let value = t.transform_expression(value);
			Define {
				ident,
				is_mutable,
				type_value,
				value,
			}
		}

		Assign { ident, value } => {
			let ident = t.transform_ident(ident);
			let value = t.transform_expression(value);
			Assign { ident, value }
		}

		Return(e) => Return(t.transform_expression(e)),
	}
}

pub fn noop_transform_code_block<T>(t: &mut T, block: CodeBlock) -> CodeBlock
where
	T: Transformer,
{
	let statements = block
		.statements
		.into_iter()
		.map(|s| t.transform_statement(s))
		.collect();
	CodeBlock { statements }
}

pub fn noop_transform_parameter<T>(t: &mut T, param: Parameter) -> Parameter
where
	T: Transformer,
{
	let ident = t.transform_ident(param.ident);
	let type_value = param.type_value.map(|tv| t.transform_type_value(tv));
	Parameter { ident, type_value }
}

pub fn noop_transform_function_body<T>(t: &mut T, body: FunctionBody) -> FunctionBody
where
	T: Transformer,
{
	use FunctionBody::*;

	match body {
		Expr(e) => Expr(t.transform_expression(e)),
		Block(b) => Block(t.transform_code_block(b)),
	}
}

pub fn noop_transform_function_definition<T>(
	t: &mut T,
	func: FunctionDefinition,
) -> FunctionDefinition
where
	T: Transformer,
{
	let ident = t.transform_ident(func.ident);
	let parameters = func
		.parameters
		.into_iter()
		.map(|p| t.transform_parameter(p))
		.collect();
	let return_type = func.return_type.map(|rt| t.transform_type_value(rt));
	let body = t.transform_function_body(func.body);
	FunctionDefinition {
		ident,
		parameters,
		return_type,
		body,
	}
}

pub fn noop_transform_translation_unit<T>(t: &mut T, tu: TranslationUnit) -> TranslationUnit
where
	T: Transformer,
{
	let functions = tu
		.functions
		.into_iter()
		.map(|f| t.transform_function_definition(f))
		.collect();
	TranslationUnit { functions }
}
