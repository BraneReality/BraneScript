/**Typed node `anonStruct`

This node has named children of type `memberInit*` ([`Memberinit`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Anonstruct<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Anonstruct<'tree> {
    /**Get the node's not-extra named children.

These children have type `memberInit*` ([`Memberinit`])*/
    #[inline]
    pub fn memberinits<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Memberinit<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Memberinit<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Anonstruct<'tree> {
    type Child = Memberinit<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Anonstruct<'tree> {
    type WithLifetime<'a> = Anonstruct<'a>;
    const KIND: &'static str = "anonStruct";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "anonStruct" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "anonStruct");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `anonStructType`

This node has named children of type `valueDef*` ([`Valuedef`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Anonstructtype<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Anonstructtype<'tree> {
    /**Get the node's not-extra named children.

These children have type `valueDef*` ([`Valuedef`])*/
    #[inline]
    pub fn valuedefs<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Valuedef<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Valuedef<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Anonstructtype<'tree> {
    type Child = Valuedef<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Anonstructtype<'tree> {
    type WithLifetime<'a> = Anonstructtype<'a>;
    const KIND: &'static str = "anonStructType";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "anonStructType" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "anonStructType");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `assign`

This node has these fields:

- `left`: `expression` ([`Expression`])
- `right`: `expression` ([`Expression`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Assign<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Assign<'tree> {
    /**Get the field `left`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn left(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("left")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `right`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn right(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("right")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Assign<'tree> {
    type WithLifetime<'a> = Assign<'a>;
    const KIND: &'static str = "assign";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "assign" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "assign");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `binary_operator`

This node has these fields:

- `left`: `expression` ([`Expression`])
- `operator`: `{!= | % | & | && | * | + | - | -> | . | / | < | << | <= | == | > | >= | >> | ^ | | | ||}` ([`symbols::NotEq`] | [`symbols::Mod`] | [`symbols::And`] | [`symbols::AndAnd`] | [`symbols::Mul`] | [`symbols::Add`] | [`symbols::Sub`] | [`symbols::SubGt`] | [`symbols::Dot`] | [`symbols::Div`] | [`symbols::Lt`] | [`symbols::LtLt`] | [`symbols::LtEq`] | [`symbols::EqEq`] | [`symbols::Gt`] | [`symbols::GtEq`] | [`symbols::GtGt`] | [`symbols::BitXor`] | [`symbols::Or`] | [`symbols::OrOr`])
- `right`: `expression` ([`Expression`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct BinaryOperator<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> BinaryOperator<'tree> {
    /**Get the field `left`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn left(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("left")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `operator`.

This child has type `{!= | % | & | && | * | + | - | -> | . | / | < | << | <= | == | > | >= | >> | ^ | | | ||}`:

- [`symbols::NotEq`]
- [`symbols::Mod`]
- [`symbols::And`]
- [`symbols::AndAnd`]
- [`symbols::Mul`]
- [`symbols::Add`]
- [`symbols::Sub`]
- [`symbols::SubGt`]
- [`symbols::Dot`]
- [`symbols::Div`]
- [`symbols::Lt`]
- [`symbols::LtLt`]
- [`symbols::LtEq`]
- [`symbols::EqEq`]
- [`symbols::Gt`]
- [`symbols::GtEq`]
- [`symbols::GtGt`]
- [`symbols::BitXor`]
- [`symbols::Or`]
- [`symbols::OrOr`]
*/
    #[inline]
    pub fn operator(
        &self,
    ) -> ::type_sitter::NodeResult<
        'tree,
        anon_unions::NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
            'tree,
        >,
    > {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("operator")
            .map(
                <anon_unions::NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `right`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn right(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("right")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for BinaryOperator<'tree> {
    type WithLifetime<'a> = BinaryOperator<'a>;
    const KIND: &'static str = "binary_operator";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "binary_operator" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "binary_operator");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `block`

This node has these fields:

- `expressions`: `{; | expression}*` ([`symbols::Semicolon`] | [`Expression`])
- `return`: `expression?` ([`Expression`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Block<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Block<'tree> {
    /**Get the children of field `expressions`.

These children have type `{; | expression}*`:

- [`symbols::Semicolon`]
- [`Expression`]
*/
    #[inline]
    pub fn expressionss<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, anon_unions::Semicolon_Expression<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("expressions", &mut c.0)
            .map(
                <anon_unions::Semicolon_Expression<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
    /**Get the optional field `return`.

This child has type `expression?` ([`Expression`])*/
    #[inline]
    pub fn r#return(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Expression<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("return")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Block<'tree> {
    type WithLifetime<'a> = Block<'a>;
    const KIND: &'static str = "block";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "block" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "block");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `call`

This node has these fields:

- `args`: `anonStruct` ([`Anonstruct`])
- `func`: `expression` ([`Expression`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Call<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Call<'tree> {
    /**Get the field `args`.

This child has type `anonStruct` ([`Anonstruct`])*/
    #[inline]
    pub fn args(&self) -> ::type_sitter::NodeResult<'tree, Anonstruct<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("args")
            .map(<Anonstruct<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `func`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn func(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("func")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Call<'tree> {
    type WithLifetime<'a> = Call<'a>;
    const KIND: &'static str = "call";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "call" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "call");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `callSig`

This node has these fields:

- `input`: `anonStructType` ([`Anonstructtype`])
- `output`: `anonStructType?` ([`Anonstructtype`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Callsig<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Callsig<'tree> {
    /**Get the field `input`.

This child has type `anonStructType` ([`Anonstructtype`])*/
    #[inline]
    pub fn input(&self) -> ::type_sitter::NodeResult<'tree, Anonstructtype<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("input")
            .map(<Anonstructtype<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the optional field `output`.

This child has type `anonStructType?` ([`Anonstructtype`])*/
    #[inline]
    pub fn output(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Anonstructtype<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("output")
            .map(<Anonstructtype<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Callsig<'tree> {
    type WithLifetime<'a> = Callsig<'a>;
    const KIND: &'static str = "callSig";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "callSig" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "callSig");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `comment`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Comment<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Comment<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Comment<'tree> {
    type WithLifetime<'a> = Comment<'a>;
    const KIND: &'static str = "comment";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "comment" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "comment");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `expression`

This node has a named child of type `{anonStruct | assign | binary_operator | call | expression | number | scopedIdentifier | unary_operator | variableDefinition}`:

- [`Anonstruct`]
- [`Assign`]
- [`BinaryOperator`]
- [`Call`]
- [`Expression`]
- [`Number`]
- [`Scopedidentifier`]
- [`UnaryOperator`]
- [`Variabledefinition`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Expression<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Expression<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for Expression<'tree> {
    type Child = anon_unions::Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Expression<'tree> {
    type WithLifetime<'a> = Expression<'a>;
    const KIND: &'static str = "expression";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "expression" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "expression");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `function`

This node has named children of type `{block | callSig | scopedIdentifier}+`:

- [`Block`]
- [`Callsig`]
- [`Scopedidentifier`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Function<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Function<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Function<'tree> {
    type Child = anon_unions::Block_Callsig_Scopedidentifier<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Function<'tree> {
    type WithLifetime<'a> = Function<'a>;
    const KIND: &'static str = "function";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "function" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "function");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `identifier`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Identifier<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Identifier<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Identifier<'tree> {
    type WithLifetime<'a> = Identifier<'a>;
    const KIND: &'static str = "identifier";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "identifier" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "identifier");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `memberInit`

This node has these fields:

- `id`: `identifier` ([`Identifier`])
- `value`: `expression` ([`Expression`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Memberinit<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Memberinit<'tree> {
    /**Get the field `id`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `value`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn value(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("value")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Memberinit<'tree> {
    type WithLifetime<'a> = Memberinit<'a>;
    const KIND: &'static str = "memberInit";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "memberInit" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "memberInit");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `module`

This node has these fields:

- `defs`: `{function | module | pipeline}*` ([`Function`] | [`Module`] | [`Pipeline`])
- `id`: `identifier` ([`Identifier`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Module<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Module<'tree> {
    /**Get the children of field `defs`.

These children have type `{function | module | pipeline}*`:

- [`Function`]
- [`Module`]
- [`Pipeline`]
*/
    #[inline]
    pub fn defss<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<
            'tree,
            anon_unions::Function_Module_Pipeline<'tree>,
        >,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("defs", &mut c.0)
            .map(
                <anon_unions::Function_Module_Pipeline<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
    /**Get the field `id`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Module<'tree> {
    type WithLifetime<'a> = Module<'a>;
    const KIND: &'static str = "module";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "module" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "module");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `number`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Number<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Number<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Number<'tree> {
    type WithLifetime<'a> = Number<'a>;
    const KIND: &'static str = "number";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "number" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "number");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `pipeline`

This node has these fields:

- `callSig`: `callSig` ([`Callsig`])
- `id`: `identifier` ([`Identifier`])
- `stages`: `pipelineStage*` ([`Pipelinestage`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Pipeline<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Pipeline<'tree> {
    /**Get the field `callSig`.

This child has type `callSig` ([`Callsig`])*/
    #[inline]
    pub fn callSig(&self) -> ::type_sitter::NodeResult<'tree, Callsig<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("callSig")
            .map(<Callsig<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `id`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the children of field `stages`.

These children have type `pipelineStage*` ([`Pipelinestage`])*/
    #[inline]
    pub fn stagess<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Pipelinestage<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("stages", &mut c.0)
            .map(<Pipelinestage<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Pipeline<'tree> {
    type WithLifetime<'a> = Pipeline<'a>;
    const KIND: &'static str = "pipeline";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "pipeline" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "pipeline");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `pipelineStage`

This node has these fields:

- `body`: `block` ([`Block`])
- `callSig`: `callSig?` ([`Callsig`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Pipelinestage<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Pipelinestage<'tree> {
    /**Get the field `body`.

This child has type `block` ([`Block`])*/
    #[inline]
    pub fn body(&self) -> ::type_sitter::NodeResult<'tree, Block<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("body")
            .map(<Block<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the optional field `callSig`.

This child has type `callSig?` ([`Callsig`])*/
    #[inline]
    pub fn callSig(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Callsig<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("callSig")
            .map(<Callsig<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Pipelinestage<'tree> {
    type WithLifetime<'a> = Pipelinestage<'a>;
    const KIND: &'static str = "pipelineStage";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "pipelineStage" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "pipelineStage");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `refOp`

This node has these fields:

- `const`: `const?` ([`unnamed::Const`])
- `mut`: `mut?` ([`unnamed::Mut`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Refop<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Refop<'tree> {
    /**Get the optional field `const`.

This child has type `const?` ([`unnamed::Const`])*/
    #[inline]
    pub fn r#const(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, unnamed::Const<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("const")
            .map(<unnamed::Const<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the optional field `mut`.

This child has type `mut?` ([`unnamed::Mut`])*/
    #[inline]
    pub fn r#mut(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, unnamed::Mut<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("mut")
            .map(<unnamed::Mut<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Refop<'tree> {
    type WithLifetime<'a> = Refop<'a>;
    const KIND: &'static str = "refOp";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "refOp" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "refOp");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `scopedIdentifier`

This node has named children of type `scopedIdentifierSegment+` ([`Scopedidentifiersegment`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Scopedidentifier<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Scopedidentifier<'tree> {
    /**Get the node's not-extra named children.

These children have type `scopedIdentifierSegment+` ([`Scopedidentifiersegment`])*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn scopedidentifiersegments<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Scopedidentifiersegment<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(
                <Scopedidentifiersegment<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Scopedidentifier<'tree> {
    type Child = Scopedidentifiersegment<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Scopedidentifier<'tree> {
    type WithLifetime<'a> = Scopedidentifier<'a>;
    const KIND: &'static str = "scopedIdentifier";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "scopedIdentifier" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "scopedIdentifier");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `scopedIdentifierSegment`

This node has these fields:

- `generic`: `templateArguments?` ([`Templatearguments`])
- `id`: `identifier` ([`Identifier`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Scopedidentifiersegment<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Scopedidentifiersegment<'tree> {
    /**Get the optional field `generic`.

This child has type `templateArguments?` ([`Templatearguments`])*/
    #[inline]
    pub fn generic(
        &self,
    ) -> ::std::option::Option<
        ::type_sitter::NodeResult<'tree, Templatearguments<'tree>>,
    > {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("generic")
            .map(<Templatearguments<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the field `id`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Scopedidentifiersegment<'tree> {
    type WithLifetime<'a> = Scopedidentifiersegment<'a>;
    const KIND: &'static str = "scopedIdentifierSegment";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "scopedIdentifierSegment" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "scopedIdentifierSegment");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `source_file`

This node has named children of type `module*` ([`Module`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct SourceFile<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> SourceFile<'tree> {
    /**Get the node's not-extra named children.

These children have type `module*` ([`Module`])*/
    #[inline]
    pub fn modules<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Module<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Module<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for SourceFile<'tree> {
    type Child = Module<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for SourceFile<'tree> {
    type WithLifetime<'a> = SourceFile<'a>;
    const KIND: &'static str = "source_file";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "source_file" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "source_file");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `templateArgument`

This node has a named child of type `type` ([`Type`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Templateargument<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Templateargument<'tree> {
    /**Get the node's only not-extra named child.

This child has type `type` ([`Type`])*/
    #[inline]
    pub fn r#type(&self) -> ::type_sitter::NodeResult<'tree, Type<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<Type<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for Templateargument<'tree> {
    type Child = Type<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Templateargument<'tree> {
    type WithLifetime<'a> = Templateargument<'a>;
    const KIND: &'static str = "templateArgument";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "templateArgument" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "templateArgument");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `templateArguments`

This node has named children of type `templateArgument+` ([`Templateargument`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Templatearguments<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Templatearguments<'tree> {
    /**Get the node's not-extra named children.

These children have type `templateArgument+` ([`Templateargument`])*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn templatearguments<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Templateargument<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Templateargument<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Templatearguments<'tree> {
    type Child = Templateargument<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Templatearguments<'tree> {
    type WithLifetime<'a> = Templatearguments<'a>;
    const KIND: &'static str = "templateArguments";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "templateArguments" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "templateArguments");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type`

This node has these fields:

- `id`: `scopedIdentifier` ([`Scopedidentifier`])
- `refOp`: `refOp*` ([`Refop`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Type<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Type<'tree> {
    /**Get the field `id`.

This child has type `scopedIdentifier` ([`Scopedidentifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Scopedidentifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Scopedidentifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the children of field `refOp`.

These children have type `refOp*` ([`Refop`])*/
    #[inline]
    pub fn refOps<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Refop<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("refOp", &mut c.0)
            .map(<Refop<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Type<'tree> {
    type WithLifetime<'a> = Type<'a>;
    const KIND: &'static str = "type";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `unary_operator`

This node has these fields:

- `expression`: `expression` ([`Expression`])
- `operator`: `{! | & | * | - | ~}` ([`symbols::Not`] | [`symbols::And`] | [`symbols::Mul`] | [`symbols::Sub`] | [`symbols::BitNot`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct UnaryOperator<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> UnaryOperator<'tree> {
    /**Get the field `expression`.

This child has type `expression` ([`Expression`])*/
    #[inline]
    pub fn expression(&self) -> ::type_sitter::NodeResult<'tree, Expression<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("expression")
            .map(<Expression<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `operator`.

This child has type `{! | & | * | - | ~}`:

- [`symbols::Not`]
- [`symbols::And`]
- [`symbols::Mul`]
- [`symbols::Sub`]
- [`symbols::BitNot`]
*/
    #[inline]
    pub fn operator(
        &self,
    ) -> ::type_sitter::NodeResult<'tree, anon_unions::Not_And_Mul_Sub_BitNot<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("operator")
            .map(
                <anon_unions::Not_And_Mul_Sub_BitNot<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for UnaryOperator<'tree> {
    type WithLifetime<'a> = UnaryOperator<'a>;
    const KIND: &'static str = "unary_operator";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "unary_operator" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "unary_operator");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `valueDef`

This node has these fields:

- `id`: `identifier` ([`Identifier`])
- `mut`: `mut?` ([`unnamed::Mut`])
- `type`: `type?` ([`Type`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Valuedef<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Valuedef<'tree> {
    /**Get the field `id`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn id(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("id")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the optional field `mut`.

This child has type `mut?` ([`unnamed::Mut`])*/
    #[inline]
    pub fn r#mut(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, unnamed::Mut<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("mut")
            .map(<unnamed::Mut<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the optional field `type`.

This child has type `type?` ([`Type`])*/
    #[inline]
    pub fn r#type(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Type<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("type")
            .map(<Type<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Valuedef<'tree> {
    type WithLifetime<'a> = Valuedef<'a>;
    const KIND: &'static str = "valueDef";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "valueDef" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "valueDef");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `variableDefinition`

This node has these fields:

- `def`: `valueDef` ([`Valuedef`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Variabledefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Variabledefinition<'tree> {
    /**Get the field `def`.

This child has type `valueDef` ([`Valuedef`])*/
    #[inline]
    pub fn def(&self) -> ::type_sitter::NodeResult<'tree, Valuedef<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("def")
            .map(<Valuedef<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Variabledefinition<'tree> {
    type WithLifetime<'a> = Variabledefinition<'a>;
    const KIND: &'static str = "variableDefinition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "variableDefinition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "variableDefinition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
pub mod unnamed {
    #[allow(unused_imports)]
    use super::*;
    /**Typed node `const`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Const<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Const<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Const<'tree> {
        type WithLifetime<'a> = Const<'a>;
        const KIND: &'static str = "const";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "const" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "const");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `func`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Func<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Func<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Func<'tree> {
        type WithLifetime<'a> = Func<'a>;
        const KIND: &'static str = "func";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "func" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "func");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `let`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Let<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Let<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Let<'tree> {
        type WithLifetime<'a> = Let<'a>;
        const KIND: &'static str = "let";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "let" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "let");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `mod`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Mod<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Mod<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Mod<'tree> {
        type WithLifetime<'a> = Mod<'a>;
        const KIND: &'static str = "mod";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "mod" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "mod");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `mut`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Mut<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Mut<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Mut<'tree> {
        type WithLifetime<'a> = Mut<'a>;
        const KIND: &'static str = "mut";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "mut" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "mut");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `pipe`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Pipe<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Pipe<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Pipe<'tree> {
        type WithLifetime<'a> = Pipe<'a>;
        const KIND: &'static str = "pipe";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "pipe" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "pipe");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
}
pub mod symbols {
    #[allow(unused_imports)]
    use super::*;
    /**Typed node `!`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Not<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Not<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Not<'tree> {
        type WithLifetime<'a> = Not<'a>;
        const KIND: &'static str = "!";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "!" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "!");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `!=`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct NotEq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> NotEq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for NotEq<'tree> {
        type WithLifetime<'a> = NotEq<'a>;
        const KIND: &'static str = "!=";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "!=" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "!=");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `%`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Mod<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Mod<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Mod<'tree> {
        type WithLifetime<'a> = Mod<'a>;
        const KIND: &'static str = "%";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "%" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "%");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `&`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct And<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> And<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for And<'tree> {
        type WithLifetime<'a> = And<'a>;
        const KIND: &'static str = "&";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "&" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "&");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `&&`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct AndAnd<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> AndAnd<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for AndAnd<'tree> {
        type WithLifetime<'a> = AndAnd<'a>;
        const KIND: &'static str = "&&";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "&&" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "&&");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `(`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LParen<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LParen<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LParen<'tree> {
        type WithLifetime<'a> = LParen<'a>;
        const KIND: &'static str = "(";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "(" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "(");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `)`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RParen<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RParen<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RParen<'tree> {
        type WithLifetime<'a> = RParen<'a>;
        const KIND: &'static str = ")";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ")" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ")");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `*`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Mul<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Mul<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Mul<'tree> {
        type WithLifetime<'a> = Mul<'a>;
        const KIND: &'static str = "*";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "*" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "*");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `+`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Add<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Add<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Add<'tree> {
        type WithLifetime<'a> = Add<'a>;
        const KIND: &'static str = "+";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "+" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "+");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `,`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Comma<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Comma<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Comma<'tree> {
        type WithLifetime<'a> = Comma<'a>;
        const KIND: &'static str = ",";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "," {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ",");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `-`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Sub<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Sub<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Sub<'tree> {
        type WithLifetime<'a> = Sub<'a>;
        const KIND: &'static str = "-";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "-" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "-");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `->`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct SubGt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> SubGt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for SubGt<'tree> {
        type WithLifetime<'a> = SubGt<'a>;
        const KIND: &'static str = "->";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "->" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "->");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `.`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Dot<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Dot<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Dot<'tree> {
        type WithLifetime<'a> = Dot<'a>;
        const KIND: &'static str = ".";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "." {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ".");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `/`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Div<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Div<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Div<'tree> {
        type WithLifetime<'a> = Div<'a>;
        const KIND: &'static str = "/";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "/" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "/");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `:`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Colon<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Colon<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Colon<'tree> {
        type WithLifetime<'a> = Colon<'a>;
        const KIND: &'static str = ":";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ":" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ":");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `::`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct ColonColon<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> ColonColon<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for ColonColon<'tree> {
        type WithLifetime<'a> = ColonColon<'a>;
        const KIND: &'static str = "::";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "::" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "::");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `;`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Semicolon<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Semicolon<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Semicolon<'tree> {
        type WithLifetime<'a> = Semicolon<'a>;
        const KIND: &'static str = ";";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ";" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ";");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `<`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Lt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Lt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Lt<'tree> {
        type WithLifetime<'a> = Lt<'a>;
        const KIND: &'static str = "<";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "<" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "<");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `<<`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LtLt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LtLt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LtLt<'tree> {
        type WithLifetime<'a> = LtLt<'a>;
        const KIND: &'static str = "<<";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "<<" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "<<");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `<=`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LtEq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LtEq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LtEq<'tree> {
        type WithLifetime<'a> = LtEq<'a>;
        const KIND: &'static str = "<=";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "<=" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "<=");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `=`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Eq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Eq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Eq<'tree> {
        type WithLifetime<'a> = Eq<'a>;
        const KIND: &'static str = "=";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "=" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "=");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `==`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct EqEq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> EqEq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for EqEq<'tree> {
        type WithLifetime<'a> = EqEq<'a>;
        const KIND: &'static str = "==";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "==" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "==");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `>`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Gt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Gt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Gt<'tree> {
        type WithLifetime<'a> = Gt<'a>;
        const KIND: &'static str = ">";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ">" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ">");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `>=`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct GtEq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> GtEq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for GtEq<'tree> {
        type WithLifetime<'a> = GtEq<'a>;
        const KIND: &'static str = ">=";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ">=" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ">=");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `>>`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct GtGt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> GtGt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for GtGt<'tree> {
        type WithLifetime<'a> = GtGt<'a>;
        const KIND: &'static str = ">>";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ">>" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ">>");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `[`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LBracket<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LBracket<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LBracket<'tree> {
        type WithLifetime<'a> = LBracket<'a>;
        const KIND: &'static str = "[";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "[" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "[");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `]`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RBracket<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RBracket<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RBracket<'tree> {
        type WithLifetime<'a> = RBracket<'a>;
        const KIND: &'static str = "]";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "]" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "]");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `^`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct BitXor<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> BitXor<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for BitXor<'tree> {
        type WithLifetime<'a> = BitXor<'a>;
        const KIND: &'static str = "^";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "^" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "^");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `{`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LBrace<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LBrace<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LBrace<'tree> {
        type WithLifetime<'a> = LBrace<'a>;
        const KIND: &'static str = "{";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "{" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "{");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `|`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Or<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Or<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Or<'tree> {
        type WithLifetime<'a> = Or<'a>;
        const KIND: &'static str = "|";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "|" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "|");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `||`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct OrOr<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> OrOr<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for OrOr<'tree> {
        type WithLifetime<'a> = OrOr<'a>;
        const KIND: &'static str = "||";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "||" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "||");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `}`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RBrace<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RBrace<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RBrace<'tree> {
        type WithLifetime<'a> = RBrace<'a>;
        const KIND: &'static str = "}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "}" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "}");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `~`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct BitNot<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> BitNot<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for BitNot<'tree> {
        type WithLifetime<'a> = BitNot<'a>;
        const KIND: &'static str = "~";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "~" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "~");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
}
pub mod anon_unions {
    #[allow(unused_imports)]
    use super::*;
    /**One of `{anonStruct | assign | binary_operator | call | expression | number | scopedIdentifier | unary_operator | variableDefinition}`:
- [`Anonstruct`]
- [`Assign`]
- [`BinaryOperator`]
- [`Call`]
- [`Expression`]
- [`Number`]
- [`Scopedidentifier`]
- [`UnaryOperator`]
- [`Variabledefinition`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition<
        'tree,
    > {
        Anonstruct(Anonstruct<'tree>),
        Assign(Assign<'tree>),
        BinaryOperator(BinaryOperator<'tree>),
        Call(Call<'tree>),
        Expression(Expression<'tree>),
        Number(Number<'tree>),
        Scopedidentifier(Scopedidentifier<'tree>),
        UnaryOperator(UnaryOperator<'tree>),
        Variabledefinition(Variabledefinition<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<
        'tree,
    > Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition<
        'tree,
    > {
        ///Returns the node if it is of type `anonStruct` ([`Anonstruct`]), otherwise returns `None`
        #[inline]
        pub fn as_anonstruct(self) -> ::std::option::Option<Anonstruct<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Anonstruct(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `assign` ([`Assign`]), otherwise returns `None`
        #[inline]
        pub fn as_assign(self) -> ::std::option::Option<Assign<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Assign(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `binary_operator` ([`BinaryOperator`]), otherwise returns `None`
        #[inline]
        pub fn as_binary_operator(self) -> ::std::option::Option<BinaryOperator<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::BinaryOperator(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `call` ([`Call`]), otherwise returns `None`
        #[inline]
        pub fn as_call(self) -> ::std::option::Option<Call<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Call(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `expression` ([`Expression`]), otherwise returns `None`
        #[inline]
        pub fn as_expression(self) -> ::std::option::Option<Expression<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Expression(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `number` ([`Number`]), otherwise returns `None`
        #[inline]
        pub fn as_number(self) -> ::std::option::Option<Number<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Number(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `scopedIdentifier` ([`Scopedidentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_scopedidentifier(
            self,
        ) -> ::std::option::Option<Scopedidentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Scopedidentifier(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `unary_operator` ([`UnaryOperator`]), otherwise returns `None`
        #[inline]
        pub fn as_unary_operator(self) -> ::std::option::Option<UnaryOperator<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::UnaryOperator(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `variableDefinition` ([`Variabledefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_variabledefinition(
            self,
        ) -> ::std::option::Option<Variabledefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Variabledefinition(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition<
        'tree,
    > {
        type WithLifetime<'a> = Anonstruct_Assign_BinaryOperator_Call_Expression_Number_Scopedidentifier_UnaryOperator_Variabledefinition<
            'a,
        >;
        const KIND: &'static str = "{anonStruct | assign | binary_operator | call | expression | number | scopedIdentifier | unary_operator | variableDefinition}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "anonStruct" => {
                    Ok(unsafe {
                        Self::Anonstruct(
                            <Anonstruct<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "assign" => {
                    Ok(unsafe {
                        Self::Assign(
                            <Assign<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "binary_operator" => {
                    Ok(unsafe {
                        Self::BinaryOperator(
                            <BinaryOperator<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "call" => {
                    Ok(unsafe {
                        Self::Call(
                            <Call<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "expression" => {
                    Ok(unsafe {
                        Self::Expression(
                            <Expression<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "number" => {
                    Ok(unsafe {
                        Self::Number(
                            <Number<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "scopedIdentifier" => {
                    Ok(unsafe {
                        Self::Scopedidentifier(
                            <Scopedidentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "unary_operator" => {
                    Ok(unsafe {
                        Self::UnaryOperator(
                            <UnaryOperator<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "variableDefinition" => {
                    Ok(unsafe {
                        Self::Variabledefinition(
                            <Variabledefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Anonstruct(x) => ::type_sitter::Node::raw(x),
                Self::Assign(x) => ::type_sitter::Node::raw(x),
                Self::BinaryOperator(x) => ::type_sitter::Node::raw(x),
                Self::Call(x) => ::type_sitter::Node::raw(x),
                Self::Expression(x) => ::type_sitter::Node::raw(x),
                Self::Number(x) => ::type_sitter::Node::raw(x),
                Self::Scopedidentifier(x) => ::type_sitter::Node::raw(x),
                Self::UnaryOperator(x) => ::type_sitter::Node::raw(x),
                Self::Variabledefinition(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Anonstruct(x) => ::type_sitter::Node::raw_mut(x),
                Self::Assign(x) => ::type_sitter::Node::raw_mut(x),
                Self::BinaryOperator(x) => ::type_sitter::Node::raw_mut(x),
                Self::Call(x) => ::type_sitter::Node::raw_mut(x),
                Self::Expression(x) => ::type_sitter::Node::raw_mut(x),
                Self::Number(x) => ::type_sitter::Node::raw_mut(x),
                Self::Scopedidentifier(x) => ::type_sitter::Node::raw_mut(x),
                Self::UnaryOperator(x) => ::type_sitter::Node::raw_mut(x),
                Self::Variabledefinition(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Anonstruct(x) => x.into_raw(),
                Self::Assign(x) => x.into_raw(),
                Self::BinaryOperator(x) => x.into_raw(),
                Self::Call(x) => x.into_raw(),
                Self::Expression(x) => x.into_raw(),
                Self::Number(x) => x.into_raw(),
                Self::Scopedidentifier(x) => x.into_raw(),
                Self::UnaryOperator(x) => x.into_raw(),
                Self::Variabledefinition(x) => x.into_raw(),
            }
        }
    }
    /**One of `{block | callSig | scopedIdentifier}`:
- [`Block`]
- [`Callsig`]
- [`Scopedidentifier`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Block_Callsig_Scopedidentifier<'tree> {
        Block(Block<'tree>),
        Callsig(Callsig<'tree>),
        Scopedidentifier(Scopedidentifier<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Block_Callsig_Scopedidentifier<'tree> {
        ///Returns the node if it is of type `block` ([`Block`]), otherwise returns `None`
        #[inline]
        pub fn as_block(self) -> ::std::option::Option<Block<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Block(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `callSig` ([`Callsig`]), otherwise returns `None`
        #[inline]
        pub fn as_callsig(self) -> ::std::option::Option<Callsig<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Callsig(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `scopedIdentifier` ([`Scopedidentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_scopedidentifier(
            self,
        ) -> ::std::option::Option<Scopedidentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Scopedidentifier(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Block_Callsig_Scopedidentifier<'tree> {
        type WithLifetime<'a> = Block_Callsig_Scopedidentifier<'a>;
        const KIND: &'static str = "{block | callSig | scopedIdentifier}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "block" => {
                    Ok(unsafe {
                        Self::Block(
                            <Block<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "callSig" => {
                    Ok(unsafe {
                        Self::Callsig(
                            <Callsig<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "scopedIdentifier" => {
                    Ok(unsafe {
                        Self::Scopedidentifier(
                            <Scopedidentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => ::type_sitter::Node::raw(x),
                Self::Callsig(x) => ::type_sitter::Node::raw(x),
                Self::Scopedidentifier(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => ::type_sitter::Node::raw_mut(x),
                Self::Callsig(x) => ::type_sitter::Node::raw_mut(x),
                Self::Scopedidentifier(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => x.into_raw(),
                Self::Callsig(x) => x.into_raw(),
                Self::Scopedidentifier(x) => x.into_raw(),
            }
        }
    }
    /**One of `{function | module | pipeline}`:
- [`Function`]
- [`Module`]
- [`Pipeline`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Function_Module_Pipeline<'tree> {
        Function(Function<'tree>),
        Module(Module<'tree>),
        Pipeline(Pipeline<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Function_Module_Pipeline<'tree> {
        ///Returns the node if it is of type `function` ([`Function`]), otherwise returns `None`
        #[inline]
        pub fn as_function(self) -> ::std::option::Option<Function<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Function(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `module` ([`Module`]), otherwise returns `None`
        #[inline]
        pub fn as_module(self) -> ::std::option::Option<Module<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Module(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `pipeline` ([`Pipeline`]), otherwise returns `None`
        #[inline]
        pub fn as_pipeline(self) -> ::std::option::Option<Pipeline<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Pipeline(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Function_Module_Pipeline<'tree> {
        type WithLifetime<'a> = Function_Module_Pipeline<'a>;
        const KIND: &'static str = "{function | module | pipeline}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "function" => {
                    Ok(unsafe {
                        Self::Function(
                            <Function<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "module" => {
                    Ok(unsafe {
                        Self::Module(
                            <Module<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "pipeline" => {
                    Ok(unsafe {
                        Self::Pipeline(
                            <Pipeline<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Function(x) => ::type_sitter::Node::raw(x),
                Self::Module(x) => ::type_sitter::Node::raw(x),
                Self::Pipeline(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Function(x) => ::type_sitter::Node::raw_mut(x),
                Self::Module(x) => ::type_sitter::Node::raw_mut(x),
                Self::Pipeline(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Function(x) => x.into_raw(),
                Self::Module(x) => x.into_raw(),
                Self::Pipeline(x) => x.into_raw(),
            }
        }
    }
    /**One of `{!= | % | & | && | * | + | - | -> | . | / | < | << | <= | == | > | >= | >> | ^ | | | ||}`:
- [`symbols::NotEq`]
- [`symbols::Mod`]
- [`symbols::And`]
- [`symbols::AndAnd`]
- [`symbols::Mul`]
- [`symbols::Add`]
- [`symbols::Sub`]
- [`symbols::SubGt`]
- [`symbols::Dot`]
- [`symbols::Div`]
- [`symbols::Lt`]
- [`symbols::LtLt`]
- [`symbols::LtEq`]
- [`symbols::EqEq`]
- [`symbols::Gt`]
- [`symbols::GtEq`]
- [`symbols::GtGt`]
- [`symbols::BitXor`]
- [`symbols::Or`]
- [`symbols::OrOr`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
        'tree,
    > {
        NotEq(symbols::NotEq<'tree>),
        Mod(symbols::Mod<'tree>),
        And(symbols::And<'tree>),
        AndAnd(symbols::AndAnd<'tree>),
        Mul(symbols::Mul<'tree>),
        Add(symbols::Add<'tree>),
        Sub(symbols::Sub<'tree>),
        SubGt(symbols::SubGt<'tree>),
        Dot(symbols::Dot<'tree>),
        Div(symbols::Div<'tree>),
        Lt(symbols::Lt<'tree>),
        LtLt(symbols::LtLt<'tree>),
        LtEq(symbols::LtEq<'tree>),
        EqEq(symbols::EqEq<'tree>),
        Gt(symbols::Gt<'tree>),
        GtEq(symbols::GtEq<'tree>),
        GtGt(symbols::GtGt<'tree>),
        BitXor(symbols::BitXor<'tree>),
        Or(symbols::Or<'tree>),
        OrOr(symbols::OrOr<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<
        'tree,
    > NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
        'tree,
    > {
        ///Returns the node if it is of type `!=` ([`symbols::NotEq`]), otherwise returns `None`
        #[inline]
        pub fn as_not_eq(self) -> ::std::option::Option<symbols::NotEq<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::NotEq(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `%` ([`symbols::Mod`]), otherwise returns `None`
        #[inline]
        pub fn as_mod(self) -> ::std::option::Option<symbols::Mod<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Mod(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `&` ([`symbols::And`]), otherwise returns `None`
        #[inline]
        pub fn as_and(self) -> ::std::option::Option<symbols::And<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::And(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `&&` ([`symbols::AndAnd`]), otherwise returns `None`
        #[inline]
        pub fn as_and_and(self) -> ::std::option::Option<symbols::AndAnd<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::AndAnd(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `*` ([`symbols::Mul`]), otherwise returns `None`
        #[inline]
        pub fn as_mul(self) -> ::std::option::Option<symbols::Mul<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Mul(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `+` ([`symbols::Add`]), otherwise returns `None`
        #[inline]
        pub fn as_add(self) -> ::std::option::Option<symbols::Add<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Add(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `-` ([`symbols::Sub`]), otherwise returns `None`
        #[inline]
        pub fn as_sub(self) -> ::std::option::Option<symbols::Sub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Sub(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `->` ([`symbols::SubGt`]), otherwise returns `None`
        #[inline]
        pub fn as_sub_gt(self) -> ::std::option::Option<symbols::SubGt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::SubGt(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `.` ([`symbols::Dot`]), otherwise returns `None`
        #[inline]
        pub fn as_dot(self) -> ::std::option::Option<symbols::Dot<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Dot(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `/` ([`symbols::Div`]), otherwise returns `None`
        #[inline]
        pub fn as_div(self) -> ::std::option::Option<symbols::Div<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Div(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `<` ([`symbols::Lt`]), otherwise returns `None`
        #[inline]
        pub fn as_lt(self) -> ::std::option::Option<symbols::Lt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Lt(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `<<` ([`symbols::LtLt`]), otherwise returns `None`
        #[inline]
        pub fn as_lt_lt(self) -> ::std::option::Option<symbols::LtLt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::LtLt(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `<=` ([`symbols::LtEq`]), otherwise returns `None`
        #[inline]
        pub fn as_lt_eq(self) -> ::std::option::Option<symbols::LtEq<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::LtEq(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `==` ([`symbols::EqEq`]), otherwise returns `None`
        #[inline]
        pub fn as_eq_eq(self) -> ::std::option::Option<symbols::EqEq<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::EqEq(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `>` ([`symbols::Gt`]), otherwise returns `None`
        #[inline]
        pub fn as_gt(self) -> ::std::option::Option<symbols::Gt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Gt(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `>=` ([`symbols::GtEq`]), otherwise returns `None`
        #[inline]
        pub fn as_gt_eq(self) -> ::std::option::Option<symbols::GtEq<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::GtEq(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `>>` ([`symbols::GtGt`]), otherwise returns `None`
        #[inline]
        pub fn as_gt_gt(self) -> ::std::option::Option<symbols::GtGt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::GtGt(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `^` ([`symbols::BitXor`]), otherwise returns `None`
        #[inline]
        pub fn as_bit_xor(self) -> ::std::option::Option<symbols::BitXor<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::BitXor(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `|` ([`symbols::Or`]), otherwise returns `None`
        #[inline]
        pub fn as_or(self) -> ::std::option::Option<symbols::Or<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Or(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `||` ([`symbols::OrOr`]), otherwise returns `None`
        #[inline]
        pub fn as_or_or(self) -> ::std::option::Option<symbols::OrOr<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::OrOr(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
        'tree,
    > {
        type WithLifetime<'a> = NotEq_Mod_And_AndAnd_Mul_Add_Sub_SubGt_Dot_Div_Lt_LtLt_LtEq_EqEq_Gt_GtEq_GtGt_BitXor_Or_OrOr<
            'a,
        >;
        const KIND: &'static str = "{!= | % | & | && | * | + | - | -> | . | / | < | << | <= | == | > | >= | >> | ^ | | | ||}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "!=" => {
                    Ok(unsafe {
                        Self::NotEq(
                            <symbols::NotEq<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "%" => {
                    Ok(unsafe {
                        Self::Mod(
                            <symbols::Mod<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "&" => {
                    Ok(unsafe {
                        Self::And(
                            <symbols::And<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "&&" => {
                    Ok(unsafe {
                        Self::AndAnd(
                            <symbols::AndAnd<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "*" => {
                    Ok(unsafe {
                        Self::Mul(
                            <symbols::Mul<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "+" => {
                    Ok(unsafe {
                        Self::Add(
                            <symbols::Add<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "-" => {
                    Ok(unsafe {
                        Self::Sub(
                            <symbols::Sub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "->" => {
                    Ok(unsafe {
                        Self::SubGt(
                            <symbols::SubGt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "." => {
                    Ok(unsafe {
                        Self::Dot(
                            <symbols::Dot<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "/" => {
                    Ok(unsafe {
                        Self::Div(
                            <symbols::Div<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "<" => {
                    Ok(unsafe {
                        Self::Lt(
                            <symbols::Lt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "<<" => {
                    Ok(unsafe {
                        Self::LtLt(
                            <symbols::LtLt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "<=" => {
                    Ok(unsafe {
                        Self::LtEq(
                            <symbols::LtEq<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "==" => {
                    Ok(unsafe {
                        Self::EqEq(
                            <symbols::EqEq<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                ">" => {
                    Ok(unsafe {
                        Self::Gt(
                            <symbols::Gt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                ">=" => {
                    Ok(unsafe {
                        Self::GtEq(
                            <symbols::GtEq<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                ">>" => {
                    Ok(unsafe {
                        Self::GtGt(
                            <symbols::GtGt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "^" => {
                    Ok(unsafe {
                        Self::BitXor(
                            <symbols::BitXor<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "|" => {
                    Ok(unsafe {
                        Self::Or(
                            <symbols::Or<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "||" => {
                    Ok(unsafe {
                        Self::OrOr(
                            <symbols::OrOr<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::NotEq(x) => ::type_sitter::Node::raw(x),
                Self::Mod(x) => ::type_sitter::Node::raw(x),
                Self::And(x) => ::type_sitter::Node::raw(x),
                Self::AndAnd(x) => ::type_sitter::Node::raw(x),
                Self::Mul(x) => ::type_sitter::Node::raw(x),
                Self::Add(x) => ::type_sitter::Node::raw(x),
                Self::Sub(x) => ::type_sitter::Node::raw(x),
                Self::SubGt(x) => ::type_sitter::Node::raw(x),
                Self::Dot(x) => ::type_sitter::Node::raw(x),
                Self::Div(x) => ::type_sitter::Node::raw(x),
                Self::Lt(x) => ::type_sitter::Node::raw(x),
                Self::LtLt(x) => ::type_sitter::Node::raw(x),
                Self::LtEq(x) => ::type_sitter::Node::raw(x),
                Self::EqEq(x) => ::type_sitter::Node::raw(x),
                Self::Gt(x) => ::type_sitter::Node::raw(x),
                Self::GtEq(x) => ::type_sitter::Node::raw(x),
                Self::GtGt(x) => ::type_sitter::Node::raw(x),
                Self::BitXor(x) => ::type_sitter::Node::raw(x),
                Self::Or(x) => ::type_sitter::Node::raw(x),
                Self::OrOr(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::NotEq(x) => ::type_sitter::Node::raw_mut(x),
                Self::Mod(x) => ::type_sitter::Node::raw_mut(x),
                Self::And(x) => ::type_sitter::Node::raw_mut(x),
                Self::AndAnd(x) => ::type_sitter::Node::raw_mut(x),
                Self::Mul(x) => ::type_sitter::Node::raw_mut(x),
                Self::Add(x) => ::type_sitter::Node::raw_mut(x),
                Self::Sub(x) => ::type_sitter::Node::raw_mut(x),
                Self::SubGt(x) => ::type_sitter::Node::raw_mut(x),
                Self::Dot(x) => ::type_sitter::Node::raw_mut(x),
                Self::Div(x) => ::type_sitter::Node::raw_mut(x),
                Self::Lt(x) => ::type_sitter::Node::raw_mut(x),
                Self::LtLt(x) => ::type_sitter::Node::raw_mut(x),
                Self::LtEq(x) => ::type_sitter::Node::raw_mut(x),
                Self::EqEq(x) => ::type_sitter::Node::raw_mut(x),
                Self::Gt(x) => ::type_sitter::Node::raw_mut(x),
                Self::GtEq(x) => ::type_sitter::Node::raw_mut(x),
                Self::GtGt(x) => ::type_sitter::Node::raw_mut(x),
                Self::BitXor(x) => ::type_sitter::Node::raw_mut(x),
                Self::Or(x) => ::type_sitter::Node::raw_mut(x),
                Self::OrOr(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::NotEq(x) => x.into_raw(),
                Self::Mod(x) => x.into_raw(),
                Self::And(x) => x.into_raw(),
                Self::AndAnd(x) => x.into_raw(),
                Self::Mul(x) => x.into_raw(),
                Self::Add(x) => x.into_raw(),
                Self::Sub(x) => x.into_raw(),
                Self::SubGt(x) => x.into_raw(),
                Self::Dot(x) => x.into_raw(),
                Self::Div(x) => x.into_raw(),
                Self::Lt(x) => x.into_raw(),
                Self::LtLt(x) => x.into_raw(),
                Self::LtEq(x) => x.into_raw(),
                Self::EqEq(x) => x.into_raw(),
                Self::Gt(x) => x.into_raw(),
                Self::GtEq(x) => x.into_raw(),
                Self::GtGt(x) => x.into_raw(),
                Self::BitXor(x) => x.into_raw(),
                Self::Or(x) => x.into_raw(),
                Self::OrOr(x) => x.into_raw(),
            }
        }
    }
    /**One of `{! | & | * | - | ~}`:
- [`symbols::Not`]
- [`symbols::And`]
- [`symbols::Mul`]
- [`symbols::Sub`]
- [`symbols::BitNot`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Not_And_Mul_Sub_BitNot<'tree> {
        Not(symbols::Not<'tree>),
        And(symbols::And<'tree>),
        Mul(symbols::Mul<'tree>),
        Sub(symbols::Sub<'tree>),
        BitNot(symbols::BitNot<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Not_And_Mul_Sub_BitNot<'tree> {
        ///Returns the node if it is of type `!` ([`symbols::Not`]), otherwise returns `None`
        #[inline]
        pub fn as_not(self) -> ::std::option::Option<symbols::Not<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Not(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `&` ([`symbols::And`]), otherwise returns `None`
        #[inline]
        pub fn as_and(self) -> ::std::option::Option<symbols::And<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::And(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `*` ([`symbols::Mul`]), otherwise returns `None`
        #[inline]
        pub fn as_mul(self) -> ::std::option::Option<symbols::Mul<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Mul(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `-` ([`symbols::Sub`]), otherwise returns `None`
        #[inline]
        pub fn as_sub(self) -> ::std::option::Option<symbols::Sub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Sub(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `~` ([`symbols::BitNot`]), otherwise returns `None`
        #[inline]
        pub fn as_bit_not(self) -> ::std::option::Option<symbols::BitNot<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::BitNot(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Not_And_Mul_Sub_BitNot<'tree> {
        type WithLifetime<'a> = Not_And_Mul_Sub_BitNot<'a>;
        const KIND: &'static str = "{! | & | * | - | ~}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "!" => {
                    Ok(unsafe {
                        Self::Not(
                            <symbols::Not<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "&" => {
                    Ok(unsafe {
                        Self::And(
                            <symbols::And<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "*" => {
                    Ok(unsafe {
                        Self::Mul(
                            <symbols::Mul<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "-" => {
                    Ok(unsafe {
                        Self::Sub(
                            <symbols::Sub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "~" => {
                    Ok(unsafe {
                        Self::BitNot(
                            <symbols::BitNot<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Not(x) => ::type_sitter::Node::raw(x),
                Self::And(x) => ::type_sitter::Node::raw(x),
                Self::Mul(x) => ::type_sitter::Node::raw(x),
                Self::Sub(x) => ::type_sitter::Node::raw(x),
                Self::BitNot(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Not(x) => ::type_sitter::Node::raw_mut(x),
                Self::And(x) => ::type_sitter::Node::raw_mut(x),
                Self::Mul(x) => ::type_sitter::Node::raw_mut(x),
                Self::Sub(x) => ::type_sitter::Node::raw_mut(x),
                Self::BitNot(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Not(x) => x.into_raw(),
                Self::And(x) => x.into_raw(),
                Self::Mul(x) => x.into_raw(),
                Self::Sub(x) => x.into_raw(),
                Self::BitNot(x) => x.into_raw(),
            }
        }
    }
    /**One of `{; | expression}`:
- [`symbols::Semicolon`]
- [`Expression`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Semicolon_Expression<'tree> {
        Semicolon(symbols::Semicolon<'tree>),
        Expression(Expression<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Semicolon_Expression<'tree> {
        ///Returns the node if it is of type `;` ([`symbols::Semicolon`]), otherwise returns `None`
        #[inline]
        pub fn as_semicolon(self) -> ::std::option::Option<symbols::Semicolon<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Semicolon(x) = self { Some(x) } else { None }
        }
        ///Returns the node if it is of type `expression` ([`Expression`]), otherwise returns `None`
        #[inline]
        pub fn as_expression(self) -> ::std::option::Option<Expression<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Expression(x) = self { Some(x) } else { None }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Semicolon_Expression<'tree> {
        type WithLifetime<'a> = Semicolon_Expression<'a>;
        const KIND: &'static str = "{; | expression}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                ";" => {
                    Ok(unsafe {
                        Self::Semicolon(
                            <symbols::Semicolon<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "expression" => {
                    Ok(unsafe {
                        Self::Expression(
                            <Expression<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Semicolon(x) => ::type_sitter::Node::raw(x),
                Self::Expression(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Semicolon(x) => ::type_sitter::Node::raw_mut(x),
                Self::Expression(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Semicolon(x) => x.into_raw(),
                Self::Expression(x) => x.into_raw(),
            }
        }
    }
}
