use crate::parser::ty;

#[derive(Debug, Clone)]
pub enum SSAInstruction {
    NOP,
    // Loads a constant into a register
    MovI {
        value: i64,
        target: SSAValue,
    },
    // Load variable into register
    Load {
        source: String,
        target: SSAValue,
    },
    // Store register value in variable
    Store {
        target: String,
        source: SSAValue,
    },
    // Allocate a new variable
    Allocation {
        name: String,
        ty: SSAType,
    },
    Compare {
        operand: SSACompareOp,
        left: SSAValue,
        right: SSAValue,
        target: SSAValue,
    },
    ArithmeticBinary {
        operand: SSABinaryOp,
        left: SSAValue,
        right: SSAValue,
        target: SSAValue,
    },
    Label {
        name: SSALabel,
    },
    Branch {
        label: SSALabel,
    },
    BranchCond {
        condition: SSAValue,
        on_true: SSALabel,
        on_false: SSALabel,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<(String, SSAType)>,
        body: Vec<SSAInstruction>,
        return_type: SSAType,
    },
    FunctionInvocation {
        name: String,
        registers: Vec<SSAValue>,
        target: SSAValue,
    },
    Return {
        register: Option<SSAValue>,
    },
    Phi {
        target: SSAValue,
        values: Vec<SSAValue>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSAUnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSABinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSACompareOp {
    Eq,
    NotEq,
    Lt,
    Gt,
}

#[derive(Debug, Clone)]
pub enum SSAType {
    I64,
    Void,
}

impl From<ty::Type> for SSAType {
    fn from(ty: ty::Type) -> Self {
        match ty {
            ty::Type::Int => SSAType::I64,
            ty::Type::Bool => SSAType::I64,
        }
    }
}

impl From<Option<ty::Type>> for SSAType {
    fn from(ty: Option<ty::Type>) -> Self {
        match ty {
            Some(ty) => SSAType::from(ty),
            None => SSAType::Void,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SSALabel(pub usize);

#[derive(Debug, Copy, Clone)]
pub struct SSAValue(pub usize);
