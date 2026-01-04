use anyhow::Result;
use std::collections::HashMap;

use brane_core::ir::*;

pub fn _fold_constants(_module: &mut Module) -> Result<usize> {
    todo!()
}

pub fn prune_phi_nodes(module: &mut Module) -> Result<usize> {
    let mut removed: HashMap<Value, Value> = HashMap::new();
    let mut shifted: HashMap<Value, Value> = HashMap::new();

    let mut reductions = 0;
    for func in module.functions.iter_mut() {
        for (block_id, block) in func.blocks.iter_mut().enumerate() {
            let mut remove_count = 0;
            block.phi_nodes = block
                .phi_nodes
                .iter()
                .enumerate()
                .filter_map(|(phi_id, phi)| {
                    if phi.variants.len() == 1 {
                        let inner = phi.variants.first().unwrap().value;
                        let inner = if let Some(inner) = removed.get(&inner) {
                            inner.clone()
                        } else {
                            inner
                        };

                        map_update_value(
                            Value::PhiArg {
                                block: block_id as u32,
                                arg: phi_id as u32,
                            },
                            inner,
                            &mut removed,
                        );
                        remove_count += 1;
                        None
                    } else {
                        shifted.insert(
                            Value::PhiArg {
                                block: block_id as u32,
                                arg: phi_id as u32,
                            },
                            Value::PhiArg {
                                block: block_id as u32,
                                arg: (phi_id - remove_count) as u32,
                            },
                        );
                        Some(phi.clone())
                    }
                })
                .collect();
            reductions += remove_count;
        }
    }

    // TODO enable with flag
    /*
    println!("removed mappings:");
    for (before, after) in removed.iter() {
        println!("{} -> {}", before, after);
    }

    println!("shifted mappings:");
    for (before, after) in shifted.iter() {
        println!("{} -> {}", before, after);
    }
    */

    for func in module.functions.iter_mut() {
        for block in func.blocks.iter_mut() {
            map_values(block, &mut removed);
            map_values(block, &mut shifted);
        }
    }

    Ok(reductions)
}

pub fn map_update_value(before: Value, after: Value, map: &mut HashMap<Value, Value>) {
    for (_, a) in map.iter_mut() {
        if *a == before {
            *a = after
        }
    }
    map.insert(before, after);
}

pub fn map_values(block: &mut Block, map: &mut HashMap<Value, Value>) {
    // Helper: replace the value in place if it has a mapping.
    fn replace(v: &mut Value, map: &HashMap<Value, Value>) {
        if let Some(&mapped) = map.get(v) {
            *v = mapped;
        }
    }

    // 1. Map values inside phi nodes
    for phi in &mut block.phi_nodes {
        for phi_val in &mut phi.variants {
            replace(&mut phi_val.value, map);
        }
    }

    // 2. Map values inside ops
    for op in &mut block.ops {
        match op {
            Op::Load { ty: _, ptr } => replace(ptr, map),
            Op::Store { src, ptr } => {
                replace(src, map);
                replace(ptr, map);
            }
            Op::Unary { op: _, value } => replace(value, map),
            Op::Binary { op: _, left, right } => {
                replace(left, map);
                replace(right, map);
            }
            Op::Call { func: _, input } => {
                for v in input.iter_mut() {
                    match v {
                        ValueOrObj::Value(value) => replace(value, map),
                        ValueOrObj::Obj(values) => {
                            for v in values.iter_mut() {
                                replace(v, map);
                            }
                        }
                    }
                }
            }
            Op::CallIndirect {
                func_handle: func,
                sig: _,
                input,
            } => {
                replace(func, map);
                for v in input.iter_mut() {
                    match v {
                        ValueOrObj::Value(value) => replace(value, map),
                        ValueOrObj::Obj(values) => {
                            for v in values.iter_mut() {
                                replace(v, map);
                            }
                        }
                    }
                }
            }
            Op::Cast {
                src,
                from_ty: _,
                to_ty: _,
            } => replace(src, map),
        }
    }

    // 3. Map values inside the terminator
    match &mut block.terminator {
        TermOp::Ret(Some(values)) => {
            for v in values {
                replace(v, map)
            }
        }
        TermOp::JumpIf { cond, .. } => replace(cond, map),
        TermOp::JumpMap { cond, .. } => replace(cond, map),
        TermOp::Ret(None) | TermOp::Jump { .. } => {}
    }
}
