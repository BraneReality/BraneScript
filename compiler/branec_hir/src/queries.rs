use super::hir::*;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum NodeDepth {
    Orphaned(usize),
    InTree(usize),
}

// Return the maximum between this node and output
pub fn node_max_depth(node: NodeId, block: &Block) -> NodeDepth {
    use NodeDepth::*;
    let mut depth = Orphaned(0);
    let refs = node_referenced_by(node, block);
    for referer in refs {
        println!("node was referenced by: {}", referer);
        depth = match node_max_depth(referer, block) {
            NodeDepth::Orphaned(ref_depth) => match depth {
                Orphaned(refs) => Orphaned(refs.max(ref_depth + 1)),
                InTree(refs) => InTree(refs),
            },
            NodeDepth::InTree(ref_depth) => match depth {
                Orphaned(_) => InTree(ref_depth + 1),
                InTree(refs) => InTree(refs.max(ref_depth + 1)),
            },
        }
    }
    if let Orphaned(_) = depth {
        for output in block.outputs.iter() {
            match output {
                LocalValue::BrokenRef => (),
                LocalValue::BlockInput { index: _ } => (),
                LocalValue::NodeOutput {
                    node: input_node,
                    index: _,
                } => {
                    println!("checking node {} against ouput {}", node, input_node);
                    if *input_node == node {
                        return InTree(0);
                    }
                }
            }
        }
    }
    depth
}

// Get all nodes that reference this node
// SLOW, refacor?
pub fn node_referenced_by(node_id: NodeId, block: &Block) -> Vec<NodeId> {
    let mut refs = Vec::new();
    for (id, node) in block.nodes.iter() {
        for input in node.inputs.iter() {
            match input {
                LocalValue::BrokenRef => (),
                LocalValue::BlockInput { index: _ } => (),
                LocalValue::NodeOutput {
                    node: input_node_id,
                    index: _,
                } => {
                    if *input_node_id == node_id {
                        refs.push(*id)
                    }
                }
            }
        }
    }
    refs
}
