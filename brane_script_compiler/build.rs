use std::fs;
use std::path::PathBuf;
use type_sitter_gen::generate_nodes;

fn main() {
    // Type sitter node generation
    let out_dir = PathBuf::from("src/ast/");

    // To generate nodes
    fs::write(
        out_dir.join("nodes.rs"),
        generate_nodes(tree_sitter_branescript::NODE_TYPES)
            .unwrap()
            .into_string(),
    )
    .unwrap();
}
