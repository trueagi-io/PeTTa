use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::{Hash, DefaultHasher, Hasher};
use std::io::{self, Write};
use smallvec::{SmallVec, ToSmallVec};
use super::alloc::Allocator;
use super::PathMap;
use super::trie_core::node::{TaggedNodeRef, TrieNodeODRc, NODE_ITER_FINISHED};
use super::zipper::*;
use super::TrieValue;
use super::utils::debug::{render_debug_path, PathRenderMode};

/// Rendering modes that can be used for visualization
#[derive(Debug, Default)]
pub enum VizMode {
    /// Output [Mermaid](https://mermaid.js.org) markup commands to render a graph of the trie
    #[default]
    Mermaid,
    /// Output a terminal-friendly ascii tree rendering
    Ascii
}

/// Configuration settings for rendering a graph of a `pathmap` trie
pub struct DrawConfig {
    /// The rendering mode to use for the visualization
    pub mode: VizMode,
    /// If `true`, render path substrings as ascii, otherwise render them as strings of
    /// byte-sized numbers
    pub ascii_path: bool,
    /// If `true`, skips rendering of the paths that terminate in values, otherwise renders
    /// all paths in the trie
    pub hide_value_paths: bool,
    /// If `true`, skips rendering values, but still renders paths leading to values unless
    /// `hide_value_paths` is also `true`
    pub minimize_values: bool,
    /// If `true`, renders the trie irrespective of the pysical (in-memory) representation,
    /// otherwise also renders the nodes that comprise the layout of the trie structure
    pub logical: bool,
    /// If `true`, use color in the rendering
    pub color: bool,
}

impl Default for DrawConfig {
    fn default() -> Self {
        Self{
            mode: VizMode::Mermaid,
            ascii_path: false,
            hide_value_paths: false,
            minimize_values: false,
            logical: true,
            color: true,
        }
    }
}

struct NodeMeta {
    /// bit-mask indicating which top-level pathmaps include the node
    shared: u64,
    /// Number of references to this node from upstream
    ref_cnt: usize,
    /// Whether a graph node has already been rendered for this trie node
    taken: bool,
}

#[derive(Debug)]
enum NodeType {
    Dense, Pair, Tiny, Empty, Cell
}

enum DrawCmd {
    Node(u64, NodeType),
    /// (from_node, to_node, label)
    Edge(u64, u64, SmallVec<[u8; 8]>),
    /// (from_node, *const V, label)
    Value(u64, *const (), SmallVec<[u8; 8]>),
    /// (idx of map arg, root_node)
    Map(usize, u64),
}

struct DrawState {
    root: usize,
    nodes: HashMap<u64, NodeMeta>,
    cmds: Vec<DrawCmd>
}

/// Output a render trie within the provided [PathMap]s.  See [VizMode] and [DrawConfig] for rendering options
pub fn viz_maps<V: TrieValue + Debug + Hash, A: Allocator, W: Write>(btms: &[PathMap<V, A>], dc: &DrawConfig, out: W) -> io::Result<()> {
    match dc.mode {
        VizMode::Mermaid => viz_maps_mermaid(btms, dc, out),
        VizMode::Ascii => viz_maps_ascii(btms, dc, out),
    }
}

/// Output [Mermaid](https://mermaid.js.org) markup commands to render a graph of the trie within the provided [PathMap]s
fn viz_maps_mermaid<V: TrieValue + Debug + Hash, A: Allocator, W: Write>(btms: &[PathMap<V, A>], dc: &DrawConfig, mut out: W) -> io::Result<()> {
    writeln!(out, "flowchart LR")?;
    let mut ds = DrawState{ root: 0, nodes: HashMap::new(), cmds: vec![] };

    if dc.logical {
        for btm in btms.iter() {
            pre_init_node_hashes(btm.root().unwrap(), &mut ds);
            ds.root += 1;
        }
    }

    ds.root = 0;
    for btm in btms.iter() {
        if dc.logical {
            viz_zipper_logical(btm.read_zipper(), dc, &mut ds);
        } else {
            viz_map_physical(btm, dc, &mut ds);
        }
        ds.root += 1;
    }

    for cmd in ds.cmds {
        match cmd {
            DrawCmd::Map(map_idx, root_node_addr) => {
                if dc.logical {
                    //Draw the map *as* its root node
                    writeln!(out, "g{root_node_addr}@{{ shape: cylinder, label: \"PathMap[{map_idx}]\"}}")?;
                } else {
                    //Draw the map connecting to its root node
                    writeln!(out, "m{map_idx}@{{ shape: cylinder, label: \"PathMap[{map_idx}]\"}}")?;
                    writeln!(out, "m{map_idx} --> g{root_node_addr}")?;
                }
            },
            DrawCmd::Node(address, ntype) => {
                if !dc.logical {
                    //Render the node as a box
                    if let Some(meta) = ds.nodes.get(&address) {
                        let color = color_for_bitmask(meta.shared);
                        writeln!(out, "g{address}@{{ shape: rect, label: \"{ntype:?}\"}}")?;
                        writeln!(out, "style g{address} fill:{color}")?;
                    } else {
                        writeln!(out, "g{address}@{{ shape: rect, label: \"{ntype:?}\"}}")?;
                    }
                } else {
                    //Render it as a tiny dot (as small as Mermaid will draw)
                    let color = if let Some(meta) = ds.nodes.get(&address) {
                       color_for_bitmask(meta.shared)
                    } else {
                        "black"
                    };
                    writeln!(out, "g{address}@{{ shape: circle, label: \".\"}}")?;
                    writeln!(out, "style g{address} fill:{color},stroke:none,color:transparent,font-size:0px")?;
                }
            }
            DrawCmd::Edge(src, dst, key_bytes) => {
                let debug_jump = format!("{:?}", key_bytes);
                let jump = if dc.ascii_path { std::str::from_utf8(&key_bytes[..]).unwrap_or_else(|_| debug_jump.as_str()) }
                else { debug_jump.as_str() };

                if jump.len() > 0 {
                    writeln!(out, "g{src} --\"{jump:?}\"--> g{dst}")?;
                } else {
                    writeln!(out, "g{src} --> g{dst}")?;
                }
            }
            DrawCmd::Value(parent, address, key_bytes) => {
                if dc.hide_value_paths { continue }
                let debug_jump = format!("{:?}", key_bytes);
                let jump = if dc.ascii_path { std::str::from_utf8(&key_bytes[..]).unwrap_or_else(|_| debug_jump.as_str()) }
                else { debug_jump.as_str() };

                let address_string = format!("{parent}");
                let address_str = address_string.as_str();

                let value_address_string = format!("{address:p}");
                let value_address_str = value_address_string.as_str();

                let show_v = format!("{:?}", unsafe{ (address as *const V).as_ref().unwrap() });

                if jump.len() > 0 {
                    writeln!(out, "g{address_str} --\"{jump:?}\"--> v{value_address_str}{address_str}")?;
                } else {
                    writeln!(out, "g{address_str} --> v{value_address_str}{address_str}")?;
                }
                if dc.minimize_values {
                    writeln!(out, "v{value_address_str}{address_str}@{{ shape: circle, label: \".\"}}")?;
                    writeln!(out, "style v{value_address_str}{address_str} fill:black,stroke:none,color:transparent,font-size:0px")?;
                } else {
                    writeln!(out, "v{value_address_str}{address_str}@{{ shape: rounded, label: \"{show_v}\" }}")?;
                }
            },
        }
    }
    Ok(())
}

/// Output a logical ascii art representation of the trie within the provided [PathMap]s. Panics if `dc.logical` is `false`.
fn viz_maps_ascii<V: TrieValue + Debug + Hash, A: Allocator, W: Write>(btms: &[PathMap<V, A>], dc: &DrawConfig, mut out: W) -> io::Result<()> {
    assert!(dc.logical, "viz_maps_ascii only supports logical rendering");

    let mut ds = DrawState{ root: 0, nodes: HashMap::new(), cmds: vec![] };
    for map in btms.iter() {
        pre_init_node_hashes(map.root().unwrap(), &mut ds);
        ds.root += 1;
    }

    ds.root = 0;
    for (idx, map) in btms.iter().enumerate() {
        if idx > 0 {
            writeln!(out)?;
        }

        for meta in ds.nodes.values_mut() {
            meta.taken = false;
        }

        let (root_id, graph) = build_ascii_graph_logical(map.read_zipper(), dc, &mut ds);
        render_ascii_graph(idx, root_id, &graph, dc, &mut out)?;

        ds.root += 1;
    }

    Ok(())
}

struct AsciiGraphNode<V> {
    inline_value: Option<*const V>,
    children: Vec<AsciiEdge<V>>,
    shared_addr: Option<u64>,
}

struct AsciiEdge<V> {
    label: Vec<u8>,
    target: AsciiEdgeTarget<V>,
}

enum AsciiEdgeTarget<V> {
    Node(u64),
    Value(*const V),
}

fn build_ascii_graph_logical<V: TrieValue + Debug, Z: zipper_priv::ZipperPriv + ZipperMoving + ZipperIteration + ZipperValues<V>>(
    mut z: Z,
    dc: &DrawConfig,
    ds: &mut DrawState,
) -> (u64, HashMap<u64, AsciiGraphNode<V>>) {
    let root_focus = z.get_focus();
    let root_node = root_focus.borrow().unwrap();
    let root_addr = root_node.shared_node_id();
    let root_id = hash_pair(root_addr, &[]);

    let mut graph: HashMap<u64, AsciiGraphNode<V>> = HashMap::new();
    let root_shared = ds.nodes.get(&root_addr).map_or(false, |meta| meta.ref_cnt > 1);
    ensure_ascii_graph_node(&mut graph, root_id, root_shared.then_some(root_addr));

    if !dc.hide_value_paths {
        if let Some(v) = z.val() {
            if let Some(node) = graph.get_mut(&root_id) {
                node.inline_value = Some(v as *const V);
            }
        }
    }

    let mut trie_stack = vec![(0, root_addr)];
    let mut graph_stack = vec![(0, root_id)];
    let mut skip_node = false;

    while z.to_next_step() {
        if skip_node {
            z.ascend_byte();
            while !z.to_next_sibling_byte() {
                if !z.ascend_byte() {
                    return (root_id, graph);
                }
            }
        }
        skip_node = false;

        let path = z.path();

        while path.len() <= trie_stack.last().unwrap().0 {
            trie_stack.pop();
        }

        let new_focus = z.get_focus();
        let mut node_is_shared = false;
        if let Some(node) = new_focus.borrow() {
            let node_addr = node.shared_node_id();
            trie_stack.push((path.len(), node_addr));
            if let Some(meta) = ds.nodes.get_mut(&node_addr) {
                if meta.ref_cnt > 1 {
                    node_is_shared = true;
                }
                skip_node = meta.taken;
                meta.taken = true;
            }
        }

        let node_addr = trie_stack.last().unwrap().1;
        let node_key = &path[trie_stack.last().unwrap().0..];

        while path.len() <= graph_stack.last().unwrap().0 {
            graph_stack.pop();
        }

        if z.child_count() > 1 || (z.is_val() && z.child_count() == 1 && !dc.hide_value_paths) || node_is_shared {
            let parent_node_id = graph_stack.last().unwrap().1;
            let edge_path = &path[graph_stack.last().unwrap().0..];
            let graph_node_id = hash_pair(node_addr, node_key);
            graph_stack.push((z.path().len(), graph_node_id));

            let shared_addr = node_is_shared.then_some(node_addr);
            ensure_ascii_graph_node(&mut graph, graph_node_id, shared_addr);
            push_ascii_edge(&mut graph, parent_node_id, edge_path, AsciiEdgeTarget::Node(graph_node_id));
        }

        let graph_node_id = graph_stack.last().unwrap().1;
        let edge_path = &path[graph_stack.last().unwrap().0..];

        if !skip_node && !dc.hide_value_paths {
            if let Some(v) = z.val() {
                if edge_path.is_empty() {
                    if let Some(node) = graph.get_mut(&graph_node_id) {
                        if node.inline_value.is_none() {
                            node.inline_value = Some(v as *const V);
                        }
                    }
                } else {
                    push_ascii_edge(&mut graph, graph_node_id, edge_path, AsciiEdgeTarget::Value(v as *const V));
                }
            }
        }
    }

    (root_id, graph)
}

fn ensure_ascii_graph_node<V>(
    graph: &mut HashMap<u64, AsciiGraphNode<V>>,
    node_id: u64,
    shared_addr: Option<u64>,
) {
    let entry = graph.entry(node_id).or_insert(AsciiGraphNode {
        inline_value: None,
        children: Vec::new(),
        shared_addr,
    });
    if entry.shared_addr.is_none() {
        entry.shared_addr = shared_addr;
    }
}

fn push_ascii_edge<V>(
    graph: &mut HashMap<u64, AsciiGraphNode<V>>,
    parent_id: u64,
    label: &[u8],
    target: AsciiEdgeTarget<V>,
) {
    if let Some(parent) = graph.get_mut(&parent_id) {
        parent.children.push(AsciiEdge {
            label: label.to_vec(),
            target,
        });
    }
}

fn render_ascii_graph<V: Debug, W: Write>(
    map_idx: usize,
    root_id: u64,
    graph: &HashMap<u64, AsciiGraphNode<V>>,
    dc: &DrawConfig,
    out: &mut W,
) -> io::Result<()> {
    let mut shared_ids = BTreeMap::new();
    let mut shared_addrs: Vec<u64> = graph.values().filter_map(|node| node.shared_addr).collect();
    shared_addrs.sort_unstable();
    shared_addrs.dedup();
    for (idx, addr) in shared_addrs.into_iter().enumerate() {
        shared_ids.insert(addr, idx + 1);
    }

    let mut visited_shared: HashSet<u64> = HashSet::new();
    let root_node = match graph.get(&root_id) {
        Some(node) => node,
        None => {
            writeln!(out, "PathMap[{map_idx}]")?;
            writeln!(out, "(empty)")?;
            return Ok(());
        }
    };

    let mut root_line = format!("PathMap[{map_idx}]{}", render_value_ptr_part(root_node.inline_value, dc));
    if let Some(addr) = root_node.shared_addr {
        if let Some(id) = shared_ids.get(&addr) {
            visited_shared.insert(addr);
            root_line.push_str(&format!(" {}", render_shared_marker(*id, dc)));
        }
    }

    writeln!(out, "{root_line}")?;
    if root_node.children.is_empty() && root_node.inline_value.is_none() {
        writeln!(out, "(empty)")?;
        return Ok(());
    }
    render_ascii_graph_children(root_node, graph, dc, &shared_ids, &mut visited_shared, "", out)
}

fn render_ascii_graph_children<V: Debug, W: Write>(
    node: &AsciiGraphNode<V>,
    graph: &HashMap<u64, AsciiGraphNode<V>>,
    dc: &DrawConfig,
    shared_ids: &BTreeMap<u64, usize>,
    visited_shared: &mut HashSet<u64>,
    prefix: &str,
    out: &mut W,
) -> io::Result<()> {
    let mut edges: Vec<&AsciiEdge<V>> = node.children.iter().collect();
    edges.sort_by(|a, b| a.label.cmp(&b.label));

    for (idx, edge) in edges.iter().enumerate() {
        let is_last = idx + 1 == edges.len();
        render_ascii_edge(edge, graph, dc, shared_ids, visited_shared, prefix, is_last, out)?;
    }

    Ok(())
}

fn render_ascii_edge<V: Debug, W: Write>(
    edge: &AsciiEdge<V>,
    graph: &HashMap<u64, AsciiGraphNode<V>>,
    dc: &DrawConfig,
    shared_ids: &BTreeMap<u64, usize>,
    visited_shared: &mut HashSet<u64>,
    prefix: &str,
    is_last: bool,
    out: &mut W,
) -> io::Result<()> {
    let connector = if is_last { "└── " } else { "├── " };
    let label_str = render_path_label(&edge.label, dc);
    match edge.target {
        AsciiEdgeTarget::Value(v) => {
            let value_part = render_value_ptr_part(Some(v), dc);
            writeln!(out, "{prefix}{connector}{label_str}{value_part}")?;
        }
        AsciiEdgeTarget::Node(node_id) => {
            if let Some(child_node) = graph.get(&node_id) {
                let value_part = render_value_ptr_part(child_node.inline_value, dc);
                if let Some(shared_addr) = child_node.shared_addr {
                    if let Some(shared_id) = shared_ids.get(&shared_addr).copied() {
                        if visited_shared.contains(&shared_addr) {
                            writeln!(out, "{prefix}{connector}{label_str}{value_part} ↩ {}", render_shared_marker(shared_id, dc))?;
                            return Ok(());
                        }
                        visited_shared.insert(shared_addr);
                        writeln!(out, "{prefix}{connector}{label_str}{value_part} {}", render_shared_marker(shared_id, dc))?;
                    } else {
                        writeln!(out, "{prefix}{connector}{label_str}{value_part}")?;
                    }
                } else {
                    writeln!(out, "{prefix}{connector}{label_str}{value_part}")?;
                }

                let next_prefix = if is_last {
                    format!("{prefix}    ")
                } else {
                    format!("{prefix}│   ")
                };

                render_ascii_graph_children(child_node, graph, dc, shared_ids, visited_shared, &next_prefix, out)?;
            }
        }
    }

    Ok(())
}

fn render_path_label(path: &[u8], dc: &DrawConfig) -> String {
    let mode = if dc.ascii_path { PathRenderMode::TryAscii } else { PathRenderMode::ByteList };
    render_debug_path(path, mode)
        .unwrap_or_else(|| format!("{path:?}").into())
        .into_owned()
}

fn render_value_part<V: Debug>(value: Option<&V>, dc: &DrawConfig) -> String {
    match value {
        None => String::new(),
        Some(v) => {
            if dc.minimize_values {
                String::new()
            } else {
                format!(" = {v:?}")
            }
        }
    }
}

fn render_value_ptr_part<V: Debug>(value: Option<*const V>, dc: &DrawConfig) -> String {
    let vref = value.map(|ptr| unsafe { &*ptr });
    render_value_part(vref, dc)
}

fn render_shared_marker(id: usize, dc: &DrawConfig) -> String {
    if dc.color {
        const SHARED_COLORS: [&str; 6] = ["31", "32", "33", "34", "35", "36"];
        let color = SHARED_COLORS[(id - 1) % SHARED_COLORS.len()];
        format!("\x1b[{color}m◆{id}\x1b[0m")
    } else {
        format!("◆{id}")
    }
}

fn color_for_bitmask(mask: u64) -> &'static str {
    match mask {
        0b000 => { "black" }
        0b100 => { "red" }
        0b010 => { "green" }
        0b001 => { "blue" }
        0b011 => { "#0aa" }
        0b101 => { "#a0a" }
        0b110 => { "#aa0" }
        0b111 => { "gray" }
        _ => todo!()
    }
}

fn pre_init_node_hashes<V : TrieValue + Debug + Hash, A : Allocator>(node: &TrieNodeODRc<V, A>, ds: &mut DrawState) {
    if update_node_hash(node, ds) {
        let node_ref = node.as_tagged();
        let mut token = node_ref.new_iter_token();
        while token != NODE_ITER_FINISHED {
            let (new_token, _key_bytes, rec, _value) = node_ref.next_items(token);
            if let Some(child) = rec {
                pre_init_node_hashes(child, ds);
            }
            token = new_token;
        }
    }
}

/// Updates the node hash table for a single node.  Returns `true` if a new hash entry was
/// created, indicating the calling code should descend the subtrie recursively
fn update_node_hash<V : TrieValue + Debug + Hash, A : Allocator>(node: &TrieNodeODRc<V, A>, ds: &mut DrawState) -> bool {
    let node_addr = node.shared_node_id();
    match ds.nodes.get_mut(&node_addr) {
        None => {
            ds.nodes.insert(node_addr, NodeMeta{ shared: 1 << ds.root, ref_cnt: 1, taken: false });
            true
        }
        Some(meta) => {
            meta.shared |= 1 << ds.root;
            meta.ref_cnt += 1;
            false
        }
    }
}

fn viz_zipper_logical<V : TrieValue + Debug + Hash, Z: zipper_priv::ZipperPriv + ZipperMoving + ZipperIteration + ZipperValues<V>>(mut z: Z, dc: &DrawConfig, ds: &mut DrawState) {
    let root_focus = z.get_focus();
    let root_node = root_focus.borrow().unwrap();
    let root_node_id = hash_pair(root_node.shared_node_id(), &[]);
    ds.cmds.push(DrawCmd::Map(ds.root, root_node_id));

    //We keep two separate stacks.  `trie_stack` is the physical nodes, and `graph_stack` is
    // the nodes that will get rendered.  This is necessary because the correspondence is
    // not straightforward.  For example, many physical trie nodes can end up subsumed under
    // a single graph edge, in the case of a long straight path, but it's also the case that
    // a single physical trie node can produce many graph nodes, when a trie node contains
    // internal graph structure.
    let mut trie_stack = vec![(0, root_node.shared_node_id())];
    let mut graph_stack = vec![(0, root_node_id)];
    let mut skip_node = false;
    while z.to_next_step() {
        //Skip a whole branch if we've already rendered it elsewhere
        if skip_node {
            z.ascend_byte();
            while !z.to_next_sibling_byte() {
                if !z.ascend_byte() {
                    return; //We skipped all the way to the root
                }
            }
        }
        skip_node = false;

        let path = z.path();

        //See if we have ascended and therefore need to pop the trie stack
        while path.len() <= trie_stack.last().unwrap().0 {
            trie_stack.pop();
        }

        //See if we have descended into a new node and therefore need to push onto the trie stack
        let new_focus = z.get_focus();
        let mut node_is_shared = false;
        if let Some(node) = new_focus.borrow() {
            let node_addr = node.shared_node_id();
            trie_stack.push((path.len(), node_addr));
            if let Some(meta) = ds.nodes.get_mut(&node_addr) {
                if meta.ref_cnt > 1 {
                    node_is_shared = true;
                }
                skip_node = meta.taken;
                meta.taken = true;
            }
        }

        let node_addr = trie_stack.last().unwrap().1;
        let node_key = &path[trie_stack.last().unwrap().0..];

        //See if we have ascended and therefore need to pop the graph stack
        while path.len() <= graph_stack.last().unwrap().0 {
            graph_stack.pop();
        }

        //See if we have met one of the conditions to push a node onto the graph stack
        if z.child_count() > 1 || (z.is_val() && z.child_count() == 1 && !dc.hide_value_paths) || node_is_shared {
            let parent_node_id = graph_stack.last().unwrap().1;
            let edge_path = &path[graph_stack.last().unwrap().0..];

            let graph_node_id = hash_pair(node_addr, node_key);
            graph_stack.push((z.path().len(), graph_node_id));

            ds.cmds.push(DrawCmd::Edge(parent_node_id, graph_node_id, edge_path.to_smallvec()));
            if !skip_node {
                ds.cmds.push(DrawCmd::Node(graph_node_id, NodeType::Cell));
            }
        }

        let graph_node_id = graph_stack.last().unwrap().1;
        let edge_path = &path[graph_stack.last().unwrap().0..];

        if let Some(v) = z.val() {
            ds.cmds.push(DrawCmd::Value(graph_node_id, (v as *const V).cast(), edge_path.to_smallvec()));
        }
    }
}

/// A simple function to hash an address with a partial path, to make new node_ids for logical nodes
fn hash_pair(addr: u64, key: &[u8]) -> u64 {
    if key.len() > 0 {
        let mut hasher = DefaultHasher::new();
        addr.hash(&mut hasher);
        key.hash(&mut hasher);
        hasher.finish()
    } else {
        addr
    }
}

fn viz_map_physical<V : TrieValue + Debug + Hash, A : Allocator>(map: &PathMap<V, A>, dc: &DrawConfig, ds: &mut DrawState) {
    let root_node = map.root().unwrap();
    ds.cmds.push(DrawCmd::Map(ds.root, root_node.shared_node_id()));
    update_node_hash(root_node, ds);
    viz_node_physical(root_node, dc, ds);
}

fn viz_node_physical<V : TrieValue + Debug + Hash, A : Allocator>(n: &TrieNodeODRc<V, A>, dc: &DrawConfig, ds: &mut DrawState) {
    let address = n.shared_node_id();

    let bn = n.as_tagged();
    let ntype = match bn {
        TaggedNodeRef::DenseByteNode(_) => { NodeType::Dense }
        TaggedNodeRef::LineListNode(_) => { NodeType::Pair }
        TaggedNodeRef::TinyRefNode(_) => { NodeType::Tiny }
        // TaggedNodeRef::BridgeNode(_) => { NodeType::Bridge }
        TaggedNodeRef::CellByteNode(_) => { NodeType::Cell }
        TaggedNodeRef::EmptyNode => { NodeType::Empty }
    };
    ds.cmds.push(DrawCmd::Node(address, ntype));

    let mut token = bn.new_iter_token();
    while token != NODE_ITER_FINISHED {
        let (new_token, key_bytes, rec, value) = bn.next_items(token);
        // println!("iterating over {:?}: {:?} ({:?} {:?})", address, key_bytes, rec.is_some(), value.is_some());

        if let Some(r) = rec {
            let other_address = r.shared_node_id();
            ds.cmds.push(DrawCmd::Edge(address, other_address, key_bytes.to_smallvec()));
            if update_node_hash(r, ds) {
                viz_node_physical(r, dc, ds);
            }
        }

        if let Some(v) = value {
            ds.cmds.push(DrawCmd::Value(address, (v as *const V).cast(), key_bytes.to_smallvec()));
        }

        token = new_token;
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod test {
    use super::zipper::{ZipperCreation, ZipperMoving, ZipperWriting};
    use super::*;

    #[test]
    fn small_viz() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        rs.iter().enumerate().for_each(|(i, r)| { btm.insert(r.as_bytes(), i); });

        let mut out_buf = Vec::new();
        viz_maps(&[btm], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: false, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn logical_viz_tiny() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon"];
        rs.iter().for_each(|path| { btm.insert(path.as_bytes(), ()); });

        let mut out_buf = Vec::new();
        viz_maps(&[btm], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: true, logical: true, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    /// Constructs the PathMap to render the diagram from the "Structural Sharing" intro in the book
    #[test]
    fn logical_viz_4x4() {
        let bytes = [b'a', b'b', b'c', b'd'];
        let mut l3_map = PathMap::new();
        bytes.iter().for_each(|&byte| { l3_map.insert(&[byte], ()); });
        let mut l2_map = PathMap::new();
        let mut wz = l2_map.write_zipper();
        bytes.iter().for_each(|&byte| {
            wz.reset();
            wz.descend_to_byte(byte);
            wz.graft_map(l3_map.clone());
        });
        drop(wz);
        let mut l1_map = PathMap::new();
        let mut wz = l1_map.write_zipper();
        bytes.iter().for_each(|&byte| {
            wz.reset();
            wz.descend_to_byte(byte);
            wz.graft_map(l2_map.clone());
        });
        drop(wz);
        let mut l0_map = PathMap::new();
        let mut wz = l0_map.write_zipper();
        bytes.iter().for_each(|&byte| {
            wz.reset();
            wz.descend_to_byte(byte);
            wz.graft_map(l1_map.clone());
        });
        drop(wz);

        let mut out_buf = Vec::new();
        viz_maps(&[l0_map], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: true, logical: true, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn logical_viz_small() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        rs.iter().enumerate().for_each(|(i, r)| { btm.insert(r.as_bytes(), i); });

        let mut out_buf = Vec::new();
        viz_maps(&[btm], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: true, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn joined_viz() {
        let mut a = PathMap::<usize>::new();
        let mut b = PathMap::<usize>::new();
        let rs = ["Abbotsford", "Abbottabad", "Abcoude", "Abdul Hakim", "Abdulino", "Abdullahnagar", "Abdurahmoni Jomi", "Abejorral", "Abelardo Luz"];
        for (i, path) in rs.into_iter().enumerate() {
            if i % 2 == 0 {
                a.insert(path, i);
            } else {
                b.insert(path, i);
            }
        }

        let joined = a.join(&b);

        let mut out_buf = Vec::new();
        viz_maps(&[a, b, joined], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: false, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    /// From the Rust lightning talk
    #[test]
    fn person_viz() {
        let mut hand = PathMap::<char>::new();
        hand.insert("Finger.0-Thumb", '👍');
        hand.insert("Finger.1-Index", '🫵');
        hand.insert("Finger.2-Middle", '🖕');
        hand.insert("Finger.3-Ring", '💍');
        hand.insert("Finger.4-Pinky", '🤙');

        let mut body = PathMap::<char>::new();
        body.insert("0-Head.0-Eyes", '👀');
        body.insert("0-Head.1-Nose", '🤥');
        body.insert("0-Head.3-Mouth", '👄');

        let mut wz = body.write_zipper();
        wz.move_to_path("1-Hand.0-Left.");
        wz.graft_map(hand.clone());
        wz.move_to_path("1-Hand.1-Right.");
        wz.graft_map(hand);

        let mut out_buf = Vec::new();
        viz_maps(&[body], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: true, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn fizzbuzz() {
        let n = 50;

        let mut space = PathMap::<()>::new();
        let zh = space.zipper_head();

        let mut m3_path = b"[2]".to_vec();
        let m3_symbol = "m3".as_bytes();
        m3_path.extend(b"<2>");
        m3_path.extend(m3_symbol);
        let mut m3_zipper = zh.write_zipper_at_exclusive_path(&m3_path[..]).unwrap();
        m3_zipper.descend_to(b"<4>");
        m3_zipper.graft_map(super::utils::ints::gen_int_range::<(), 4, u32>(3, n as u32, 3, ()));
        m3_zipper.reset();

        let mut m5_path = b"[2]".to_vec();
        let m5_symbol = "m5".as_bytes();
        m5_path.extend(b"<2>");
        m5_path.extend(m5_symbol);
        let mut m5_zipper = zh.write_zipper_at_exclusive_path(&m5_path[..]).unwrap();
        m5_zipper.descend_to(b"<4>");
        m5_zipper.graft_map(super::utils::ints::gen_int_range::<(), 4, u32>(5, n as u32, 5, ()));
        m5_zipper.reset();

        let mut r_path = b"[2]".to_vec();
        let r_symbol = "r".as_bytes();
        r_path.extend(b"<1>");
        r_path.extend(r_symbol);
        let mut r_zipper = zh.write_zipper_at_exclusive_path(&r_path[..]).unwrap();
        r_zipper.descend_to(b"<4>");
        r_zipper.graft_map(super::utils::ints::gen_int_range::<(), 4, u32>(1, n as u32, 1, ()));
        r_zipper.reset();

        let mut m35_path = b"[2]".to_vec();
        let m35_symbol = "m35".as_bytes();
        m35_path.extend(b"<3>");
        m35_path.extend(m35_symbol);
        let mut m35_zipper = zh.write_zipper_at_exclusive_path(&m35_path[..]).unwrap();
        m35_zipper.meet_2(&m3_zipper, &m5_zipper);

        let mut m3n5_path = b"[2]".to_vec();
        let m3n5_symbol = "m3n5".as_bytes();
        m3n5_path.extend(b"<4>");
        m3n5_path.extend(m3n5_symbol);
        let mut m3n5_zipper = zh.write_zipper_at_exclusive_path(&m3n5_path[..]).unwrap();
        m3n5_zipper.graft(&m5_zipper);
        m3n5_zipper.subtract_into(&m3_zipper, true);

        let mut m5n3_path = b"[2]".to_vec();
        let m5n3_symbol = "m5n3".as_bytes();
        m5n3_path.extend(b"<4>");
        m5n3_path.extend(m5n3_symbol);
        let mut m5n3_zipper = zh.write_zipper_at_exclusive_path(&m5n3_path[..]).unwrap();
        m5n3_zipper.graft(&m3_zipper);
        m5n3_zipper.subtract_into(&m5_zipper, true);

        let mut m3m5_path = b"[2]".to_vec();
        let m3m5_symbol = "m3m5".as_bytes();
        m3m5_path.extend(b"<4>");
        m3m5_path.extend(m3m5_symbol);
        let mut m3m5_zipper = zh.write_zipper_at_exclusive_path(&m3m5_path[..]).unwrap();
        m3m5_zipper.graft(&m3_zipper);
        m3m5_zipper.join_into(&m5_zipper);

        let mut n3n5_path = b"[2]".to_vec();
        let n3n5_symbol = "n3n5".as_bytes();
        n3n5_path.extend(b"<4>");
        n3n5_path.extend(n3n5_symbol);
        let mut n3n5_zipper = zh.write_zipper_at_exclusive_path(&n3n5_path[..]).unwrap();
        n3n5_zipper.graft(&r_zipper);
        n3n5_zipper.subtract_into(&m3m5_zipper, true);
        drop(m3m5_zipper);

        drop(m3_zipper);
        drop(m5_zipper);
        drop(r_zipper);

        let mut fizzbuzz_path = b"[2]".to_vec();
        let fizzbuzz_symbol = "FizzBuzz".as_bytes();
        fizzbuzz_path.extend(b"<8>");
        fizzbuzz_path.extend(fizzbuzz_symbol);
        let mut fizz_buzz_zipper = zh.write_zipper_at_exclusive_path(fizzbuzz_path).unwrap();
        fizz_buzz_zipper.graft(&m35_zipper);
        drop(fizz_buzz_zipper);
        drop(m35_zipper);

        let mut nothing_path = b"[2]".to_vec();
        let nothing_symbol = "Nothing".as_bytes();
        nothing_path.extend(b"<7>");
        nothing_path.extend(nothing_symbol);
        let mut nothing_zipper = zh.write_zipper_at_exclusive_path(nothing_path).unwrap();
        nothing_zipper.graft(&n3n5_zipper);
        drop(nothing_zipper);
        drop(n3n5_zipper);

        let mut fizz_path = b"[2]".to_vec();
        let fizz_symbol = "Fizz".as_bytes();
        fizz_path.extend(b"<4>");
        fizz_path.extend(fizz_symbol);
        let mut fizz_zipper = zh.write_zipper_at_exclusive_path(fizz_path).unwrap();
        fizz_zipper.graft(&m3n5_zipper);
        drop(fizz_zipper);
        drop(m3n5_zipper);

        let mut buzz_path = b"[2]".to_vec();
        let buzz_symbol = "Buzz".as_bytes();
        buzz_path.extend(b"<4>");
        buzz_path.extend(buzz_symbol);
        let mut buzz_zipper = zh.write_zipper_at_exclusive_path(buzz_path).unwrap();
        buzz_zipper.graft(&m5n3_zipper);
        drop(buzz_zipper);
        drop(m5n3_zipper);

        drop(zh);

        // println!("space size {}", space.val_count());

        let mut out_buf = Vec::new();
        viz_maps(&[space], &DrawConfig{ mode: VizMode::Mermaid, ascii_path: false, hide_value_paths: true, minimize_values: true, logical: false, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn small_ascii_viz() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        rs.iter().enumerate().for_each(|(i, r)| { btm.insert(r.as_bytes(), i); });

        let mut out_buf = Vec::new();
        viz_maps(&[btm], &DrawConfig{ mode: VizMode::Ascii, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: true, color: false }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }

    #[test]
    fn person_viz_ascii() {
        let mut hand = PathMap::<char>::new();
        hand.insert("Finger.0-Thumb", '👍');
        hand.insert("Finger.1-Index", '🫵');
        hand.insert("Finger.2-Middle", '🖕');
        hand.insert("Finger.3-Ring", '💍');
        hand.insert("Finger.4-Pinky", '🤙');

        let mut body = PathMap::<char>::new();
        body.insert("0-Head.0-Eyes", '👀');
        body.insert("0-Head.1-Nose", '🤥');
        body.insert("0-Head.3-Mouth", '👄');

        let mut wz = body.write_zipper();
        wz.move_to_path("1-Hand.0-Left.");
        wz.graft_map(hand.clone());
        wz.move_to_path("1-Hand.1-Right.");
        wz.graft_map(hand);

        let mut out_buf = Vec::new();
        viz_maps(&[body], &DrawConfig{ mode: VizMode::Ascii, ascii_path: true, hide_value_paths: false, minimize_values: false, logical: true, color: true }, &mut out_buf).unwrap();
        // println!("{}", String::from_utf8_lossy(&out_buf));
    }
}
