use super::super::PathMap;
use super::zipper::{*, zipper_priv::ZipperPriv};
use super::trie_core::node::{TaggedNodeRef, TrieNode};

/// Example usage of counters
///
/// ```ignore
/// pathmap::counters::print_traversal(&map.read_zipper());
/// let counters = pathmap::counters::Counters::count_ocupancy(&map);
/// counters.print_histogram_by_depth();
/// counters.print_run_length_histogram();
/// counters.print_list_node_stats();
/// ```ignore
pub struct Counters {
    total_nodes_by_depth: Vec<usize>,
    total_child_items_by_depth: Vec<usize>,
    max_child_items_by_depth: Vec<usize>,

    /// Counts the number of each node type at a given depth
    total_dense_byte_nodes_by_depth: Vec<usize>,
    total_list_nodes_by_depth: Vec<usize>,

    /// List-node-specific counters
    total_slot0_length_by_depth: Vec<usize>,
    slot1_occupancy_count_by_depth: Vec<usize>,
    total_slot1_length_by_depth: Vec<usize>,
    list_node_single_byte_keys_by_depth: Vec<usize>,

    /// Counts the runs of distance (in bytes) that end at each byte depth
    /// [run_length][ending_byte_depth]
    run_length_histogram_by_ending_byte_depth: Vec<Vec<usize>>,
    cur_run_start_depth: usize,
}
impl Counters {
    pub const fn new() -> Self {
        Self {
            total_nodes_by_depth: vec![],
            total_child_items_by_depth: vec![],
            max_child_items_by_depth: vec![],
            total_dense_byte_nodes_by_depth: vec![],
            total_list_nodes_by_depth: vec![],
            total_slot0_length_by_depth: vec![],
            slot1_occupancy_count_by_depth: vec![],
            total_slot1_length_by_depth: vec![],
            list_node_single_byte_keys_by_depth: vec![],
            run_length_histogram_by_ending_byte_depth: vec![],
            cur_run_start_depth: 0,
        }
    }
    pub fn total_nodes(&self) -> usize {
        let mut total = 0;
        self.total_nodes_by_depth.iter().for_each(|cnt| total += cnt);
        total
    }
    pub fn total_child_items(&self) -> usize {
        let mut total = 0;
        self.total_child_items_by_depth.iter().for_each(|cnt| total += cnt);
        total
    }
    pub fn print_histogram_by_depth(&self) {
        println!("\n\ttotal_nodes\ttot_child_cnt\tavg_branch\tmax_child_items\tdense_nodes\tlist_nodes");
        for depth in 0..self.total_nodes_by_depth.len() {
            println!("{depth}\t{}\t\t{}\t\t{:1.4}\t\t{}\t\t{}\t\t{}",
                self.total_nodes_by_depth[depth],
                self.total_child_items_by_depth[depth],
                self.total_child_items_by_depth[depth] as f32 / self.total_nodes_by_depth[depth] as f32,
                self.max_child_items_by_depth[depth],
                self.total_dense_byte_nodes_by_depth[depth],
                self.total_list_nodes_by_depth[depth],
            );
        }
        println!("TOTAL nodes: {}, items: {}, avg children-per-node: {}", self.total_nodes(), self.total_child_items(), self.total_child_items() as f32 / self.total_nodes() as f32);
    }
    pub fn print_run_length_histogram(&self) {
        println!("run_len\trun_cnt\trun_end_mean_depth");
        for (run_length, depths) in self.run_length_histogram_by_ending_byte_depth.iter().enumerate() {
            let total = depths.iter().fold(0, |mut sum, cnt| {sum += cnt; sum});
            let depth_sum = depths.iter().enumerate().fold(0, |mut sum, (depth, cnt)| {sum += cnt*(depth+1); sum});
            println!("{run_length}\t{total}\t{}", depth_sum as f32 / total as f32);
        }
    }
    pub fn print_list_node_stats(&self) {
        println!("\n\ttotal_nodes\tlist_node_cnt\tlist_node_rto\tavg_slot0_len\tslot1_cnt\tslot1_used_rto\tavg_slot1_len\tone_byte_keys\tone_byte_rto");
        for depth in 0..self.total_nodes_by_depth.len() {
            println!("{depth}\t{}\t\t{}\t\t{:2.1}%\t\t{:1.4}\t\t{}\t\t{:2.1}%\t\t{:1.4}\t\t{}\t\t{:2.1}%",
                self.total_nodes_by_depth[depth],
                self.total_list_nodes_by_depth[depth],
                self.total_list_nodes_by_depth[depth] as f32 / self.total_nodes_by_depth[depth] as f32 * 100.0,
                self.total_slot0_length_by_depth[depth] as f32 / self.total_list_nodes_by_depth[depth] as f32,
                self.slot1_occupancy_count_by_depth[depth],
                self.slot1_occupancy_count_by_depth[depth] as f32 / self.total_list_nodes_by_depth[depth] as f32 * 100.0,
                self.total_slot1_length_by_depth[depth] as f32 / self.slot1_occupancy_count_by_depth[depth] as f32,
                self.list_node_single_byte_keys_by_depth[depth],
                self.list_node_single_byte_keys_by_depth[depth] as f32 / self.total_list_nodes_by_depth[depth] as f32 * 100.0,
            );
        }
    }
    pub fn count_ocupancy<V: Clone + Send + Sync + Unpin>(map: &PathMap<V>) -> Self {
        let mut counters = Counters::new();

        counters.count_node(map.root().unwrap().as_tagged(), 0);

        let mut zipper = map.read_zipper();
        while zipper.to_next_step() {
            let depth = zipper.path().len();

            counters.run_counter_update(depth);
            if let Some(focus_node) = zipper.get_focus().try_as_tagged() {
                counters.count_node(focus_node, depth);
            } else {
                counters.end_run(depth-1);
            }
        }

        counters
    }
    fn count_node<V: Clone + Send + Sync, A : super::alloc::Allocator>(&mut self, node: TaggedNodeRef<V, A>, depth: usize) {
        if let Some(dbn) = node.as_dense() {
            if dbn.item_count() != 1 {
                self.end_run(depth);
            }
            self.increment_common_counters(node, depth);
            self.total_dense_byte_nodes_by_depth[depth] += 1;
        }
        if let Some(lln) = node.as_list() {
            if lln.item_count() != 1 {
                self.end_run(depth);
            }
            self.increment_common_counters(node, depth);
            self.total_list_nodes_by_depth[depth] += 1;

            let (key0, key1) = lln.get_both_keys();
            self.total_slot0_length_by_depth[depth] += key0.len();
            if key1.len() > 0 {
                self.slot1_occupancy_count_by_depth[depth] += 1;
                self.total_slot1_length_by_depth[depth] += key1.len();
            }
            if key0.len() == 1 || key1.len() == 1 {
                self.list_node_single_byte_keys_by_depth[depth] += 1;
            }
        }
    }
    fn resize_all_historgrams(&mut self, depth: usize) {
        if self.total_nodes_by_depth.len() <= depth {
            self.total_nodes_by_depth.resize(depth+1, 0);
            self.total_child_items_by_depth.resize(depth+1, 0);
            self.max_child_items_by_depth.resize(depth+1, 0);
            self.total_dense_byte_nodes_by_depth.resize(depth+1, 0);
            self.total_list_nodes_by_depth.resize(depth+1, 0);
            self.total_slot0_length_by_depth.resize(depth+1, 0);
            self.slot1_occupancy_count_by_depth.resize(depth+1, 0);
            self.total_slot1_length_by_depth.resize(depth+1, 0);
            self.list_node_single_byte_keys_by_depth.resize(depth+1, 0);
        }
    }
    fn increment_common_counters<V: Clone + Send + Sync, A : super::alloc::Allocator>(&mut self, node: TaggedNodeRef<V, A>, depth: usize) {
        self.resize_all_historgrams(depth);
        let child_item_count = node.item_count();
        self.total_nodes_by_depth[depth] += 1;
        self.total_child_items_by_depth[depth] += child_item_count;
        if self.max_child_items_by_depth[depth] < child_item_count {
            self.max_child_items_by_depth[depth] = child_item_count;
        }
    }
    fn end_run(&mut self, depth: usize) {
        if depth > self.cur_run_start_depth {
            let cur_run_length = depth - self.cur_run_start_depth;
            self.push_run(cur_run_length, depth-1);
        }
        self.cur_run_start_depth = depth;
    }
    fn run_counter_update(&mut self, depth: usize) {
        if self.cur_run_start_depth > depth {
            self.cur_run_start_depth = depth;
        }
    }
    fn push_run(&mut self, cur_run_length: usize, byte_depth: usize) {
        if self.run_length_histogram_by_ending_byte_depth.len() <= cur_run_length {
            self.run_length_histogram_by_ending_byte_depth.resize(cur_run_length+1, vec![]);
        }
        if self.run_length_histogram_by_ending_byte_depth[cur_run_length].len() <= byte_depth {
            self.run_length_histogram_by_ending_byte_depth[cur_run_length].resize(byte_depth+1, 0);
        }
        self.run_length_histogram_by_ending_byte_depth[cur_run_length][byte_depth] += 1;
    }
}

pub fn print_traversal<'a, V: 'a + Clone + Unpin, Z: ZipperIteration + Clone>(zipper: &Z) {
    let mut zipper = zipper.clone();

    println!("{:?}", zipper.path());
    while zipper.to_next_val() {
        println!("{:?}", zipper.path());
    }
}