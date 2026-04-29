//! Criterion benchmarks for PeTTa backends
//!
//! Run with: cargo bench --features mork

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use petta::engine::{Backend, EngineConfig, PeTTaEngine};
use std::path::Path;

fn benchmark_simple_arithmetic(c: &mut Criterion) {
    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let mut group = c.benchmark_group("simple_arithmetic");
    group.throughput(Throughput::Elements(1));

    #[cfg(feature = "mork")]
    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Mork)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("mork", |b| {
            b.iter(|| {
                engine.process_metta_string("!(+ 1 2)").unwrap();
            })
        });
    }

    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Swipl)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("swipl", |b| {
            b.iter(|| {
                engine.process_metta_string("!(+ 1 2)").unwrap();
            })
        });
    }

    group.finish();
}

fn benchmark_pattern_matching(c: &mut Criterion) {
    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let mut group = c.benchmark_group("pattern_matching");
    group.throughput(Throughput::Elements(1));

    let pattern = "(parent $x $y)";

    #[cfg(feature = "mork")]
    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Mork)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("mork", |b| {
            b.iter(|| {
                engine.process_metta_string(pattern).unwrap();
            })
        });
    }

    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Swipl)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("swipl", |b| {
            b.iter(|| {
                engine.process_metta_string(pattern).unwrap();
            })
        });
    }

    group.finish();
}

fn benchmark_list_operations(c: &mut Criterion) {
    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let mut group = c.benchmark_group("list_operations");
    group.throughput(Throughput::Elements(1));

    let map_expr = "!(map (λ x (* x 2)) (1 2 3 4 5))";

    #[cfg(feature = "mork")]
    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Mork)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("mork", |b| {
            b.iter(|| {
                engine.process_metta_string(black_box(map_expr)).unwrap();
            })
        });
    }

    {
        let config = EngineConfig::new(project_root)
            .backend(Backend::Swipl)
            .verbose(false);
        let mut engine = PeTTaEngine::with_config(&config).unwrap();

        group.bench_function("swipl", |b| {
            b.iter(|| {
                engine.process_metta_string(black_box(map_expr)).unwrap();
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_simple_arithmetic,
    benchmark_pattern_matching,
    benchmark_list_operations
);
criterion_main!(benches);
