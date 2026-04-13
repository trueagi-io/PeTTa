//! Execution profiling for PeTTa queries.
//!
//! Provides timing instrumentation for each stage of MeTTa query execution:
//! - Engine initialization
//! - Query serialization
//! - Prolog round-trip
//! - Result parsing

use std::time::Duration;
use tracing::info;

/// Timing profile for a single query.
#[derive(Debug, Clone)]
pub struct QueryProfile {
    pub query_type: String,
    pub payload_size: usize,
    pub serialization_time: Duration,
    pub round_trip_time: Duration,
    pub parse_time: Duration,
    pub total_time: Duration,
    pub result_count: usize,
}

impl QueryProfile {
    pub fn new(query_type: &str, payload_size: usize) -> Self {
        Self {
            query_type: query_type.to_string(),
            payload_size,
            serialization_time: Duration::ZERO,
            round_trip_time: Duration::ZERO,
            parse_time: Duration::ZERO,
            total_time: Duration::ZERO,
            result_count: 0,
        }
    }

    pub fn summary(&self) -> String {
        format!(
            "Query '{}' ({} bytes): serialize={:.3}ms, round_trip={:.3}ms, parse={:.3}ms, total={:.3}ms, results={}",
            self.query_type,
            self.payload_size,
            self.serialization_time.as_secs_f64() * 1000.0,
            self.round_trip_time.as_secs_f64() * 1000.0,
            self.parse_time.as_secs_f64() * 1000.0,
            self.total_time.as_secs_f64() * 1000.0,
            self.result_count,
        )
    }
}

/// Aggregated profile statistics across multiple queries.
#[derive(Debug, Default)]
pub struct ProfileStats {
    pub query_count: u64,
    pub total_serialization: Duration,
    pub total_round_trip: Duration,
    pub total_parse: Duration,
    pub total_time: Duration,
    pub total_results: u64,
    pub total_bytes_processed: u64,
}

impl ProfileStats {
    pub fn add(&mut self, p: &QueryProfile) {
        self.query_count += 1;
        self.total_serialization += p.serialization_time;
        self.total_round_trip += p.round_trip_time;
        self.total_parse += p.parse_time;
        self.total_time += p.total_time;
        self.total_results += p.result_count as u64;
        self.total_bytes_processed += p.payload_size as u64;
    }

    pub fn summary(&self) -> String {
        if self.query_count == 0 {
            return "No queries profiled.".to_string();
        }
        let avg = |d: Duration| d.as_secs_f64() * 1000.0 / self.query_count as f64;
        format!(
            "Profile Summary: {} queries, {} bytes processed\n\
             Total: {:.3}ms (avg {:.3}ms/query)\n\
             Serialization: {:.3}ms (avg {:.3}ms)\n\
             Round-trip: {:.3}ms (avg {:.3}ms)\n\
             Parse: {:.3}ms (avg {:.3}ms)\n\
             Total results: {}",
            self.query_count,
            self.total_bytes_processed,
            self.total_time.as_secs_f64() * 1000.0,
            avg(self.total_time),
            self.total_serialization.as_secs_f64() * 1000.0,
            avg(self.total_serialization),
            self.total_round_trip.as_secs_f64() * 1000.0,
            avg(self.total_round_trip),
            self.total_parse.as_secs_f64() * 1000.0,
            avg(self.total_parse),
            self.total_results,
        )
    }

    pub fn log(&self) {
        info!("{}", self.summary());
    }
}
