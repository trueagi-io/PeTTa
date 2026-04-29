//! Observability module for PeTTa
//!
//! Provides production-grade monitoring and metrics capabilities.

use std::sync::atomic::{AtomicU64};
use std::time::Duration;

/// Metrics collector
#[derive(Debug)]
pub struct Metrics {
queries_total: AtomicU64,
query_errors_total: AtomicU64,
query_duration_sum_ns: AtomicU64,
query_duration_count: AtomicU64,
uptime_start: std::time::Instant,
}

impl Default for Metrics {
fn default() -> Self {
Self {
queries_total: AtomicU64::new(0),
query_errors_total: AtomicU64::new(0),
query_duration_sum_ns: AtomicU64::new(0),
query_duration_count: AtomicU64::new(0),
uptime_start: std::time::Instant::now(),
}
}
}

impl Metrics {
/// Create new metrics instance
pub fn new() -> Self {
Self::default()
}

pub fn increment_queries_total(&self) {
self.queries_total.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

pub fn increment_query_errors(&self) {
self.query_errors_total.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

pub fn record_query_duration(&self, duration: Duration) {
let ns = duration.as_nanos() as u64;
self.query_duration_sum_ns.fetch_add(ns, std::sync::atomic::Ordering::Relaxed);
self.query_duration_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

pub fn queries_total(&self) -> u64 {
self.queries_total.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn query_errors_total(&self) -> u64 {
self.query_errors_total.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn query_duration_avg(&self) -> Duration {
let sum = self.query_duration_sum_ns.load(std::sync::atomic::Ordering::Relaxed);
let count = self.query_duration_count.load(std::sync::atomic::Ordering::Relaxed);
Duration::from_nanos(sum.checked_div(count).unwrap_or(0))
}

pub fn uptime(&self) -> Duration {
self.uptime_start.elapsed()
}

/// Export metrics in Prometheus format
pub fn export_prometheus(&self) -> String {
format!(
r#"# HELP petta_queries_total Total number of queries
# TYPE petta_queries_total counter
petta_queries_total {}

# HELP petta_uptime_seconds Uptime in seconds
# TYPE petta_uptime_seconds gauge
petta_uptime_seconds {}
"#,
self.queries_total(),
self.uptime().as_secs_f64(),
)
}

/// Export metrics as JSON
pub fn export_json(&self) -> serde_json::Value {
serde_json::json!({
"queries_total": self.queries_total(),
"uptime_seconds": self.uptime().as_secs_f64(),
})
}
}

/// Observability configuration
#[derive(Debug, Clone)]
pub struct ObservabilityConfig {
pub service_name: String,
pub version: String,
pub enable_tracing: bool,
pub enable_metrics: bool,
pub log_level: String,
}

impl Default for ObservabilityConfig {
fn default() -> Self {
Self {
service_name: "petta".to_string(),
version: env!("CARGO_PKG_VERSION").to_string(),
enable_tracing: true,
enable_metrics: true,
log_level: "info".to_string(),
}
}
}

impl ObservabilityConfig {
pub fn builder() -> ConfigBuilder {
ConfigBuilder::new()
}
}

pub struct ConfigBuilder {
config: ObservabilityConfig,
}

impl ConfigBuilder {
pub fn new() -> Self {
Self {
config: ObservabilityConfig::default(),
}
}

pub fn service_name(mut self, name: impl Into<String>) -> Self {
self.config.service_name = name.into();
self
}

pub fn version(mut self, version: impl Into<String>) -> Self {
self.config.version = version.into();
self
}

pub fn enable_tracing(mut self, enable: bool) -> Self {
self.config.enable_tracing = enable;
self
}

pub fn enable_metrics(mut self, enable: bool) -> Self {
self.config.enable_metrics = enable;
self
}

pub fn log_level(mut self, level: impl Into<String>) -> Self {
self.config.log_level = level.into();
self
}

pub fn build(self) -> ObservabilityConfig {
self.config
}
}

impl Default for ConfigBuilder {
fn default() -> Self {
Self::new()
}
}

/// Initialize observability
pub fn init_observability(_config: &ObservabilityConfig) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
// In a full implementation, this would set up tracing subscribers
Ok(())
}

/// Shutdown observability
pub fn shutdown_observability() {
// Cleanup
}

/// Health status
#[derive(Debug, Clone)]
pub struct HealthStatus {
pub status: ServiceStatus,
pub version: String,
pub uptime: Duration,
}

impl HealthStatus {
pub fn is_healthy(&self) -> bool {
self.status == ServiceStatus::Healthy
}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ServiceStatus {
Healthy,
Degraded,
Unhealthy,
}

#[cfg(test)]
mod tests {
use super::*;

#[test]
fn test_metrics_collection() {
let metrics = Metrics::new();
metrics.increment_queries_total();
assert_eq!(metrics.queries_total(), 1);
}
}
