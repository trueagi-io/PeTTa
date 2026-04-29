//! ASCII visualization for MeTTa expressions
//!
//! Provides text-based tree visualization of expressions for debugging and tracing.

use crate::engine::MettaValue;

/// Visualize an expression as an ASCII tree
pub fn visualize_expression(value: &MettaValue) -> String {
    let mut output = String::new();
    visualize_value(value, "", true, &mut output);
    output
}

fn visualize_value(
    value: &MettaValue,
    prefix: &str,
    is_last: bool,
    output: &mut String,
) {
    let connector = if is_last { "└── " } else { "├── " };
    let child_prefix = if is_last { "    " } else { "│   " };

    match value {
        MettaValue::Integer(n) => {
            output.push_str(&format!("{}{} (int: {})\n", prefix, connector, n));
        }
        MettaValue::Float(n) => {
            output.push_str(&format!("{}{} (float: {})\n", prefix, connector, n));
        }
        MettaValue::Bool(b) => {
            output.push_str(&format!("{}{} (bool: {})\n", prefix, connector, b));
        }
        MettaValue::Atom(name) => {
            output.push_str(&format!("{}{} (atom: {})\n", prefix, connector, name));
        }
        MettaValue::List(items) => {
            output.push_str(&format!("{}{} (list: {} items)\n", prefix, connector, items.len()));
            for (i, item) in items.iter().enumerate() {
                visualize_value(item, &format!("{}{}", prefix, child_prefix), i == items.len() - 1, output);
            }
        }
        MettaValue::Expression(op, args) => {
            output.push_str(&format!("{}{} (expr: {})\n", prefix, connector, op));
            for (i, arg) in args.iter().enumerate() {
                visualize_value(arg, &format!("{}{}", prefix, child_prefix), i == args.len() - 1, output);
            }
        }
    }
}

/// Format execution statistics as an ASCII table
pub fn format_stats_table(stats: &[(&str, &str)]) -> String {
    let mut output = String::new();
    let max_key_len = stats.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
    
    output.push_str("╭─");
    output.push_str(&"─".repeat(max_key_len + 7));
    output.push_str("─╮\n");
    
    for (key, value) in stats {
        output.push_str(&format!("│ {:<width$} │ {} │\n", key, value, width = max_key_len));
    }
    
    output.push_str("╰─");
    output.push_str(&"─".repeat(max_key_len + 7));
    output.push_str("─╯\n");
    
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visualize_integer() {
        let value = MettaValue::Integer("42".into());
        let viz = visualize_expression(&value);
        assert!(viz.contains("42"));
        assert!(viz.contains("int"));
    }

    #[test]
    fn test_visualize_atom() {
        let value = MettaValue::Atom("foo".into());
        let viz = visualize_expression(&value);
        assert!(viz.contains("foo"));
        assert!(viz.contains("atom"));
    }
}
