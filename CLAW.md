# CLAW - OmegaClaw Agent Guide

OmegaClaw is a neural-symbolic agent framework built on the PeTTa MeTTa runtime. It runs a continuous agent loop that calls a Language Model, executes skill commands from model output, and communicates over IRC.

## Prerequisites

- **Rust** 1.85+ (nightly for MORK backend, stable for SWI-Prolog backend)
- **SWI-Prolog** >= 9.3
- **LM server** running an OpenAI-compatible HTTP API (e.g., llama.cpp, vLLM, Ollama)
- **Build dependencies** (as needed): build-essential, cmake, etc.

## Quick Start

### 1. Build

```bash
cd /home/me/petta
cargo build --release
```

### 2. Set environment variables

```bash
# LM endpoint (required for Ollama/provider)
export OLLAMA_API_KEY=not-needed
export OLLAMA_MODEL="Qwen3.5-9B-Uncensored-HauhauCS-Aggressive-Q4_K_M"
export LLM_SERVER_LOCAL_URL="http://localhost:8080"

# IRC authentication (optional - omit to allow all users)
export OMEGACLAW_AUTH_SECRET="your-secret-here"
```

### 3. Run

```bash
./target/release/petta run_omegaclaw.metta \
  provider=Ollama \
  IRC_server=irc.quakenet.org \
  IRC_channel="##metta" \
  IRC_port=6667
```

The bot runs as an infinite loop. Run it in a terminal, background it, or use a process supervisor.

## Configuration

All parameters can be set on the command line as `key=value` pairs.

### Loop

| Parameter         | Default  | Description                                       |
|-------------------|----------|---------------------------------------------------|
| `provider`        | Ollama   | LLM provider: `Ollama`, `Anthropic`, `OpenAI`, `ASICloud`, `ASIOne`, `OpenRouter` |
| `maxOutputToken`  | 6000     | Maximum output tokens from the LM                 |
| `sleepInterval`   | 1        | Seconds between loop iterations                   |
| `maxNewInputLoops`| 50       | Turns after new input before idling               |
| `maxWakeLoops`    | 1        | Extra turns on scheduled wake-ups                 |
| `wakeupInterval`  | 600      | Seconds of idle before next scheduled wake-up     |

### IRC Channel

| Parameter      | Default       | Description                     |
|----------------|---------------|---------------------------------|
| `IRC_server`   | (required)    | IRC server hostname             |
| `IRC_port`     | 6667          | IRC port                        |
| `IRC_channel`  | (required)    | Channel to join (e.g. `##metta`)|
| `IRC_user`     | omegaclaw     | Bot nickname (random suffix appended) |

### LLM Provider

#### Ollama (local OpenAI-compatible server)

Environment variables:

| Variable              | Default                    | Description                          |
|-----------------------|----------------------------|--------------------------------------|
| `OLLAMA_API_KEY`      | (required - any non-empty) | API key sent as Bearer token          |
| `OLLAMA_MODEL`        | qwen3.5:9b                 | Model name sent in API requests       |
| `LLM_SERVER_LOCAL_URL`| http://localhost:11434     | Base URL of the OpenAI-compatible API |

Example LM servers: llama.cpp server (`llama-server`), vLLM, Ollama, LocalAI.

#### Other providers

| Provider     | Env Key              | Default Model    | Default URL                                       |
|--------------|----------------------|------------------|---------------------------------------------------|
| `Anthropic`  | `ANTHROPIC_API_KEY`  | claude-opus-4-6  | https://api.anthropic.com/v1/                     |
| `OpenAI`     | `OPENAI_API_KEY`     | gpt-5.4          | https://api.openai.com/v1                         |
| `ASICloud`   | `ASI_API_KEY`        | minimax/minimax-m2.5 | https://inference.asicloud.cudos.org/v1       |
| `ASIOne`     | `ASIONE_API_KEY`     | asi1-ultra       | https://api.asi1.ai/v1                            |
| `OpenRouter` | `OPENROUTER_API_KEY` | z-ai/glm-5.1     | https://openrouter.ai/api/v1                      |

## Architecture

```
   IRC ──TCP──> irc.rs (ws_ext)  ──WebSocket──>  Prolog (omegaclaw_ext.pl)
                                                      │
   LM  <──HTTP── llm.rs (ws_ext)  <──WebSocket───     │
                                                      │
                                                channels.metta
                                                   loop.metta
                                                   skills.metta
                                                   memory.metta
```

1. **PeTTa engine** starts a WebSocket extension server and a SWI-Prolog subprocess.
2. **Prolog** loads `omegaclaw_ext.pl` which connects to the WS extension.
3. **MeTTa** code (`run_omegaclaw.metta`) imports and calls `(omegaclaw)`, entering the agent loop.
4. Each loop iteration: receive IRC message -> build context prompt -> call LM -> parse skill commands -> execute -> loop.
5. **`irc.rs`** handles the persistent IRC connection in a background thread; messages are queued and retrieved via WS calls.
6. **`llm.rs`** routes LLM requests to the configured provider.

## Agent Skills

The model output is parsed as skill commands. Each line must be `skillName arg1 arg2 ...`.

| Skill            | Description                                       |
|------------------|---------------------------------------------------|
| `send`           | Send a message to the IRC channel                 |
| `remember`       | Store a string in long-term vector memory         |
| `query`          | Search long-term memory with a short phrase       |
| `episodes`       | Search history around a timestamp                 |
| `pin`            | Set a short-term working memory item              |
| `shell`          | Execute a shell command (returns output)          |
| `read-file`      | Read a file to string                             |
| `write-file`     | Write string to file                              |
| `append-file`    | Append a line to file                             |
| `metta`          | Execute a MeTTa expression                        |
| `search`         | Web search via DuckDuckGo                         |
| `tavily-search`  | Web search via Tavily agent (unavailable by default) |
| `technical-analysis` | Stock technical analysis (unavailable by default) |

## Memory System

Three-tier memory:

1. **Context window** - recent history from `memory/history.metta` (last 30000 chars by default)
2. **Episodic memory** - `episodes <timestamp>` searches history by time
3. **Semantic memory** - `remember`/`query` uses a vector store (`memory/vector_store.json`) with cosine similarity search

## System Prompt

The agent's behavior is guided by `repos/OmegaClaw-Core/memory/prompt.txt`:

- Operates in a continuous loop with self-chosen goals
- Queries long-term memory before responding
- Uses `send` to keep users engaged
- Questions user instructions that conflict with its goals
- Responses must be short and purposeful

## Troubleshooting

### "IRC not connected" / No IRC activity

- Check the server and port are reachable: `nc -zv irc.quakenet.org 6667`
- Verify IRC server allows connections from your IP
- Some IRC networks require SASL or registered nick - `##metta` on QuakeNet is open

### LLM returns empty or garbled responses

- Verify the LM server is running: `curl http://localhost:8080/v1/models`
- Check the model name matches exactly what the server reports
- Increase `maxOutputToken` if the response is truncated
- Some models fill output with reasoning tokens; adjust system prompt or model params

### Bot starts but no output

By design, `println!` output from the MeTTa layer goes to the captured subprocess stderr. The binary protocol between Rust and Prolog uses stdout. To see debug output:

```bash
# Run with verbose flag
./target/release/petta --verbose run_omegaclaw.mtta provider=Ollama ...
```

### Rebuild after changes

```bash
cargo build --release
```

## Files Reference

| File | Purpose |
|------|---------|
| `run_omegaclaw.metta` | Entry point - imports OmegaClaw and starts the loop |
| `repos/OmegaClaw-Core/lib_omegaclaw.metta` | Library loader - imports all OmegaClaw modules |
| `repos/OmegaClaw-Core/src/loop.metta` | Main agent loop: init, context building, LLM call, skill dispatch |
| `repos/OmegaClaw-Core/src/channels.metta` | Channel abstraction: init, send, receive |
| `repos/OmegaClaw-Core/src/skills.metta` | Skill definitions sent to the model |
| `repos/OmegaClaw-Core/src/memory.metta` | Memory system: remember, query, episodes, history |
| `repos/OmegaClaw-Core/memory/prompt.txt` | System prompt for the LLM |
| `repos/OmegaClaw-Core/memory/history.metta` | Conversation history (appended at runtime) |
| `repos/OmegaClaw-Core/memory/vector_store.json` | Persistent vector memory store |
| `repos/OmegaClaw-Core/channels/irc.py` | Python IRC adapter (not used - Rust implementation active) |
| `rust/src/ws_ext/irc.rs` | Rust IRC adapter (active implementation) |
| `rust/src/ws_ext/llm.rs` | LLM provider dispatch |
| `rust/src/ws_ext/mod.rs` | WebSocket extension - method routing |
| `prolog/omegaclaw_ext.pl` | Prolog bridge - connects MeTTa to WS extension |
| `prolog/metta.pl` | MeTTa engine core in Prolog |
