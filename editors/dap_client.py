#!/usr/bin/env python3
"""A tiny, readable DAP client for the PeTTa debug adapter (src/dap_server.pl).

It launches `sh dap.sh`, sets a function breakpoint, runs a .metta file, and at
each stop prints the call stack and MeTTa variables, then continues. This is a
demonstration / smoke client -- a real editor (VS Code, etc.) does the same DAP
exchange behind a graphical UI.

Usage:
    python3 editors/dap_client.py <file.metta> <function-to-break-on> [max_stops]
Example:
    python3 editors/dap_client.py examples/fib_buggy.metta fib 4
"""
import json
import os
import subprocess
import sys


class Dap:
    def __init__(self, cmd):
        self.p = subprocess.Popen(
            cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL, cwd=ROOT)
        self.seq = 0

    def send(self, command, arguments=None):
        self.seq += 1
        msg = {"seq": self.seq, "type": "request", "command": command,
               "arguments": arguments or {}}
        body = json.dumps(msg).encode("utf-8")
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
        self.p.stdin.flush()

    def read(self):
        # Read one Content-Length framed message; return the decoded dict.
        length = None
        while True:
            line = self.p.stdout.readline()
            if not line:
                return None
            line = line.decode("utf-8").strip()
            if line.lower().startswith("content-length:"):
                length = int(line.split(":", 1)[1].strip())
            elif line == "" and length is not None:
                body = self.p.stdout.read(length)
                return json.loads(body.decode("utf-8"))

    def wait_for(self, *, event=None, response_to=None):
        # Read messages until a given event or a response to a command arrives,
        # returning that message. Other messages are skipped.
        while True:
            msg = self.read()
            if msg is None:
                return None
            if event and msg.get("type") == "event" and msg.get("event") == event:
                return msg
            if response_to and msg.get("type") == "response" \
                    and msg.get("command") == response_to:
                return msg


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(2)
    program = os.path.abspath(sys.argv[1])
    func = sys.argv[2]
    max_stops = int(sys.argv[3]) if len(sys.argv) > 3 else 3

    dap = Dap(["sh", os.path.join(ROOT, "dap.sh")])

    dap.send("initialize", {"adapterID": "petta"})
    dap.wait_for(response_to="initialize")
    print("→ initialized; setting a breakpoint on function:", func)
    dap.send("setFunctionBreakpoints", {"breakpoints": [{"name": func}]})
    dap.wait_for(response_to="setFunctionBreakpoints")
    dap.send("launch", {"program": program})
    dap.wait_for(response_to="launch")
    dap.send("configurationDone")
    # configurationDone response and the run start; wait for the first stop.

    stops = 0
    while True:
        msg = dap.wait_for(event=None, response_to=None) if False else dap.read()
        if msg is None:
            break
        if msg.get("type") != "event":
            continue
        ev = msg.get("event")
        if ev == "stopped":
            stops += 1
            print(f"\n■ STOPPED (#{stops}) — reason: {msg['body'].get('reason')}")
            dap.send("stackTrace", {"threadId": 1})
            st = dap.wait_for(response_to="stackTrace")
            frames = st["body"]["stackFrames"]
            top = frames[0]["name"] if frames else "?"
            chain = " → ".join(f["name"] for f in frames)
            print(f"    where : {top}")
            print(f"    stack : {chain}")
            dap.send("variables", {"variablesReference": 1})
            va = dap.wait_for(response_to="variables")
            vs = va["body"]["variables"]
            if vs:
                print("    vars  : " + ", ".join(f"{v['name']}={v['value']}" for v in vs))
            else:
                print("    vars  : (none in this frame)")
            if stops >= max_stops:
                print("\n→ reached max stops; disconnecting.")
                dap.send("disconnect")
                break
            print("    action: continue")
            dap.send("continue", {"threadId": 1})
        elif ev in ("terminated", "exited"):
            print(f"\n■ program {ev}.")
            break

    dap.p.wait(timeout=10)


if __name__ == "__main__":
    main()
