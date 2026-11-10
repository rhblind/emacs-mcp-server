#!/usr/bin/env python3
"""Lazy Emacs MCP bridge with manual connect.

Stays alive when Emacs is down. Only local tools show
before a connection exists; Emacs tools appear after
connect. Forwards tool calls to Emacs per call.
"""

import argparse
import glob
import hashlib
import json
import os
import socket
import stat
import sys

NAME = "emacs-mcp-lazy"
VERSION = "1.2.0"
PROTOCOL_VERSION = "2024-11-05"

DEFAULT_CANDIDATES = [
    "~/.emacs.d/.local/cache/emacs-mcp-server.sock",
    "~/.config/emacs/.local/cache/emacs-mcp-server.sock",
    "/tmp/emacs-mcp-server.sock",
]

SEARCH_DIRS = [
    "~/.emacs.d/.local/cache",
    "~/.config/emacs/.local/cache",
    "/tmp",
]


def debug(msg):
    if os.getenv("EMACS_MCP_DEBUG"):
        print(f"[{NAME}] {msg}", file=sys.stderr)


def error(msg):
    print(f"[{NAME} ERROR] {msg}", file=sys.stderr)


def is_socket(path):
    try:
        return os.path.exists(path) and stat.S_ISSOCK(os.stat(path).st_mode)
    except OSError:
        return False


def expand(path):
    return os.path.expanduser(os.path.expandvars(path))


def connection_id(target, existing=()):
    """Short deterministic id for a socket path.

    Uses 7 hex chars of sha256. Probes offsets on collision.
    """
    digest = hashlib.sha256(target.encode("utf-8")).hexdigest()
    taken = set(existing)
    for start in range(0, len(digest) - 7 + 1):
        cand = digest[start:start + 7]
        if cand not in taken:
            return cand
    return digest[:16]


def probe_live(path, timeout=0.5):
    """True when something answers on the socket, not just a file."""
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect(path)
        sock.close()
        return True
    except OSError:
        return False


def find_sockets():
    """Return socket files, live ones first."""
    found = []
    dirs = list(SEARCH_DIRS)
    xdg = os.getenv("XDG_RUNTIME_DIR")
    if xdg:
        dirs.append(xdg)
    for raw in dirs:
        d = expand(raw)
        if not os.path.isdir(d):
            continue
        for sock in glob.glob(os.path.join(d, "emacs-mcp-server*.sock")):
            if is_socket(sock):
                found.append(sock)
    found = sorted(set(found))
    return sorted(found, key=lambda p: not probe_live(p))


def resolve_socket(cli_path, mode):
    """Pick socket without exiting when none exists."""
    if cli_path:
        return expand(cli_path)
    if mode not in ("auto", "manual"):
        return expand(mode)  # explicit target path
    found = find_sockets()
    live = [p for p in found if probe_live(p)]
    if live:
        return live[0]
    if found:
        return found[0]
    for raw in DEFAULT_CANDIDATES:
        p = expand(raw)
        if is_socket(p):
            return p
    return expand(DEFAULT_CANDIDATES[0])


LOCAL_TOOLS = [
    {
        "name": "get_targets",
        "description": "Discover available Emacs socket paths.",
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "connect",
        "description": "Connect to an Emacs socket from get_targets. "
        "Cache the connection_id returned upon successful "
        "connection for future use.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "target": {
                    "type": "string",
                    "description": "Socket path from get_targets",
                }
            },
            "required": ["target"],
        },
    },
    {
        "name": "disconnect",
        "description": "Disconnect from Emacs instance.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "connection_id": {
                    "type": "string",
                    "description": "Unique identifier for the "
                    "target Emacs instance",
                },
            },
            "required": ["connection_id"],
        },
    },
    {
        "name": "server_status",
        "description": "Show connections and reachability.",
        "inputSchema": {"type": "object", "properties": {}},
    },
]

LOCAL_NAMES = {t["name"] for t in LOCAL_TOOLS}

RESOURCES = [
    {
        "uri": "emacs-connections://",
        "name": "Active Emacs connections",
        "mimeType": "application/json",
    },
    {
        "uri": "emacs-diagnostics://workspace",
        "name": "Workspace diagnostics via Emacs",
        "mimeType": "application/json",
    },
]


def text_result(text, is_error=False):
    return {
        "content": [{"type": "text", "text": text}],
        "isError": is_error,
    }


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def send_result(msg_id, result):
    send({"jsonrpc": "2.0", "id": msg_id, "result": result})


def send_error(msg_id, code, message):
    send({"jsonrpc": "2.0", "id": msg_id,
          "error": {"code": code, "message": message}})


def emacs_roundtrip(sock_path, payload, timeout):
    """Send one JSON-RPC message to Emacs, return parsed reply."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.settimeout(timeout)
    try:
        sock.connect(sock_path)
        f = sock.makefile("r", encoding="utf-8")
        sock.sendall((json.dumps(payload) + "\n").encode("utf-8"))
        line = f.readline()
        if not line:
            raise ConnectionError("Emacs closed connection")
        return json.loads(line)
    finally:
        try:
            sock.close()
        except OSError:
            pass


def emacs_tools(sock_path, timeout):
    """Fetch tool list from Emacs, or None when unreachable."""
    try:
        reply = emacs_roundtrip(
            sock_path,
            {"jsonrpc": "2.0", "id": "probe",
             "method": "tools/list", "params": {}},
            timeout,
        )
    except Exception as exc:
        debug(f"Emacs probe failed: {exc}")
        return None
    if not isinstance(reply, dict):
        return None
    result = reply.get("result")
    if not isinstance(result, dict):
        return None
    tools = result.get("tools")
    return tools if isinstance(tools, list) else None


class Bridge:
    def __init__(self, sock_path, timeout, probe_timeout=2,
                 mode="manual"):
        self.active = sock_path
        self.timeout = timeout
        self.probe_timeout = probe_timeout
        self.mode = mode
        self.next_id = 0
        self.connections = {}
        # Manual mode starts disconnected. Auto
        # or explicit target modes connect when live.
        if mode != "manual" and probe_live(sock_path):
            self.connections[self.id_for_target(sock_path)] = sock_path

    def id_for_target(self, target):
        """Existing id for target, else a fresh one."""
        for cid, path in self.connections.items():
            if path == target:
                return cid
        return connection_id(target, self.connections)

    def notify_tools_changed(self):
        send({"jsonrpc": "2.0",
              "method": "notifications/tools/list_changed"})

    def connections_instruction(self):
        """Status text for get_targets."""
        lines = ["## Connection Status", "",
                 f"Connection mode: `{self.mode}`", ""]
        if not self.connections:
            lines.append("**Active Connections:** None")
        else:
            lines.append("**Active Connections:**")
            lines.append("")
            for cid, target in self.connections.items():
                lines.append(f"- **Connection ID: `{cid}`** "
                             f"-> Target: `{target}`")
            lines.append("")
            lines.append("**Ready to use!** You can immediately "
                         "use any connection-aware tools with the "
                         "connection IDs above.")
        return "\n".join(lines)

    def status(self):
        return {
            "active": self.active,
            "reachable": probe_live(self.active),
            "connections": dict(self.connections),
            "found": find_sockets(),
        }

    def handle_list(self, msg_id):
        tools = [dict(t) for t in LOCAL_TOOLS]
        for t in tools:
            if t.get("name") == "get_targets":
                t["description"] = (
                    t["description"] + "\n\n"
                    + self.connections_instruction()).strip()
        if self.connections:
            seen = {t["name"] for t in tools}
            sockets = list(dict.fromkeys(
                self.connections.values())) or [self.active]
            for sock in sockets:
                remote = emacs_tools(sock, self.probe_timeout)
                if not remote:
                    debug(f"Emacs probe failed for {sock}")
                    continue
                for tool in remote:
                    if not isinstance(tool, dict):
                        continue
                    name = tool.get("name")
                    if not name or name in seen:
                        continue
                    # Inject connection_id for dynamic
                    # tools, then strip it before
                    # forwarding since Emacs ignores it.
                    schema = tool.get("inputSchema")
                    if not isinstance(schema, dict):
                        schema = {"type": "object",
                                  "properties": {}}
                        tool["inputSchema"] = schema
                    props = schema.get("properties")
                    if not isinstance(props, dict):
                        props = {}
                        schema["properties"] = props
                    props.setdefault(
                        "connection_id",
                        {"type": "string",
                         "description": "Unique identifier for "
                         "the target Emacs instance"})
                    req = schema.get("required")
                    if not isinstance(req, list):
                        req = []
                        schema["required"] = req
                    if "connection_id" not in req:
                        req.append("connection_id")
                    tools.append(tool)
                    seen.add(name)
            if not any(t.get("name") not in LOCAL_NAMES
                       for t in tools):
                debug("Emacs unreachable, listing local only")
        if not self.connections:
            # Hide connection-aware tools
            # (schemas with connection_id) when detached.
            tools = [t for t in tools if "connection_id" not in (
                (t.get("inputSchema") or {}).get("properties")
                or {})]
        send_result(msg_id, {"tools": tools})

    def handle_call(self, msg_id, params):
        name = (params or {}).get("name")
        args = (params or {}).get("arguments") or {}
        if name == "get_targets":
            found = find_sockets()
            if not found:
                send_result(msg_id, text_result(
                    "No Emacs targets found", is_error=True))
            else:
                send_result(msg_id, text_result(json.dumps(found)))
            return
        if name == "server_status":
            send_result(msg_id, text_result(json.dumps(self.status())))
            return
        if name == "connect":
            target = args.get("target") or args.get("socket_path") or ""
            target = expand(target) if target else ""
            if not target:
                send_result(msg_id, text_result("target required",
                                                is_error=True))
                return
            cid = self.id_for_target(target)
            if probe_live(target):
                first = len(self.connections) == 0
                self.connections[cid] = target
                self.active = target
                send_result(msg_id, text_result(
                    json.dumps({"connection_id": cid, "target": target})))
                if first:
                    self.notify_tools_changed()
            else:
                send_result(msg_id, text_result(
                    f"Socket unreachable: {target} "
                    f"found={find_sockets()}",
                    is_error=True))
            return
        if name == "disconnect":
            cid = args.get("connection_id", "")
            if cid in self.connections:
                target = self.connections.pop(cid)
                if self.active == target and self.connections:
                    self.active = list(
                        self.connections.values())[-1]
                send_result(msg_id, text_result(json.dumps(
                    {"connection_id": cid, "target": target})))
                if not self.connections:
                    self.notify_tools_changed()
            else:
                send_result(msg_id, text_result(
                    f"Unknown connection_id: {cid}", is_error=True))
            return
        # Route by connection_id. Strip it
        # before forwarding since Emacs ignores it.
        cid = args.pop("connection_id", None) \
            if isinstance(args, dict) else None
        if cid is not None and cid in self.connections:
            sock_path = self.connections[cid]
        elif self.connections:
            if cid is not None:
                send_result(msg_id, text_result(
                    f"No Emacs connection found for ID: {cid}. "
                    "Use get_targets then connect.",
                    is_error=True))
                return
            if len(self.connections) == 1:
                sock_path = next(iter(self.connections.values()))
            elif self.active in self.connections.values():
                sock_path = self.active
            else:
                send_result(msg_id, text_result(
                    "Multiple Emacs connections; pass "
                    "connection_id. Use get_targets then "
                    "server_status.",
                    is_error=True))
                return
        else:
            send_result(msg_id, text_result(
                f"Not connected to Emacs (tool '{name}'). "
                "Use get_targets then connect.",
                is_error=True))
            return
        # Forward to live Emacs per call.
        self.next_id += 1
        try:
            reply = emacs_roundtrip(
                sock_path,
                {"jsonrpc": "2.0", "id": self.next_id,
                 "method": "tools/call",
                 "params": {"name": name, "arguments": args}},
                self.timeout,
            )
        except Exception as exc:
            send_result(msg_id, text_result(
                f"Emacs unreachable at {sock_path}: {exc}. "
                "Use get_targets then connect.",
                is_error=True))
            return
        if "result" in reply:
            send_result(msg_id, reply["result"])
        elif "error" in reply:
            send_result(msg_id, text_result(
                f"Emacs error: {reply['error']}", is_error=True))
        else:
            send_result(msg_id, text_result(
                f"Bad reply from Emacs: {reply}", is_error=True))

    def handle_read(self, msg_id, params):
        uri = (params or {}).get("uri", "")
        if uri == "emacs-connections://":
            send_result(msg_id, {"contents": [{
                "uri": uri, "mimeType": "application/json",
                "text": json.dumps(self.status()),
            }]})
            return
        if uri == "emacs-diagnostics://workspace":
            if not self.connections:
                send_error(msg_id, -32000,
                           "Not connected to Emacs. "
                           "Use get_targets then connect.")
                return
            if self.active in self.connections.values():
                sock_path = self.active
            else:
                sock_path = next(iter(self.connections.values()))
            self.next_id += 1
            try:
                reply = emacs_roundtrip(
                    sock_path,
                    {"jsonrpc": "2.0", "id": self.next_id,
                     "method": "tools/call",
                     "params": {"name": "get-diagnostics",
                                "arguments": {}}},
                    self.timeout,
                )
            except Exception as exc:
                send_error(msg_id, -32000,
                           f"Emacs unreachable: {exc}")
                return
            result = reply.get("result", reply)
            send_result(msg_id, {"contents": [{
                "uri": uri, "mimeType": "application/json",
                "text": json.dumps(result),
            }]})
            return
        send_error(msg_id, -32602, f"Unknown resource: {uri}")

    def handle(self, msg):
        if not isinstance(msg, dict):
            return
        method = msg.get("method")
        msg_id = msg.get("id")
        if method == "initialize":
            send_result(msg_id, {
                "protocolVersion": PROTOCOL_VERSION,
                "capabilities": {
                    "tools": {"listChanged": True},
                    "resources": {"subscribe": False, "listChanged": True},
                },
                "serverInfo": {"name": NAME, "version": VERSION},
            })
        elif method == "ping":
            if msg_id is not None:
                send_result(msg_id, {})
        elif method == "tools/list":
            if msg_id is not None:
                self.handle_list(msg_id)
        elif method == "tools/call":
            if msg_id is not None:
                self.handle_call(msg_id, msg.get("params"))
        elif method == "resources/list":
            if msg_id is not None:
                send_result(msg_id, {"resources": RESOURCES})
        elif method == "resources/read":
            if msg_id is not None:
                self.handle_read(msg_id, msg.get("params"))
        elif method in ("notifications/initialized",
                        "notifications/cancelled"):
            pass
        elif msg_id is not None:
            send_error(msg_id, -32601, f"Method not found: {method}")

    def run(self):
        debug(f"active socket: {self.active}")
        for line in sys.stdin:
            if not line.strip():
                continue
            try:
                self.handle(json.loads(line))
            except json.JSONDecodeError as exc:
                error(f"Bad JSON from client: {exc}")


def main():
    ap = argparse.ArgumentParser(description="Lazy Emacs MCP bridge")
    ap.add_argument("socket_path", nargs="?",
                    help="Emacs socket path, else discovery")
    ap.add_argument("--connect", default="manual",
                    help="auto, manual, or explicit socket path")
    ap.add_argument("--timeout", type=int, default=10)
    ap.add_argument("--list-sockets", action="store_true")
    ap.add_argument("--test-connection", action="store_true")
    ap.add_argument("--version", action="version",
                    version=f"{NAME} {VERSION}")
    args = ap.parse_args()

    if args.list_sockets:
        for sock in find_sockets():
            print(sock)
        return

    sock_path = resolve_socket(args.socket_path, args.connect)
    timeout = int(os.getenv("EMACS_MCP_TIMEOUT", str(args.timeout)))
    # Explicit target connects now instead of waiting
    # for connect tool.
    mode = args.connect
    if args.socket_path or mode not in ("auto", "manual"):
        mode = args.socket_path or args.connect

    if args.test_connection:
        print(f"Testing connection to: {sock_path}")
        if is_socket(sock_path):
            print("Connection successful!")
        else:
            print(f"Missing: {sock_path} found={find_sockets()}")
            sys.exit(1)
        return

    Bridge(sock_path, timeout, mode=mode).run()


if __name__ == "__main__":
    main()
