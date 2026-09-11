#!/usr/bin/env python3
"""Static file server for local dev that never lets the browser cache
anything -- avoids the stale-module problem where editing a .js file and
reloading still serves the browser's cached copy (seen repeatedly during
this app's development: reloads and even hard-reloads kept serving old
charts.js/app.js content despite the file on disk being current).
"""
import http.server
import os
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))


class NoCacheHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=SCRIPT_DIR, **kwargs)

    def end_headers(self):
        self.send_header("Cache-Control", "no-store, no-cache, must-revalidate")
        self.send_header("Pragma", "no-cache")
        super().end_headers()


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8000
    http.server.test(HandlerClass=NoCacheHandler, port=port)
