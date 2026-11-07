"""The default browser (as xdg-open runs it): says what it was asked to open,
appending it to the file $1 names, and for a sign-in page
(#cli-signin=PORT.STATE) does what the user's Allow does there: posts the
proxy's cookie to the listener (here the fake sidecar's)."""

import re
import sys
import urllib.parse
import urllib.request

out, url = sys.argv[1], sys.argv[2]
with open(out, "a") as f:
    f.write(f"default browser: {url}\n")
m = re.search(r"#cli-signin=(\d+)\.([0-9a-f]+)", url)
if m:
    body = urllib.parse.urlencode({"state": m.group(2), "proxy_cookie": "proxy=ok"}).encode()
    urllib.request.urlopen(f"http://127.0.0.1:{m.group(1)}/callback", body).read()
    with open(out, "a") as f:
        f.write("default browser: allowed\n")
