"""Check the rendered pages and local downloads before deployment."""
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.refs = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"a", "img", "link", "script"}:
            ref = attrs.get("href") or attrs.get("src")
            if ref:
                self.refs.append(ref)


site = Path("_site")
pages = ["index", "intro", "tidy", "np", "reg", "ml", "resources"]
for name in pages:
    page = site / f"{name}.html"
    html = page.read_text(encoding="utf-8")
    if name in {"np", "reg", "ml"}:
        assert "Under construction" in html, page
    parser = Links()
    parser.feed(html)
    for ref in parser.refs:
        url = urlsplit(ref)
        if url.scheme or url.netloc or not url.path:
            continue
        path = unquote(url.path)
        if path.startswith("/tidysurv/"):
            target = site / path.removeprefix("/tidysurv/")
        elif path.startswith("/"):
            target = site / path.lstrip("/")
        else:
            target = page.parent / path
        assert target.exists(), f"Missing local resource: {page}: {ref}"

assert not list(site.rglob("Sauerbrei*.pdf")), "Reference article is not a site download"
assert not (site / "course-before-redesign.zip").exists()
print("Checked seven pages, construction notes, and local resource links.")
