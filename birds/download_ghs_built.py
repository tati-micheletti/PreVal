"""Download GHS-BUILT-S (built-up surface, 100 m, Mollweide) from the JRC open-data server for the 4 tiles that cover Germany, epochs 2005-2020.
No login needed. Each file is checked against the size the server reports and opened as a zip.   python birds/download_ghs_built.py
Output: data_birds/downloads/ghs_built_s/E<epoch>/<tile>.zip  (a file that already exists and is complete is skipped)."""
import os, sys, urllib.request, zipfile
EPOCHS = [2005, 2010, 2015, 2020]
TILES = [(3, 19), (3, 20), (4, 19), (4, 20)]
ROOT = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/"
out = os.path.join("data_birds", "downloads", "ghs_built_s")
ok = bad = 0
for e in EPOCHS:
    d = os.path.join(out, "E%d" % e); os.makedirs(d, exist_ok=True)
    for r, c in TILES:
        name = "GHS_BUILT_S_E%d_GLOBE_R2023A_54009_100_V1_0_R%d_C%d.zip" % (e, r, c)
        url = "%sGHS_BUILT_S_E%d_GLOBE_R2023A_54009_100/V1-0/tiles/%s" % (ROOT, e, name)
        dest = os.path.join(d, name)
        try:
            with urllib.request.urlopen(urllib.request.Request(url, method="HEAD"), timeout=60) as h: size = int(h.headers["Content-Length"])
            if os.path.exists(dest) and os.path.getsize(dest) == size and zipfile.is_zipfile(dest):
                print("have", name, flush=True); ok += 1; continue
            with urllib.request.urlopen(url, timeout=300) as resp, open(dest + ".part", "wb") as f:
                while True:
                    b = resp.read(1 << 20)
                    if not b: break
                    f.write(b)
            if os.path.getsize(dest + ".part") != size: raise RuntimeError("size %d != %d" % (os.path.getsize(dest + ".part"), size))
            os.replace(dest + ".part", dest)
            with zipfile.ZipFile(dest) as z:
                if z.testzip() is not None: raise RuntimeError("corrupt zip")
            print("ok  ", name, round(size / 1e6, 1), "MB", flush=True); ok += 1
        except Exception as ex:
            print("FAIL", name, ex, flush=True); bad += 1
print("Finished: %d ok, %d failed" % (ok, bad))
sys.exit(1 if bad else 0)
