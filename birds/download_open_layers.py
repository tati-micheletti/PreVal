"""Download the open-data layers for the birds covariates (no login needed). Resumable: a file that already exists with the size the server reports is skipped;
a partial download continues where it stopped. Zenodo files are checked against the MD5 the server publishes.   python -I birds/download_open_layers.py [group ...]
Groups (smallest first): dwd, hansen, dlr, eth, fda (forest disturbance atlas), age (forest age).  No argument = all.
Output: data_birds/downloads/<group folder>/ (git-ignored)."""
import hashlib, json, os, re, sys, urllib.request

OUT = os.path.join("data_birds", "downloads")
HDR = {"User-Agent": "Mozilla/5.0 (PreVal research download)"}
fails = []

def req(url, method="GET", extra=None):
    h = dict(HDR); h.update(extra or {})
    return urllib.request.Request(url, method=method, headers=h)

def fetch(url, dest, md5=None):
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    try:
        with urllib.request.urlopen(req(url, "HEAD"), timeout=60) as h: size = int(h.headers["Content-Length"])
    except Exception as e:
        print("FAIL (no size)", os.path.basename(dest), e, flush=True); fails.append(dest); return
    if os.path.exists(dest) and os.path.getsize(dest) == size:
        print("have", os.path.basename(dest), flush=True); return
    part = dest + ".part"; have = os.path.getsize(part) if os.path.exists(part) else 0
    try:
        with urllib.request.urlopen(req(url, extra={"Range": "bytes=%d-" % have} if have else None), timeout=300) as r, open(part, "ab" if have and r.status == 206 else "wb") as f:
            while True:
                b = r.read(1 << 20)
                if not b: break
                f.write(b)
        if os.path.getsize(part) != size: raise RuntimeError("size %d != %d" % (os.path.getsize(part), size))
        if md5:
            m = hashlib.md5()
            with open(part, "rb") as f:
                for b in iter(lambda: f.read(1 << 22), b""): m.update(b)
            if m.hexdigest() != md5: raise RuntimeError("md5 mismatch")
        os.replace(part, dest); print("ok  ", os.path.basename(dest), round(size / 1e6, 1), "MB", flush=True)
    except Exception as e:
        print("FAIL", os.path.basename(dest), e, flush=True); fails.append(dest)

def listing(url): return [m for m in re.findall(r'href="([^"?/][^"?]*)"', urllib.request.urlopen(req(url), timeout=60).read().decode("utf-8", "replace"))]

def dwd():
    base = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/"
    for var in ["snowcover_days", "frost_days", "ice_days", "vegetation_begin", "vegetation_end"]:
        for f in listing(base + var + "/"):
            if re.search(r"_(200[5-9]|201\d|202[0-4])(_17)?\.asc\.gz$", f) or f.lower().endswith(".pdf") and "_en" in f:
                fetch(base + var + "/" + f, os.path.join(OUT, "dwd_annual", var, f))

def hansen():
    for t in ["50N_000E", "50N_010E", "60N_000E", "60N_010E"]:
        for layer in ["lossyear", "treecover2000"]:
            n = "Hansen_GFC-2024-v1.12_%s_%s.tif" % (layer, t)
            fetch("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2024-v1.12/" + n, os.path.join(OUT, "hansen_gfc", n))

def dlr():
    base = "https://download.geoservice.dlr.de/TREE_SPECIES_DE/files/"
    for n in ["treespecies_de_2016.tif", "treespecies_de_2016_readme.txt", "treespecies_de_2016.qml", "treespecies_de_2022.tif", "treespecies_de_2022_readme.txt", "treespecies_de_2022.qml"]:
        fetch(base + n, os.path.join(OUT, "dlr_treespecies", n))

def eth():
    for t in "N45E006,N45E009,N45E012,N48E006,N48E009,N48E012,N51E003,N51E006,N51E009,N51E012,N54E006,N54E009,N54E012".split(","):
        n = "ETH_GlobalCanopyHeight_10m_2020_%s_Map.tif" % t
        fetch("https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=" + n, os.path.join(OUT, "eth_canopy_height", n))

def zenodo(rid, names, folder):
    j = json.load(urllib.request.urlopen(req("https://zenodo.org/api/records/%s" % rid), timeout=60))
    for f in j["files"]:
        if f["key"] in names:
            fetch("https://zenodo.org/api/records/%s/files/%s/content" % (rid, f["key"]), os.path.join(OUT, folder, f["key"]), (f.get("checksum") or "").replace("md5:", "") or None)

GROUPS = {"dwd": dwd, "hansen": hansen, "dlr": dlr, "eth": eth,
          "fda": lambda: zenodo("13333034", ["germany.zip"], "forest_disturbance_atlas"),
          "age": lambda: zenodo("17636330", ["PF_Europe.7z", "NF_Europe.7z"], "forest_age")}
for g in (sys.argv[1:] or list(GROUPS)):
    print("=== group", g, flush=True); GROUPS[g]()
print("Finished. Failed: %d" % len(fails), fails, flush=True)
sys.exit(1 if fails else 0)
