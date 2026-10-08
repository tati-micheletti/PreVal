# Running the caribou refit on EVE (SpaDES, your runMe.R)

`runMe.R` is your original script with a few marked `# REFIT` changes. Environment variables choose the stage
(`PREVAL_STAGE` = `prep` | `train` | `analyze`; the sbatch files set it). Packages are installed ONCE from a login node
(same pattern as birdMonitor); jobs only load them. EVE facts come from your birdMonitor notes and the EVE wiki pages in
`!BirdFuture\Work\EVE Cluster Info`.

## Folders on EVE
| What | Where |
|---|---|
| code (git clone of this repository) | `~/projects/PreVal` (in /home: software and scripts only) |
| data and outputs, one folder for all datasets | `/data/birds/PreVal/{caribou,birds,trees}/{data,outputs}` (group `eve_birds`) |
| scratch and job logs | `/work/michelet/preval/` (60-day deletion by last access: only temporary things) |
| R packages | `~/.local/share/R/PreVal/packages/` (created by setupProject, like birdMonitor) |

## Rules from the EVE wiki that shape these scripts
- Code goes to the cluster with **git**; data with WinSCP/rsync. Never run data-intensive work in /home.
- Many similar jobs = one **array job** (we use one array); the `testing` partition allows 15 minutes.
- Memory is enforced: a job above its `--mem-per-cpu` request is killed (so the numbers below matter).
- Install packages on a login node, not inside jobs. GPUs exist but this small network (batch 128) fits CPU arrays better.

## Steps (one at a time; check each before the next)
1. **VPN, then login**: `ssh -l michelet frontend1.eve.ufz.de` (done: `/data/birds` exists, group `eve_birds`).
2. **Folders**: `mkdir -p ~/projects /work/michelet/preval/logs /data/birds/PreVal/caribou/data /data/birds/PreVal/caribou/outputs`
   Anyone in group `eve_birds` can read these (`getent group eve_birds` lists the members). If you want the caribou folder
   for yourself only: `chmod 700 /data/birds/PreVal/caribou` (optional).
3. **Code** (git, HTTPS, as for birdMonitor):
   `git clone https://github.com/tati-micheletti/PreVal.git ~/projects/PreVal && cd ~/projects/PreVal`
   `git checkout feature/caribou-refit`
   `git -c url."https://github.com/".insteadOf=git@github.com: submodule update --init modules/caribouNN modules/caribouNN_Global`
   Later updates: `cd ~/projects/PreVal && git pull` (and the same `submodule update`).
4. **Data table** (1.8 GB, about 7 min at your upload speed): WinSCP with **"Preserve timestamp" OFF** and keep-alive on, to
   `/data/birds/PreVal/caribou/data/`. Check the copy: EVE `md5sum <file>` against Windows `Get-FileHash -Algorithm MD5 <file>`.
5. **One-time setup** (login node; installs R packages and libtorch, then submits the smoke test):
   `cd ~/projects/PreVal && bash --login eve/setup_eve.sh`   (first time 30-60 minutes; keep the window open)
6. **Smoke test result**: `cat /work/michelet/preval/logs/smoketest_<jobid>.out` must end with `PREVAL SMOKETEST: ALL OK`.
7. **Submit everything**: `bash eve/submit_refit.sh` (prep, then 100 training tasks at most 50 at once, then analysis).
   Monitor: `squeue -u $USER`; logs in `/work/michelet/preval/logs/`.
8. **Tighten resources after the first tasks**: `sacct -j <jobid> --format=JobID,Elapsed,MaxRSS,State`
9. **Results**: `/data/birds/PreVal/caribou/outputs/refit/analysis/`; copy to your PC with WinSCP.

## Resources (measured on a laptop with the same code; verify on EVE)
- prep: ~7 min, peak 34 GB -> `--cpus-per-task=8 --mem-per-cpu=6G`.
- train task: largest model 8.5 s/epoch (about 7 min for 50 epochs), peak 4.3 GB -> `--cpus-per-task=1 --mem-per-cpu=6G`.
  Whole experiment ~50 core-hours before early stopping (patience 10 saves about a quarter).
- Fair share: 100 queued tasks lower your priority; `MAXPAR` throttles it (`MAXPAR=25 bash eve/submit_refit.sh`).

## If something fails
Prep failed: read `/work/michelet/preval/logs/prep_<id>.err`, fix, run `bash eve/submit_refit.sh` again (the saved plan is reused and re-verified).
A training task failed: failures are listed in `.../outputs/refit/testedModels/errors/`; resubmit the array (finished models are skipped).
The analysis refuses to run while any model has no result (`modelsMissing.txt`).
