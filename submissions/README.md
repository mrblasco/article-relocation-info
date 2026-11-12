# Submission Log

Each subfolder of `submissions/<journal>/` holds the files for one stage of the review process. Only sources are committed (`.tex`, `.Rmd`, `.md`, `_output.yml`); PDFs are gitignored and rebuilt on demand.

## JEBO (Journal of Economic Behavior & Organization)

| Folder | Contents | Notes |
|---|---|---|
| `jebo/initial/` | `manuscript.tex` | First submission |
| `jebo/round1/` | `manuscript.tex`, `response.Rmd`, reviews (`report_R1.docx`, ...) | Revision after round 1 and its response to reviewers |
| `jebo/round2/` | `response.Rmd`, `reviewers.md` | Response to round 2 (current) |

## Round 2 workflow

```bash
# 1. Build the revised manuscript (runs run_analysis.R first)
make manuscript

# 2. Diff against the round 1 manuscript (submissions/jebo/round1/manuscript.tex)
make diff            # -> output/diff/main_diff.pdf

# 3. Build the response to reviewers
make review          # -> submissions/jebo/round2/response.{pdf,docx}

# 4. Snapshot the submitted manuscript
cp output/main.tex submissions/jebo/round2/manuscript.tex
git add submissions/jebo/round2/ && git commit -m "snapshot jebo round2"
```

`make review` copies the root `refs.bib` into `round2/` before rendering; that copy and the built outputs are not meant to be committed.
