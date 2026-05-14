# Submission Log

Each subfolder contains `.tex` snapshots for one journal. Only `.tex` and `diff.tex` files are committed; PDFs are gitignored and can be rebuilt with `make diff-pdf`.

## Journals

### JEBO (Journal of Economic Behavior & Organization)

| Revision | Source draft | Date | Notes |
|---|---|---|---|
| `initial` | `draft/first-revision/` | — | First submission |
| `rev1` | `draft/jebo-rev1/` | — | Revision 1 |

## Adding a new revision

```bash
# 1. Build the manuscript on your revision branch
make pdf

# 2. Snapshot the .tex
make submit JOURNAL=jebo REV=rev2

# 3. Generate a diff vs. the previous revision
make diff JOURNAL=jebo FROM=rev1 TO=rev2

# 4. Commit both
git add submissions/jebo/rev2/
git commit -m "snapshot jebo rev2 + diff vs rev1"
```
