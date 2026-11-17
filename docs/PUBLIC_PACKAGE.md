# Standalone code package

This repository was extracted into a new Git repository. Its root commit is
`8338790bcc26d90060055e8a64f9780d6cb92ba9`; the manuscript repository's Git objects
and history were not copied. It contains no manuscript LaTeX, article/slide PDFs,
speaker notes, or Overleaf synchronization settings. PDFs under research/original
are computational figures, and bibliography files there are software citations.

The R entry point, synthetic smoke test, both environment definitions and all
1,831 manifest-listed uploaded files remain byte-identical to the validated code
package. After extraction, the smoke test passed 9/9 checks and the uncached
three-model run passed 93/93 reference checks using the locked Netlib environment.
Records are in docs/validation/standalone-*. No full 17.9-minute run was repeated
for this file-only extraction; the prior successful full run is documented in
VALIDATION.md and uses identical computational files and dependencies.

Historical commit IDs in validation records identify provenance only; none of
those earlier commits are included in this repository. The code package was
checked for manuscript files and common credential/private-key patterns.

## Public repository

The standalone repository is:
https://github.com/Tianyi-wtyyy6/STAT5200-Reproducible

It is separate from the manuscript repository. Its default branch contains only
this extracted computational package and its new history. Installation and
reproduction commands are in the root README.
