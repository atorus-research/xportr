# Contributing to {xportr}

This outlines how to propose a change to
[xportr](https://atorus-research.github.io/xportr/). We aim to follow
common tidyverse and pharmaverse contribution practices while keeping
the review process manageable for the maintainers.

## Before you start

Before you make a substantial contribution, please [open an
issue](https://github.com/atorus-research/xportr/issues) and make sure a
maintainer agrees with the problem statement and proposed scope of work.

If you have found a bug, please include a minimal reproducible example
when possible. If you want to work on an existing issue, leave a comment
so the team knows you plan to pick it up.

## AI-assisted contributions

We welcome responsible use of AI tools when contributing to
[xportr](https://atorus-research.github.io/xportr/).

However, **before using AI for a code contribution, you must first open
an issue and get agreement from the maintainers on the scope of work**.
We do not have the review capacity for unsolicited AI-generated pull
requests, even when the proposed changes appear valuable. Pull requests
created with AI assistance but without prior agreement on scope will not
be reviewed.

AI-assisted contributions are held to the same standards as any other
contribution. Contributors remain responsible for the correctness,
scope, tests, documentation, and maintainability of the final pull
request.

## Pull request process

- Create a branch for your work.
- Keep the pull request focused on the agreed scope.
- Follow the [tidyverse style guide](https://style.tidyverse.org/).
- Add or update unit tests when behavior changes.
- Update relevant roxygen documentation and run `devtools::document()`
  when needed.
- Update `NEWS.md` for user-facing changes.
- Update affected vignettes or examples when applicable.
- Review and complete the pull request template checklist.

## Development guidance

The following project-specific resources are helpful when preparing a
contribution:

- [Pull request
  template](https://github.com/atorus-research/xportr/blob/main/.github/pull_request_template.md)
- [Roxygen header style
  guide](https://github.com/atorus-research/xportr/wiki/Style-Guide-for-Roxygen-Headers)
- [Unit test style
  guide](https://github.com/atorus-research/xportr/wiki/Style-Guide-for-Unit-Tests)

If your proposed change may extend beyond the core scope of
[xportr](https://atorus-research.github.io/xportr/), please discuss it
in an issue before starting implementation.

## Need help?

If you are unsure whether an idea fits the package, open an issue first.
For broader discussion, you can also reach out in the [pharmaverse Slack
workspace](https://pharmaverse.slack.com/archives/C030EB2M4GM).
