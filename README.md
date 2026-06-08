### QuickSort
QuickSort is a small Haskell product repository used as a controlled Git
repository fixture for `git-client` validation.  Its source files, commit
history, branches, and tags provide realistic Git content that Amber-driven
tests can clone, branch, commit, push, merge, revert, and tag.

When cloned under `~/soup/quicksort`, this repository is treated as a complete
product checkout.  `git-client` prepares a temporary bare remote from that local
checkout under `/tmp/git-client/remote/quicksort.git` during validation, so the
Git CLI tests do not depend on network access or external repository state.
