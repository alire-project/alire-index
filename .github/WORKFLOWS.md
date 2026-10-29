# Maintaining the workflows

Rules for anyone editing `.github/workflows`. The rationale is in
[AEP-6](https://github.com/alire-project/alire/blob/master/doc/AEPs/aep-0006-auto-merge.md),
which hardens the index CI towards automatic merging of releases.

For how to test workflow changes before merging them, see [TESTING](TESTING).

## Checks live in alire-index-checks

The scripts run by these workflows live in
[alire-index-checks](https://github.com/alire-project/alire-index-checks), and
are checked out at the branch matching the PR base. Every check here needs a
mirror workflow there, which runs it on that repository's sample index. See
"Mirror workflows" in its README.

## Required checks

Branch protection on the stable branch requires the checks and builds to pass
before merging, matching them **by job name**. Therefore:

- Required job names must be unique across workflows and stable. The required
  checks are:
  - `check / Release-only`, `check / Author`, `check / Crate Name`,
    `check / Schema` and `Index`;
  - one summary job per build workflow: `native / all`, `alr dev / all`,
    `distro / all` and `macports / all`.

  Build matrix jobs (e.g. `native / ubuntu-latest`) are never required
  directly. Each build workflow ends with an `all` job that succeeds only if
  the whole matrix succeeded, so runners can be added, removed or renamed
  without touching branch protection. Renaming a required job, or adding a
  workflow that must pass, still requires updating the settings.
- A summary job must use `if: always()` and check `needs.<job>.result`
  explicitly. Without it, the job is skipped when the matrix fails, and a
  skipped required check counts as passed.
- Checks and builds that only make sense for manifests keep their
  `paths: index/**.toml` filter. This is fail-safe: a required check that does
  not run never counts as passed, it only blocks the PR. The PRs it skips
  change no manifest, so they fail `Release-only` and need an admin anyway.
  `Release-only` and `Index` have no filter, so they report on every PR.
- Some checks fail by design to request manual review (e.g. `Release-only`
  on PRs touching `.github`, `Crate Name` on discouraged names). Admins merge
  those after review, bypassing requirements.

## Permissions

- Every workflow declares an explicit `permissions:` block. Use
  `contents: read` unless a write permission is justified, and grant writes
  per job where possible.
- The repository default for workflow tokens is read-only, and Actions may not
  approve PRs (Settings → Actions → General → Workflow permissions).
- Do not pass `GITHUB_TOKEN` to steps that build or run crate code. That code
  is untrusted. Build labeling is disabled for this reason, and because fork
  PRs only get a read-only token anyway.

## Untrusted values

Values that come from the PR (title, branch name, author login, file names)
must never be expanded with `${{ }}` inside a `run:` script. Pass them through
`env:` and quote them in the shell:

```yaml
- run: '"$CHECKS_REPO/scripts/$SCRIPT" "$SCRIPT_ARG"'
  env:
    SCRIPT_ARG: ${{ inputs.script_arg }}
```

## Pinning actions

Actions from GitHub (`actions/*`) and from alire-project or its maintainers are
trusted like this repository, and use version tags (`@v4`, `@latest`). Pin to
a full commit SHA, with the version as a comment, in these cases:

- **third-party actions**, always;
- **every action in a workflow that holds a write token or secrets**, such as
  `cleaner.yml` or `auto-merge.yml`.

```yaml
uses: melusina-org/setup-macports@103eded5f10979e927bffcd6d0b86cbbdb2c3bb3 # v1.1.6
```

## Auto-merge bot

`auto-merge.yml` runs hourly `scripts/automerge-bot.py` from
alire-index-checks, which merges eligible releases of existing crates as
described in AEP-6 (see the README there for the rules). It is not a check:
it has no mirror, and branch protection does not refer to it.

It is controlled by repository variables (Settings → Secrets and variables →
Actions → Variables):

- `AUTO_MERGE`: `off` or unset (nothing runs; this is the kill switch),
  `dry-run` (decisions are only logged in the tracking issue) or `on` (PRs
  are labeled, commented and merged). For example:
  `gh variable set AUTO_MERGE --body off --repo alire-project/alire-index`.
- `AUTO_MERGE_ISSUE`: number of the pinned issue where `dry-run` logs every
  change of decision.
- `AUTO_MERGE_APP_ID`: ID of the GitHub App that merges in `on` mode, with
  its private key in the `AUTO_MERGE_APP_KEY` secret.

The workflow's own token can only write issues, for the tracking issue. Only
the App can merge: it must be installed on this repository alone, with
contents, issues and pull requests write and checks read permissions (the
workflow asks for exactly these), and be allowed to bypass required pull
requests and to push in the `stable-*` branch protection rule. Required
status checks still apply to its merges.

Labels used by the bot: `auto-merge: veto` (set by maintainers to stop a
merge), and, set by the bot to show its decision, `auto-merge: pending`
(eligible, waiting for checks), `auto-merge: cooling` (ready, cooling off
before the merge), `auto-merge: manual` and `auto-merged`.
