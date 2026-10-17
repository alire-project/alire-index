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
  `cleaner.yml`, or a future auto-merge bot.

```yaml
uses: melusina-org/setup-macports@103eded5f10979e927bffcd6d0b86cbbdb2c3bb3 # v1.1.6
```
