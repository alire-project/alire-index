# Automatic merging of releases

New releases of crates already in this index are merged automatically when
they meet the rules below. Everything else is reviewed by the index
maintainers, as described in [REVIEWING.md](REVIEWING.md). The full rules and
their rationale are (as of 2026-11-15, that AEP will not further updated and
this document instead should be considered authoritative) in
[AEP-6](https://github.com/alire-project/alire/blob/master/doc/AEPs/aep-0006-auto-merge.md#eligibility-policy-for-auto-merge).

## Which PRs are merged automatically

A PR is merged automatically if all of these hold:

- It only adds releases, of one or more crates that already have a release in
  the index. Withdrawals (setting `available = false` in an existing release)
  are also accepted.
- You are listed in `maintainers-logins` of the latest release of each crate.
- Each version is a valid semantic version that is new to the index, even
  when build metadata (`+...`) is ignored.
- Repository origins are on a trusted host: GitHub,
  GitLab, Codeberg, Bitbucket, Launchpad or Savannah as of this writing 
  (see `alr help settings` and `origins.git.trusted_sites` for the current 
   allowlist).
  - Source archive origins are not subject to this check and can be hosted
    anywhere.
- Compared with that latest release, these are unchanged:
  `maintainers-logins`, `provides`, `forbids`, `[[actions]]`,
  `[environment]`, `[gpr-set-externals]`, the mirrors, and the kind and
  repository of the origin. Versions, commits, hashes, dependencies,
  descriptions and the like may change freely.
- All checks passed, and each release built (or was fetched) on at least one
  platform.
- No index maintainer vetoed it, and it has not changed for one hour (not
  needed for withdrawals).
- You have had at most 10 releases merged automatically in the last 24 hours,
  counting this PR.

These always need a manual review:

- the first release of a crate;
- externals (`*-external.toml`);
- any other change, such as modifying or deleting an existing release.

## What to expect

The bot checks open PRs every hour, ignoring drafts. `alr publish` opens PRs as
drafts, so mark yours as ready for review once its checks pass (manually or with
`alr publish --request-review`). The bot shows
its decision with a label and a comment on the PR, which it updates as things
change:

| Label | Meaning |
|---|---|
| `auto-merge: pending` | Eligible, waiting for the checks to finish |
| `auto-merge: cooling` | Ready; it will be merged after the time in the comment |
| `auto-merged` | Merged automatically |
| `auto-merge: manual` | A maintainer will review it; the comment says why |

Pushing to the PR, reopening it or moving it out of draft starts the hour
again. A PR with conflicts is left on hold until they are fixed. In absence
of conflicts, it is *not* necessary to rebase or merge to have your PR at the
tip of the branch; only rebase if you depend on some new release in the index or
to trigger a re-run of automated checks.

Maintainers can stop the automatic merge of any PR with the
`auto-merge: veto` label or by requesting changes. Every decision of the bot
is also logged in the current pinned issue "Auto-merge log".
