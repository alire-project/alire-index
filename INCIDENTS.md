This document describes how index maintainers respond to a harmful release in
the community index: malicious code, a compromised account, or a release that
must be withdrawn urgently. For the review of new submissions, see
[REVIEWING.md](REVIEWING.md).

## What reaches users, and when

- `alr` refreshes the community index every 24 hours by default (setting
  `index.auto_update`), or immediately with `alr index --update-all`. A fix
  merged into the index reaches most users within a day.
- Workspaces keep using the releases in their lockfile (`alire.lock`), which
  stores the full release, origin included. Withdrawing or deleting a release
  does not affect them until they run `alr update`.
- Deployed sources stay on disk: in the shared cache (shown by `alr cache`) or
  in each workspace's `alire/cache/dependencies`.
- Manifest actions (`post-fetch`, `pre-build`, `post-build`) run arbitrary
  commands. A malicious release may already have run code on the machines
  that deployed or built it.

## 1. Stop further damage

If auto-merge is enabled, disable it first, so that nothing else is merged
while the incident is handled:

- set the kill switch repository variable described in the auto-merge
  workflow;
- if in doubt, also disable the workflow itself:
  `gh workflow disable <workflow> --repo alire-project/alire-index`.

If the incident involves a GitHub account, block it at the organization
level. This prevents it from opening pull requests or commenting in any
repository of the organization:

```
gh api -X PUT /orgs/alire-project/blocks/<login>
```

or, from the web, Organization settings > Moderation > Blocked users. When the
account belongs to a crate maintainer, also follow the ownership transfer
procedure in the Alire documentation (`doc/policies.md`) for the crates it
maintains.

## 2. Remove the release

Choose depending on the harm:

- **Broken or unwanted release** (e.g. a serious bug, a packaging mistake):
  withdraw it by setting `available = false` in its manifest. This is the
  only change allowed to a published manifest, and its maintainers can submit
  it themselves.
- **Malicious release, or a release by a compromised account:** delete its
  manifest file. Deleting is better than withdrawing here because:
  - the manifest, its origin and its actions disappear from the index,
    instead of remaining visible through `alr show`;
  - the authority over the crate (who may publish its next release) comes
    from the latest release in the index. Withdrawing a release that changed
    `maintainers-logins` would leave those logins in charge; deleting it
    returns authority to the previous release.

  Deletions fail the index checks by design, so the pull request must be
  merged by an administrator. If the harmful change came in a pull request
  touching several files, revert its merge commit in a new pull request
  instead.

Then search the index for other releases from the same author or origin
(`git log --author`, or the origin URL), and handle them in the same way.

## 3. Notify users

Notify as soon as the release is removed from the index, as users must act
on their side. Post in:

- an issue in https://github.com/alire-project/alire-index, kept open and
  pinned until the incident is closed;
- the Alire channel on Gitter (https://gitter.im/ada-lang/Alire);
- the Ada forum (https://forum.ada-lang.io).

State the affected crates and versions, what the harm is, and what users must
do:

1. Refresh the index: `alr index --update-all`.
2. In each workspace that depends on an affected release (check `alire.lock`
   or `alr show --tree`), run `alr update` to move to another release.
3. Delete deployed copies of the release: its folders in the shared cache
   (`alr cache` shows its location) and in `alire/cache/dependencies` of
   sandboxed workspaces.
4. For malicious releases: if the release was deployed or built, its actions
   may have run on that machine. Treat the machine and any credentials on it
   as compromised.

## 4. Close the incident

- Re-enable auto-merge only once the cause is understood, and the checks that
  should have caught it are fixed in
  [alire-index-checks](https://github.com/alire-project/alire-index-checks).
- Summarize the incident, its timeline and the follow-up changes in the
  pinned issue, and then close it.
