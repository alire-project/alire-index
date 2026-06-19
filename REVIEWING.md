This document describes the manual process to review a PR for a release
submission to the Alire's community index.

## Purpose of the review process

- Ensure submitted releases build and resolve on at least some platforms.
- Keep crate names clear and unambiguous (see [POLICY.md](POLICY.md)).
- Protect users from broken, misleading, or malicious manifests.
- Help submitters land good releases, not gatekeep them.

## Checks for all releases

- [ ] Wait for CI checks to pass. Do not merge with failing tests unless a
      justified reason is given in the PR comments.
- [ ] Help the submitter when an error is well known or the cause is readily
      apparent.
- [ ] Missing dependencies are not a failure by themselves: verify that at
      least some platforms resolve and build.
- [ ] Build failures caused by dependencies out of the control of the
      submitter are not a reason to block the PR.
- [ ] Inspect the submitted manifest for anything untoward.
- [ ] For repeat releases, check the Diff workflow output to quickly spot the
      important changes against the previous release.
- [ ] Tags are meaningful and descriptive.
- [ ] `gpr-set-externals` settings are appropriate (see Common pitfalls below).
- [ ] System library dependencies are declared and not left implicit. If a
      needed external crate does not exist yet, request its external to be
      added in the same PR. Examples:
      - https://github.com/alire-project/alire-index/blob/stable-1.4.0/index/li/libcurl/libcurl-external.toml
      - https://github.com/alire-project/alire-index/blob/stable-1.4.0/index/ma/make/make-external.toml

## Checks for new crates (first release)

- [ ] The crate name does not collide with a popular existing Ada/SPARK
      project not yet indexed (see [POLICY.md](POLICY.md)). Do not let a
      submission impersonate or shadow an established project.
- [ ] Inspect the upstream repository: confirm it is a real project with real
      code, not a placeholder or squatting attempt, or otherwise deceitful.

## Common pitfalls

- **Test-only dependencies in the root crate.** Crates depending on e.g.
      `gnattest` or `gnatprove` must not have them in the root crate; those are
      only for testing the crate itself. Request moving them to a nested crate
      and point the submitter to the catalog spec section on testing with pins:
      https://github.com/alire-project/alire/blob/master/doc/catalog-format-spec.md#using-pins-for-crate-testing
- **Use of `gpr-set-externals`.** Setting a GPR external value is:
    - OK to configure the build for the current environment (OS, architecture).
    - OK for crates that produce a binary and are not intended as dependencies.
    - NOT OK to fix the build mode (devel/release) on a library crate; clients
      must be able to set it themselves.

## Approving

- [ ] Once everything checks out, tag the @alire-project/crate-reviewers team
      in a comment stating that the PR is OK to merge.
