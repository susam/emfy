Meta
====

Release Checklist
-----------------

- Update version number in init.el.
- Update year in LICENSE.md.
- Update CHANGES.md.
- Add new screenshots if necessary.
- Tag the release.

  ```
  VER=

  git add -p
  git commit -em "Set version to $VER"
  git tag $VER -m "Emfy $VER"
  git push origin main $VER
  ```
