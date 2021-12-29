Meta
====

Screenshots
-----------

This directory contains some example files useful for taking
screenshots used in the [README.md](../README.md) document of this
project.

Use the following window dimensions while taking screenshots:

- `example.el`, `example.md`: 150 x 32
- `lorem1.txt`: 80 x 12
- `lorem2.txt`: 80 x 12
- `lorem3.txt`: 80 x 12
- `lorem4.txt`: 80 x 12


Release Checklist
-----------------

  - Update version number in .emacs.
  - Update year in LICENSE.md.
  - Update CHANGES.md.
  - Add new screenshots if necessary.
  - Commit changes.

        git add -p
        git commit

  - Tag the release.

        VER=

        git commit -em "Set version to $VER"
        git tag $VER -m "Emfy $VER"
        git push origin master $VER
