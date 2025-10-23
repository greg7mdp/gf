### Release process

- update version in `gf.hpp`
- commit, tag (`v0.7.0` for example) & push -T
- update version in `gf.hpp` -> `#define GF_VERSION_MINOR X_dev`
- create the new release on github using tag created above.
- download the tar.gz from github, and use `sha256sum <...tar.gz>` to get the sha256
