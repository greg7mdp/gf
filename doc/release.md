### Release process

- update version in `gf.hpp`
- commit, tag (`v0.7.0` for example) & push
- update version in `gf.hpp` -> `#define GF_VERSION_PATCH dev`
- create the new release on github using tag created above.
- download the tar.gz from github, and use `sha256sum <...tar.gz>` to get the sha256
