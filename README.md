# purescript-fixdeps

fix deps in spago.dhall

```
npm install
npm run build
cat spago.dhall | dhall-to-json | ./bin/purescript-fixdeps | dhall format
```
