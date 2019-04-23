# purescript-fixdeps

fix deps in spago.dhall

```
npm install
npm run build
cat spago.dhall | dhall-to-json | npx @bouzuya/purescript-fixdeps | dhall format
```
