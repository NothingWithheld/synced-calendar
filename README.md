## Developing

`npm run start-frontend` -> http://localhost:8000/

`npm run start-server` -> http://localhost:3000/

## Docs

- [Mostly Adequate Guide to Functional Programming](https://mostly-adequate.gitbooks.io/mostly-adequate-guide/)
- [elm docs](https://guide.elm-lang.org/)
- [elm core](https://package.elm-lang.org/packages/elm/core/)
- elm-mdc docs
  - ```
    cd elm-mdc
    elm make --docs=docs.json
    ```
  - Navigate to [here](https://elm-doc-preview.netlify.com/)
  - "Open Files"
  - `docs.json`
- [Yesod book](https://www.yesodweb.com/book)
- [Yesod Cookbook](https://github.com/yesodweb/yesod-cookbook/blob/master/Cookbook.md)

## Elm Setup

- Install Elm `npm install --global elm`
- For VSCode
  - Install [this](https://marketplace.visualstudio.com/items?itemName=Elmtooling.elm-ls-vscode) plugin
  - Install elm-test and elm-format `npm install --global elm-test elm-format`
- Install [elm-live](https://github.com/wking-io/elm-live) `npm install --global elm-live`
- Install [elm-mdc](https://github.com/aforemny/elm-mdc) into your local project
  - start in this project's root directory
  - `git clone https://github.com/aforemny/elm-mdc.git`
  - ```
    cd elm-mdc
    make
    cd ..
    ```

## Haskell + Yesod Setup

- Download the Haskell Stack from [here](https://tech.fpcomplete.com/haskell/get-started)
- Install yesod `stack install yesod-bin --install-ghc`
