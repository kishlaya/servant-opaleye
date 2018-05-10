# servant-opaleye

Haskell backend to support [CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete) operations using PostgreSQL

We will be using [servant](https://hackage.haskell.org/package/servant) to setup the Web API, [opaleye](https://hackage.haskell.org/package/opaleye) to handle PostgreSQL queries and [Aeson](hackage.haskell.org/package/aeson/docs/Data-Aeson.html) for parsing JSON.

## Usage

1. `git clone https://github.com/kishlaya/servant-opaleye.git`
2. `cd servant-opaleye`
3. `stack build`
4. `stack exec servant-opaleye`
