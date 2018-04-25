# gaspasser

Empirically test gas usages of various ways to store stuff in mapping(string => string)

## Quickstart

```bash
npm install
npm run compile-contracts
npm run deploy-contracts
npm run collect
```

The metric fuckton of errors due to overloading geth is perfectly normal! It keeps on retrying!

Then open `collected-results/results.html` in your browser of choice!

## Huge batches make node run out of heap space :(

`NODE_OPTIONS=--max-old-space-size=4096 npm run collect` ;)
