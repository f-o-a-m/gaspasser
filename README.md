# gaspasser

Empirically test gas usages of various ways to store stuff in `mapping(string => string)`

## Quickstart

```bash
npm install               # Install dependencies
npm run compile-contracts # Compile our test contracts, generate PureScript bindings, and create a genesis block with our libraries pre-deployed.
./cb.sh                   # Fire up Cliquebait with the genesis block that was generated.
sleep 10                  # Give Cliquebait some time to generate its accounts and start.
npm run deploy-contracts  # Deploy our testing contracts to Cliquebait.
npm run collect           # Run the testing script, and generate our pretty charts.
```

The huge amount of printed errors due to overloading geth is perfectly normal! It'll keep on retrying,
being flooded with the amount of Web3 requests we make for this isn't exactly one of the design criteria
of most Web3 providers.

When it's done, just open `collected-results/results.html` in your browser of choice!

## Huge batches make node run out of heap space :(

`NODE_OPTIONS=--max-old-space-size=4096 npm run collect` ;)
