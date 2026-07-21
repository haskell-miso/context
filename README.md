# 🍜 🧵 miso-context

A demo of miso's React-style, app-global [`context`](https://react.dev/learn/passing-data-deeply-with-context) feature.

A top-level component seeds the global `context` and mounts three children. Two of them opt into context updates (`mountUseContext`) and can mutate it (`modifyContext` / `putContext`); the third opts out (`mount_`) and stays stale. Modifying the context from any context-aware component re-renders all of them together.

## Source

[src/Main.hs](src/Main.hs)

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```
