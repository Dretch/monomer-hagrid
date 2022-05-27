# Monomer-Hagrid

A datagrid widget for the [Monomer](https://github.com/fjvallarino/monomer) library.

## Current Status
- Buggy and incomplete. Just a toy.

## Goals
- Have complete and helpful documentation.
- Be reasonably configurable and inspectable.
- Be no uglier than standard Monomer widgets.
- Be performant with ten thousand rows (but not with ten million).
- Have some tests.

## To build and run examples
```bash
stack run
```

## Contribution Guide
- This is "free as in mattress" software!
- You are welcome to open an issue with comments and feature requests, but you may not get a response.
- Unsolicited pull requests will likely be ignored.

### To format the source code

```bash
ormolu --mode inplace $(find . -name '*.hs')
```