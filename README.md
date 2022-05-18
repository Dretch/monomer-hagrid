# Haskell Grid (HaGrid)

A datagrid for the [Monomer](https://github.com/fjvallarino/monomer) library.

# Goals
- Be reasonably configurable and inspectable.
- Be no uglier than standard Monomer widgets.
- Be performant with ten thousand rows (but not with ten million).

## To build and run examples
```bash
stack run
```

## To format the source code

```bash
ormolu --mode inplace $(find . -name '*.hs')
```