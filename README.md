# Monomer-Hagrid

A datagrid widget for the [Monomer](https://github.com/fjvallarino/monomer) library.

![Example hagrid widget screenshot](readme/example.png)

## Current Status
- Somewhat buggy and incomplete. Don't build your business on this.

## Features
- Vertical and horizontal scrolling.
- Resizable columns.
- Column sorting, with a customizable sort key.
- Configurable column alignment (left or right).
- Custom widgets for each cell, to override the standard label.
- Custom width/height padding for each cell, configured per column.
- Scrolling to a particular row when sent a message.
- Custom footer widgets on the bottom of each column.

## Goals
- Have complete and helpful documentation.
- Be reasonably configurable and inspectable.
- Be no uglier than standard Monomer widgets.
- Be performant with up to ten thousand rows.
- Have some tests.

## Todo List
- Improve performance with lots of rows (e.g. 1000).
- Explore providing an API where current sort/column widths/etc are stored in the parent model and updated by hagrid via a lens. This may remove some complexity around merging and the specification of initial sorts/column widths.

## API Documentation

See the generated docs on [Hackage](https://hackage.haskell.org/package/monomer-hagrid).

## To build and run examples
```bash
stack run example-basic
stack run example-big-grid
stack run example-resizing-cells
```

## Contribution Guide
- This is "free as in mattress" software!
- You are welcome to open an issue with bug reports or feature requests, but you may not get a response.
- Unsolicited pull requests will likely be ignored.

### To format the source code

```bash
# This needs at least ormolu 0.5.0.0 to avoid breaking dot-record syntax
ormolu --mode inplace $(find . -name '*.hs')
```