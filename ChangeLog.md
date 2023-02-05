## 0.2.0.2 (Not yet released)
### Fixed
- Always expand into all available space, even with no items in the grid, for consistency.

## 0.2.0.1
### Fixed
- Fix a bug where the grid did not update when the model went from empty to non-empty.

## 0.2.0.0
### Added
- Speed up large grids by only creating widgets that are visible.
- Use `Seq` rather than lists, for better performance with large grids (breaks API).
- New "resizing-cells" example to show what happens when cells resize.
### Fixed
- The footer now only takes up vertical space if there are some footer widgets defined.

## 0.1.0.0

Initial release.