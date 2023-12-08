## 0.3.1.2
### Changed
- Mark as compatible with Monomer 1.6.

## 0.3.1.1
### Fixed
- Fixed a bug where row background color was determined by the original row index rather than the sorted index.

## 0.3.1.0
### Changed
- Remove ilist package dependency, since it is not in the latest Stackage LTS and is easily replaced.

## 0.3.0.1
### Fixed
- Fix a bug where header buttons were drawn outside the grid during horizontal scrolling.

## 0.3.0.0
### Changed
- Use NoFieldSelectors.

## 0.2.1.1
### Changed
- No longer build examples by default, since library users probably will not want them. 

## 0.2.1.0
### Changed
- Require the latest monomer (1.5.1.0), to get the fix for https://github.com/fjvallarino/monomer/issues/225
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