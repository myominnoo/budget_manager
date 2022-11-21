### Control
- The control box control the individual figure height and figure columns in plots in `Model Selection`, `Home range`, `Occurrence` pages, since they are all similar, and most of them are based on same models subset.
- The slider control the Time-lag range in variogram plots. It's in Logarithmic scale, ranged from `0.001 (0.1%)` to `1 (100%)` of the total range. Some browser/platform may have compatibility issues and showing the labels wrong. Firefox and Chrome are recommended browsers.
- The units of X (Time-lag) and Y (Semi-variance) may change according to data to better represent the values.
- `Absolute` mode operate on the max Time-lag range individual in group, and all others scaled with same X, Y axes for easier comparison.
- `Relative` mode zoom every plot by fraction of their own Time-lag range. The X, Y axes are not synced.

### Pool
- You can use pool variogram to replace the individual variogram, and it will be used for modeling.

