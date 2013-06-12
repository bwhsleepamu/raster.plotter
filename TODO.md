## 0.1.1
- ~~Fix Issue #1 - Events spanning two days display incorrectly~~

## 0.2.0
- Allow df creation from excel file
- Add support for lab times (decimal format for now)
- Add support for 4 event types
    * Blocks
      - start time, end time
      - start time, duration
      - single event time
    * Linear
      - event time, variable level (y value)
- Visualization Enhancements:
    * options for setting the height of block events bar.
    * options for setting the position (including order) of block events bar.
    * Add x-axis labels to top of raster plot.
    * options for setting y-axis label format.
    * flip y-axis label 90 degrees.
    * options for setting height of a single day. total plot height will then be computed as (single day height * number of days)
