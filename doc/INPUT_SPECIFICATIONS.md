# Specifications for JSON input files

## General Options
	{
	  "title":"__plot-title__",
	  "save_path":"__folder-for-saving-raster-plots__",
	  "filename":"__file-name__.png",
	  "day_height":__height of one day, in inches, at a resolution of 80dpi__,
	  "base_block_height":__base height for block and timepoint events__,		
	}

### Explanations:
- Day Height: 
	PNG images are made at a resolution of 80dpi. The width of the images is 13 inches for double-plotted and 9 inches for single-plotted rasters. 
- Base Block Height: See [Positioning and Height](#Positioning and Height)



## Events
The "events" object holds an array of event definitions:
	{
		...
		"events":[
			{_event1_},
			{_event2_},
			...
		]
	}

The contents of these individual definitions depend on the type of event. See the desired event type's section for more information.

### Supported Event Types
- [Block Events](#Block Events)
- [Single Timepoint Events](#Single Timepoint Events)
- [Linear Events](#Linear Events)

### Block Events
Each one of these events has a defined **start time** and a defined **end time**. Optionally, each event also contains a specific **color** (in hex coding) and a **text value**. 

These events are displayed as rectangles on the timeline, stretching from start to end time. If a color is defined for each event, that is the color used for each rectangle's fill. If a text value is defined, the provided text is printed in the midpoint of the respective block. 

#### Event Definition
	{
      "name":"_event-name_",
      "type":"block",
      "position":_[event-position](#Positioning and Height)_,
      "height":_[event-height](#Positioning and Height)_,
      "color":_Hex color definition. Needed if individual events do not have assigned colors. See [Color Definitions](#Color Definitions) for more information_,
      "blocks":[
      	[
      		"YYYY-MM-DDTHH:MM:SS",
      		"YYYY-MM-DDTHH:MM:SS",
          	"#2D2D00BB",
          	"10 LUX" 
      	],
      	[
      		__start-time__, 
      		__end-time__,
      		_color(optional)_,
      		_text(optional)_
      	],
      	...
      ]
  	}

### Single Timepoint Events
Each one of these events has a defined **time**. Optionally, each event has an assigned **color**.

These events are displayed as vertical lines on the timeline at the specified time.

#### Event Definition
	{
      "name":"_event-name_",
      "type":"timepoint",
      "position":_[event-position](#Positioning and Height)_,
      "height":_[event-height](#Positioning and Height)_,
      "color":_Hex color definition. Needed if individual events do not have assigned colors. See [Color Definitions](#Color Definitions) for more information_,
      "times":[
      	[
      		"YYYY-MM-DDTHH:MM:SS",
          	"#2D2D00BB"
      	],
      	[
      		__event-time__, 
      		_color(optional)_
      	],
      	...
      ]
  	}

### Linear Events
*For now, only a single linear event is allowed per raster plot. Double-plotting multiple events might be implemented later*

Linear events are a **(time, value)** pair that show up as a linear plot on the timeline. The linear data needs to have a defined y-value range for correct raster plot creation.

#### Event Definition
	{
		"name":"__event-name__",
		"type":"linear",
		"color":_Hex color definition. See [Color Definitions](#Color Definitions) for more information_,
		"limits":[__min-y-value__,__max-y-value__],
		"points":[
			["2000-09-02T23:17:41", 2],
			[__event-time__, __y-value__],
			...
		]
	}

## Positioning and Height
The height and position of block and timepoint events is, for now, controlled by two options:
- position
- height

The y-axis of each individual day is divided into block sections of the **base_block_height**, which can be set in the [General Options](#General Options) If no **base_block_height** is defined, this height is set to the maximum height of any block or timepoint events. 

The `0` position corresponds to the section in the [0, base_block_height] range. The position numbering continues to both sides of this section, with position `-1` corresponding to the range [-base_block_height, 0], position `1` to [base_block_height, 2*base_block_height], etc.

Each event is assigned it's own section with the **position** option. The height option determines how far up the section the event will go. A height of **base_block_height** will result in the whole vertical range of the section being occupied. 

## Color Definitions
Hex strings are used as definitions for Raster Plotter colors. Two formats for these strings are allowed:

- 6 value long RGB Definition: "#000000"
- 8 value long RGB + Transparancy Definition: "#000000CC"

First two digits determine amount of *red*, in the [0, 255] range

Next two digits determine amount of *green*, in the [0, 255] range

Next two digits determine amount of *blue*, in the [0, 255] range

Last two digits determine degree of transparancy in the [0, 255] range, with 0 being completely transparent and 255 being completely opaque.

