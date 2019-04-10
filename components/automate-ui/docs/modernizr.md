# Modernizr Usage

We use [Modernizr](https://modernizr.com/) to help with fallbacks for better browser support.
Check out the website for more documentation.

We use a custom build of Modernizr, so select the link in the
`/a2/components/automate-ui/src/modernizr.js` file to read more about what our build currently
supports.

To show/hide elements using CSS classes, add a class that matches the class that Modernizr adds
to the `<html>` element with `m--` as the prefix. For example, if you wanted to use the
`<canvas>` element but you wanted to provide an `<img>` fallback, you could do that with CSS
classes like this:

HTML Code:
```
<canvas class="m--canvas" id="someID" width="200" height="200">
</canvas>

<img class="m--no-canvas" src="some/path.png"/>
```

If you are adding something to the Modernizr build, you will also need to update the global
styles file (`/a2/components/automate-ui/src/styles.scss`) with a pattern like this:

```
html.canvas {
  .m--no-canvas {
    display: none;
  }
}

html.no-canvas {
  .m--canvas {
    display: none;
  }

  .m--no-canvas {
    display: block;
  }
}
```
