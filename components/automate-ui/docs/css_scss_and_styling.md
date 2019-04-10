
## CSS do

* Use Sass in our files. We have chosen to use Sass in Automate 2.0. Sass is an extension of CSS3, adding nested rules, variables, mixins, selector inheritance, and more. It's translated to well-formatted, standard CSS when the project is compiled.
* Always use an external `.scss` file for styles and do not inline or template-inline within `.ts` files. When you scaffold a component with Angular CLI it automatically includes a .scss file for you to add specific styles.
* Keep style changes in line with Automate's current initiatives:
  * Does it work on Automate's targeted browsers? Check [Can I Use](http://caniuse.com/#index)
  * Is it accessible? Make sure semantic styling is applied and double check with the [aXe-Core Plugin](https://chrome.google.com/webstore/detail/axe-coconut/iobddmbdndbbbfjopjdgadphaoihpojp) for your browser
  * Does it fail linting checks? VSCode's ESLint plugin and the other plugin's listed below will assist you in real time.
* Other items and libraries you can leverage for styling components include:
  * [Angular Material](https://material.angular.io/)
  * [Flexbox](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout/Using_CSS_flexible_boxes)
  * Small sample of reusable components currently available in the `/component` library

## CSS dont

* `!important`s should NOT be used
* comments disabling the linter should and will be reviewed with extreme skepticism
* *tl;dr do not add [encapsulation: ViewEncapsulation.None] to the @Component.import.* Turning off view Encapsulation should not be done. This will enable a component’s CSS to load globally but there is a better way to do this without slowing down our application.  If you have issues that lead you to believe this might be the only way to fix them, please track down a team member of engineering excellence and we will assist you.
* adding styles to the `app/styles.scss` or `app/styles/*` files unless they are needed for more than 5 components isn't necessary. As much as possible styling can go into the component file, or the parent of the component to style multiple children. Too much global styling will slow down browser performance dramatically and make it hard to debug issues.

## Visual Studio Code Helpful Plugins
1. SCSS IntelliSense
2. ESLint
3. Code Spell Checker
4. EditorConfig for VS Code

## Troubleshooting

### Styles are applied in the order shown. So if the same style is defined in multiple places, the last one applied (lower in this list) wins.

* Component implementation
    * Styles defined at @Component.styles (following array order)
    * Template Inline Styles
    * External styles @Component.styleUrls (following array order)
* Container
    * Inline style. Eg: <... style="">
    * ngClass and ngStyle
