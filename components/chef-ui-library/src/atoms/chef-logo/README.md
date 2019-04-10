Handles rendering various logos, ranging from our own to partners and other
vendors.

## Syntax

```html
<chef-logo company="Chef Software"></chef-logo>
```

If you forget to specify a company, you will get a helpful 'Unknown' image.

If you specify a company that does not exist, you will get a helpful 'Unknown' image.

## Supported Logos

Put any of these names in the `company` property, and marvel at the good
times:

* Chef Software: Your place of employment.
* Slack: Chatty goodtimes.

## Styling

You can style the logos with:

```css
chef-logo[company="Chef Software"] img {
  width: 50px;
}
```

## Adding Logos

Put your image in `src/assets/logos`, then add your company to the
`companyData` map in `chef-logo.tsx`.
