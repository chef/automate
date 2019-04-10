import { Component, Prop } from '@stencil/core';

/**
 * @description
 * Generic dropdown atom. This is a basic atom to be used for constructing
 * things like drop down menus and select boxes. This atom makes no assumptions
 * about how it's content is styled. It's up to the developer to style content
 * appropriately.
 *
 * @example
 * <chef-button id='drop-toggle' primary>Click Me!</chef-button>
 * <chef-dropdown id='drop'>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 *   <p>Some content to go in my dropdown</p>
 * </chef-dropdown>
 *
 * <script>
 *   const toggle = document.getElementById('drop-toggle');
 *   const dropdown = document.getElementById('drop');
 *   toggle.addEventListener('click', e => dropdown.visible = !dropdown.visible);
 * </script>
 */
@Component({
  tag: 'chef-dropdown',
  styleUrl: './chef-dropdown.scss'
})
export class ChefDropdown {

  @Prop({ reflectToAttr: true }) visible = false;

  render() {
    return (
      <slot />
    );
  }

}
