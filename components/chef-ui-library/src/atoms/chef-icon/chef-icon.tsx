import { Component } from '@stencil/core';

/**
 * @description
 * Display a material icon.
 *
 * @example
 * <chef-icon style='font-size: 5em'>accessibility</chef-icon>
 *
 * @example
 * <chef-icon style='font-size: 5em; color: red;'>alarm</chef-icon>
 */
@Component({
  tag: 'chef-icon',
  styleUrl: 'chef-icon.scss'
})
export class ChefIcon {

  render() {
    return (
      <slot />
    );
  }

}
