import { Component } from '@stencil/core';

/**
 * @description
 * Belongs within <chef-layout>: Defines content aside from the page content ex: sidebar
 *
 * @example
 * <chef-aside>Sidebar Markup Goes Here</chef-aside>
 *
 */
@Component({
  tag: 'chef-aside',
  styleUrl: 'chef-aside.scss'
})
export class ChefAside {

  render() {
    return (
      <slot />
    );
  }

}
