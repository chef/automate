import { Component } from '@stencil/core';

/**
 * @description
 * Belongs within <chef-layout>: Specifies the  header for the application, that contains the main navigation.
 *
 * @example
 * <chef-header>Main Navigation Goes HERE</chef-header>
 *
 */
@Component({
  tag: 'chef-header',
  styleUrl: 'chef-header.scss'
})
export class ChefHeader {

  render() {
    return (
      <slot />
    );
  }

}
