import { Component } from '@stencil/core';

/**
 * @description
 * Belongs within <chef-layout>: Specifies the main content of a document
 *
 * @example
 * <chef-main> Main Part of Page goes here </chef-main>
 *
 */
@Component({
  tag: 'chef-main',
  styleUrl: 'chef-main.scss'
})
export class ChefMain {

  render() {
    return (
      <slot />
    );
  }

}
