import { Component } from '@stencil/core';

/**
 * @description
 * Child component for chef-page-header that displays the sub-heading (the text below the heading).
 * Only text and inline elements should be nested inside of this element.
 *
 * @example
 * <chef-page-header>
 *   <chef-heading>Some Heading Text</chef-heading>
 *   <chef-subheading>Some subheading text that goes below the heading.</chef-subheading>
 * </chef-page-header>
 */

@Component({
  tag: 'chef-subheading',
  styleUrl: './chef-subheading.scss'
})
export class ChefSubheading {
  render() {
    return (
      <p class="page-subtitle"><slot/></p>
    );
  }
}
