import { Component } from '@stencil/core';

/**
 * @description
 * Child component for chef-page-header that displays the heading. Only text and inline
 * elements should be nested in this component.
 *
 * @example
 * <chef-page-header>
 *   <chef-heading>Some Heading Text</chef-heading>
 *   <chef-subheading>Some subheading text that goes below the heading.</chef-subheading>
 * </chef-page-header>
 */

@Component({
  tag: 'chef-heading',
  styleUrl: './chef-heading.scss'
})
export class ChefHeading {
  render() {
    return (
     <h1 class="page-title"><slot/></h1>
    );
  }
}
