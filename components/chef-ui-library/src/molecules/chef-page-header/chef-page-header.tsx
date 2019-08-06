import { Component, Element, Prop, h } from '@stencil/core';

/**
 * @description
 * Header for most pages of the Automate UI. Note that only text and inline elements should be nested
 * inside of the chef-heading and chef-subheading elements; if you want to nest block level elements
 * in the header, put them below the chef-heading and chef-subheading (as shown in the last example).
 *
 * @example
 * <chef-page-header>
 *   <chef-heading>Some Heading Text</chef-heading>
 *   <chef-subheading>Some subheading text that goes below the heading.</chef-subheading>
 * </chef-page-header>
 *
 * @example
 * <chef-page-header>
 *   <chef-heading>Heading</chef-heading>
 *   <chef-subheading>
 *    If you want to have <a href="#">custom html and whatnot</a> in your text underneath
 *    the header you can add it like this.
 *   </chef-subheading>
 * </chef-page-header>
 *
 * @example
 * <chef-page-header>
 *   <chef-heading>Heading</chef-heading>
 *   <chef-subheading>If you need to add buttons, use the header-buttons slot.</chef-subheading>
 *   <div slot="header-buttons">
 *     <chef-button primary>Do a thing</chef-button>
 *   </div>
 * </chef-page-header>
 *
 * @example
 * <style>
 *  .box-with-stuff {
 *    padding: 10px;
 *    margin-top: 10px;
 *    background: #648FFF;
 *    color: #FFF;
 *    border-radius: 4px;
 *  }
 * </style>
 *
 * <chef-page-header>
 *   <chef-heading>Heading</chef-heading>
 *   <chef-subheading>
 *     If you need to add some block level elements underneath the subheading, add it below.
 *   </chef-subheading>
 *   <div class="box-with-stuff">
 *     A box with some stuff.
 *   </div>
 * </chef-page-header>
 */

@Component({
  tag: 'chef-page-header',
  styleUrl: './chef-page-header.scss'
})
export class ChefPageHeader {
  /**
   * Toggles a white/transparent background. The background is white by default, it will
   * be transparent if you set this to 'false'.
   */
  @Prop() contrastingBackground = true;

  @Element() el: HTMLElement;

  componentDidLoad() {
    this.el.classList.add('contrasting');
  }

  render() {
    return (
    <header>
      <slot />
      <div class="header-buttons">
        <slot name="header-buttons" />
      </div>
    </header>
    );
  }

}
