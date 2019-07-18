import { Component, Method, Prop, h } from '@stencil/core';

/**
 * @description
 * The `<chef-expander>` molecule is used to expand/collapse content.
 *
 * @example
 * <chef-expander>
 *   <h1>Some collapsed content</h1>
 * </chef-expander>
 * <chef-expander opened>
 *   <h1>Some expanded content</h1>
 * </chef-expander>
 *
 * @example
 * <chef-button secondary id='open-btn'>Open</chef-button>
 * <chef-button secondary id='close-btn'>Close</chef-button>
 * <chef-button secondary id='toggle-btn'>Toggle</chef-button>
 * <chef-expander id='expander'>
 *   <h1>Some content</h1>
 * </chef-expander>
 *
 * <style>
 *  chef-button,
 *  chef-expander[opened] { margin: 1em; }
 * </style>
 *
 * <script>
 *   const openBtn = document.getElementById('open-btn');
 *   const closeBtn = document.getElementById('close-btn');
 *   const toggleBtn = document.getElementById('toggle-btn');
 *   const expander = document.getElementById('expander');
 *
 *   openBtn.addEventListener('click', e => expander.open());
 *   closeBtn.addEventListener('click', e => expander.close());
 *   toggleBtn.addEventListener('click', e => expander.toggle());
 * </script>
 */
@Component({
  tag: 'chef-expander',
  styleUrl: 'chef-expander.scss'
})
export class ChefExpander {

  /**
   * Indicates that the expander is opened.
   */
  @Prop({ mutable: true, reflectToAttr: true }) opened = false;

  @Method()
  open() {
    this.opened = true;
  }

  @Method()
  close() {
    this.opened = false;
  }

  @Method()
  toggle() {
    this.opened = !this.opened;
  }

  render() {
    return (
      <slot />
    );
  }
}
