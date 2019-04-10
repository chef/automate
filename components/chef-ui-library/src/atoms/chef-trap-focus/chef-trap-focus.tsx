import { Component, Element, Prop } from '@stencil/core';

/**
 * @description
 * A wrapper element that traps focus inside of the element. Very useful for accessibility purposes.
 *
 * @example
 *
 * <chef-trap-focus>
 *  <input type="text"/>
 *  <p>Some non-focus-able text</p>
 *  <p tabindex="0">Some focus-able text</p>
 *  <input type="text"/>
 *  <button>Button</button>
 * </chef-trap-focus>
 */

@Component({
  tag: 'chef-trap-focus'
})

export class ChefTrapFocus {

  @Element() el: HTMLElement;

  /**
   * Class name to omit
   */
  @Prop() trap = true;

  render() {
    return (
      <slot />
    );
  }

  componentDidLoad() {
    const tabbableChildren = this.el.querySelectorAll('input, button, select, textarea, a, *[tabindex]');
    const firstTabbableChild = tabbableChildren.item(0) as HTMLElement;
    const lastTabbableChild = tabbableChildren.item(tabbableChildren.length - 1) as HTMLElement;

    this.el.addEventListener('keydown', (event) => {
      if (this.trap === false) {
        return;
      }

      if (event.key === 'Tab') {
        if (event.shiftKey) { // shift + tab
          if (document.activeElement === firstTabbableChild) {
            lastTabbableChild.focus();
            event.preventDefault();
          }
        } else { // tab
          if (document.activeElement === lastTabbableChild) {
            firstTabbableChild.focus();
            event.preventDefault();
          }
        }
      }
    });

  }
}
