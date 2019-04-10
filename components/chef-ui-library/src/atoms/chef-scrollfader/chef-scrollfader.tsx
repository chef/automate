import { Component, Element } from '@stencil/core';
import delay from 'lodash/fp/delay';

/**
 * @description
 * Scrollfader behavior. The element contained within the scrollfader will remain invisible until the page is scrolled to the scrollfader. The element will then fade in. This is generally used for elements that will have a fixed position on the page, otherwise they would never be visible.
 *
 * Note: IE11 Support is dependent on a polyfill.
 *
 * @example
 * <div style='height: 2000px'>
 *   <h1>Scroll to see the scrollfader in action</h1>
 *   <chef-scrollfader>
 *     <h2 style='position: fixed;'>I should fade in when the document is scrolled</h2>
 *   </chef-scrollfader>
 * </div>
 */
@Component({
  tag: 'chef-scrollfader',
  styleUrl: 'chef-scrollfader.scss'
})
export class ChefScrollfader {

  @Element() el: HTMLElement;

  render() {
    return [
      <div class="sentinel"></div>,
      <slot />
    ];
  }

  componentDidLoad() {
    const observer = new IntersectionObserver((sentinels) => {
      const [sentinel] = sentinels;
      if (!sentinel.isIntersecting && sentinel.boundingClientRect.top <= 0) {
        this.el.classList.add('visible');
      } else {
        this.el.classList.remove('visible');
      }
    });

    // This isn't ideal, but when loading in angular, elements are hidden
    // until they all load and then displayed. Since this component uses
    // an animation to fade in, it flashes for a second on load and fades
    // out. The delay just prevents the IntersectionObserver from binding
    // until hopefully after angular has finished loading everything.
    // TODO: Find a better way to handle this.
    delay(2000, () => {
      observer.observe(this.el.querySelector('.sentinel'));
    });
  }
}
