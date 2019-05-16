import { Component, Element, Event, EventEmitter, Listen, Prop } from '@stencil/core';

/**
 * @description
 * A wrapper element that fires an event when a user clicks outside of the wrapper. This can be used
 * within modals/dialogs for closing the element when a user clicks outside of it, for example.
 *
 * The 'omit' property allows you to prevent the 'clickOutside' event from being dispatched if a
 * user clicks on or within an element that contains the provided class name. This is useful if you
 * want to use `chef-click-outside` within a modal but you don't want the event to fire when the
 * user clicks on the button to open the modal.
 *
 * @example
 * <chef-click-outside id="wrapper" omit="omitted">
 *   <chef-markdown id="content" text="# Click outside of me."></chef-markdown>
 * </chef-click-outside>
 * <chef-button class="omitted" primary>An omitted click target</chef-button>
 * <chef-button class="omitted" primary>Another omitted click target</chef-button>
 *
 * <script>
 *   const wrapper = document.getElementById('wrapper');
 *   const content = document.getElementById('content');
 *   wrapper.addEventListener('clickOutside', () => {
 *     content.text = '# Thanks!';
 *     setTimeout(() => content.text = '# Click outside of me.', 1000);
 *   });
 * </script>
 *
 * <style>
 *   #wrapper { display: block; background: var(--chef-white); }
 *   #content { padding: 1em; border: 1px solid; border-color: var(--chef-grey); }
 *   .omitted { margin: 1em 1em 0 0; }
 * </style>
 */
@Component({
  tag: 'chef-click-outside'
})
export class ChefClickOutside {

  @Element() el: HTMLElement;

  /**
   * Expression to run when a user clicks outside of the element
   */
  @Event() clickOutside: EventEmitter;

  /**
   * Class name to omit. Any click event targets that are on or within an element that has this
   * class name will not dispatch a `clickOutside` event.
   */
  @Prop() omit = '';

  render() {
    return (
      <slot />
    );
  }

  @Listen('document:click') onDocumentClick(event) {
    const clickedInside = this.el.contains(event.target);
    const clickedOmitted = this.omit && this.omit.length && event.target.closest(`.${this.omit}`);

    if (!clickedInside && !clickedOmitted) {
      this.clickOutside.emit(null);
    }
  }
}
