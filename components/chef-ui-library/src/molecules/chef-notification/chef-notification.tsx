import { Component, Element, Event, EventEmitter, Prop, State, h } from '@stencil/core';
import isUndefined from 'lodash/fp/isUndefined';

/**
 * @description
 * Displays a notification bar with the specified content. When the
 * notification is dismissed, either automatically or by clicking the
 * close button, a 'dismissed' event is emitted.
 *
 * @example
 * <chef-notification type="error">This is an error!</chef-notification>
 *
 * @example
 * <chef-notification type="info">This is just info.</chef-notification>
 *
 * @example
 * <chef-notification>This is also an info message.</chef-notification>
 */
@Component({
  tag: 'chef-notification',
  styleUrl: 'chef-notification.scss'
})
export class ChefNotification {

  /**
   * The type of notification to show. Can be one of 'error' or 'info'.
   */
  @Prop() type = 'info';

  /**
   * The length of time in seconds to show the message before automatically
   * being dismissed. Set to 0 to disable the timeout.
   */
  @Prop() timeout = 0;

  @Event() dismissed: EventEmitter;

  @Element() el: HTMLElement;

  @State() visible = false;

  private observer: MutationObserver;
  private timeoutRef: any;

  constructor() {
    this.observer = new MutationObserver(this.setVisibility.bind(this));
  }

  hostData() {
    return {
      class: [
        this.className(this.type),
        this.visible ? 'visible' : ''
      ]
      .filter(className => className.length > 0)
      .join(' ')
    };
  }

  componentDidLoad() {
    this.observer.observe(this.el, { childList: true });
    this.setVisibility();
  }

  componentDidUpdate() {
    this.setVisibility();
  }

  setVisibility() {
    // The close icon has 5 characters. Anything more than that
    // means that there are additional elements being rendered.
    this.visible = this.el.textContent.length > 5;
    if (this.visible && this.timeout > 0 && isUndefined(this.timeoutRef)) {
      this.timeoutRef = setTimeout(this.handleClose.bind(this), this.timeout * 1000);
    }
  }

  private className(type) {
    switch (type) {
      case 'error':
        return 'error';
      default:
        return 'info';
    }
  }

  render() {
    return [
      <slot />,
      <chef-icon onClick={this.handleClose.bind(this)}>close</chef-icon>
    ];
  }

  handleClose() {
    clearTimeout(this.timeoutRef);
    while (this.el.firstChild) {
      this.el.removeChild(this.el.firstChild);
    }
    this.dismissed.emit();
  }

}
