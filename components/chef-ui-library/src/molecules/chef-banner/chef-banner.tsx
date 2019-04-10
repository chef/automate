import {
    Component,
    Event,
    EventEmitter,
    Prop
} from '@stencil/core';

/**
 * @description
 * Displays a header bar with the specified content and a button to take corrective action (if passed).
 *
 * @example
 * <chef-banner type="warn">This is a warning without a button!</chef-banner>
 *
 * @example
 * <chef-banner type="warn" button-text="Click me">
 *    This is a warning with a button!&nbsp<a href="https://www.chef.io/contact-us/" target="_blank">Contact support for help</a>.
 * </chef-banner>
 *
 * @example
 * <chef-banner type="info" button-text="Click me">This is just info.</chef-banner>
 *
 * @example
 * <chef-banner>This is also an info message.</chef-banner>
 */
@Component({
    tag: 'chef-banner',
    styleUrl: 'chef-banner.scss'
})
export class ChefBanner {

  /**
   * The type of header to show. Can be one of 'warn' or 'info'.
   */
  @Prop() type = 'info';

  /**
   * The text of the chef-button. If nothing is passed, there will not be a button.
   */
  @Prop() buttonText: string;

  @Event() buttonClicked: EventEmitter;

  hostData() {
    return {
      class: this.type
    };
  }

  render() {
    let html = [
      <slot />
    ];
    if (this.buttonText) {
      html = [
        ...html,
        <chef-button onClick={this.handleClick.bind(this)}>{this.buttonText}</chef-button>,
      ];
    }
    if (this.getIconString()) {
      html = [
        <chef-icon>{this.getIconString()}</chef-icon>,
        ...html
      ];
    }
    return html;
  }

  private handleClick() {
    this.buttonClicked.emit();
  }

  private getIconString() {
    switch (this.type) {
      case 'warn':
        return 'warning';
      default:
        return '';
    }
  }
}
