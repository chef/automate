import { Component, Host, Prop, h } from '@stencil/core';

/**
 * @description
 * Description
 *
 * @example
 * <chef-alert type='success'>It's a success!</chef-alert>
 *
 * @description
 * Description
 *
 * @example
 * <chef-alert type='error'>Oh, no!</chef-alert>
 */
@Component({
  tag: 'chef-alert',
  styleUrl: './chef-alert.scss'
})
export class ChefAlert {
  /**
   * The alert type (either 'success' or 'error')
   */
  @Prop() type: string;

  render() {
    return (
      <Host role="alert">
        <chef-icon aria-hidden="true">{ this.alertIcon(this.type) }</chef-icon>
        <slot />
      </Host>
    );
  }

  private alertIcon(type: string) {
    switch (type) {
      case 'success': {
        return 'check_circle';
      }
      case 'error': {
        return 'report_problem';
      }
    }
  }

}
