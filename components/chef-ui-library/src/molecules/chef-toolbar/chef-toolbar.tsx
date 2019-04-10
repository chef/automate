import {
    Component
} from '@stencil/core';

/**
 * @description
 * A container for buttons for above tables
 *
 * @example
 *
 * <chef-toolbar>
 *  <chef-button>A button</chef-button>
 * </chef-toolbar>
 */
@Component({
    tag: 'chef-toolbar',
    styleUrl: 'chef-toolbar.scss'
})
export class ChefToolbar {
  render() {
    return (
      <div>
        <slot />
      </div>
    );
  }
}
