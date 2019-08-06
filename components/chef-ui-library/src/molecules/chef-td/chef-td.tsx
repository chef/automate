import { Component, Host, h } from '@stencil/core';

/**
 * @description
 * The `<chef-td>` molecule is used to display a cell within a
 * [`<chef-table>`](/molecules/chef-table).
 *
 * @example
 * <chef-table>
 *   <chef-tr>
 *     <chef-th>ID</chef-th>
 *     <chef-th>Name</chef-th>
 *     <chef-th>Platform</chef-th>
 *     <chef-th>Status</chef-th>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-td>1</chef-td>
 *     <chef-td>node-1</chef-td>
 *     <chef-td>platform-1</chef-td>
 *     <chef-td>passed</chef-td>
 *   </chef-tr>
 * </chef-table>
 */
@Component({
  tag: 'chef-td'
})
export class ChefTd {

  render() {
    return (
      <Host role="cell">
        <slot />
      </Host>
    );
  }
}
