import { Component, h } from '@stencil/core';

/**
 * @description
 * The `<chef-tfoot>` molecule is used to display a group of footer rows within a
 * [`<chef-table>`](/molecules/chef-table).
 *
 * @example
 * <chef-table>
 *   <chef-thead>
 *     <chef-tr>
 *       <chef-th>ID</chef-th>
 *       <chef-th>Name</chef-th>
 *       <chef-th>Platform</chef-th>
 *       <chef-th>Status</chef-th>
 *     </chef-tr>
 *   </chef-thead>
 *   <chef-tbody>
 *     <chef-tr>
 *       <chef-td>1</chef-td>
 *       <chef-td>node-1</chef-td>
 *       <chef-td>platform-1</chef-td>
 *       <chef-td>passed</chef-td>
 *     </chef-tr>
 *   </chef-tbody>
 *   <chef-tfoot>
 *     <chef-tr>
 *       <chef-td>ID</chef-td>
 *       <chef-td>Name</chef-td>
 *       <chef-td>Platform</chef-td>
 *       <chef-td>Status</chef-td>
 *     </chef-tr>
 *   </chef-tfoot>
 * </chef-table>
 */
@Component({
  tag: 'chef-tfoot'
})
export class ChefTfoot {

  hostData() {
    return {
      role: 'rowgroup'
    };
  }

  render() {
    return (
      <slot />
    );
  }
}
