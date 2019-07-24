import { Component, Host, h } from '@stencil/core';

/**
 * @description
 * The `<chef-table>` molecule is used to display tabular content using the following molecules:
 *
 * - [`chef-tr`](/molecules/chef-tr) - used to display a row of cells
 * - [`chef-td`](/molecules/chef-td) - used to display a cell
 * - [`chef-th`](/molecules/chef-th) - used to display a header cell
 * - [`chef-tbody`](/molecules/chef-tbody) - used to display a group of rows
 * - [`chef-thead`](/molecules/chef-thead) - used to display a group of header rows
 * - [`chef-tfoot`](/molecules/chef-tfoot) - used to display a group of footer rows
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
 *   <chef-tr>
 *     <chef-td>2</chef-td>
 *     <chef-td>node-2</chef-td>
 *     <chef-td>platform-1</chef-td>
 *     <chef-td>passed</chef-td>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-td>3</chef-td>
 *     <chef-td>node-3</chef-td>
 *     <chef-td>platform-2</chef-td>
 *     <chef-td>failed</chef-td>
 *   </chef-tr>
 * </chef-table>
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
 *     <chef-tr>
 *       <chef-td>2</chef-td>
 *       <chef-td>node-2</chef-td>
 *       <chef-td>platform-1</chef-td>
 *       <chef-td>passed</chef-td>
 *     </chef-tr>
 *     <chef-tr>
 *       <chef-td>3</chef-td>
 *       <chef-td>node-3</chef-td>
 *       <chef-td>platform-2</chef-td>
 *       <chef-td>failed</chef-td>
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
 *
 * @example
 * <chef-table>
 *   <chef-tr>
 *     <chef-th></chef-th>
 *     <chef-th>Passed</chef-th>
 *     <chef-th>Failed</chef-th>
 *     <chef-th>Skipped</chef-th>
 *     <chef-th>Total</chef-th>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-th>2018</chef-th>
 *     <chef-td>590</chef-td>
 *     <chef-td>320</chef-td>
 *     <chef-td>34</chef-td>
 *     <chef-td>944</chef-td>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-th>2017</chef-th>
 *     <chef-td>484</chef-td>
 *     <chef-td>649</chef-td>
 *     <chef-td>85</chef-td>
 *     <chef-td>1,218</chef-td>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-th>2016</chef-th>
 *     <chef-td>55</chef-td>
 *     <chef-td>194</chef-td>
 *     <chef-td>86</chef-td>
 *     <chef-td>335</chef-td>
 *   </chef-tr>
 * </chef-table>
 */
@Component({
  tag: 'chef-table',
  styleUrl: 'chef-table.scss'
})
export class ChefTable {

  render() {
    return (
      <Host role="table">
        <slot />
      </Host>
    );
  }
}
