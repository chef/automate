import { Component, Event, EventEmitter, Host, Method, Prop, h } from '@stencil/core';

/**
 * @description
 * The `<chef-sort-toggle>` atom is used to display a sort button.
 *
 * @example
 * <chef-sort-toggle order='none'></chef-sort-toggle>
 * <chef-sort-toggle order='asc'></chef-sort-toggle>
 * <chef-sort-toggle order='desc'></chef-sort-toggle>
 * <chef-sort-toggle></chef-sort-toggle>
 *
 * @example
 * <chef-table class='table'>
 *   <chef-tr>
 *     <chef-th>
 *       Name
 *       <chef-sort-toggle sort="name"></chef-sort-toggle>
 *     </chef-th>
 *     <chef-th>
 *       Date Created
 *       <chef-sort-toggle sort="date" order='asc'></chef-sort-toggle>
 *     </chef-th>
 *     <chef-th>
 *       Status
 *       <chef-sort-toggle sort="status" order='desc'></chef-sort-toggle>
 *     </chef-th>
 *   </chef-tr>
 * </chef-table>
 * <chef-snippet class='snippet' lang='js'></chef-snippet>
 *
 * <style>
 *   .table { margin: 1em; }
 * </style>
 *
 * <script>
 *   const table = document.querySelector('.table');
 *   const snippet = document.querySelector('.snippet');
 *   table.addEventListener('sort-toggled', event => {
 *     snippet.code = JSON.stringify(event.detail, null, 2);
 *   });
 * </script>
 */
@Component({
  tag: 'chef-sort-toggle',
  styleUrl: 'chef-sort-toggle.scss'
})
export class ChefSortToggle {

  /**
   * Indicates whether sort button is displayed as `asc` ↑, `desc` ↓, or `none` ↕.
   */
  @Prop({ mutable: true, reflectToAttr: true }) order: 'asc' | 'desc' | 'none' = 'none';

  /**
   * Optionally assign a column sort name.
   */
  @Prop() sort = '';

  /**
   * `sort-toggled` event is emitted whenever the `order` prop is changed.
   *
   * Detail: `{ order: 'asc' | 'desc' | 'none', sort: string }`
   */
  @Event({ eventName: 'sort-toggled', bubbles: true }) sortToggled: EventEmitter;

  /**
   * Toggles order `prop` between `asc` ↑, `desc` ↓, or `none` ↕.
   */
  @Method()
  async toggle() {
    switch (this.order) {
      case 'none':
        this.order = 'asc';
        break;
      case 'asc':
        this.order = 'desc';
        break;
      case 'desc':
      default:
        this.order = 'none';
    }

    this.sortToggled.emit({
      order: this.order,
      sort: this.sort
    });
  }

  render() {
    return (
      <Host role="button">
        <chef-button tertiary onClick={this.onClick.bind(this)}>
          <chef-icon class="asc-icon">arrow_drop_up</chef-icon>
          <chef-icon class="desc-icon">arrow_drop_down</chef-icon>
        </chef-button>
      </Host>
    );
  }

  private onClick() {
    this.toggle();
  }
}
