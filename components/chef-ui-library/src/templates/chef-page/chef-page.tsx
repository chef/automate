import { 
  Component,
  Event,
  EventEmitter,
  Prop
} from '@stencil/core';

/**
 * @description
 * Simple atom to wrap content in chef styled "page". Provides a slot named 'label' to provide
 * a label for the page. Labels can be one or multiple elements and can be styled as necessary.
 *
 * @example
 * <chef-page 
 *  class="example-page-width"
 *  heading="Add Members to Administrator"
 *  subheading="Automate only displays local users and local teams. For other types of members add a member expression."
 *  secondary-btn-text="Add Member Expression"
 *  confirm-btn-text="Add Member"
 *  disableConfirm="true"
 *  cancel-btn-text="Cancel"
 * >
 *  <chef-table>
 *    <chef-thead>
*       <chef-tr>
*         <chef-th class="checkbox-row"></chef-th>
*         <chef-th>ID</chef-th>
*         <chef-th>Type</chef-th>
*       </chef-tr>
 *    </chef-thead>
 *    <chef-tbody>
 *      <chef-tr >
*        <chef-td>
*         <chef-checkbox></chef-checkbox>
*        </chef-td>
*        <chef-td>1</chef-td>
*        <chef-td>apple</chef-td>
 *      </chef-tr>
 *    </chef-tbody>
 *  </chef-table>
 * </chef-page>
 */
@Component({
  tag: 'chef-page',
  styleUrl: './chef-page.scss'
})
export class ChefPage {

  /**
   * Text for page heading
   */
  @Prop({ reflectToAttr: true }) heading: string;

  /**
   * Text for page subheading
   */
  @Prop({ reflectToAttr: true }) subheading: string;

  /**
   * Text for error message
   */
  @Prop({ reflectToAttr: true }) errorMessage: string;

   /**
   * Text for secondary button
   */
  @Prop({ reflectToAttr: true }) secondaryBtnText: string;

  /**
   * Text for confirm button
   */
  @Prop({ reflectToAttr: true }) confirmBtnText: string = 'Confirm';

  /**
   * Text for cancel button
   */
  @Prop({ reflectToAttr: true }) cancelBtnText: string = 'Cancel';

  /**
   * Indicate confirm is loading
   */
  @Prop({ reflectToAttr: true }) confirmLoading: boolean = false;

  /**
   * Disable confirm buttom
   */
  @Prop({ reflectToAttr: true }) disableConfirm: boolean = true;

  @Event() secondaryConfirm: EventEmitter;
  @Event() confirm: EventEmitter;
  @Event() close: EventEmitter;

  render() {
    return [
      <div id="page-header" class="flex">

        <div id="header-description" class="flex-left">
          <chef-page-header>
            <chef-heading>{this.heading}</chef-heading>
            <chef-subheading>{this.subheading}</chef-subheading>
          </chef-page-header>
        </div>

        <div id="close-button" class="flex-right">
          <chef-button 
            secondary
            class="close-button"
            aria-label="Close"
            onClick={this.handleClose.bind(this)}>
            <chef-icon aria-hidden="true">close</chef-icon>
          </chef-button>
        </div>

      </div>,

      <div id="page-container">
        <slot />
      </div>,

      <div id="page-footer">

        {this.secondaryBtnText
          ? <chef-button
              secondary
              onClick={this.handleSecondaryConfirm.bind(this)}>
              {this.secondaryBtnText}
            </chef-button>
          : ''
        }

        <div id="right-buttons">

          {(this.disableConfirm || this.confirmLoading)
            ? <chef-button primary disabled>
                {this.getLoading()}
                <ng-container>{this.confirmBtnText}</ng-container>
              </chef-button>
            : <chef-button primary onClick={this.handleConfirm.bind(this)}>
                <ng-container>{this.confirmBtnText}</ng-container>
              </chef-button>
          }

          {this.errorMessage
            ? <chef-error>{this.errorMessage}</chef-error>
            : ''
          }

          <chef-button
            tertiary
            onClick={this.handleClose.bind(this)}>
            {this.cancelBtnText}
          </chef-button>

        </div>

      </div>
    ];
  }

  private getLoading() {
    return this.confirmLoading 
      ? <chef-loading-spinner></chef-loading-spinner>
      : '';
  }

  private handleSecondaryConfirm() {
    this.secondaryConfirm.emit();
  }

  private handleConfirm() {
    this.confirm.emit();
  }

  private handleClose() {
    this.close.emit();
  }
}
