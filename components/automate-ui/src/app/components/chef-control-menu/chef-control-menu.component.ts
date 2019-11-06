import { Component, OnInit, HostBinding } from '@angular/core';

@Component({
  selector: 'app-chef-control-menu',
  templateUrl: './chef-control-menu.component.html',
  styleUrls: ['./chef-control-menu.component.scss']
})
export class ChefControlMenuComponent implements OnInit {

  public isFocused = false;
  public isActive = false;
  public isDisabled = false;

  public selectedIndex = 0;
  public focusedIndex = 0;

  // can i use a ternarys here?
  @HostBinding('attr.role') comboBox = 'combobox';
  @HostBinding('attr.tabindex') isTabable: string = this.isDisabled === true ? '-1' : '0';
  @HostBinding('class.focused') focused: string = this.isFocused ? 'focused' : '';
  @HostBinding('class.active') active: string = this.isActive ? 'active' : '';
  @HostBinding('class.disabled') disabled: string = this.isDisabled ? 'disabled' : '';

  constructor() { }

  ngOnInit() {
  }

  handleClickActivation(event) {
    if (this.isDisabled) {
      console.log('isDisabled')
      return;
    } else if (this.isActive) {
      const option = event.target.closest('chef-option');
      console.log(event.target.closest('chef-option'));
      if (!option) {
        return;
      }
      const optionId = option.optionId;
      // this.makeSelection(findIndex(['optionId', optionId], this.options));
      console.log('isActive')
    } else {
      // this.activate(this.selectedIndex);
      console.log('Not isActive')
    }
  }

}
