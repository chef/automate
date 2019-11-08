import { Component, OnInit, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-option-new',
  templateUrl: './chef-option.component.html',
  styleUrls: ['./chef-option.component.scss']
})
export class ChefOptionComponent implements OnInit {

  public selected = false;
  public optionId: string;
  public id = 0;

  @HostBinding('attr.role') role = 'option';
  @HostBinding('attr.id') get assignId() { return this.optionId; }
  @HostBinding('class.selected') get isSelected() { return this.selected; }

  constructor() { 
    this.id = this.id + 1;
    this.optionId = this.optionId || `chef-option${this.id}`;
  }

  ngOnInit() {
  }

}
