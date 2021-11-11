import { Component, Input } from '@angular/core';
import { animate, state, style, transition, trigger } from '@angular/animations';
@Component({
  selector: 'app-accordion',
  templateUrl: './accordion.component.html',
  styleUrls: ['./accordion.component.scss'],
  animations: [
    trigger('tabState', [state('default', style({
      transform: 'translateY(0)'
    })),
      state('open', style({
        bottom: 'initial',
        top: '20px'
      })),
      transition('default <=> open', animate(500))
    ])
  ]
})
export class AccordionComponent {
  public showIncludedPolicies = false;
  public activeIncludedPolicies: string;
  public policyFileDetailsLoading = true;
  public state = 'default';
  @Input() title: string;


  constructor() { }

  handlePolicyFileSelected() {
    this.state === 'default' ? this.state = 'open' : this.state = 'default';
    if (!this.showIncludedPolicies) {
      this.showIncludedPolicies = true;
      this.activeIncludedPolicies = 'autoHeight';
    } else {
      this.showIncludedPolicies = false;
      this.activeIncludedPolicies = '';
    }
  }
}
