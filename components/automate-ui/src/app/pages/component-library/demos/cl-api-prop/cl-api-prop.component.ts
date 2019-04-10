import { Component, Input, HostBinding } from '@angular/core';
import { isEmpty } from 'lodash';

@Component({
  selector: 'app-cl-api-prop, api-prop',
  templateUrl: './cl-api-prop.component.html',
  styleUrls: ['./cl-api-prop.component.scss']
})
export class ClApiPropComponent {
  @HostBinding('attr.role') role = 'row';

  @Input() name: string;
  @Input() type: string;
  @Input() required = false;
  @Input() default: string;

  // List of any items that represent a NON-literal;
  // CSS styling will be applied to differentiate such items.
  private meta_items_list = [
    'empty string'
  ];

  isEmpty(value): boolean {
    return isEmpty(value) || /^\s+$/.test(value) ;
  }

  isRequired(): boolean {
    // Default to false if unspecified
    return !(this.required === false);
  }

  isMeta(value): boolean {
    return this.meta_items_list.indexOf(value) >= 0;
  }

  // The commonly proposed solution to check for an empty <ngContent>...
  //    <span *ngIf="ref.childNodes.length == 0">
  // ... fails for text only content. This function corrects that inaccuracy.
  // Source: https://github.com/angular/angular/issues/12530
  isContentEmpty(element: HTMLElement): boolean {
    const nodes = element.childNodes;
    for (let i = 0; i < nodes.length; i++) {
      const node = nodes.item(i);
      if (node.nodeType !== 8 && nodes.item(i).textContent.trim().length !== 0) {
        return false;
      }
    }
    return true;
  }
}
