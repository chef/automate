import { Component, EventEmitter, Input, Output, OnChanges, OnInit, SimpleChanges } from '@angular/core';

import { ChefSorters } from 'app/helpers/auth/sorter';

export interface ResourceChecked {
  id: string;
  name: string;
  checked: boolean;
}

export interface ResourceCheckedMap {
  [id: string]: ResourceChecked;
}

@Component({
  selector: 'app-resource-dropdown',
  templateUrl: './resource-dropdown.component.html',
  styleUrls: ['./resource-dropdown.component.scss']
})

export class ResourceDropdownComponent implements OnInit, OnChanges {

  // The map of resources to operate on.
  @Input() resources: ResourceChecked[] = [];

  // (Optional) Used to re-synchronize summary label if the set of checked items has changed.
  // This input is needed only when re-displaying the resource dropdown
  // for *additional* resources, as with the create-object-modal-component.
  // Consumers that use it only for a single resource, e.g., team-details.component, should omit.
  @Input() resourcesUpdated: EventEmitter<boolean>;

  // (Optional) Label to use with no resources selected; defaulted if not provided.
  @Input() noneSelectedLabel = 'None';

  // Plural display name of resource (e.g. "policies", "projects").
  @Input() objectNounPlural = 'MISSING REQUIRED PARAMETER';

  // Emits checked set of resource IDs upon completion.
  @Output() onDropdownClosing = new EventEmitter<string[]>();

  // filteredResources is merely a container to hold the resources
  // that can be altered
  public filteredResources: ResourceChecked[] = [];
  public active = false;
  public label = this.noneSelectedLabel;
  public filterValue = '';

  ngOnInit(): void {
    if (this.resourcesUpdated) { // an optional setting
      this.resourcesUpdated.subscribe(() => {
        this.updateLabel();
      });
    }
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.resources) {
      this.updateLabel();
      if (changes.resources.firstChange) { // only update on initialization/first change
        this.filteredResources = this.resourcesInOrder;
      }
    }
  }

  get resourcesInOrder(): ResourceChecked[] {
    return ChefSorters.naturalSort(this.resources, 'name');
  }

  toggleDropdown(event: MouseEvent): void {
    event.stopPropagation();
    if (this.disabled) {
      return;
    }
    if (!this.active) { // opening
      this.filterValue = '';
      this.filteredResources = this.resourcesInOrder;
      this.active = true;
    } else { // closing
      this.closeDropdown();
    }
  }

  resourceChecked(checked: boolean, resource: ResourceChecked): void {
    resource.checked = checked;
    this.updateLabel();
  }

  closeDropdown(): void {
    if (this.active) {
      this.active = false;
      this.onDropdownClosing.emit(
        this.resources.filter(r => r.checked).map(r => r.id));
    }
  }

  handleFilterKeyUp(): void {
    this.filteredResources = this.resourcesInOrder.filter(resource =>
      resource.id.toLowerCase().indexOf(this.filterValue.toLowerCase()) > -1
    );
  }

  moveFocus(event: KeyboardEvent): void {
    event.preventDefault();
    let nextElement: HTMLElement;

    const targetElement = <Element>event.target;
    if (event.key === 'ArrowUp') {
      nextElement = <HTMLElement>targetElement.previousElementSibling;
    } else if (event.key === 'ArrowDown') {
      nextElement = <HTMLElement>targetElement.nextElementSibling;
    }

    if (nextElement == null) {
      return;
    } else {
      nextElement.focus();
    }
  }

  private updateLabel(): void {
    const checkedResources = this.resources.filter(p => p.checked);
    this.label = checkedResources.length === 0 ? this.noneSelectedLabel
      : checkedResources.length === 1 ? checkedResources[0].name
        : `${checkedResources.length} ${this.objectNounPlural}`;
  }

  get disabled(): boolean {
    return this.resources.length === 0;
  }
}
