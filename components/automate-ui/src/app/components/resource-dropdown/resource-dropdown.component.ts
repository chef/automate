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

  // The map of ResourceChecked by id. Any checked changes propagated via onResourceChecked.
  // Updates should be applied to parent component state.
  @Input() resources: ResourceCheckedMap = {};

  // Used to re-synchronize summary label if the set of checked items has changed.
  // This optional input is needed only when re-displaying the resource dropdown
  // for *additional* resources, as with the create-object-modal-component.
  // Other consumers, e.g. team-details.component use it only for a single resource.
  @Input() resourcesUpdated: EventEmitter<boolean>;

  // Label to use when none are selected.
  @Input() noneSelectedLabel = 'None';

  // Plural display name of resource.
  @Input() objectNounPlural = 'MISSING REQUIRED PARAMETER';

  // Emits a resource that changed as a result of a check or uncheck.
  @Output() onResourceChecked = new EventEmitter<ResourceChecked>();

  // filteredResources is merely a container to hold the resourcesArray
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
    // TODO convert to self-contained modal so this only fires when component is visible.
    if (changes.resources) {
      this.updateLabel();
      if (changes.resources.firstChange) { // only update on initialization/first change
        this.filteredResources = this.resourcesArray;
      }
    }
  }

  get resourcesArray(): ResourceChecked[] {
    return ChefSorters.naturalSort(Object.values(this.resources), 'name');
  }

  toggleDropdown(event: MouseEvent): void {
    event.stopPropagation();
    if (this.disabled) {
      return;
    }
    if (!this.active) {
      this.filterValue = '';
      this.filteredResources = this.resourcesArray;
    }

    this.active = !this.active;
  }

  resourceChecked(checked: boolean, resource: ResourceChecked): void {
    resource.checked = checked;
    this.updateLabel();
    this.onResourceChecked.emit(resource);
  }

  closeDropdown(): void {
    if (this.active) {
      this.active = false;
    }
  }

  handleFilterKeyUp(): void {
    this.filteredResources = this.resourcesArray.filter(resource =>
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
    const checkedResources = Object.values(this.resources).filter(p => p.checked);
    this.label = checkedResources.length === 0 ? this.noneSelectedLabel
      : checkedResources.length === 1 ? checkedResources[0].name
        : `${checkedResources.length} ${this.objectNounPlural}`;
  }

  get disabled(): boolean {
    return Object.values(this.resources).length === 0;
  }
}
