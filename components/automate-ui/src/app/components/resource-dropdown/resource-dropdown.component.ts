import { Component, EventEmitter, Input, Output, OnChanges, OnInit, SimpleChanges } from '@angular/core';
import { cloneDeep } from 'lodash/fp';


export interface ResourceChecked {
  id: string;
  name: string;
  checked: boolean;
}

export interface ResourceCheckedSection {
  title?: string;
  itemList: ResourceChecked[];
}

@Component({
  selector: 'app-resource-dropdown',
  templateUrl: './resource-dropdown.component.html',
  styleUrls: ['./resource-dropdown.component.scss']
})

export class ResourceDropdownComponent implements OnInit, OnChanges {

  // The map of resources to operate on.
  @Input() resources: ResourceCheckedSection[] = [];

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

  // Provides a level of indirection so that we do not lose *unsaved* checked items:
  // (a) while dropdown is open
  // (b) when dropdown is closed then re-opened
  public snapshotResources: ResourceCheckedSection[] = [];

  // Transitory subset of snapshotResources for display based on user's entered filter.
  public filteredResources: ResourceCheckedSection[] = [];

  public dropdownState: 'closed' | 'opening' | 'open' = 'closed';
  public label = this.noneSelectedLabel;
  public filterValue = '';
  public disabled = false;

  ngOnInit(): void {
    // an optional setting allowing caller to force reset to initial state
    if (this.resourcesUpdated) {
      this.resourcesUpdated.subscribe(() => {
        this.snapshotResources = cloneDeep(this.resources);
        this.resetFilteredResources();
        this.updateLabel();
      });
    }
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.resources) {
      if (changes.resources.firstChange) { // only update on initialization/first change
        this.snapshotResources = cloneDeep(this.resources);
        this.resetFilteredResources();
      } else {
      }
      this.updateLabel();
      this.disabled = this.isDisabled();
    }
  }

  toggleDropdown(): void {
    if (this.disabled) {
      return;
    }
    if (this.dropdownState === 'closed') { // opening
      this.filterValue = '';
      this.resetFilteredResources();
      // we cannot go directly to 'open' because handleClickOutside,
      // firing next on the same event that arrived here, would then immediately close it.
      this.dropdownState = 'opening';
    } else { // closing
      this.closeDropdown();
    }
  }

  resourceChecked(checked: boolean, resource: ResourceChecked): void {
    resource.checked = checked;
    this.updateLabel();
  }

  handleClickOutside(): void {
    // Arriving here, the main goal is to close this dropdown if it is open.
    if (this.dropdownState === 'open') {
      this.closeDropdown();
    }
    // Now that we have checked the above,
    // it is safe to complete opening if the click occurred when the dropdown was closed.
    if (this.dropdownState === 'opening') {
      this.dropdownState = 'open';
    }
  }

  closeDropdown(): void {
    this.dropdownState = 'closed';
    const flattenedIDs = [].concat(
      ...this.snapshotResources.map(
        resource => resource.itemList.filter(r => r.checked).map(r => r.id)));
    this.onDropdownClosing.emit(flattenedIDs);
  }

  handleFilterKeyUp(): void {
    for (let i = 0; i < this.filteredResources.length; i++) {
      this.filteredResources[i].itemList =
        this.snapshotResources[i].itemList.filter(r =>
          r.name.toLowerCase().indexOf(this.filterValue.toLowerCase()) > -1);
    }
  }

  private resetFilteredResources(): void {

    // Not deep and not shallow!
    // Need the individual resources to point to the same objects
    // so that when one is checked, it is checked in BOTH structures.
    this.filteredResources = [].concat(
      ...this.snapshotResources.map(section =>
        ({
          title: section.title,
          itemList: section.itemList
        })));
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
    const checkedResources = this.allCheckedResourceNames;
    this.label = checkedResources.length === 0 ? this.noneSelectedLabel
      : checkedResources.length === 1 ? checkedResources[0]
        : `${checkedResources.length} ${this.objectNounPlural}`;
  }

  // Context is a *closed* dropdown, so use the raw resources, not the last snapshot
  private isDisabled(): boolean {
    return this.resources.length === 0 || this.allResourcesCount === 0;
  }

  private get allResourcesCount(): number {
    return this.resources.reduce(
      (sum, group) => sum + group.itemList.length, 0);
  }

  private get allCheckedResourceNames(): string[] {
    return [].concat(
      ...this.snapshotResources.map(
        resource => resource.itemList.filter(r => r.checked).map(r => r.name)));
  }

  get allFilteredResourcesCount(): number {
    return this.filteredResources.reduce(
      (sum, group) => sum + group.itemList.length, 0);
  }

}
