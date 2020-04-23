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

  // Setting disabled to true means the dropdown will be unusable and will have a grey background
  @Input() disabled = false;

  // Used to re-synchronize summary label if the set of checked items has changed.
  // This optional input is needed only when re-displaying the project dropdown
  // for *additional* resources, as with the create-object-modal-component.
  // Other consumers, e.g. team-details.component use it only for a single resource.
  @Input() resourcesUpdated: EventEmitter<boolean>;

  // Label to use when none are selected
  @Input() noneSelectedLabel = 'None';

  // plural display name of resource
  @Input() objectNounPlural = 'MISSING REQUIRED PARAMETER';

  // Emits a project that changed as a result of a check or uncheck.
  @Output() onResourceChecked = new EventEmitter<ResourceChecked>();

  // filteredProjects is merely a container to hold the projectsArray
  // that can be altered
  public filteredProjects: ResourceChecked[] = [];
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
        this.filteredProjects = this.projectsArray;
      }
    }
  }

  get projectsArray(): ResourceChecked[] {
    return ChefSorters.naturalSort(Object.values(this.resources), 'name');
  }

  toggleDropdown(event: MouseEvent): void {
    event.stopPropagation();
    if (this.disabled) {
      return;
    }
    if (!this.active) {
      this.filterValue = '';
      this.filteredProjects = this.projectsArray;
    }

    this.active = !this.active;
  }

  projectChecked(checked: boolean, project: ResourceChecked): void {
    project.checked = checked;
    this.updateLabel();
    this.onResourceChecked.emit(project);
  }

  closeDropdown(): void {
    if (this.active) {
      this.active = false;
    }
  }

  handleFilterKeyUp(): void {
    this.filteredProjects = this.filterProjects(this.filterValue);
  }

  filterProjects(value: string): ResourceChecked[]  {
    return this.projectsArray.filter(project =>
      project.id.toLowerCase().indexOf(value.toLowerCase()) > -1
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
    const checkedProjects = Object.values(this.resources).filter(p => p.checked);
    this.label = checkedProjects.length === 0 ? this.noneSelectedLabel
      : checkedProjects.length === 1 ? checkedProjects[0].name
        : `${checkedProjects.length} ${this.objectNounPlural}`;
  }
}
