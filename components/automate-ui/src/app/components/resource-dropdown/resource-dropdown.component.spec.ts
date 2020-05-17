import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ResourceDropdownComponent } from './resource-dropdown.component';

describe('ResourceDropdownComponent', () => {
  let component: ResourceDropdownComponent;
  let fixture: ComponentFixture<ResourceDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ResourceDropdownComponent ],
      imports: [ ChefPipesModule, FormsModule ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ResourceDropdownComponent);
    component = fixture.componentInstance;
    component.resourcesUpdated = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('dropdown', () => {
    beforeEach(() => {
      component.dropdownState = 'open';
      component.filteredResources = [
        {
          name: 'Project 1',
          id: 'project-1',
          checked: false
        },
        {
          name: 'Project 2',
          id: 'project-2',
          checked: true
        }
      ];
      fixture.detectChanges();
    });

    it('displays a list of checkbox options', () => {
      const options = Array.from(fixture.nativeElement.querySelectorAll('chef-checkbox'));
      expect(options.length).toEqual(component.filteredResources.length);
      options.forEach((option: HTMLInputElement, index: number) => {
        const { name, checked } = component.filteredResources[index];
        expect(option.textContent).toEqual(name);
        expect(option.checked).toEqual(checked);
      });
    });

  });

});
