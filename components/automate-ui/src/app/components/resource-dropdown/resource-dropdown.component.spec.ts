import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter, SimpleChange } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { using } from 'app/testing/spec-helpers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ResourceDropdownComponent, ResourceChecked } from './resource-dropdown.component';

describe('ResourceDropdownComponent', () => {
  let component: ResourceDropdownComponent;
  let fixture: ComponentFixture<ResourceDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ResourceDropdownComponent],
      imports: [ChefPipesModule, FormsModule],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
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
      const options: HTMLInputElement[] = Array.from(fixture.nativeElement.querySelectorAll('chef-checkbox'));
      expect(options.length).toEqual(component.filteredResources.length);
      options.forEach((option, index) => {
        const { name, checked } = component.filteredResources[index];
        expect(option.textContent).toEqual(name);
        expect(option.checked).toEqual(checked);
      });
    });

    it('allows customizing "none" designator', () => {
      component.noneSelectedLabel = 'zilch';
      component.ngOnChanges(
        { resources: new SimpleChange([], [], true) });
      expect(component.label).toEqual('zilch');
    });

    using([
      ['single unchecked resource',
        [
          genResource('name1', false)
        ]],
      ['two resources but none checked',
        [
          genResource('name1', false),
          genResource('name2', false)
        ]]
    ], function (description: string, resources: ResourceChecked[]) {
      it(`displays 'none' designator for ${description}`, () => {
        component.resources = resources;
        component.ngOnChanges(
          { resources: new SimpleChange([], resources, true) });
        expect(component.label).toEqual('None');
      });
    });

    using([
      ['single checked resource',
        [
          genResource('target resource', true)
        ]],
      ['two resources but just one checked',
        [
          genResource('other resource2', false),
          genResource('target resource', true)
        ]],
      ['multiple resources but just one checked in the middle',
        [
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('target resource', true),
          genResource('other resource3', false)
        ]],
      ['multiple resources but just one checked at the top',
        [
          genResource('target resource', true),
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('other resource3', false),
          genResource('other resource4', false)
        ]],
      ['multiple resources but just one checked at the bottom',
        [
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('target resource', true)
        ]]
    ], function (description: string, resources: ResourceChecked[]) {
      it(`displays resource name for ${description}`, () => {
        component.resources = resources;
        component.ngOnChanges(
          { resources: new SimpleChange([], resources, true) });
        expect(component.label).toEqual('target resource');
      });
    });

    using([
      ['some checked and some not checked', 3,
        [
          genResource('name1', true),
          genResource('name2', false),
          genResource('name3', true),
          genResource('name4', true),
          genResource('name5', false)
        ]],
      ['all checked', 4,
        [
          genResource('name1', true),
          genResource('name2', true),
          genResource('name3', true),
          genResource('name4', true)
        ]],
      ['just two and both checked', 2,
        [
          genResource('name1', true),
          genResource('name2', true)
        ]]
    ], function (description: string, count: number, resources: ResourceChecked[]) {
      it(`displays resource designator with count for multiple resources with ${description}`,
        () => {
          component.resources = resources;
          component.objectNounPlural = 'lemurs';
          component.ngOnChanges(
            { resources: new SimpleChange([], resources, true) });
          expect(component.label).toEqual(`${count} lemurs`);
        });
    });
  });

  function genResource(name: string, checked: boolean): ResourceChecked {
    return {
      id: name.replace(' ', '-'),
      name,
      checked
    };
  }
});
