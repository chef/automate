import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter, SimpleChange } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { using } from 'app/testing/spec-helpers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import {
  ResourceDropdownComponent, ResourceChecked, ResourceCheckedSection
} from './resource-dropdown.component';

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

    it('displays a list of checkbox options', () => {
      component.filteredResources = [
        genResourceList(
          genResource('project 1', false),
          genResource('project 2', true),
          genResource('project 3', true)
        )
      ];
      fixture.detectChanges();

      const options: HTMLInputElement[] = Array.from(fixture.nativeElement.querySelectorAll('chef-checkbox'));
      expect(options.length).toEqual(component.filteredResources[0].itemList.length);
      options.forEach((option, index) => {
        const { name, checked } = component.filteredResources[0].itemList[index];
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
        [genResourceList(
          genResource('name1', false)
        )]],
      ['two resources but none checked',
        [genResourceList(
          genResource('name1', false),
          genResource('name2', false)
        )]]
    ], function (description: string, resources: ResourceCheckedSection[]) {
      it(`displays 'none' designator for ${description}`, () => {
        component.resources = resources;
        component.ngOnChanges(
          { resources: new SimpleChange([], resources, true) });
        expect(component.label).toEqual('None');
      });
    });

    using([
      ['single checked resource',
        [genResourceList(
          genResource('target resource', true)
        )]],
      ['two resources but just one checked',
        [genResourceList(
          genResource('other resource2', false),
          genResource('target resource', true)
        )]],
      ['multiple resources but just one checked in the middle',
        [genResourceList(
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('target resource', true),
          genResource('other resource3', false)
        )]],
      ['multiple resources but just one checked at the top',
        [genResourceList(
          genResource('target resource', true),
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('other resource3', false),
          genResource('other resource4', false)
        )]],
      ['multiple resources but just one checked at the bottom',
        [genResourceList(
          genResource('other resource1', false),
          genResource('other resource2', false),
          genResource('target resource', true)
        )]]
    ], function (description: string, resources: ResourceCheckedSection[]) {
      it(`displays resource name for ${description}`, () => {
        component.resources = resources;
        component.ngOnChanges(
          { resources: new SimpleChange([], resources, true) });
        expect(component.label).toEqual('target resource');
      });
    });

    using([
      ['some checked and some not checked', 3,
        [genResourceList(
          genResource('name1', true),
          genResource('name2', false),
          genResource('name3', true),
          genResource('name4', true),
          genResource('name5', false)
        )]],
      ['all checked', 4,
        [genResourceList(
          genResource('name1', true),
          genResource('name2', true),
          genResource('name3', true),
          genResource('name4', true)
        )]],
      ['just two and both checked', 2,
        [genResourceList(
          genResource('name1', true),
          genResource('name2', true)
        )]]
    ], function (description: string, count: number, resources: ResourceCheckedSection[]) {
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

  function genResourceList(...resources: ResourceChecked[]): ResourceCheckedSection {
    return {
      title: 'any',
      itemList: resources
    };
  }

  function genResource(name: string, checked: boolean): ResourceChecked {
    return {
      id: name.replace(' ', '-'),
      name,
      checked
    };
  }
});
