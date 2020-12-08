import { CUSTOM_ELEMENTS_SCHEMA, EventEmitter, SimpleChange } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { using } from 'app/testing/spec-helpers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import {
  ResourceDropdownComponent, ResourceChecked, ResourceCheckedSection
} from './resource-dropdown.component';

describe('ResourceDropdownComponent', () => {
  let component: ResourceDropdownComponent;
  let fixture: ComponentFixture<ResourceDropdownComponent>;

  beforeEach(waitForAsync(() => {
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

  it('renders resources WITHOUT sorting ', () => { // thus allowing custom ordering
    const resources = [genResourceList(
      genResource('zz', false),
      genResource('cc', true),
      genResource('aa', false),
      genResource('bb', true)
    )];
    component.objectNounPlural = 'policies';

    toggleDropdown(); // open
    changeResources([], resources, true);

    expect(component.filteredResources[0].itemList.map(r => r.name))
      .toEqual(['zz', 'cc', 'aa', 'bb']);
  });

  describe('user scenarios', () => {
    const resources = [genResourceList( // 3 checked items
      genResource('name1', true),
      genResource('name2', false),
      genResource('other_name3', true),
      genResource('name4', true),
      genResource('other_name5', false)
    )];

    const resourcesNew = [genResourceList( // 2 checked items
      genResource('name1', true),
      genResource('name4', true),
      genResource('other_name5', false)
    )];

    it('retains checked items after closing then re-opening dropdown', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      toggleDropdown(); // close
      expect(component.dropdownState).toEqual('closed');

      toggleDropdown(); // ... and reopen
      expect(component.dropdownState).toEqual('open');
      expect(component.label).toEqual('4 lemurs');
    });

    it('retains checked items after closing, updating resources, then re-opening dropdown', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      toggleDropdown(); // close
      expect(component.dropdownState).toEqual('closed');

      changeResources(resources, resourcesNew, false); // <----------Update resources

      toggleDropdown(); // ... and reopen
      expect(component.dropdownState).toEqual('open');
      expect(component.label).toEqual('4 lemurs');
    });

    it('clears checked items after force resetting', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      toggleDropdown(); // close
      expect(component.dropdownState).toEqual('closed');

      component.resourcesUpdated.emit(true); // force update here

      toggleDropdown(); // ... and reopen
      expect(component.dropdownState).toEqual('open');
      expect(component.label).toEqual('3 lemurs');
    });

    it('clears checked items after updating resources then force resetting', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      toggleDropdown(); // close
      expect(component.dropdownState).toEqual('closed');

      changeResources(resources, resourcesNew, false); // <----------Update resources

      component.resourcesUpdated.emit(true); // force update here

      toggleDropdown(); // ... and reopen
      expect(component.dropdownState).toEqual('open');
      expect(component.label).toEqual('2 lemurs');
    });

    it('applies filter but leaves label unchanged', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      // apply filter
      component.filterValue = 'other';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(component.filteredResources[0].itemList[0].name).toEqual('other_name3');
      expect(component.filteredResources[0].itemList[0].checked).toEqual(true);
      expect(component.filteredResources[0].itemList[1].name).toEqual('other_name5');
      expect(component.filteredResources[0].itemList[1].checked).toEqual(false);
    });

    it('applies filter and updates resources but leaves label unchanged', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      // apply filter
      component.filterValue = 'other';
      component.handleFilterKeyUp();

      changeResources(resources, resourcesNew, false); // <----------Update resources

      expect(component.label).toEqual('4 lemurs');
      expect(component.filteredResources[0].itemList[0].name).toEqual('other_name3');
      expect(component.filteredResources[0].itemList[0].checked).toEqual(true);
      expect(component.filteredResources[0].itemList[1].name).toEqual('other_name5');
      expect(component.filteredResources[0].itemList[1].checked).toEqual(false);
    });

    it('retains checked items after filter applied', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      const target = component.filteredResources[0].itemList[1];
      expect(target.checked).toEqual(false);
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);

      // apply filter
      component.filterValue = 'other';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);

      // clear filter
      component.filterValue = '';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);
    });

    it('retains checked items after filter applied and resources updated', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      const target = component.filteredResources[0].itemList[1];
      expect(target.checked).toEqual(false);
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);

      // apply filter
      component.filterValue = 'other';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);

      changeResources(resources, resourcesNew, false); // <----------Update resources

      // clear filter
      component.filterValue = '';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(target.checked).toEqual(true);
    });

    it('clears filter upon re-opening', () => {
      specifyLemursAndOpenDropdown();

      // make a change
      component.resourceChecked(true, component.filteredResources[0].itemList[1]);
      expect(component.label).toEqual('4 lemurs');

      // apply filter
      component.filterValue = 'other';
      component.handleFilterKeyUp();
      expect(component.label).toEqual('4 lemurs');
      expect(component.filteredResources[0].itemList.length).toEqual(2);

      toggleDropdown(); // close
      expect(component.dropdownState).toEqual('closed');

      toggleDropdown(); // re-open
      expect(component.filteredResources[0].itemList.length).toEqual(5);
    });

    function specifyLemursAndOpenDropdown() {
      expect(component.dropdownState).toEqual('closed');

      component.objectNounPlural = 'lemurs';
      changeResources([], resources, true);
      toggleDropdown();

      expect(component.filteredResources).toEqual(resources);
      expect(component.label).toEqual('3 lemurs');
      expect(component.dropdownState).toEqual('open');
    }
  });

  describe('label', () => {
    it('allows customizing "none" designator', () => {
      component.noneSelectedLabel = 'zilch';
      changeResources([], [], true);
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
        changeResources([], resources, true);
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
        changeResources([], resources, true);
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
          component.objectNounPlural = 'lemurs';
          changeResources([], resources, true);
          expect(component.label).toEqual(`${count} lemurs`);
        });
    });
  });

  function toggleDropdown() {
    // The event from an actual click on the dropdown fires *both* of these in the order given.
    component.toggleDropdown();
    component.handleClickOutside();
  }

  function changeResources(
    original: ResourceCheckedSection[], changed: ResourceCheckedSection[], firstChange) {
    // Programmatic change to an @Input does *not* trigger ngOnChanges like in the actual view,
    // so have to do that explicitly
    component.resources = changed;
    component.ngOnChanges({ resources: new SimpleChange(original, changed, firstChange) });
  }

  function genResourceList(...resources: ResourceChecked[]): ResourceCheckedSection {
    // Though title is technically optional, we need to supply it here so we can compare
    // filteredResources to resources in one step in specifyLemursAndOpenDropdown().
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
