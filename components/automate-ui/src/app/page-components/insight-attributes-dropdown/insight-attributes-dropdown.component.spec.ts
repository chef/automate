import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { InsightAttributesDropdownComponent } from './insight-attributes-dropdown.component';

import { FilterName } from './insight-attributes-dropdown.model';

const testOptions = [
  { name: FilterName.PlatformVersion, id: 'platform_version_filter' },
  { name: FilterName.Domain, id: 'domain_filter' },
  { name: FilterName.MacAddress, id: 'mac_address_filter' },
  { name: FilterName.CloudProvider, id: 'cloud_provider_filter' },
  { name: FilterName.Hostname, id: 'hostname_filter' },
  { name: FilterName.Tag, id: 'tag_filter' }
];

describe('OverviewTrendComponent', () => {
  let fixture: ComponentFixture<InsightAttributesDropdownComponent>;
  let component: InsightAttributesDropdownComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
      ],
      declarations: [
        InsightAttributesDropdownComponent
      ],
      providers: [
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
    .compileComponents();

    fixture = TestBed.createComponent(InsightAttributesDropdownComponent);
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('by default', () => {
    beforeEach(() => {
      component.options = testOptions;
      fixture.detectChanges();
    });

    it('displays all the options provided as buttons', () => {
      const buttonsArray = fixture.debugElement.queryAll(By.css('.filter-button'));

      expect(buttonsArray.length).toBe(component.options.length);
    });

    it('the update button is disabled', () => {
      const updateButton = fixture.debugElement.nativeElement.querySelector('chef-button[primary]');

      expect(updateButton.disabled).toBeTruthy();
    });
  });

  describe('when making selections to filter', () => {
    beforeEach(() => {
      component.options = testOptions;
      fixture.detectChanges();
    });

    it('adds the selected button id to the selectedOptions array', () => {
      const list = fixture.debugElement.nativeElement.querySelectorAll('.filter-button');
      list[0].click();
      list[1].click();
      list[4].click();

      expect(component.selectedOptions.length).toBe(3);
      expect(component.selectedOptions).toContain('platform_version_filter');
      expect(component.selectedOptions).toContain('domain_filter');
      expect(component.selectedOptions).toContain('hostname_filter');
      expect(component.selectedOptions).not.toContain('mac_address_filter');
      expect(component.selectedOptions).not.toContain('cloud_provider_filter');
    });

    it('allows a maximum of 5 selected options', () => {
      const list = fixture.debugElement.nativeElement.querySelectorAll('.filter-button');
      expect(list.length).toBeGreaterThan(5);

      list.forEach(option => option.click());

      expect(component.selectedOptions.length).toBe(5);
    });

    it('enables the submit button when filters are different than previous selection', () => {
      component.selectedOptions = ['platform_version_filter', 'domain_filter'];
      component.lastSelectedOptions = ['platform_version_filter', 'domain_filter'];
      const submitButton = fixture.debugElement.nativeElement.querySelector('chef-button[primary]');
      const list = fixture.debugElement.nativeElement.querySelectorAll('.filter-button');
      const PLATFORM_FILTER_INDEX = 0;
      const DOMAIN_FILTER_INDEX = 4;

      expect(submitButton.disabled).toBeTruthy();

      list[PLATFORM_FILTER_INDEX].click();
      fixture.detectChanges();
      expect(submitButton.disabled).toBeFalsy();

      list[DOMAIN_FILTER_INDEX].click();
      fixture.detectChanges();
      expect(submitButton.disabled).toBeFalsy();

      list[PLATFORM_FILTER_INDEX].click();
      list[DOMAIN_FILTER_INDEX].click();
      fixture.detectChanges();
      expect(submitButton.disabled).toBeTruthy();
    });

    it('updates selected class and aria-pressed value', () => {
      let allSelected = fixture.debugElement.nativeElement
        .querySelectorAll('.filter-button.selected');
      let allPressed = fixture.debugElement.nativeElement
        .querySelectorAll('[aria-pressed="true"]');

      expect(allSelected.length).toBe(0);
      expect(allPressed.length).toBe(0);

      const list = fixture.debugElement.nativeElement.querySelectorAll('.filter-button');
      list[0].click();
      list[1].click();
      list[4].click();
      fixture.detectChanges();

      allSelected = fixture.debugElement.nativeElement
        .querySelectorAll('.filter-button.selected');
      allPressed = fixture.debugElement.nativeElement
        .querySelectorAll('[aria-pressed="true"]');

      expect(allSelected.length).toBe(3);
      expect(allPressed.length).toBe(3);
    });
  });


});
