import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { InsightAttributesDropdownComponent } from './insight-attributes-dropdown.component';
import { DesktopColumnName, DesktopColumnLabel } from 'app/entities/desktop/desktop.model';

const testOptions = [
  { label: DesktopColumnLabel.Platform, name: DesktopColumnName.Platform, checked: false },
  { label: DesktopColumnLabel.Domain, name: DesktopColumnName.Domain, checked: false },
  { label: DesktopColumnLabel.MacAddress, name: DesktopColumnName.MacAddress, checked: false },
  { label: DesktopColumnLabel.Status, name: DesktopColumnName.Status, checked: false },
  { label: DesktopColumnLabel.Hostname, name: DesktopColumnName.Hostname, checked: false },
  { label: DesktopColumnLabel.Tag, name: DesktopColumnName.Tag, checked: false }
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
      component.ngOnChanges({ options: {
        previousValue: [],
        currentValue: testOptions,
        firstChange: true,
        isFirstChange: () => true
      }});
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

  describe('when making selections', () => {
    beforeEach(() => {
      component.options = testOptions;
      component.ngOnChanges({ options: {
        previousValue: [],
        currentValue: testOptions,
        firstChange: true,
        isFirstChange: () => true
      }});
      fixture.detectChanges();
    });

    it('marks the selected option as checked', () => {
      component.handleOptionChange({ detail: true }, 0);
      component.handleOptionChange({ detail: true }, 1);
      component.handleOptionChange({ detail: true }, 4);

      expect(component.activeOptions.length).toBe(3);
      expect(component.activeOptions[0].name).toEqual(testOptions[0].name);
      expect(component.activeOptions[1].name).toEqual(testOptions[1].name);
      expect(component.activeOptions[2].name).toEqual(testOptions[4].name);
    });

    it('allows a maximum of 5 selected options', () => {
      expect(component.maxOptionsActive).toBeFalsy();

      component.handleOptionChange({ detail: true }, 0);
      component.handleOptionChange({ detail: true }, 1);
      component.handleOptionChange({ detail: true }, 2);
      component.handleOptionChange({ detail: true }, 3);
      component.handleOptionChange({ detail: true }, 4);
      fixture.detectChanges();

      expect(component.maxOptionsActive).toBeTruthy();
    });

    it('enables the submit button when filters are different than previous selection', () => {
      const submitButton = fixture.debugElement.nativeElement.querySelector('chef-button[primary]');
      const PLATFORM_FILTER_INDEX = 0;
      const DOMAIN_FILTER_INDEX = 4;

      expect(submitButton.disabled).toBeTruthy();

      component.handleOptionChange({ detail: true }, PLATFORM_FILTER_INDEX);
      fixture.detectChanges();
      expect(submitButton.disabled).toBeFalsy();

      component.handleOptionChange({ detail: true }, DOMAIN_FILTER_INDEX);
      fixture.detectChanges();
      expect(submitButton.disabled).toBeFalsy();

      component.handleOptionChange({ detail: false }, PLATFORM_FILTER_INDEX);
      component.handleOptionChange({ detail: false }, DOMAIN_FILTER_INDEX);
      fixture.detectChanges();
      expect(submitButton.disabled).toBeTruthy();
    });
  });
});
