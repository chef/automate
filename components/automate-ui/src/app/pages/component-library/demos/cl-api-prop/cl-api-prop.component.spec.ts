import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClApiPropComponent } from './cl-api-prop.component';
import { using } from 'app/testing/spec-helpers';

describe('ClApiPropComponent', () => {
  let component: ClApiPropComponent;
  let fixture: ComponentFixture<ClApiPropComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ClApiPropComponent]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClApiPropComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  using([
    ['true'],
    [true],
    [undefined]
  ], function (setting: any) {
    it(`isRequired returns TRUE when required is set to "${setting}"`,
      () => {
        component.required = setting;
        expect(component.isRequired()).toBe(true);
      });
  });

  it('isRequired returns FALSE when required is unspecified', () => {
    expect(component.isRequired()).toBe(false);
  });

  using([
    ['empty string', true],
    ['', false],
    [undefined, false],
    ['anything else', false]
  ], function (setting: string, expected: boolean) {
    it(`isMeta returns ${expected} when default is "${setting}"`, () => {
      expect(component.isMeta(setting)).toBe(expected);

    });
  });

  using([
    ['', true],
    [' ', true],
    [undefined, true],
    ['anything else', false]
  ], function (setting: string, expected: boolean) {
    it(`isEmpty returns ${expected} when value is "${setting}"`, () => {
      expect(component.isEmpty(setting)).toBe(expected);
    });
  });

});
