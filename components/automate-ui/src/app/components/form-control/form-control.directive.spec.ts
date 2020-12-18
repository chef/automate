import { waitForAsync, ComponentFixture, fakeAsync, TestBed, tick } from '@angular/core/testing';
import { Component, DebugElement } from '@angular/core';
import { By } from '@angular/platform-browser';
import { FormControl, FormGroup, ReactiveFormsModule } from '@angular/forms';
import { using } from 'app/testing/spec-helpers';
import { FormControlDirective } from './form-control.directive';

const originalValue = 'originalValue';
const newValue = 'newValue';

@Component({
  template: '<input [formControl]="control" [resetOrigin]="origin" />'
})
class InputFormControlHostComponent {
  control: FormControl = new FormControl(originalValue);
  origin = false;
}

@Component({
  template: `
    <form [formGroup]="form">
      <input formControlName="control" [resetOrigin]="origin" />
    </form>
  `
})
class InputFormControlNameHostComponent {
  form: FormGroup = new FormGroup({
    control: new FormControl(originalValue)
  });
  origin = false;
}

@Component({
  template: `
    <select [formControl]="control" [resetOrigin]="origin">
      <option value="${originalValue}">${originalValue}</option>
      <option value="${newValue}">${newValue}</option>
    </select>
  `
})
class SelectFormControlHostComponent {
  control: FormControl = new FormControl(originalValue);
  origin = false;
}

@Component({
  template: `
    <form [formGroup]="form">
      <select formControlName="control" [resetOrigin]="origin">
        <option value="${originalValue}">${originalValue}</option>
        <option value="${newValue}">${newValue}</option>
      </select>
    </form>
  `
})
class SelectFormControlNameHostComponent {
  form: FormGroup = new FormGroup({
    control: new FormControl(originalValue)
  });
  origin = false;
}

type TestHostComponents =
  | InputFormControlHostComponent
  | InputFormControlNameHostComponent
  | SelectFormControlHostComponent
  | SelectFormControlNameHostComponent;

const scenarios = [
  [InputFormControlHostComponent, 'input', 'input'],
  [InputFormControlNameHostComponent, 'input[formcontrolname]', 'input'],
  [SelectFormControlHostComponent, 'select', 'change'],
  [SelectFormControlNameHostComponent, 'select[formcontrolname]', 'change']
];

using(scenarios, (host, selector, eventName) => {
  describe(`FormControlDirective using '${host.name}'`, () => {
    let fixture: ComponentFixture<TestHostComponents>;
    let input: DebugElement;
    let inputEl: HTMLInputElement | HTMLSelectElement;

    beforeEach(waitForAsync(() => {
      TestBed.configureTestingModule({
        imports: [
          ReactiveFormsModule
        ],
        declarations: [
          host,
          FormControlDirective
        ]
      })
        .compileComponents();
    }));

    beforeEach(fakeAsync(() => {
      fixture = TestBed.createComponent(host);
      input = fixture.debugElement.query(By.css(selector));
      inputEl = input.nativeElement;
      fixture.detectChanges();
      tick();
    }));

    describe('when initialized', () => {
      it('is marked as pristine', () => {
        expect(inputEl.value).toEqual(originalValue);
        expect(inputEl.classList.contains('ng-dirty')).toEqual(false);
        expect(inputEl.classList.contains('ng-pristine')).toEqual(true);
      });
    });

    describe('when value changes from original value', () => {
      it('is marked as dirty', fakeAsync(() => {
        inputEl.value = newValue;
        inputEl.dispatchEvent(new Event(eventName));
        fixture.detectChanges();
        tick();

        expect(inputEl.value).toEqual(newValue);
        expect(inputEl.classList.contains('ng-dirty')).toEqual(true);
        expect(inputEl.classList.contains('ng-pristine')).toEqual(false);
      }));
    });

    describe('when value changes to original value', () => {
      it('is marked as pristine', fakeAsync(() => {
        inputEl.value = newValue;
        inputEl.dispatchEvent(new Event(eventName));
        fixture.detectChanges();
        tick();

        expect(inputEl.value).toEqual(newValue);
        expect(inputEl.classList.contains('ng-dirty')).toEqual(true);
        expect(inputEl.classList.contains('ng-pristine')).toEqual(false);

        inputEl.value = originalValue;
        inputEl.dispatchEvent(new Event(eventName));
        fixture.detectChanges();
        tick();

        expect(inputEl.value).toEqual(originalValue);
        expect(inputEl.classList.contains('ng-dirty')).toEqual(false);
        expect(inputEl.classList.contains('ng-pristine')).toEqual(true);
      }));

      describe('when resetOrigin emits', () => {
        it('is marked as pristine', fakeAsync(() => {
          inputEl.value = newValue;
          inputEl.dispatchEvent(new Event(eventName));
          fixture.detectChanges();
          tick();

          expect(inputEl.value).toEqual(newValue);
          expect(inputEl.classList.contains('ng-dirty')).toEqual(true);
          expect(inputEl.classList.contains('ng-pristine')).toEqual(false);

          input.componentInstance.origin = true;
          fixture.detectChanges();
          tick();

          expect(inputEl.value).toEqual(newValue);
          expect(inputEl.classList.contains('ng-dirty')).toEqual(false);
          expect(inputEl.classList.contains('ng-pristine')).toEqual(true);
        }));
      });
    });
  });
});
