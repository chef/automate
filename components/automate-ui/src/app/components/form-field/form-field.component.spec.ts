import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { FormFieldComponent } from './form-field.component';
import { FormControl } from '@angular/forms';
import { Component } from '@angular/core';

describe('FormFieldComponent', () => {
  let component: FormFieldComponent;
  let fixture: ComponentFixture<FormFieldComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ FormFieldComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FormFieldComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});

@Component({
  template: '<chef-form-field></chef-form-field>'
})
class TestEmptyHostComponent {
  myWatch: string;
  myControl: FormControl;
}
describe('FormFieldComponent without watched control', () => {
  let component: TestEmptyHostComponent;
  let fixture: ComponentFixture<TestEmptyHostComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [FormFieldComponent, TestEmptyHostComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestEmptyHostComponent);
    component = fixture.componentInstance;
  });

  it('can create chef-form-field without a watched control', () => {
    expect(component).toBeTruthy();
  });
});

@Component({
  template: '<chef-form-field [watch]="myControl.value" [control]="myControl"></chef-form-field>'
})
class TestHostComponent {
  myWatch: string;
  myControl: FormControl;
}

describe('FormFieldComponent with watched control', () => {
  let component: TestHostComponent;
  let fixture: ComponentFixture<TestHostComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [FormFieldComponent, TestHostComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    component.myControl = new FormControl();
    spyOn(component.myControl, 'markAsPristine');
  });

  it('can create chef-form-field with a watched control', () => {
    expect(component).toBeTruthy();
  });

  it('setting watched form control to first value marks it pristine', () => {
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(0);
    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);
  });

   it('setting watched form control to different value does not mark it pristine', () => {
    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('bar');
    fixture.detectChanges();
    // not called again, thus dirty
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);
  });

    it('setting watched form control to original value marks it pristine again', () => {
    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('bar');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(2);
  });

  it('setting watched form control to empty does not mark it pristine', () => {
    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('bar');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);
  });

  it('setting watched form control to non-empty after empty does not mark it pristine', () => {
    component.myControl.setValue('foo');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);

    component.myControl.setValue('non-empty');
    fixture.detectChanges();
    expect(component.myControl.markAsPristine).toHaveBeenCalledTimes(1);
  });

});
