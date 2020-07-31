import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EmptyDataComponent } from './empty-data.component';
import { By } from '@angular/platform-browser';

describe('EmptyDataComponent', () => {
  let fixture: ComponentFixture<EmptyDataComponent>;
  let component: EmptyDataComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [
        EmptyDataComponent
      ],
      providers: [],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(EmptyDataComponent);
    component = fixture.componentInstance;
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });

  it('displays help message for missing data', () => {
    const message = fixture.debugElement.query(By.css('.empty-message')).nativeElement;
    expect(message.innerText).toContain('Data not found.');
  });
});
