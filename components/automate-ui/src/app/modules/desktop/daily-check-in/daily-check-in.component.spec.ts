import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DailyCheckInComponent } from './daily-check-in.component';
import { DebugElement } from '@angular/core';



describe('DailyCheckInComponent', () => {
  let fixture: ComponentFixture<DailyCheckInComponent>;
  let component: DailyCheckInComponent;
  let element: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [ DailyCheckInComponent ],
      providers: [],
      schemas: []
    });

    fixture = TestBed.createComponent(DailyCheckInComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('should exist', () => {
    expect(component).toBeTruthy();
  });
});
