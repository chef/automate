import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UnknownDesktopDurationCountsComponent } from './unknown-desktop-duration-counts.component';

describe('UnknownDesktopDurationCountsComponent', () => {
  let fixture: ComponentFixture<UnknownDesktopDurationCountsComponent>;
  let component: UnknownDesktopDurationCountsComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [
        UnknownDesktopDurationCountsComponent
      ],
      providers: [],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(UnknownDesktopDurationCountsComponent);
    component = fixture.componentInstance;
  });

  it('should exist', () => {
    expect(component).toBeTruthy();
  });
});
