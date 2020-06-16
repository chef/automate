import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UnknownDesktopDurationCountsComponent } from './unknown-desktop-duration-counts.component';
import { By } from '@angular/platform-browser';

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

  describe('information tooltip', () => {
    it('should have an element on page with matching id', () => {
      const debugElement = fixture.debugElement.query(By.css('#since-last-checkin-info'));
      expect(debugElement).toBeTruthy();
    });

    it('should contain "for" attribute linked to matching element', () => {
      const tooltip = fixture.debugElement.query(By.css('chef-tooltip')).nativeElement;
      expect(tooltip.getAttribute('for')).toEqual('since-last-checkin-info');
    });
  });
});
