import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TopErrorsComponent } from './top-errors.component';
import { By } from '@angular/platform-browser';

describe('TopErrorsComponent', () => {
  let fixture: ComponentFixture<TopErrorsComponent>;
  let component: TopErrorsComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [],
      declarations: [
        TopErrorsComponent
      ],
      providers: [],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(TopErrorsComponent);
    component = fixture.componentInstance;
  });

  it('should exist', () => {
    expect(component).toBeTruthy();
  });

  describe('information tooltip', () => {
    it('should have an element on page with matching id', () => {
      const debugElement = fixture.debugElement.query(By.css('#top-errors-info'));
      expect(debugElement).toBeTruthy();
    });

    it('should contain "for" attribute linked to matching element', () => {
      const tooltip = fixture.debugElement.query(By.css('chef-tooltip')).nativeElement;
      expect(tooltip.getAttribute('for')).toEqual('top-errors-info');
    });
  });
});
