import { ComponentFixture, TestBed } from '@angular/core/testing';

import { InfraSearchBarComponent } from './infra-search-bar.component';
import { MockComponent } from 'ng2-mock-component';

describe('InfraSearchBarComponent', () => {
  let component: InfraSearchBarComponent;
  let fixture: ComponentFixture<InfraSearchBarComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-icon' }),
        InfraSearchBarComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(InfraSearchBarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('When a user toggles focus', () => {
    // Toggling focus provides an active class to the form
    // which gives our form an accessibilty ring
    it('By default, the form should not be active', () => {
      expect(component.formActive).toBe(false);
    });

    it('Should activate the form when input is focused', () => {
      component.toggleFocus();
      expect(component.formActive).toBe(true);
    });
  });
});
