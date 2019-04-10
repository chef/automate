import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClSectionComponent } from './cl-section.component';

describe('ClSectionComponent', () => {
  let component: ClSectionComponent;
  let fixture: ComponentFixture<ClSectionComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ClSectionComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClSectionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
