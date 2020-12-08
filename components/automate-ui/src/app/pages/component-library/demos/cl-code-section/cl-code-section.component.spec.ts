import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClCodeSectionComponent } from './cl-code-section.component';

describe('ClCodeSectionComponent', () => {
  let component: ClCodeSectionComponent;
  let fixture: ComponentFixture<ClCodeSectionComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ClCodeSectionComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClCodeSectionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
