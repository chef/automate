import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClApiComponent } from './cl-api.component';

describe('ClApiComponent', () => {
  let component: ClApiComponent;
  let fixture: ComponentFixture<ClApiComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ClApiComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClApiComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
