import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ResetUserKeyComponent } from './reset-user-key.component';

describe('ResetUserKeyComponent', () => {
  let component: ResetUserKeyComponent;
  let fixture: ComponentFixture<ResetUserKeyComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ResetUserKeyComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ResetUserKeyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
