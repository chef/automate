import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SsoComponent } from './sso.component';

describe('SsoComponent', () => {
  let component: SsoComponent;
  let fixture: ComponentFixture<SsoComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SsoComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SsoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
