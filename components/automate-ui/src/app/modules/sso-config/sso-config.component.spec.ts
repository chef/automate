import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SsoConfigComponent } from './sso-config.component';

describe('SsoConfigComponent', () => {
  let component: SsoConfigComponent;
  let fixture: ComponentFixture<SsoConfigComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SsoConfigComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SsoConfigComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
