import { ComponentFixture, TestBed } from '@angular/core/testing';

import { InfraTabChangeComponent } from './infra-tab-change.component';

describe('InfraTabChangeComponent', () => {
  let component: InfraTabChangeComponent;
  let fixture: ComponentFixture<InfraTabChangeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ InfraTabChangeComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(InfraTabChangeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
