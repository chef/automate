import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateClientModalComponent } from './create-client-modal.component';

describe('CreateClientModalComponent', () => {
  let component: CreateClientModalComponent;
  let fixture: ComponentFixture<CreateClientModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateClientModalComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateClientModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
