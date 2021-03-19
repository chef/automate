import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditEnvironmentAttributeModalComponent } from './edit-environment-attribute-modal.component';

describe('EditEnvironmentAttributeModalComponent', () => {
  let component: EditEnvironmentAttributeModalComponent;
  let fixture: ComponentFixture<EditEnvironmentAttributeModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditEnvironmentAttributeModalComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditEnvironmentAttributeModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  // TODO
  xit('should create', () => {
    expect(component).toBeTruthy();
  });
});
