import { ComponentFixture, TestBed } from '@angular/core/testing';

import { InfraSearchBarComponent } from './infra-search-bar.component';

describe('InfraSearchBarComponent', () => {
  let component: InfraSearchBarComponent;
  let fixture: ComponentFixture<InfraSearchBarComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ InfraSearchBarComponent ]
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
});
