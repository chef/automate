import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SearchBarEnvironmentsComponent } from './search-bar-environments.component';

describe('SearchBarEnvironmentsComponent', () => {
  let component: SearchBarEnvironmentsComponent;
  let fixture: ComponentFixture<SearchBarEnvironmentsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SearchBarEnvironmentsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SearchBarEnvironmentsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
