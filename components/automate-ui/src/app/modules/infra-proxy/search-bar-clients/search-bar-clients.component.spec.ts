import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SearchBarClientsComponent } from './search-bar-clients.component';

describe('SearchBarClientsComponent', () => {
  let component: SearchBarClientsComponent;
  let fixture: ComponentFixture<SearchBarClientsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SearchBarClientsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SearchBarClientsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
