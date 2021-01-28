import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SearchBarDataBagesComponent } from './search-bar-data-bages.component';

describe('SearchBarDataBagesComponent', () => {
  let component: SearchBarDataBagesComponent;
  let fixture: ComponentFixture<SearchBarDataBagesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SearchBarDataBagesComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SearchBarDataBagesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
