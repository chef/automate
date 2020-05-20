import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { InsightAttributesDropdownComponent } from './insight-attributes-dropdown.component';

describe('OverviewTrendComponent', () => {
  let fixture: ComponentFixture<InsightAttributesDropdownComponent>;
  let component: InsightAttributesDropdownComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
      ],
      declarations: [
        InsightAttributesDropdownComponent
      ],
      providers: [
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    })
    .compileComponents();

    fixture = TestBed.createComponent(InsightAttributesDropdownComponent);
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

});
