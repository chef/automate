import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import { ComplianceReportingSidebarComponent } from './compliance-reporting-sidebar.component';

describe('ComplianceReportingSidebarComponent', () => {
  let component: ComplianceReportingSidebarComponent;
  let fixture: ComponentFixture<ComplianceReportingSidebarComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        ComplianceReportingSidebarComponent
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ComplianceReportingSidebarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });
});
