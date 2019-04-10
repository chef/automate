import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UserTableComponent } from './user-table.component';
import { Component, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';

const baseUrl = '/some/path';

@Component({
  selector: 'app-test-user-table-wrapper',
  template: '<app-user-table [baseUrl]="baseUrl"></app-user-table>'
})
class TestUserTableWrapperComponent {
  baseUrl = baseUrl;
}

describe('UserTableComponent', () => {
  let component: UserTableComponent;
  let fixture: ComponentFixture<TestUserTableWrapperComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        UserTableComponent,
        TestUserTableWrapperComponent
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
    fixture = TestBed.createComponent(TestUserTableWrapperComponent);
    component = fixture.debugElement.children[0].componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  describe('when a baseUrl is passed', () => {
    it('should populate the permission paths', () => {
      fixture.detectChanges();
      expect(component.getPermissionsPath).toEqual([baseUrl, 'get']);
      expect(component.createPermissionsPath).toEqual([baseUrl, 'post']);
    });
  });

});
