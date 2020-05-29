import { Component } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';
import { UserTableComponent } from './user-table.component';

const baseUrl = '/some/path';

@Component({
  selector: 'app-test-user-table-wrapper',
  template: '<app-user-table [baseUrl]="baseUrl"></app-user-table>'
})
class TestUserTableComponent {
  baseUrl = baseUrl;
}

describe('UserTableComponent', () => {
  let component: UserTableComponent;
  let fixture: ComponentFixture<TestUserTableComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        MockComponent({ selector: 'app-authorized', inputs: ['allOf'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'chef-toolbar' }),
        UserTableComponent,
        TestUserTableComponent
      ]
    })
    .compileComponents();
    fixture = TestBed.createComponent(TestUserTableComponent);
    component = fixture.debugElement.children[0].componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  describe('when {get,create}PermissionsPath are not passed', () => {
    it('should populate the permission paths using baseUrl', () => {
      component.baseUrl = baseUrl;
      fixture.detectChanges();
      expect(component.getPermissionsPath).toEqual([baseUrl, 'get']);
      expect(component.createPermissionsPath).toEqual([baseUrl, 'post']);
    });
  });

  describe('when {get,create}PermissionsPath are not passed', () => {
    it('should not populate the permission paths using baseUrl', () => {
      const getPath = ['/some/parameterized/{path}', 'get', 'my-path'];
      const postPath = ['/some/parameterized/{path}', 'post', 'my-path'];
      component.getPermissionsPath = getPath;
      component.createPermissionsPath = postPath;
      fixture.detectChanges();
      expect(component.getPermissionsPath).toEqual(getPath);
      expect(component.createPermissionsPath).toEqual(postPath);
    });
  });
});
