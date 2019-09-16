import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { CUSTOM_ELEMENTS_SCHEMA } from "@angular/core";

import { ProjectsDropdownComponent } from "./projects-dropdown.component";

describe("ProjectsDropdownComponent", () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ProjectsDropdownComponent],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsDropdownComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should create", () => {
    expect(component).toBeTruthy();
  });

  describe("#projectsArray", () => {
    it("#projectsArray returns an object naturally ordered by name", () => {
      const unsortedProjects: {
        checked: boolean;
        id: string;
        name: string;
        status: string;
        type: string;
      }[] = [
        {
          checked: false,
          id: "123abc",
          name: "123abc",
          status: "NO_RULES",
          type: "CUSTOM"
        },
        {
          checked: false,
          id: "project-6",
          name: "project-6",
          status: "NO_RULES",
          type: "CUSTOM"
        },
        {
          checked: false,
          id: "12345zero",
          name: "12345zero",
          status: "NO_RULES",
          type: "CUSTOM"
        },
        {
          checked: false,
          id: "123xyz",
          name: "123xyz",
          status: "NO_RULES",
          type: "CUSTOM"
        },
        {
          checked: false,
          id: "abc123",
          name: "abc123",
          status: "NO_RULES",
          type: "CUSTOM"
        }
      ];
      // usage and expects go here
      const dropdownComponent = new ProjectsDropdownComponent();
      dropdownComponent.projects = unsortedProjects;

      expect(dropdownComponent.projectsArray().length).toBe(5);
      expect(dropdownComponent.projectsArray()[0]).toEqual(
        jasmine.objectContaining({ name: "123abc" })
      );
      expect(dropdownComponent.projectsArray()[1]).toEqual(
        jasmine.objectContaining({ name: "123xyz" })
      );
      expect(dropdownComponent.projectsArray()[2]).toEqual(
        jasmine.objectContaining({ name: "123xyz" })
      );
      expect(dropdownComponent.projectsArray()[3]).toEqual(
        jasmine.objectContaining({ name: "123xyz" })
      );
      expect(dropdownComponent.projectsArray()[4]).toEqual(
        jasmine.objectContaining({ name: "123xyz" })
      );
    });
  });
});
