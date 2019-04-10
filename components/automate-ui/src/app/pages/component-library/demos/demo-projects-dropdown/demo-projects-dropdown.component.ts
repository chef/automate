import { Component } from '@angular/core';

@Component({
  selector: 'app-demo-projects-dropdown',
  templateUrl: './demo-projects-dropdown.component.html',
  styleUrls: []
})
export class DemoProjectsDropdownComponent {
  dropdownProjects = [{
    'name': 'Project 1',
    'id': 'project-1',
    'enabled': false
  }, {
    'name': 'Project 2',
    'id': 'project-2',
    'enabled': false
  }, {
    'name': 'Project 3',
    'id': 'project-3',
    'enabled': false
  }, {
    'name': 'Project 4',
    'id': 'project-4',
    'enabled': false
  }, {
    'name': 'Project 5',
    'id': 'project-5',
    'enabled': false
  }, {
    'name':
    `Project 6 Has A Really Ridiculously Long Name and It should Stay on the same line
    Stay on the same line`,
    'id': 'project-6',
    'enabled': false
  }];


  projectsDropdownAction(projects) {
    console.log('projects:');
    console.log(projects);
  }
}
