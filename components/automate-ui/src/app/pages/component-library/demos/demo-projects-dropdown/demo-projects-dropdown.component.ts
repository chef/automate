import { Component } from '@angular/core';

@Component({
  selector: 'app-demo-projects-dropdown',
  templateUrl: './demo-projects-dropdown.component.html',
  styleUrls: []
})
export class DemoProjectsDropdownComponent {
  dropdownProjects = [{
    'name': 'Project 1',
    'id': 'project-1'
  }, {
    'name': 'Project 2',
    'id': 'project-2'
  }, {
    'name': 'Project 3',
    'id': 'project-3'
  }, {
    'name': 'Project 4',
    'id': 'project-4'
  }, {
    'name': 'Project 5',
    'id': 'project-5'
  }, {
    'name':
    `Project 6 Has A Really Ridiculously Long Name and It should Stay on the same line
    Stay on the same line`,
    'id': 'project-6'
  }];


  projectsDropdownAction(projects) {
    console.log('projects:');
    console.log(projects);
  }
}
