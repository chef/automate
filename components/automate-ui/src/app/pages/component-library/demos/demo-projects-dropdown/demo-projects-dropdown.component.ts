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
    'checked': false
  }, {
    'name': 'Project 2',
    'id': 'project-2',
    'checked': false
  }, {
    'name': 'Project 3',
    'id': 'project-3',
    'checked': false
  }, {
    'name': 'Project 4',
    'id': 'project-4',
    'checked': false
  }, {
    'name': 'Project 5',
    'id': 'project-5',
    'checked': false
  }, {
    'name':
    `Project 6 Has A Really Ridiculously Long Name and It should Stay on the same line
    Stay on the same line`,
    'id': 'project-6',
    'checked': false
  }];


  onProjectChecked(project) {
    console.log('this project was checked or unchecked:');
    console.log(project);
  }
}
