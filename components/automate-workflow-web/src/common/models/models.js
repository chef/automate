import ng from 'angular';
import Change from './change';
import Comment from './comment';
import Organization from './organization';
import Pipeline from './pipeline';
import Project from './project';
import Review from './review';
import User from './user';

export default ng
  .module('cd.common.models', [
    Change,
    Comment,
    Organization,
    Pipeline,
    Project,
    Review,
    User
  ])
  .name;
