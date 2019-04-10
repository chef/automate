import ng from 'angular';
import FlashService from './flash_service';
import FlashDirective from './flash_directive';
 
export default ng
  .module('cd.common.ui.flash', [
    FlashService,
    FlashDirective
  ])
  .name;
