import { MockComponent } from 'ng2-mock-component';

/**
 * Centralized mock components to prevent Angular component ID collisions.
 * Each mock component should only be defined once and reused across test files.
 */// Modal and Container Components
export const MockChefModal = MockComponent({
  selector: 'chef-modal',
  inputs: ['visible', 'locked', 'label'],
  outputs: ['closeModal']
});
export const MockChefPage = MockComponent({ selector: 'chef-page' });

// Button and Input Components
export const MockChefButton = MockComponent({ selector: 'chef-button', inputs: ['disabled', 'routerLink'] });
export const MockChefCheckbox = MockComponent({
  selector: 'chef-checkbox',
  inputs: ['checked', 'indeterminate', 'disabled'],
  outputs: ['change']
});
export const MockChefRadio = MockComponent({ selector: 'chef-radio', inputs: ['resetOrigin'] });
export const MockChefInput = MockComponent({ selector: 'chef-input' });
export const MockChefFormField = MockComponent({ selector: 'chef-form-field' });

// Typography Components
export const MockChefHeading = MockComponent({ selector: 'chef-heading' });
export const MockChefSubheading = MockComponent({ selector: 'chef-subheading' });

// Layout Components
export const MockChefPageHeader = MockComponent({ selector: 'chef-page-header' });
export const MockChefToolbar = MockComponent({ selector: 'chef-toolbar' });

// Table Components
export const MockChefTable = MockComponent({ selector: 'chef-table' });
export const MockChefThead = MockComponent({ selector: 'chef-thead' });
export const MockChefTbody = MockComponent({ selector: 'chef-tbody' });
export const MockChefTr = MockComponent({ selector: 'chef-tr' });
export const MockChefTh = MockComponent({ selector: 'chef-th' });
export const MockChefTd = MockComponent({ selector: 'chef-td' });

// Navigation Components
export const MockChefBreadcrumb = MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] });
export const MockChefBreadcrumbs = MockComponent({ selector: 'chef-breadcrumbs' });
export const MockChefTabSelector = MockComponent({
  selector: 'chef-tab-selector',
  inputs: ['value', 'routerLink', 'fragment']
});

// UI Elements
export const MockChefIcon = MockComponent({ selector: 'chef-icon' });
export const MockChefLoadingSpinner = MockComponent({ selector: 'chef-loading-spinner' });
export const MockChefError = MockComponent({ selector: 'chef-error' });
export const MockChefOption = MockComponent({ selector: 'chef-option' });
export const MockChefTooltip = MockComponent({ selector: 'chef-tooltip' });
export const MockChefDropdown = MockComponent({ selector: 'chef-dropdown' });
export const MockChefAlert = MockComponent({ selector: 'chef-alert' });
export const MockChefTrapFocus = MockComponent({ selector: 'chef-trap-focus' });
export const MockChefSelect = MockComponent({ selector: 'chef-select' });
export const MockChefClickOutside = MockComponent({ selector: 'chef-click-outside', outputs: ['clickOutside'] });

// Specialized Components
export const MockChefSnippet = MockComponent({ selector: 'chef-snippet', inputs: ['code'] });
export const MockChefClipboard = MockComponent({ selector: 'chef-clipboard', inputs: ['value'] });
export const MockChefSidebarEntry = MockComponent({ selector: 'chef-sidebar-entry' });
export const MockChefGuitarStringCollection = MockComponent({
  selector: 'chef-guitar-string-collection',
  inputs: ['height']
});

// App Components
export const MockAppResourceDropdown = MockComponent({
  selector: 'app-resource-dropdown',
  inputs: ['resources', 'resourcesUpdated', 'objectNounPlural']
});
export const MockAppProjectsDropdown = MockComponent({
  selector: 'app-projects-dropdown',
  inputs: ['projects', 'projectsUpdated', 'checkedProjectIDs']
});

/**
 * Common mock component arrays for easy import in test files
 */

// Table-related components
export const MockTableComponents = [
  MockChefTable,
  MockChefThead,
  MockChefTbody,
  MockChefTr,
  MockChefTh,
  MockChefTd
];

// Form-related components
export const MockFormComponents = [
  MockChefButton,
  MockChefCheckbox,
  MockChefRadio,
  MockChefInput,
  MockChefFormField,
  MockChefError
];

// Layout components
export const MockLayoutComponents = [
  MockChefPageHeader,
  MockChefHeading,
  MockChefSubheading,
  MockChefToolbar,
  MockChefLoadingSpinner
];

// Navigation components
export const MockNavigationComponents = [
  MockChefBreadcrumb,
  MockChefBreadcrumbs,
  MockChefTabSelector
];

// All common components
export const MockCommonComponents = [
  MockChefModal,
  MockChefPage,
  MockChefIcon,
  MockChefOption,
  MockChefSnippet,
  MockChefClipboard,
  MockChefTooltip,
  MockChefDropdown,
  MockChefAlert,
  MockChefTrapFocus,
  MockChefSelect,
  MockChefClickOutside,
  ...MockFormComponents,
  ...MockTableComponents,
  ...MockLayoutComponents,
  ...MockNavigationComponents
];
