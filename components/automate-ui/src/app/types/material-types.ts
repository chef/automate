export interface SelectOption {
  value: string;
  viewValue: string;
}

export type PositionOptions =
'above' | 'below' | 'left' | 'right' | 'before' | 'after';

export interface ChefKeyboardEvent extends KeyboardEvent {
  // isUserInput allows filtering to selection events and ignoring deselection events
  // when using Material Select component
  isUserInput: boolean;
}
