export enum Type {
  error = 'error',
  info = 'info',
  license = 'license'
}

export interface Notification {
  id: string;
  type: Type;
  message: string;
  timeout: number;
}
