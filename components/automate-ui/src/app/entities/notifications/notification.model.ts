export enum Type {
  error = 'error',
  info = 'info',
  /* TODO (tc) This is a stopgap because product decided it was
     too risky to ship the full experience of a license banner.
     This code will be removed first thing after ChefConf. */
  license = 'license'
}

export interface Notification {
  id: string;
  type: Type;
  message: string;
  timeout: number;
}
