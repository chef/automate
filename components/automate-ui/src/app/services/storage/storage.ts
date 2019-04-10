import { ChefSessionService } from '../chef-session/chef-session.service';

export abstract class Storage {

  constructor(protected chefSessionService: ChefSessionService) {}

  // store and retrieve should be implemented by the
  // child class.
  protected abstract store(key: string, value: string): string;
  protected abstract retrieve(key: string): string | null;

  public putBoolean(key: string, value: boolean): boolean {
    // Storing a '1' or '0' here to represent the boolean in
    // order to make conversion back to a boolean easier.
    const str: string = value ? '1' : '0';
    this.store(key, str);
    return value;
  }

  public getBoolean(key: string): boolean | null {
    const value = this.retrieve(key);
    return value ? Boolean(parseInt(value, 10)) : null;
  }

  // TODO: Add methods to store additional types. Number, Object, String, etc

  protected prefixKey(key: string): string {
    return `${this.chefSessionService.uuid}-${key}`;
  }

}
