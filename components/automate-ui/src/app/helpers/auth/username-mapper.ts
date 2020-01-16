export class UsernameMapper {

  public static transform(input: string): string {
    return input.toLowerCase().replace(/[^a-z0-9_@+\.]/g, '-');
  }
}
