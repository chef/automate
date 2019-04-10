export class IdMapper {

  public static transform(input: string): string {
    return input.toLowerCase().replace(/[^a-z0-9]/g, '-');
  }
}
